# Stocks Checker: agent guide

Stocks Checker is a Scala 3.10.0 service on Java 27 and sbt 2.0.11. It collects Finnhub profiles, listings and financials, and TwelveData prices. Cats Effect, FS2, Http4s, Sttp 4, Tapir, Circe, PureConfig and Mongo4Cats form the application stack.

## Build and verification

Select Java 27 with `JAVA_HOME` and `PATH` before running sbt.

| Task | Command |
| :--- | :--- |
| Compile | `sbt compile` |
| Affected tests | `sbt test` |
| Full regression, including unchanged suites | `sbt "core/testOnly *"` |
| One suite | `sbt "core/testOnly *SecurityServiceSpec"` |
| Format / check | `sbt scalafmtAll` / `sbt scalafmtCheck` |
| Run | `sbt "core/run"` |
| Build local image | `sbt "core/Docker/publishLocal"` |
| Regenerate CI | `sbt githubWorkflowGenerate` |

Always check formatting before committing. CI runs the full regression command; sbt 2's plain `test` selects affected suites. Repository suites start one embedded mongod per suite and isolate tests with fresh databases. Unit tests use ScalaTest and Cats Effect test helpers.

## Architecture and ownership

```text
Controllers -> Services -> Repositories -> Domain
                       -> MarketDataClient -> ProviderAccess
                       -> ActionDispatcher -> JobRepository
ActionExecutor -> ActionHandler -> Services
CommandScheduler -> CommandRepository, JobRepository
```

Services never depend on other services. Each repository has one owning service. Cross-aggregate reads belong in aggregate queries in the requesting repository; writes go through internal actions to the owning service. The composition root wires these dependencies. Keep effects inside `F`/`IO`, and keep investment scoring, selection and evaluation decisions pure under `domain.investment`.

Build Mongo pipelines with `Aggregate` methods (`lookup`, `project`, `set`, `facet`, etc.), never `new Aggregate` with raw stages. The project uses the current persistence schema, without startup migrations or legacy decoders.

| Service | Owned repositories |
| :--- | :--- |
| `SecurityService` | `SecurityRepository` |
| `CompanyProfileService` | `CompanyProfileRepository` |
| `PriceService` | `PriceAnalyticsRepository`, `PriceHistoryRepository` |
| `FinancialMetricsService` | `FinancialMetricsRepository`, `FinancialHistoryRepository` |
| `StockService` | `StockRepository` |
| `CommandService` | `CommandRepository` |
| `JobService` | `JobRepository` (diagnostic reads; dispatcher, executor and scheduler own job lifecycle operations) |
| `DataQualityService` | `DataQualityRepository` |
| `InvestmentRankingService` | `InvestmentRankingRepository` |
| `InvestmentScreenService` | `InvestmentScreenRepository` |
| `InvestmentEvaluationService` | `InvestmentEvaluationRepository` |

| Requesting repository | Cross-aggregate reads |
| :--- | :--- |
| `SecurityRepository` | Profiles for ordinary security reads; profile-fetch selection only needs active, listed securities. |
| `CompanyProfileRepository` | Active, listed securities before sorting and limiting. |
| `PriceAnalyticsRepository` | Active, listed securities, including after an in-flight refresh recreates purged analytics. |
| `StockRepository` | Securities, required profiles, analytics and financial metrics through `StockAggregates`. Activation checks only need a security ID. |
| `InvestmentRankingRepository` | Stock aggregates plus financial history. Screen references govern release of pinned rankings. |
| `InvestmentScreenRepository` | Ranking headers and separate bounded input queries; cohorts identify affected screens during purge. |
| `InvestmentEvaluationRepository` | Screens for capture contexts and deleting affected evaluations. A single adjusted-history query checks coverage/freshness and trims candles while retaining original counts. |
| `DataQualityRepository` | Dataset availability, excluded security tombstones, latest unexpired ranking, compact analytics and grouped complete ranking inputs. |
| `JobRepository` | Command existence, activation and version for execution checks; manual runs only require command existence. |

`DatasetObserver` records provider availability through `DataQualityRepository`. Availability and cooldown bookkeeping failures are logged without replacing provider results. `ProviderAccess` remains infrastructure: it owns shared persisted request budgets, account concurrency, spacing and credential cooldowns.

## Deactivation and purge

The security document is the tombstone. Ordinary stock/security/profile queries require an active, listed security and an existing profile; profile documents have no activation state. `Stock.profile` is required. Price reads independently enforce active security status.

An accepted `SetSecurityActive(false)` dispatches deterministic `PurgeStock(ticker, requestedAt)`. The caller supplies the exclusion reason: `user-excluded` for explicit exclusions or `provider-profile-empty` for confirmed empty provider profiles. The security service rejects deactivation of the VTI benchmark and activation of delisted securities. Repositories return applied, stale, delisted or missing results and preserve timestamp ordering, including equal-timestamp retries.

`PurgeStock` expands into four branches:

- A sequence deletes ranking inputs, evaluations of affected screens, and then the screens. Evaluation deletion must precede screen deletion because it joins screens. Input deletion prevents subsequent screen selection of that ticker.
- Profile deletion.
- Price deletion: analytics plus every adjustment's observations.
- Financial deletion: metrics plus history.

`DeleteScreens` freezes affected ranking IDs into a durable sequence containing `DeleteScreenRecords` and `ReleaseInvestmentRanking` actions, so retries retain the release work after screen documents disappear. Internal actions have priority over ordinary fan-out jobs and cannot be scheduled, even inside a `Sequence`.

Any cohort reference causes a screen and its evaluations to be deleted. The equal-universe cohort contains every eligible stock: purging one eligible stock can remove many screens. Research history is deliberately survivor-only. Reactivation does not restore deleted research.

Listing reconciliation preserves explicit exclusions, marks newly absent securities delisted and persists pending purge markers. The listing refresh drains these markers before fetching and after reconciliation, acknowledging each only after deterministic job dispatch succeeds. This allows recovery after restart or enqueue failure. A configurable listing-shrink guard rejects unexpectedly incomplete listings before changes. Deactivation and delisting clear the profile-fetch timestamp; relisted non-excluded stocks can refill through the normal freshness query. Explicit reactivation dispatches a profile fetch.

A provider's successful empty profile response triggers deactivation. Missing local cache data, malformed responses, transport failures and deferrals do not. Profiles use the requested ticker; a different ticker in a decoded Finnhub response logs a warning without failing the refresh. Explicit and already-running refreshes can recreate deleted cache data; the active-security guards still exclude those rows from ordinary reads.

## Rankings and evaluations

Each refresh publishes a new immutable ranking after all input rows are stored. Input IDs remain `{rankingId}:{ticker}`. Inputs contain the scorecard/evidence and compact selection fields; normalized `input.industry` is the peer key used for exact matching.

Headers and inputs have BSON-date `expiresAt` fields and zero-second TTL indexes. Unpinned rankings expire three days after publication (`RankingRetention`); usable rankings are at most two days old. Reads skip expired headers immediately, independently of the asynchronous TTL monitor.

Saving a manual or monthly screen dispatches `PinInvestmentRanking` with deterministic ID `pin-ranking-{screenId}`. Pinning unsets expiry on inputs before the header. Releasing a ranking checks for surviving screen references; without references it sets expiry to `max(now, createdAt + RankingRetention)`, then checks references again and pins if one appeared. Failed publications' input rows already carry expiry. No pruning scan or orphan lookup is required.

`RefreshInvestmentResearch` expands into a ranking-then-monthly-screen sequence and an independent evaluation-capture branch. Evaluation capture runs even if the first branch fails; mixed results produce `Partial`. Monthly creation returns typed created, already-exists, insufficient-eligible or ranking-unavailable outcomes. A new monthly screen requires a compatible usable ranking published on the same UTC day.

Evaluation reads never create captures or jobs. Capture requests and resumptions use the recorded `requestedHistory` set, updated in batches after dispatch and preserved across saves. Pruned completed history jobs are not recreated. VTI benchmark history is independent of company-profile existence.

Price observations keep one snapshot per ticker and adjustment, under `ticker:adjustment`; `data.id` retains provider provenance. Routine analytics persist a short unadjusted quote cache first. Full adjusted histories are persisted only for explicit evaluation captures. Financial history keeps one canonical ticker-keyed snapshot with provider, observation time, payload hash and mapping version. Price `data.fetchedAt`, financial `observedAt` and ranking expiry are BSON dates; API timestamps remain ISO strings. Replacements reject older or tied snapshots, retrying a duplicate-key upsert race once without upsert.

## Jobs, schedules and HTTP

The dispatcher persists durable jobs; the executor owns leases, heartbeats, retry policy, dependencies and parent aggregation. Heartbeat failures are retried while the lease is still valid. Successful action completion retries the finish write without repeating the action. Expired-lease reclaims are bounded, separately from transient retries. Provider deferrals use their own counter, preserve typed reason/context and respect capped provider cooldowns.

A `Paused` job represents a provider credential failure (HTTP/envelope 401). Its availability is moved fifteen minutes ahead; the executor automatically reclaims it after the cooldown. The matching in-memory provider account pause expires too. Startup can resume paused jobs immediately. Repeated credential failures pause again; parents and scheduled runs remain pending until the children complete. Per-symbol 403 is unavailable data and does not disable an account. Terminal jobs are not restarted.

The command scheduler reserves deterministic run IDs and repairs a missing job for an active run. Per-command failures are isolated. Manual execution returns HTTP 204 plus `X-Job-Id` without changing command scheduling state. Diagnostics expose bounded `/api/jobs` lists and `/api/jobs/status?id=...`; job views omit lease tokens and frozen plans.

Tapir endpoints use constant-time `X-API-Key` authentication. Static `/securities/tickers` precedes the ticker capture. Limits use Tapir validation (1–100); missing required non-path inputs return 422. Internal/provider failures return safe HTTP summaries and log details. Provider credentials travel in headers, never URL queries.

Configuration and all supported environment overrides are documented once in [README.md](README.md). Operational diagnosis and targeted refill are in its [troubleshooting section](README.md#troubleshooting-and-data-refill).

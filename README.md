# Stocks Checker

A Scala service for collecting stock market data, producing immutable investment rankings, saving screens and evaluating their cohorts. Finnhub supplies listings, profiles and financials; TwelveData supplies daily prices.

## Build and run

Install Java 27 and sbt; the project pins Scala 3.10.0 and sbt 2.0.11. Select Java 27 with `JAVA_HOME` and `PATH`, configure the environment below, then run:

```sh
sbt compile
sbt "core/testOnly *"
sbt "core/run"
```

The full test command includes unchanged suites; plain `sbt test` uses sbt 2's affected-suite selection. Use `sbt scalafmtAll scalafmtCheck` for formatting. `sbt "core/Docker/publishLocal"` builds a local Docker image. The manually dispatched Docker workflow publishes to the configured registry.

See [AGENTS.md](AGENTS.md) for architecture, repository ownership, cross-aggregate reads and development conventions.

## Configuration

PureConfig loads [application.conf](modules/core/src/main/resources/application.conf). Investment freshness limits are validated during configuration loading. API keys and database access belong in deployment configuration.

| Environment variable | Purpose / default |
| :--- | :--- |
| `API_KEY` | Internal API credential, sent as `X-API-Key` |
| `HOST` | HTTP bind host; `0.0.0.0` |
| `PORT` | HTTP port; `7070` |
| `FINNHUB_API_KEY` | Finnhub credential |
| `TWELVE_DATA_API_KEY` | Comma-separated TwelveData credentials; duplicates share one budget |
| `FINNHUB_REQUESTS_PER_MINUTE` | Finnhub spacing budget; `60` |
| `FINNHUB_DAILY_ALLOWANCE` | Finnhub daily request budget; `0` means unlimited |
| `TWELVE_DATA_REQUESTS_PER_MINUTE` | Per distinct TwelveData key; `8` |
| `TWELVE_DATA_DAILY_ALLOWANCE` | Per distinct TwelveData key; `800` |
| `ACTION_EXECUTOR_CONCURRENCY` | Concurrent background job workers; `2`, with provider limits enforced separately |
| `MAX_LISTING_SHRINK_FRACTION` | Maximum accepted decrease in an exchange's listed count; `0.2` |
| `MONGO_USER` | MongoDB username |
| `MONGO_PASSWORD` | MongoDB password |
| `MONGO_HOST` | MongoDB host |
| `DOCKER_USERNAME` | Image namespace when packaging/publishing |
| `DOCKER_REPO_URI` | Registry when packaging/publishing |

Provider concurrency limits, MongoDB database/timeouts and investment freshness limits are configurable in the HOCON file. The default database is `stocks-checker`. Providers receive credentials in request headers. Only credential failures (401) pause accounts and jobs; both retry after fifteen minutes. Per-symbol 403 and missing market data fail that request without disabling the account.

## Research and diagnostics

`RefreshInvestmentResearch` runs a ranking-then-monthly-screen sequence alongside independent evaluation capture. Every ranking refresh publishes a new ID. Monthly screen creation returns created, already-exists, insufficient-eligible or ranking-unavailable; successful job completion can therefore mean a documented skip. Creation requires a usable ranking from the same UTC day. Existing monthly screens keep their original ranking.

All routes below require `X-API-Key`:

| Route | Purpose |
| :--- | :--- |
| `GET /api/data-quality` | Dataset availability, security exclusions, frozen peer readiness and current analytics freshness |
| `GET /api/investment-screens?limit=20` | Saved screens; limit 1–100 |
| `GET /api/investment-screens/{id}` | Stored cohorts, candidates and bounded near misses |
| `GET /api/investment-screens/{id}?includeExclusions=true` | Include all in-scope exclusion scorecards |
| `GET /api/investment-screens/{id}/evaluation` | Read existing evaluation state; does not create jobs |
| `GET /api/investment-rankings/{id}/inputs?limit=50` | Frozen diagnostics; optional `industry`, `sector`, exclusive ordinal `after`; limit 1–100 |
| `GET /api/jobs?limit=20` | Recent jobs; optional `commandId`, `state`, `actionKind` |
| `GET /api/jobs/status?id={url-encoded-id}` | One job, including hierarchical child IDs |
| `POST /api/commands/{id}/execute` | Dispatch once; HTTP 204 carries `X-Job-Id`, independent of schedule state |

`POST /api/investment-screens` creates a screen. Its JSON `limit` controls candidate count (default 20); the detail route does not accept a candidate-limit query. `rankingReadiness` describes the frozen ranking, while `priceAnalyticsReadiness` describes current persisted analytics. A successful provider request alone does not establish that the computed analytics are fresh.

### Troubleshooting and data refill

- After a snapshot reset, restore required adjusted price histories before resuming pending evaluations. Captures retain `requestedHistory` and do not redispatch recorded requests; rerunning capture alone cannot reliably restore missing history. There is no public force-history endpoint.
- Existing financial metrics cannot reconstruct `financial-history`. Refill it through `UpdateFinancialMetrics` with explicit tickers or an unrestricted active-profile selector; freshness filters can skip records whose update timestamps still look recent.
- Creating a one-shot command with `maxExecutions: 1` already schedules it. Do not also execute it manually. Wait for refresh jobs and timestamp updates to finish before refreshing investment research.

## Deactivation and storage lifecycle

Explicit deactivation, delisting and confirmed empty profiles enqueue a purge. The security document remains as an inactive tombstone. The purge deletes profiles, analytics, price observations, financial metrics/history, ranking-input rows and every screen/evaluation whose cohorts contain the stock. VTI is protected as the evaluation benchmark. Exclusion reasons distinguish user exclusions from empty provider profiles.

The equal-universe cohort contains every eligible stock, so purging one eligible stock can delete every screen containing it. Research history is intentionally survivor-only. Reactivating a listed security refetches its profile and allows data to refill; it does not restore deleted screens. Unexpected listing shrinkage is rejected before reconciliation. Durable pending-purge markers preserve delisting cleanup across dispatch failures and restarts.

Rankings and input rows initially receive BSON-date `expiresAt = createdAt + 3 days`. Zero-second TTL indexes remove unpinned data asynchronously; reads already exclude expired headers. Screens dispatch internal pin actions, removing expiry from inputs before headers. Deleting the last referencing screen releases the ranking, restoring expiry and rechecking references for concurrent pins. A failed ranking publication's input rows also expire. If refreshes stop beyond retention, the data-quality report can correctly show no current ranking.

Price observations keep one current snapshot per ticker and adjustment under `ticker:adjustment`; `data.id` retains provider provenance. Routine analytics save a short unadjusted quote cache; full adjusted histories are persisted only for evaluation captures. Financial history keeps one canonical ticker-keyed snapshot. Snapshot replacement accepts only a newer timestamp. Scorecards preserve financial evidence/provenance without copying entire financial snapshots or raw responses. TTL controls the number of rankings retained, not the size of each scorecard.

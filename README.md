# Stocks Checker

A Scala service for collecting and analyzing stock market data.

## Architecture

Services depend on their own repositories, `MarketDataClient`, `ActionDispatcher`, clocks, logging and config. Services never depend on other services. Cross-aggregate writes are dispatched actions; cross-aggregate reads are aggregate queries in the requesting service's own repository. Pure scoring, selection and evaluation decisions live in `domain.investment`, including the `EvaluationCapture` capture decisions and report assembly.

`PriceService` owns both `PriceAnalyticsRepository` and `PriceHistoryRepository`, including quote acquisition, analytics and explicit evaluation-history refreshes. `FinancialMetricsService` owns metrics and financial history. Ranking, screen and evaluation services each own their corresponding repository. `InvestmentConfig` validates the recipe's freshness limits while loading configuration.

| Requesting repository | Collections read across aggregates |
| :--- | :--- |
| `StockRepository` | `securities`, `company-profiles`, `price-analytics` and `financial-metrics`, joined by the shared `StockAggregates` pipeline. Activation checks read only the security ID. |
| `InvestmentRankingRepository` | The stock collections through `StockAggregates`, plus `financial-history` by ticker `_id`. Pruning reads `investment-screens` to protect referenced rankings and joins old input groups to published rankings to identify orphans. |
| `InvestmentScreenRepository` | `investment-rankings` for headers and `investment-ranking-inputs` for ordinal-sorted entries. Entries remain a separate query to avoid embedding a full ranking in one MongoDB document. |
| `InvestmentEvaluationRepository` | `investment-screens` joined to saved evaluations for context and capture tasks; each price-series read queries all matching adjusted `price-observations` together, filters coverage and fetch timestamps, trims candles, and retains the original count for truncation checks. |
| `DataQualityRepository` | `dataset-availability`, excluded `securities` and the latest `investment-rankings` header. |

## Investment research actions

The following public actions can be scheduled independently:

| Action | Behavior |
| :--- | :--- |
| `RefreshInvestmentRanking` | Builds candidates through the ranking repository, publishes a ranking, and prunes expired unreferenced rankings and orphaned inputs. |
| `CreateMonthlyScreen` | Returns immediately if this month's screen already exists. Otherwise, reads a usable ranking from the same UTC day and creates the screen, unless the eligible universe is too small. |
| `CaptureMonthlyEvaluations` | Captures the current month and resumes unsettled captures, dispatching missing price-history requests through `ActionDispatcher`. |

`RefreshInvestmentResearch` expands into a sequence of ranking then screen creation, alongside an independent evaluation-capture action. The executor manages dependencies, retries and parent state. Evaluation capture still runs if ranking or screen creation fails, and mixed child outcomes make the umbrella job `Partial`. `ActionHandler` only routes leaf actions and expands composite plans; `RefreshEvaluationPrices` goes directly to `PriceService.refreshHistory`.

Evaluation reads do not create captures or dispatch jobs. Captures preserve their requested-history ticker set across saves, record dispatched batches atomically, and do not recreate pruned completed jobs. `InvestmentRanking.requireUsable` provides the shared recipe and freshness checks for screen creation.

## Storage

Routine price analytics save their calculated results and a small, 14-day unadjusted quote cache. Quotes are fetched first so a job deferred by provider request spacing can resume without downloading the large adjusted history again. Full adjusted histories are cached only for explicit portfolio evaluation captures; the analytics calculations still use the full 13-year history. A repeated forced analytics refresh may therefore require another provider request.

The price cache keeps one current snapshot per ticker and adjustment, replacing the whole vintage rather than merging prices adjusted at different times. Its stable Mongo ID is `ticker:adjustment`; the provider snapshot ID remains in `data.id` for provenance. Older snapshots are not a permanent audit archive. Financial history uses the ticker as `_id` and keeps only the latest canonical snapshot per ticker, including its payload hash, mapping version and validation issues, without a duplicate raw provider response.

Ranking inputs store the full scorecard and its evidence alongside just the exchange, country, industry, sector, old price score and exclusion explanation needed by screens. They do not duplicate complete stock records, company profiles or financial snapshots. Unreferenced rankings and their input rows expire after three days, allowing a day beyond their freshness window for screen creation; saved screens retain their rankings.

Snapshot `data.fetchedAt` (prices) and `observedAt` (financials) are BSON dates, with millisecond precision. A single conditional replacement accepts only a newer vintage; duplicate IDs and tied timestamps leave the current snapshot unchanged. A duplicate-key collision gets one conditional retry without upsert, so a concurrent first insert cannot discard a newer vintage. API timestamps remain ISO strings.

### Starting after resetting snapshot collections

**Upgrading from the old snapshot schema requires dropping `price-observations` and `financial-history` before starting the updated application.** Old price timestamps are strings, so the BSON-date comparison cannot replace them. Old financial snapshots have ObjectId IDs and a unique `id` index that is incompatible with the new ticker IDs. Also drop the retired `latest-prices` collection. If these three collections have already been dropped, that reset is complete; no additional collection drops or document conversions are needed.

Keep `securities`, `company-profiles`, `price-analytics`, `financial-metrics`, `investment-rankings`, `investment-ranking-inputs`, `investment-screens`, `investment-evaluations`, commands and jobs.

1. **Before resetting or deploying, deactivate the `RefreshInvestmentResearch` command.** Find it with `GET /api/commands`, then call `PUT /api/commands/{id}/active` with `{"isActive":false}` and the configured `X-API-Key` header. Also deactivate any independently scheduled ranking, monthly-screen or evaluation-capture actions, including sequences containing them. Leave these commands inactive through step 5: the scheduler runs as soon as the application starts. Check that no outstanding research or history jobs remain before resetting, including queued or deferred manual jobs; deactivating a command does not cancel independently dispatched manual jobs, which can resume on startup.
2. Stop all application instances. In `mongosh`, select the application's database (`stocks-checker` by default) and run the following if the reset has not already been done:

   ```javascript
   use stocks-checker
   db.getCollection("price-observations").drop()
   db.getCollection("financial-history").drop()
   db.getCollection("latest-prices").drop()
   ```

3. Start the updated application with the research commands still inactive. Provider fetches recreate `price-observations` and `financial-history` with BSON dates and stable IDs. `latest-prices` is no longer used and will not be recreated.
4. Refill financial history and complete the evaluation recovery check below before running any research action. Run `FetchFinancialMetrics` with `CompanyProfileFilter.IsActive(true)` for the active universe, or `UpdateFinancialMetrics` for explicit tickers, and wait for the refresh jobs to finish. Existing `financial-metrics` documents do not reconstruct financial snapshots. A freshness-based filter can skip tickers whose profile timestamps still look recent, so use the active-universe or explicit-ticker action for this initial refill. Price caches refill on demand: `UpdatePriceAnalytics` refreshes analytics and the small unadjusted cache; evaluation captures request adjusted histories. Existing saved price analytics can remain in place.
5. With financial history refilled and affected captures recovered, manually run the inactive `RefreshInvestmentResearch` command using `POST /api/commands/{id}/execute`, then check its results. This endpoint can dispatch an inactive command without reactivating its schedule. A ranking already published on the same UTC day for the same recipe is reused; if a ranking was published before the refill, wait until the next UTC day to build a new one. Existing monthly screens retain their original ranking.
6. Reactivate the commands paused in step 1 using `PUT /api/commands/{id}/active` with `{"isActive":true}`.

The application creates the new `{ screenId: 1, month: 1 }` index on `investment-evaluations` during startup. Dropping `financial-history` already removed its obsolete indexes; its built-in `_id` index now enforces one snapshot per ticker. Old evaluation indexes on `settled` and `(finished, screenId)` may remain without affecting correctness. Removing those indexes is optional space cleanup, not a startup requirement.

Preserve saved evaluation reports: persisted monthly reports are self-contained. The evaluation endpoint can return the current month's settled report or the latest finished report without the deleted candles; an unfinished screen's later month can require recalculation. Captures still requiring computation may need targeted history recovery after the cache reset. Their `requestedHistory` can already record the missing tickers, and even a new capture can share a terminal history job for the same ticker and cutoff date with another screen. Running `CaptureMonthlyEvaluations` again does not guarantee that these histories will be downloaded again: when no jobs remain pending, it can permanently settle an incomplete report. Check affected captures and restore their required adjusted histories before resuming them; do not broadly delete screens, reports or jobs. Ordinary analytics refreshes do not persist the adjusted histories used by evaluations, and there is no public force-history endpoint. If no evaluation history requests have ever been dispatched, this recovery case does not apply.

Atlas's free-tier quota counts uncompressed documents and indexes, so use the collection's **Data size** and **Total index size** columns, rather than compressed **Storage size**, to check the result ([MongoDB documentation](https://www.mongodb.com/docs/atlas/reference/free-shared-limitations/)).

# Investment screening recovery

Preserve existing snapshots, screens and evaluations during recovery. A successful empty company-profile response dispatches the existing `set-security-active` action with `active=false`; other missing datasets and provider errors do not. Keep the current screening thresholds while diagnosing coverage.

## Deploy before the one-shot confirmation

Deploy the missing-profile deactivation and ranking-input endpoint changes before submitting the confirmation batch below. Existing terminal jobs are historical evidence: deployment does not rerun them or rewrite their errors. Only a new successful empty profile response triggers deactivation; existing data-quality rows do not backfill activation state at startup.

Keep the existing daily profile command (`69089d166e48114b50741dc9`) unchanged: it selects active stocks with a company profile not updated for three days. Once a stock is deactivated, future scheduled profile, price and financial selection excludes it. There is no daily profile recovery or automatic reactivation. Any future reactivation is an explicit administrative action.

The prepared [recovery/profile-recheck-2026-11-02.json](recovery/profile-recheck-2026-11-02.json) body for `POST /api/commands` contains the 56 unavailable profiles from the 2 November 11:02 UTC capture. Before submitting, compare that list with fresh data and remove stocks already inactive. This is a single confirmation batch for the currently active baseline, not a recurring recovery command. The file contains no credentials and has not been submitted. Submit its creation only once; it schedules itself.

## Inspect actual execution

Use the configured `X-API-Key` header for these requests:

1. `GET /api/data-quality` and `GET /api/investment-screens` establish the baseline. Check the ranking timestamp separately from the report timestamp; exclusion counts overlap and include out-of-scope securities.
2. `POST /api/commands/{research-command-id}/execute` dispatches one run. Its HTTP 204 response includes `X-Job-Id`; neither HTTP 204 nor the command's execution count indicates successful completion.
3. `GET /api/jobs/status?id={job-id}` returns the job's state, child IDs, dependencies, retry/availability times and error. URL-encode the query value, especially child IDs containing `/`. Follow child IDs until the ranking and screen actions are terminal. Evaluation capture is an independent branch and may still be running.
4. `GET /api/jobs?limit=20&actionKind=refresh-investment-research` finds recent research runs, including manual runs created before execution returned a job ID. Optional `commandId` and `state` filters inspect scheduled runs; manual runs do not carry scheduled-command linkage. Job retention still applies, so an expired job returns 404.
5. Check the newer `rankingReadiness` and monthly screen. Ranking readiness can be `ready`, `insufficientEligible`, `staleRanking` (not the current UTC day), or `recipeMismatch`. The screen service logs its created/already-existing/skipped outcome. Every new refresh recomputes the ranking; an existing monthly screen remains pinned.

The screen job's `succeeded` state also includes the insufficient-eligible outcome; use `rankingReadiness` and the screen list to distinguish a completed action from a created screen. Ordinary screen/evaluation reads hide an entire saved screen if any holding in any stored cohort is inactive. Its stored monthly ID remains occupied; an explicit administrative reactivation can restore visibility without rewriting the saved screen.

Do not use `POST /api/investment-screens` as a diagnostic probe: it creates a saved screen.

## Refill only the missing inputs

If eligibility is below 20, use complete peer counts to identify the largest incomplete industry/sector groups. Current thresholds require at least 20 complete industry peers or 30 complete sector peers. Stocks excluded solely for `insufficient-peer-coverage` passed every earlier check.

Use the immutable ID from `rankingReadiness.rankingId` to inspect its frozen inputs without Mongo access:

```text
GET /api/investment-rankings/{rankingId}/inputs?industry=health%20care&sector=healthcare&limit=50
GET /api/investment-rankings/{rankingId}/inputs?industry=health%20care&sector=healthcare&limit=50&after={nextAfter}
```

The response contains `rankingId`, `asOf`, `items` and `nextAfter`. Each item includes its original ordinal, ticker, stored industry, sector, eligibility, peer group/count, exclusions and existing evidence. Pagination is exclusive of `after`; repeat the same ranking ID and filters until `nextAfter` is null. The default page size is 50 and maximum is 100. Unknown/pruned ranking IDs return 404; a known ranking with no matching rows returns an empty page. Industry matching trims whitespace and ignores case. These authenticated administrative diagnostics deliberately retain frozen evidence for stocks hidden from ordinary reads.

Compare a few affected canonical financial snapshots with sanitized provider responses before changing mappings. Missing growth metrics and insufficient annual history can be genuine provider gaps; the latter also includes future periods and irregular fiscal-year spacing. Missing sector counts include missing profiles, not only unmapped industry names.

Dispatch explicit `update-company-profiles`, `update-financial-metrics` or `update-price-analytics` actions for affected tickers, using a one-shot command (`maxExecutions: 1`, periodic schedule of `1second`) when needed. Explicit profile actions can confirm provider availability, but a successful fetch does not automatically reactivate a stock. Cached-profile, security and stock eligibility remain enforced through repository aggregations, and automated price/financial fan-out selects active stocks before applying its batch limit. PriceService and FinancialMetricsService have no activation checks on queued or explicit refreshes, so select only currently active tickers when preparing price or financial recovery batches. Wait for terminal child outcomes, then run research again. Ordinary analytics refreshes do not restore the adjusted history snapshots required by unfinished evaluations; preserve the evaluation recovery precautions in the README.

Security deactivation dispatches internal `delete-price-analytics` and `delete-financial-metrics` actions to their owning services. Once those actions complete, ordinary price-analytics and financial-metrics reads no longer return the deleted rows. This is eventual deletion, not an execution gate: already queued or explicit price/financial refreshes can still run and recreate those rows. Profile, security, stock and screening guards continue to exclude the inactive stock. Financial history snapshots, historical price observations and saved evaluations are retained. Accepted profile deactivation clears both analytics/metrics refresh timestamps. After explicit administrative reactivation, ordinary freshness-based batches can select the stock immediately; an explicit refill is optional if results are needed before the next batch. Listing reconciliation retains profiles and caches, so a temporary listing omission does not delete them and a later relisting restores visibility.

## Bounded production recovery

1. After deployment, fetch a fresh data-quality report. The 2 November baseline contained 56 unavailable company profiles. Select rows whose provider is Finnhub, dataset is `profile`, status is `unavailable` and failure is null (successful empty responses). Deduplicate tickers, retain only stocks that are still active, and cap this confirmation batch at 56. Do not schedule rechecks for already deactivated stocks; review any additional rows separately.
2. Create one scheduled command with action `update-company-profiles`, those explicit tickers, schedule `{"kind":"periodic","period":"1second"}` and `maxExecutions: 1`. Creation schedules its run; do not also manually execute it. Find its `update-company-profiles` parent by command ID through `/api/jobs` and follow its children. Do not create an empty batch.
3. Wait for the confirmation parent to become terminal and for its `set-security-active`, `set-company-profile-active`, `delete-price-analytics` and `delete-financial-metrics` events to settle. Missing-profile children can remain failed after dispatching deactivation; check data-quality exclusions and ordinary stock/profile 404 responses to verify deactivation, and confirm price analytics and financial metrics were deleted. Preserve previous successful profile timestamps.
4. Execute research command `6adf4ac5b8e1ad2fbd1e4016` once, follow `X-Job-Id` and its children, then read the newer ranking readiness. Save that ranking ID before inspecting input pages.
5. Start with the `health care` industry in sector `healthcare`: the baseline had 10 complete candidates against the required 20. Use the pages to identify nearby currently active candidates blocked by missing/stale inputs. Refresh only their indicated profile, financial or price inputs; do not refresh genuine negative financial criteria merely to try to change eligibility.
6. Wait for those jobs and activation events, then refresh research again on the same UTC date. Confirm a new ranking ID. If coverage now supports at least 20 eligible stocks, verify exactly one monthly screen. An existing monthly screen keeps its original ranking. Otherwise record the updated candidate counts and exact remaining exclusions.

The 2 November 12:16 UTC check found the 12:03 UTC ranking `81c45e6c-7b64-4ab0-a839-f7114cae27f8` still reporting 58 otherwise-complete candidates and zero eligible stocks. Deactivation will remove unavailable stocks from normal use; it does not by itself fill the missing peer groups.

No collection resets or historical-screen rewrites are part of this recovery. The old 600-ticker one-shot command is already exhausted even though its active flag remains true.

## Historical decoding failures and report latency

The two research failures recorded on 28 October contain only generic `MongoJsonParsingException` messages. Mongo4Cats raises these when stored BSON/JSON cannot be decoded into the requested Scala type. Those monolithic research jobs retained no collection or field context, so their exact original cause cannot be recovered from the job records. Keep standard codecs. If a new run fails, inspect its ranking/screen/evaluation child first, then add bounded collection/field context only at that failing repository read and reproduce the mismatch before repairing data.

The full data-quality report took roughly 29 seconds in the first 2 November baseline and 25.87 seconds in the 12:16 UTC check. Measure response latency independently from job completion, preserving the report's `generatedAt` and `rankingAsOf`. Compare the bounded ranking-input endpoint separately; do not attribute the full report's delay to a specific Mongo stage without query/serialization timing evidence.

## Interpret price and profile failures

- Stale/disjoint price histories and explicit TwelveData no-data responses are unavailable for this run, not immediately retried transport failures. They remain unsuccessful jobs and can be tried in a later scheduled run.
- A successful empty profile response records unavailability and deactivates the stock, retaining its previous success timestamp. Future scheduled queries exclude it, and only an explicit administrative action may reactivate it. Expected absence logs at info; malformed data still logs a warning and does not itself deactivate.
- Price validation remains strict. Errors include the ticker, adjustment, requested range and bounded date samples. Never discard bad candles merely to make a screen pass.
- Automated price/financial selection requires an active profile and active, listed security before applying its batch limit. Deactivation removes their cached rows through separate internal deletion events; it does not add PriceService or FinancialMetricsService execution-time checks.

## Check capacity and scheduling

Confirm the deployed distinct TwelveData key count and limits without printing credentials. Defaults are 8 requests/minute and 800/day per distinct key. Duplicate keys share a budget. Approximately two calls are needed per uncached analytics ticker, so a cold 600-ticker batch can consume 1,200 requests before evaluation traffic or retries. Cached quotes can reduce this, and multiple keys increase the available allowance.

The `provider-budgets` collection holds one hashed-account record per used key, including `budget.day`, `budget.used`, `budget.nextAllowed` and `budget.cooldownUntil`. Compare current UTC-day usage with the deployed limits; old/retired account records do not establish the configured key count. Without Mongo access, job deferral reasons reveal quota/spacing waits but not remaining allowance.

Cron spacing does not establish completion dependencies. Inspect actual jobs before bootstrap research; retain the normal twice-daily research schedule after recovery. A one-shot command with `executionCount == maxExecutions` will not run automatically again even if its `isActive` flag is true.

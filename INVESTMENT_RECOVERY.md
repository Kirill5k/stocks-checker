# Investment screening recovery

Preserve existing snapshots, screens and evaluations during an ordinary recovery. A provider's missing data is not evidence that a security should be deactivated. Keep the current screening thresholds while diagnosing coverage.

## Inspect actual execution

Use the configured `X-API-Key` header for these requests:

1. `GET /api/data-quality` and `GET /api/investment-screens` establish the baseline. Check the ranking timestamp separately from the report timestamp; exclusion counts overlap and include out-of-scope securities.
2. `POST /api/commands/{research-command-id}/execute` dispatches one run. Its HTTP 204 response includes `X-Job-Id`; neither HTTP 204 nor the command's execution count indicates successful completion.
3. `GET /api/jobs/status?id={job-id}` returns the job's state, child IDs, dependencies, retry/availability times and error. URL-encode the query value, especially child IDs containing `/`. Follow child IDs until the ranking and screen actions are terminal. Evaluation capture is an independent branch and may still be running.
4. `GET /api/jobs?limit=20&actionKind=refresh-investment-research` finds recent research runs, including manual runs created before execution returned a job ID. Optional `commandId` and `state` filters inspect scheduled runs; manual runs do not carry scheduled-command linkage. Job retention still applies, so an expired job returns 404.
5. Check the newer `rankingReadiness` and monthly screen. Ranking readiness can be `ready`, `insufficientEligible`, `staleRanking` (not the current UTC day), or `recipeMismatch`. The screen service logs its created/already-existing/skipped outcome. Every new refresh recomputes the ranking; an existing monthly screen remains pinned.

Do not use `POST /api/investment-screens` as a diagnostic probe: it creates a saved screen.

## Refill only the missing inputs

If eligibility is below 20, use complete peer counts to identify the largest incomplete industry/sector groups. Current thresholds require at least 20 complete industry peers or 30 complete sector peers. Stocks excluded solely for `insufficient-peer-coverage` passed every earlier check.

If Mongo read access is available, inspect the exact published ranking and its nearby candidates:

```javascript
const recoveryRanking = db.getCollection("investment-rankings").find().sort({createdAt: -1}).limit(1).next();
db.getCollection("investment-ranking-inputs").find(
  {rankingId: recoveryRanking._id, "scorecard.exclusions": ["insufficient-peer-coverage"]},
  {"scorecard.ticker": 1, "scorecard.peerCount": 1, "input.industry": 1, "input.sector": 1}
);
// Narrow this query to a sector/industry from the result above.
db.getCollection("investment-ranking-inputs").find(
  {rankingId: recoveryRanking._id, "scorecard.exclusions": {$size: 1}},
  {"scorecard.ticker": 1, "scorecard.exclusions": 1, "input.industry": 1, "input.sector": 1}
);
```

Compare a few affected canonical financial snapshots with sanitized provider responses before changing mappings. Missing growth metrics and insufficient annual history can be genuine provider gaps; the latter also includes future periods and irregular fiscal-year spacing. Missing sector counts include missing profiles, not only unmapped industry names.

Dispatch explicit `update-company-profiles`, `update-financial-metrics` or `update-price-analytics` actions for the affected tickers, using a one-shot command (`maxExecutions: 1`, periodic schedule of `1second`) when needed. Explicit ticker actions bypass successful-refresh age filters and automated active-universe selection. Wait for terminal child outcomes, then run research again. Ordinary analytics refreshes do not restore the adjusted history snapshots required by unfinished evaluations; preserve the evaluation recovery precautions in the README.

## Interpret price and profile failures

- Stale/disjoint price histories and explicit TwelveData no-data responses are unavailable for this run, not immediately retried transport failures. They remain unsuccessful jobs and can be tried in a later scheduled run.
- Missing profiles remain unavailable, retain their previous success timestamp and do not deactivate securities. Expected absence logs at info; malformed data still logs a warning.
- Price validation remains strict. Errors include the ticker, adjustment, requested range and bounded date samples. Never discard bad candles merely to make a screen pass.
- Automated price/financial queries requesting active profiles also require an active, listed security before applying their batch limit.

## Check capacity and scheduling

Confirm the deployed distinct TwelveData key count and limits without printing credentials. Defaults are 8 requests/minute and 800/day per distinct key. Duplicate keys share a budget. Approximately two calls are needed per uncached analytics ticker, so a cold 600-ticker batch can consume 1,200 requests before evaluation traffic or retries. Cached quotes can reduce this, and multiple keys increase the available allowance.

The `provider-budgets` collection holds one hashed-account record per used key, including `budget.day`, `budget.used`, `budget.nextAllowed` and `budget.cooldownUntil`. Compare current UTC-day usage with the deployed limits; old/retired account records do not establish the configured key count. Without Mongo access, job deferral reasons reveal quota/spacing waits but not remaining allowance.

Cron spacing does not establish completion dependencies. Inspect actual jobs before bootstrap research; retain the normal twice-daily research schedule after recovery. A one-shot command with `executionCount == maxExecutions` will not run automatically again even if its `isActive` flag is true.

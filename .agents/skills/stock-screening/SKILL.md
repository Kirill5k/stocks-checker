---
name: stock-screening
description: Research long-term stock candidates through Stocks Checker's investment-screen workflow, explain eligibility and rankings, and use the legacy stock filters when explicitly requested.
---

# Long-term stock research

Use the experimental investment workflow for 3–10 year stock research. The service
defaults to `http://localhost:7070`, **all data endpoints have the `/api` prefix**,
and all requests need the configured `X-API-Key` header. Do not reveal API keys.

## Default workflow

Reuse an existing screen ID when the user asks to inspect a saved screen or its
evaluation; creating another screen is unnecessary for those reads.

1. Read `GET /api/data-quality`. Inspect `rankingCoverage`, `rankingAsOf`,
   provider failures, and security exclusions.
2. New screens require a completed ranking for the current recipe that is no
   older than two days. If it is missing or stale, explain that a ranking refresh
   must finish before creating a screen; use the refresh workflow below when
   requested. Coverage describes the last published ranking, not the current
   contents of snapshot caches. Do not describe the old price score as an
   investment-quality score.
3. Create a saved screen from the latest completed ranking:

   ```sh
   curl -X POST -H "X-API-Key: $API_KEY" -H 'Content-Type: application/json' \
     http://localhost:7070/api/investment-screens \
     -d '{"recipeVersion":"quality-fair-value-v1","limit":20}'
   ```

4. Explain the returned score components, evidence units, peer cohort, exclusions,
   and warnings. An empty result is not evidence that no good businesses exist;
   inspect missing/stale/unsupported inputs and requested thresholds.
5. Use `GET /api/investment-screens/{id}` for the saved result and
   `GET /api/investment-screens/{id}/evaluation` for forward outcomes. Preserve the
   distinction between pending horizons, incomplete data, observed returns so far,
   and completed three/five-year returns.

`GET /api/investment-screens?limit=20` lists saved summaries. A POST saves a new
immutable view; it does not fetch provider data or recompute peer ranks from the
selected subset. Saved screens keep their original ranking and remain readable
after that ranking passes the freshness window for new screens.

## Research refresh and evaluation capture

Use the command API for requested refreshes. `GET /api/commands` lists existing
commands; `POST /api/commands/{id}/execute` dispatches one without changing its
schedule or execution count. `POST /api/commands` creates a persisted schedule;
preserve the user's scheduling intent. A successful manual-execution response
means work was dispatched, not that provider fetches or ranking publication have
completed. Check the resulting ranking timestamp/coverage or evaluation report.

The schedulable action `kind` values are:

| Kind | Behavior |
| --- | --- |
| `refresh-investment-ranking` | Scores stored candidates and publishes a ranking. Reuses an existing ranking for the same recipe and UTC day. It does not fetch missing financial inputs. |
| `create-monthly-screen` | Returns immediately if this month's screen exists for the recipe. Otherwise creates it using a usable ranking from today in UTC, unless fewer than 20 stocks are eligible. |
| `capture-monthly-evaluations` | Captures the latest completed month and resumes unsettled captures, dispatching missing adjusted-history requests. |
| `refresh-investment-research` | Runs ranking then monthly screen creation in a sequence, alongside an independent evaluation capture. Capture still runs if ranking or screen creation fails; mixed outcomes produce a `Partial` parent job. |

`GET /api/investment-screens/{id}/evaluation` is read-only: it can calculate from
cached histories and report `pendingDataJobs`, but does not save a capture or
request more data. Use the capture action when background collection is needed.
A settled report may still be incomplete; explain its missing data and issues.
`refresh-evaluation-prices` is internal and cannot be scheduled, even inside a
sequence. There is no public force-history endpoint.

## Request contract

All POST fields are optional. Omitted recipe selects `quality-fair-value-v1`;
unknown versions are rejected. `limit` defaults to 20 and is bounded 1–100.
`exchange` accepts `nasdaq` or `nyse`; `country` uses the stored country code.
`minScore`, `minQuality`, `minValue`, `minGrowth`, and `minResilience` are 0–100
thresholds. The display `limit` does not set portfolio size: the frozen investment
cohort contains up to 20 stocks, and baseline cohorts are selected independently
of the requested investment thresholds. Use only criteria relevant to the user's
objective. Do not present a particular threshold or the initial weights as
empirically optimal.

The recipe weights quality 35%, valuation 30%, growth 20%, and balance-sheet
resilience 15%. It uses a fixed peer universe and excludes unsupported specialist
sectors or incomplete fundamentals. At most ten near misses are returned. They
fail exactly one requested investment threshold or one supported intrinsic rule
(cash-flow durability or nonpositive P/E, P/FCF, or EPS). An intrinsic financial
near miss has no score and remains in the exclusion counts. Missing, stale, unsupported, or ambiguous
inputs are separate exclusions; results beyond the display limit are not
near misses.

The initial model covers ordinary US companies quoted in USD on supported exchanges. It is
not a model for banks, insurers, REITs, ETFs, or ADRs. No account coverage audit or
superior-return validation is implied by the existence of the feature. Use the
current [README](../../../README.md) for operational guidance and the domain code
under `modules/core/src/main/scala/stockschecker/domain/investment` for scoring
and evaluation rules.

## Missing snapshots after a cache reset

When upgrading from the old snapshot schema, dropping `price-observations` and
`financial-history` is required before starting the updated app; also drop the
retired `latest-prices` collection. Follow the
[README reset procedure](../../../README.md#starting-after-resetting-snapshot-collections).
Deactivate the research command and any independently scheduled ranking, screen
or capture actions before resetting or deploying. Use
`PUT /api/commands/{id}/active` with `{"isActive":false}`; the scheduler resumes
when the app starts. Also check that no outstanding research or history jobs
remain, including queued or deferred manual jobs that command deactivation does
not cancel. Keep schedules inactive until financial history is refilled and
affected evaluations are recovered, then manually verify research results before
reactivating with `{"isActive":true}`. Rebuild financial history with
`fetch-financial-metrics` using
`{"kind":"is-active","active":true}` as its filter, or `update-financial-metrics`
for explicit tickers, before refreshing rankings. Existing metrics and profile
freshness timestamps do not prove that canonical snapshots still exist. Wait for
the financial jobs to finish; a same-day ranking refresh still reuses that day's
published ranking.

Routine price analytics retain only the short unadjusted cache. Evaluation
captures populate adjusted histories. Preserve saved screens, rankings and
settled evaluation reports. After cache loss, recorded `requestedHistory` or a
terminal job for the same ticker/cutoff can suppress fetching, including for a
new capture sharing those jobs. Repeating capture is not guaranteed to restore
these histories and can permanently settle an incomplete report when no jobs
remain pending. Inspect affected captures and recover their histories before
resuming capture, as described in the README.

## Legacy stock filtering

Use `GET /api/stocks` when the user specifically requests raw filters or historical
price performance. Every filter is AND-combined. `kind` and `exchange` are lower
case. `sortBy` accepts `market-cap`, `overall-score`, `cagr-score`, or
`volatility-score`. The historical `overall-score` is not the new investment score.

The endpoint supports security, market-cap, price, performance, and financial
filters represented in `StockFilters`. Use `period` with price-change criteria.
Its wire values are **`one-month`, `three-month`, `six-month`, `one-year`,
`three-year`, `five-year`, `ten-year`, and `max`**.

`minConsistencyScore` filters the normalized 0–100 price-consistency score. The
raw R² value appears in returned analytics metrics and is not this filter's scale.
Financial percentage inputs such as growth, ROE, and margins use the endpoint's
documented percentage units; raw annual provider-series ratios used internally
for investment ranking are a different representation.

```sh
curl -H "X-API-Key: $API_KEY" \
  'http://localhost:7070/api/stocks?kind=stock&isActive=true&period=one-year&minChange=0&sortBy=overall-score&limit=20'
```

Do not use this legacy endpoint to claim an evidence-backed long-term recommendation.
Fundamental review, data coverage, and a saved forward evaluation remain necessary
parts of the investment research workflow.

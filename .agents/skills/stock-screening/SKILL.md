---
name: stock-screening
description: Research long-term stock candidates through Stocks Checker's audited investment-screen workflow, explain eligibility and rankings, and use the legacy stock filters when explicitly requested.
---

# Long-term stock research

Use the experimental investment workflow for 3–10 year stock research. The service
defaults to `http://localhost:7070`, **all data endpoints have the `/api` prefix**,
and all requests need the configured `X-API-Key` header. Do not reveal API keys.

## Default workflow

1. Read `GET /api/data-quality`. Inspect `rankingEnabled`, `coverageAudited`,
   `rankingCoverage`, provider failures, and security exclusions.
2. If ranking or coverage audit is disabled, explain that the app has not been
   cleared to publish investment rankings. Do not enable the switches merely to
   obtain results or describe the old price score as an investment-quality score.
3. When enabled, create a saved screen from the latest completed ranking:

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
immutable view; it does not recompute peer ranks from the selected subset.

## Request contract

All POST fields are optional. Omitted recipe selects `quality-fair-value-v1`;
unknown versions are rejected. `limit` defaults to 20 and is bounded 1–100.
`exchange` accepts `nasdaq` or `nyse`; `country` uses the stored country code.
`minScore`, `minQuality`, `minValue`, `minGrowth`, and `minResilience` are 0–100
thresholds. Use only criteria relevant to the user's objective. Do not present a
particular threshold or the initial weights as empirically optimal.

The recipe weights quality 35%, valuation 30%, growth 20%, and balance-sheet
resilience 15%. It uses a fixed peer universe and excludes unsupported specialist
sectors or incomplete fundamentals. Near misses fail exactly one requested investment threshold or one explicit
financial eligibility criterion. An intrinsic financial near miss has no score
and remains in the exclusion counts. Missing, stale, unsupported, or ambiguous
inputs are separate exclusions; results beyond the display limit are not
near misses.

The initial model covers ordinary US companies quoted in USD on supported exchanges. It is
not a model for banks, insurers, REITs, ETFs, or ADRs. No account coverage audit or
superior-return validation is implied by the existence of the feature. Consult
`docs/long-term-investment-research.md` for provenance, rollout, and limitations.

## Legacy stock filtering

Use `GET /api/stocks` when the user specifically requests raw filters or historical
price performance. Every filter is AND-combined. `kind` and `exchange` are lower
case. `sortBy` accepts `market-cap`, `overall-score`, `cagr-score`, or
`volatility-score`. The historical `overall-score` is not the new investment score.

The endpoint supports security, market-cap, price, performance, and financial
filters represented in `StockFilters`. Use `period` with price-change criteria.
Its wire values are **`one-month`, `three-month`, `six-month`, `one-year`,
`three-year`, `five-year`, `ten-year`, and `max`**.

`minConsistencyScore` is retained as the deprecated raw R² threshold on a 0–1
scale. Use `minPriceConsistencyScore` for the actual 0–100 price-consistency score.
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

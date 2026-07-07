---
name: stock-screening
description: Screen stocks for long-term investment by querying the Stocks Checker /stocks endpoint. Use when the user wants to find, filter, rank, or screen stocks by fundamentals (P/E, ROE, debt, margins, growth), performance scores (overall, CAGR, volatility), market cap, price, or exchange — or to build a screening curl request against the running service.
---

# Stock Screening for Long-Term Investment

Screen stocks by querying the `GET /stocks` endpoint of the running Stocks Checker service. The endpoint accepts a set of optional query parameters that map directly to `StockFilters`; results are returned as a JSON array of stocks.

## How to run a screen

1. The endpoint is secured — every request needs the `X-API-Key` header set to the service's configured API key (env var `API_KEY`).
2. Base URL defaults to `http://localhost:7070` (host/port configurable via `HOST`/`PORT`).
3. Combine any subset of the filter parameters below. All are optional and AND-combined.
4. Use `sortBy` + `limit` to rank and cap the result set.

```bash
# Long-term investment screening: top 20 stocks for long-term growth
curl -H "X-API-Key: $API_KEY" \
  "http://localhost:7070/stocks?\
kind=stock&\
isActive=true&\
minMarketCap=10000000000&\
minPE=15&maxPE=35&\
minROE=15&\
maxDebtToEquity=1.5&\
minProfitMargin=10&\
minFreeCashFlow=0&\
minRevenueGrowth5Y=8&\
minEpsGrowth5Y=10&\
minOverallScore=60&\
minCagrScore=50&\
minVolatilityScore=40&\
sortBy=overall-score&\
limit=20"
```

## Screening strategy

A layered strategy for long-term investment picks:

**Phase 1 — Basic filtering**
- Security type: individual stocks only (`kind=stock`; exclude ETFs, REITs)
- Market cap: large-cap for stability (`minMarketCap=10000000000`, i.e. ≥$10B)
- Exchange: NYSE or NASDAQ (`exchange=nyse` or `exchange=nasdaq`)

**Phase 2 — Financial health**
- Valuation: reasonable P/E (`minPE=15`, `maxPE=35`)
- Profitability: efficient capital use and margins (`minROE=15`, `minProfitMargin=10`)
- Stability: manageable debt (`maxDebtToEquity=1.5`)
- Cash generation: positive free cash flow (`minFreeCashFlow=0`)

**Phase 3 — Growth indicators**
- Revenue growth: 5-year annualized ≥8% (`minRevenueGrowth5Y=8`)
- EPS growth: 5-year annualized ≥10% (`minEpsGrowth5Y=10`)
- CAGR score ≥50 (`minCagrScore=50`, indicating ~15%+ CAGR)

**Phase 4 — Risk management**
- Overall score ≥60 (`minOverallScore=60`)
- Volatility score ≥40 (`minVolatilityScore=40`, moderate-to-low volatility)
- Optional price bounds via `minPrice` / `maxPrice`

## Filter parameters

All parameters are optional query-string values on `GET /stocks`.

**Basic**
- `limit` (Int) — max number of results
- `exchange` — `nasdaq` or `nyse` (lowercase)
- `kind` — security type: `stock`, `etf`, `reit`, `adr` (lowercase)
- `isActive` (Boolean) — filter active/inactive securities
- `country` (String) — country code, e.g. `US`, `GB`
- `sortBy` — sort field: `market-cap`, `overall-score`, `cagr-score`, `volatility-score` (kebab-case)

**Market**
- `minMarketCap`, `maxMarketCap` (Long) — market-cap range
- `minPrice`, `maxPrice` (BigDecimal) — current price range
- `minChange`, `maxChange` (BigDecimal) — price change over `period`
- `period` — time window for change filters: `oneMonth`, `threeMonth`, `sixMonth`, `oneYear`, `threeYear`, `fiveYear`, `tenYear`, `max`

**Performance scores** (0–100)
- `minOverallScore` — composite of CAGR, volatility, drawdown, consistency
- `minCagrScore` — CAGR score (100 = 30%+ CAGR)
- `minVolatilityScore` — volatility score (100 = ≤10% volatility)
- `maxDrawdown` — cap on drawdown
- `minConsistencyScore` — minimum consistency score

**Financial metrics**
- `minPE`, `maxPE` — P/E ratio (trailing 12 months)
- `minROE` — return on equity (TTM)
- `maxDebtToEquity` — debt-to-equity (annual)
- `minProfitMargin`, `maxProfitMargin` — net profit margin (TTM)
- `minFreeCashFlow` — free cash flow per share (TTM)
- `minDividendYield`, `maxDividendYield` — dividend yield (annual)
- `minRevenueGrowth5Y` — 5-year annualized revenue growth (%)
- `minEpsGrowth5Y` — 5-year annualized EPS growth (%)

> Note: enum-valued params must use their wire form — `kind`/`exchange` are lowercase, and `sortBy` is kebab-case (`overall-score`, not `overallScore`). Passing the wrong casing returns a validation error listing the accepted values.

## Portfolio construction

After screening, manually review results for:
1. **Sector diversification** — max ~5 stocks per industry
2. **Geographic mix** — 70–80% US, 20–30% international (ADRs)
3. **Market-cap balance** — ~70% large cap ($50B+), ~30% mid cap ($10–50B)
4. **Risk balance** — mix stable (volatility score >70) and growth (40–70) names
5. **Business moats** — competitive advantages and secular tailwinds

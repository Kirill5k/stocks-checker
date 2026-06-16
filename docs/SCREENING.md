# Stock Screening for Long-Term Investment

The application provides comprehensive filtering capabilities for selecting stocks suitable for long-term investment. The screening strategy focuses on fundamental financial health, growth potential, and risk management.

## Investment Screening Strategy

**Phase 1: Basic Filtering**
- **Security Type:** Focus on individual stocks (exclude ETFs, REITs)
- **Market Cap:** Target large-cap stocks (≥$10B) for stability
- **Exchange:** NYSE or NASDAQ listings

**Phase 2: Financial Health Metrics**
- **Valuation:** P/E ratio between 15-35 (reasonable valuation)
- **Profitability:** ROE ≥15% (efficient capital use), Profit Margin ≥10%
- **Financial Stability:** Debt-to-Equity ≤1.5 (manageable debt)
- **Cash Generation:** Positive free cash flow per share

**Phase 3: Growth Indicators**
- **Revenue Growth:** 5-year annualized ≥8%
- **EPS Growth:** 5-year annualized ≥10%
- **CAGR Score:** ≥50 (indicating 15%+ CAGR)

**Phase 4: Risk Management**
- **Overall Score:** ≥60 (composite score of CAGR, volatility, drawdown, consistency)
- **Volatility Score:** ≥40 (moderate to low volatility)
- **Price Range:** Filter by current price if needed

## Example API Request

```bash
# Long-term investment screening: Find top 20 stocks for long-term growth
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
sortBy=overallScore&\
limit=20"
```

## Available Filter Parameters

**Basic Filters:**
- `limit` - Maximum number of results
- `exchange` - Stock exchange (NYSE, NASDAQ)
- `kind` - Security type (stock, etf, reit, adr, other)
- `country` - Country code (e.g., "US", "GB")
- `minMarketCap`, `maxMarketCap` - Market capitalization range
- `minPrice`, `maxPrice` - Current stock price range

**Performance Scores:**
- `minOverallScore` - Composite score (0-100) weighted by CAGR, volatility, drawdown, consistency
- `minCagrScore` - CAGR score (0-100, where 100 = 30%+ CAGR)
- `minVolatilityScore` - Volatility score (0-100, where 100 = ≤10% volatility)

**Financial Metrics:**
- `minPE`, `maxPE` - Price-to-Earnings ratio (trailing 12 months)
- `minROE` - Return on Equity (trailing 12 months)
- `maxDebtToEquity` - Debt to Equity ratio (annual)
- `minProfitMargin` - Net profit margin (trailing 12 months)
- `minFreeCashFlow` - Free cash flow per share (trailing 12 months)

**Growth Metrics:**
- `minRevenueGrowth5Y` - 5-year annualized revenue growth (%)
- `minEpsGrowth5Y` - 5-year annualized EPS growth (%)

**Sorting:**
- `sortBy` - Sort field: `marketCap`, `overallScore`, `cagrScore`, `volatilityScore`

## Portfolio Construction Recommendations

After filtering, manually review results for:
1. **Sector Diversification:** Max 2 stocks per industry
2. **Geographic Mix:** 70-80% US, 20-30% international (ADRs)
3. **Market Cap Balance:** 70% large cap ($50B+), 30% mid cap ($10-50B)
4. **Risk Balance:** Mix of stable (volatility score >70) and growth (volatility score 40-70)
5. **Business Moats:** Companies with competitive advantages and secular tailwinds

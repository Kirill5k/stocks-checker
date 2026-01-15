# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Stocks Checker

## Project Overview

**Stocks Checker** is a Scala-based web service designed to aggregate, manage, and analyze stock market data. It integrates with multiple external financial data providers (Finnhub, AlphaVantage, TwelveData) to fetch real-time and historical stock information, which is then stored and processed using MongoDB.

### Key Technologies

*   **Language:** Scala 3.7.4
*   **Runtime:** Java 25 (Temurin)
*   **Build Tool:** sbt
*   **Core Stack:**
    *   **Effect System:** Cats Effect 3
    *   **Streaming:** FS2
    *   **HTTP Server:** Http4s (Ember)
    *   **HTTP Client:** Sttp 4
    *   **API Definition:** Tapir
    *   **Database:** MongoDB (via Mongo4Cats)
    *   **JSON:** Circe
    *   **Configuration:** PureConfig

### Architecture

The application follows a functional architecture with a clear separation of concerns:

*   **Controllers:** Handle HTTP requests and define API endpoints using Tapir.
*   **Services:** Implement business logic and orchestrate data flows.
*   **Repositories:** Manage data persistence in MongoDB.
*   **Clients:** Interface with external stock market APIs.
*   **Actions:** Define background tasks and scheduled jobs (e.g., fetching updates).

## Building and Running

### Prerequisites

*   Java 25 (Temurin recommended)
*   sbt

### Key Commands

*   **Compile:**
    ```bash
    sbt compile
    ```

*   **Run Tests:**
    ```bash
    sbt test
    ```

*   **Run Specific Test:**
    ```bash
    sbt "testOnly *YourTestClassName*"
    ```

*   **Run Application:**
    ```bash
    sbt "project core" run
    ```
    *Note: You may need to set environment variables for API keys and database connections (see Configuration).*

*   **Build Docker Image:**
    ```bash
    sbt docker:publishLocal
    ```

*   **Format Code:**
    ```bash
    sbt scalafmtAll
    ```

*   **Check Formatting:**
    ```bash
    sbt scalafmtCheck
    ```

## Configuration

The application uses `PureConfig` to load settings from `modules/core/src/main/resources/application.conf`. Key configuration overrides are handled via environment variables:

| Environment Variable | Description |
| :--- | :--- |
| `API_KEY` | Master API key for securing internal endpoints |
| `HOST` | Server host (default: 0.0.0.0) |
| `PORT` | Server port (default: 7070) |
| `FINANCIAL_MODELING_PREP_API_KEY` | API key for Financial Modeling Prep |
| `ALPHA_VANTAGE_API_KEY` | API key for Alpha Vantage |
| `FINNHUB_API_KEY` | API key for Finnhub |
| `TWELVE_DATA_API_KEY` | API key for Twelve Data |
| `MONGO_USER` | MongoDB username |
| `MONGO_PASSWORD` | MongoDB password |
| `MONGO_HOST` | MongoDB host address |

## Architecture Deep Dive

### Application Initialization

The application starts in `Application.scala` which:
1. Loads configuration from `application.conf`
2. Creates resources (HTTP backend, MongoDB connection)
3. Initializes the action dispatcher (for background tasks)
4. Constructs clients, repositories, services, and controllers
5. Starts the HTTP server and action executor in parallel using FS2 streams

### Action System

The application uses a custom action-based task system for background jobs:

*   **Action**: A sealed trait representing different types of background tasks (e.g., `FetchSecurities`, `FetchCompanyProfiles`). Actions are defined in `actions/Action.scala`.
*   **ActionDispatcher**: A queue-based dispatcher that manages pending actions. Uses an FS2 queue with a capacity of 128.
*   **ActionExecutor**: Consumes actions from the dispatcher and executes them by calling appropriate service methods. Runs as an FS2 stream in parallel with the HTTP server.

### Command System

Commands provide scheduled execution of actions:

*   **Command**: Represents a scheduled task with a cron or periodic schedule, stored in MongoDB
*   **Schedule**: Supports both periodic (fixed interval) and cron-based scheduling
*   Commands track execution count, last execution time, and can have a maximum execution limit
*   On startup, `Action.RescheduleAll` reschedules all active commands

### Layered Architecture

The codebase follows a strict functional architecture with dependency flow:

```
Application
  └─> Resources (HTTP backend, MongoDB)
      └─> Clients (external API integrations)
          └─> Repositories (MongoDB persistence)
              └─> Services (business logic)
                  └─> Controllers (HTTP endpoints)
```

*   **Resources**: Manages lifecycle of HTTP client and MongoDB connection
*   **Clients**: Interface with external APIs (Finnhub, TwelveData, AlphaVantage)
*   **Repositories**: Provide CRUD operations on MongoDB collections
*   **Services**: Orchestrate business logic, coordinate between repositories and clients
*   **Controllers**: Define Tapir endpoints and map HTTP requests/responses

### Controller Security

Controllers use Tapir for endpoint definition with built-in API key authentication:

*   `Controller.publicEndpoint`: Base endpoint with error handling
*   `Controller.secureEndpoint`: Endpoint requiring API key in `X-API-Key` header
*   API key validation uses constant-time comparison to prevent timing attacks
*   Error responses are consistently mapped to appropriate HTTP status codes

### Testing

Tests use:
*   **EmbeddedMongo**: For integration testing repositories (via mongo4cats-embedded)
*   **AsyncWordSpec**: ScalaTest style for async tests
*   Each repository test extends `RepositorySpec` which provides embedded MongoDB setup
*   Tests seed data using `Document` objects and verify operations

## Stock Screening for Long-Term Investment

The application provides comprehensive filtering capabilities for selecting stocks suitable for long-term investment. The screening strategy focuses on fundamental financial health, growth potential, and risk management.

### Investment Screening Strategy

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

### Example API Request

```bash
# Long-term investment screening: Find top 20 stocks for long-term growth
curl -H "X-API-Key: $API_KEY" \
  "http://localhost:7070/stocks?\
kind=stock&\
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

### Available Filter Parameters

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

### Portfolio Construction Recommendations

After filtering, manually review results for:
1. **Sector Diversification:** Max 2 stocks per industry
2. **Geographic Mix:** 70-80% US, 20-30% international (ADRs)
3. **Market Cap Balance:** 70% large cap ($50B+), 30% mid cap ($10-50B)
4. **Risk Balance:** Mix of stable (volatility score >70) and growth (volatility score 40-70)
5. **Business Moats:** Companies with competitive advantages and secular tailwinds

## Development Conventions

*   **Code Style:** The project uses `scalafmt` for code formatting. Always run `sbt scalafmtCheck` (or `scalafmtAll`) before committing.
*   **Functional Programming:** The codebase strictly adheres to functional programming principles using Cats and Cats Effect. Avoid side effects outside of `IO` monads.
*   **Testing:** Unit and integration tests are located in the `test` directory of the `core` module. Tests often use embedded Mongo for integration testing.
*   **CI/CD:** GitHub Actions are configured in `.github/workflows` for CI (testing) and Docker publishing.

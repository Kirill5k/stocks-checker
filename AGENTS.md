# AGENTS.md

This file provides guidance to AI agents when working with code in this repository.

# Stocks Checker

## Project Overview

**Stocks Checker** is a Scala-based web service designed to aggregate, manage, and analyze stock market data. It integrates with multiple external financial data providers (Finnhub, AlphaVantage, TwelveData) to fetch real-time and historical stock information, which is then stored and processed using MongoDB.

### Key Technologies

*   **Language:** Scala 3.10.0
*   **Runtime:** Java 27
*   **Build Tool:** sbt 2.0.11
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

*   Java 27, selected via `JAVA_HOME` and available on `PATH`
*   sbt launcher (the project pins sbt 2.0.11 in `project/build.properties`)

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
    sbt "core/run"
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
| `ALPHA_VANTAGE_API_KEY` | API key for Alpha Vantage |
| `FINNHUB_API_KEY` | API key for Finnhub |
| `TWELVE_DATA_API_KEY` | API key for Twelve Data |
| `ACTION_EXECUTOR_CONCURRENCY` | Maximum concurrent job workers across all actions (default: 16); provider limits apply separately |
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
*   **ActionDispatcher**: The service gateway to durable jobs. Persists dispatched actions in MongoDB, supports deterministic idempotent dispatch, and reports pending job IDs.
*   **ActionExecutor**: Claims durable jobs with leases and heartbeats and delegates execution to `ActionHandler`. Parent jobs aggregate child states with backoff. Provider deferrals back off from 30 seconds to five minutes, respect provider cooldowns and add jitter; these routine waits do not consume transient retries. Their provider and typed reason (`concurrency-limit`, `request-spacing`, `daily-quota`, or `provider-429`) are recorded in `job.error` without provider response text. Persisted cooldown markers distinguish HTTP 429 waits from request spacing across restarts. Transient failures use the bounded `JobExecutionPolicy`; terminal jobs are not restarted. Mongo decoding failures are terminal and report a safe field path in job errors and logs, without stored documents or invalid field values. Runs as an FS2 stream in parallel with the HTTP server.

### Command System

Commands provide scheduled execution of actions:

*   **Command**: Represents a scheduled task with a cron or periodic schedule, stored in MongoDB
*   **Schedule**: Supports both periodic (fixed interval) and cron-based scheduling
*   Commands track execution count, last execution time, and can have a maximum execution limit
*   The independent command scheduler reserves deterministic run IDs and recovers a missing job from the command's active run. Per-command failures are isolated; no outbox is needed.

### Layered Architecture

The codebase follows a strict functional architecture:

```
Controllers -> Services -> Repositories -> Domain
                       -> Clients -> ProviderAccess
                       -> ActionDispatcher -> JobRepository
ActionExecutor -> ActionHandler -> Services
CommandScheduler -> CommandRepository, JobRepository
```

*   **Resources**: Manage the HTTP client and MongoDB connection lifecycle.
*   **Clients**: Fetch external data; `MarketDataClient` selects providers and `DatasetObserver` records dataset availability. Provider mappings translate financial metrics and sectors into canonical domain values.
*   **Repositories**: Persist and query domain data. Services own business decisions. Multi-collection reads use aggregate queries in the requesting service's own repository.
*   **Services**: Coordinate repositories, clients and pure domain logic. Each aggregate repository has exactly one owning service, and services never import another aggregate's repository. Reads across aggregates go through the owning service's interface. Services access jobs through `ActionDispatcher`.
*   **Events**: Cross-aggregate writes are internal actions dispatched through `ActionDispatcher` and delivered by `ActionHandler` to the owning service. `RecordCompanyProfileUpdate`, `RecordPriceAnalyticsUpdate`, `RecordFinancialMetricsUpdate`, `SetSecurityActive` and `SetCompanyProfileActive` update the owning aggregate when the executor handles them. These changes are eventually consistent. Activation events carry the requested state and timestamp; repositories guard updates with `activeChangedAt` to reject older requests. Internal events take priority over ordinary fan-out jobs. Internal actions cannot be scheduled as commands, including inside a `Sequence`.
*   **Domain**: Pure investment scoring, selection and evaluation live under `domain.investment`. Price analytics live beside `PriceAnalytics`; there is no `utils` package.
*   **Controllers**: Define Tapir endpoints and map HTTP requests and responses. Internal command scheduling state stays out of response views.

Repository ownership:

| Owning service | Repositories |
| :--- | :--- |
| `SecurityService` | `SecurityRepository` |
| `CompanyProfileService` | `CompanyProfileRepository` |
| `PriceService` | `PriceAnalyticsRepository`, `LatestPriceRepository` |
| `PriceHistoryService` | `PriceHistoryRepository` |
| `FinancialMetricsService` | `FinancialMetricsRepository`, `FinancialHistoryRepository` |
| `StockService` | `StockRepository` (the joined stock read model) |
| `CommandService` | `CommandRepository` |
| `DataQualityService` | `DataQualityRepository` (availability and joined report reads) |
| `InvestmentRankingService` | `InvestmentRankingRepository` |
| `InvestmentScreenService` | `InvestmentScreenRepository` |
| `InvestmentEvaluationService` | `InvestmentEvaluationRepository` |

The composition root wires repositories into their owners. Job execution, command scheduling and provider-budget coordination remain infrastructure concerns handled by `ActionDispatcher`, `ActionExecutor`, `CommandScheduler` and `ProviderAccess`. `ProviderAccess` owns persisted request budgets. `DatasetObserver` records dataset availability inside `MarketDataClient` through `DataQualityRepository`; `DataQualityService` uses the same repository to aggregate availability, excluded securities and the latest published ranking into its report.

### Storage assumptions

This code targets a first deployment with an empty database. Persistence entities use the current schema; there are no legacy decoders, startup backfills or migrations. Monthly evaluation documents track both pending and settled captures in `investment-evaluations`; reads do not create captures or jobs. Each capture stores its dispatched history requests in an internal `requestedHistory` list, updated atomically after dispatch and preserved across report saves. Resuming that capture does not recreate pruned history jobs, so job retention does not depend on evaluations. `InvestmentRankingRepository` owns `investment-rankings` and `investment-ranking-inputs`, publishing metadata only after all entries are stored.

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

## Development Conventions

*   **Code Style:** The project uses `scalafmt` for code formatting. Always run `sbt scalafmtCheck` (or `scalafmtAll`) before committing.
*   **Functional Programming:** The codebase strictly adheres to functional programming principles using Cats and Cats Effect. Avoid side effects outside of `IO` monads.
*   **Testing:** Unit and integration tests are located in the `test` directory of the `core` module. Tests often use embedded Mongo for integration testing.
*   **CI/CD:** GitHub Actions are configured in `.github/workflows` for CI (testing) and Docker publishing.

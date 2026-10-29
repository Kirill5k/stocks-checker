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

*   **Run All Tests, Including Unchanged Suites:**
    ```bash
    sbt "core/testOnly *"
    ```
    *sbt 2's `test` command selects affected suites; use this command for a full regression run.*

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

The application uses `PureConfig` to load settings from `modules/core/src/main/resources/application.conf`. `InvestmentConfig` constructs the investment recipe and validates its freshness limits during configuration loading. Key configuration overrides are handled via environment variables:

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
*   **ActionExecutor**: Claims durable jobs with leases and heartbeats and delegates execution to `ActionHandler`. Parent jobs aggregate child states with backoff. Provider deferrals back off from 30 seconds to five minutes, respect provider cooldowns and add jitter; these routine waits do not consume transient retries. Their provider, typed reason (`concurrency-limit`, `request-spacing`, `daily-quota`, or `provider-429`) and optional context are recorded in `job.error`. Persisted cooldown markers distinguish HTTP 429 waits from request spacing across restarts. Transient failures use the bounded `JobExecutionPolicy`; terminal jobs are not restarted. Job errors and logs retain exception messages, except Mongo decoding failures, which are terminal and report a safe field path without stored documents or invalid field values. Runs as an FS2 stream in parallel with the HTTP server.

Investment research has three schedulable actions: `RefreshInvestmentRanking` publishes and prunes rankings using the service's current UTC date, `CreateMonthlyScreen` returns immediately if the monthly screen exists and otherwise creates it from a usable ranking published on the same UTC day, and `CaptureMonthlyEvaluations` captures or resumes portfolio evaluations. `RefreshInvestmentResearch` expands into two independent children: `Sequence(RefreshInvestmentRanking, CreateMonthlyScreen)` and `CaptureMonthlyEvaluations`. The executor waits for ranking completion before screen creation and still runs evaluation capture if the sequence fails; mixed child outcomes make the umbrella job `Partial`. `RefreshEvaluationPrices` routes directly to `PriceService.refreshHistory`.

`ActionHandler` routes each leaf action to its owning service and expands composite actions into durable job plans. The executor owns research dependencies and failure aggregation.

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
*   **Services**: Coordinate their own repositories, `MarketDataClient`, `ActionDispatcher`, clocks, logging and config. Services never depend on other services. Each aggregate repository has exactly one owning service; cross-aggregate reads are aggregate queries in the requesting service's own repository, and cross-aggregate writes are dispatched actions. Services access jobs through `ActionDispatcher`.
*   **Events**: Cross-aggregate writes are internal actions dispatched through `ActionDispatcher` and delivered by `ActionHandler` to the owning service. `RecordCompanyProfileUpdate`, `RecordPriceAnalyticsUpdate`, `RecordFinancialMetricsUpdate`, `SetSecurityActive` and `SetCompanyProfileActive` update the owning aggregate when the executor handles them. These changes are eventually consistent. Activation events carry the requested state and timestamp; repositories guard updates with `activeChangedAt` to reject older requests. Internal events take priority over ordinary fan-out jobs. Internal actions cannot be scheduled as commands, including inside a `Sequence`.
*   **Domain**: Pure investment scoring, selection and evaluation live under `domain.investment`. `EvaluationCapture` decides which histories a capture needs and assembles pending or settled reports. `InvestmentRanking.requireUsable` checks recipe compatibility and ranking freshness. Price analytics live beside `PriceAnalytics`; there is no `utils` package.
*   **Controllers**: Define Tapir endpoints and map HTTP requests and responses. Internal command scheduling state stays out of response views.

Repository ownership:

| Owning service | Repositories |
| :--- | :--- |
| `SecurityService` | `SecurityRepository` |
| `CompanyProfileService` | `CompanyProfileRepository` |
| `PriceService` | `PriceAnalyticsRepository`, `PriceHistoryRepository` |
| `FinancialMetricsService` | `FinancialMetricsRepository`, `FinancialHistoryRepository` |
| `StockService` | `StockRepository` (the joined stock read model) |
| `CommandService` | `CommandRepository` |
| `DataQualityService` | `DataQualityRepository` (availability and joined report reads) |
| `InvestmentRankingService` | `InvestmentRankingRepository` |
| `InvestmentScreenService` | `InvestmentScreenRepository` |
| `InvestmentEvaluationService` | `InvestmentEvaluationRepository` |

Cross-aggregate reads stay inside the requesting repository:

| Requesting repository | Collections read and purpose |
| :--- | :--- |
| `StockRepository` | Starts from `securities` and joins `company-profiles`, `price-analytics` and `financial-metrics` through `StockAggregates`; activation checks project only the security ID. |
| `InvestmentRankingRepository` | Reuses `StockAggregates`, then joins `financial-history` by ticker `_id` for candidates. Pruning reads `investment-screens` to retain referenced rankings and joins old input groups to `investment-rankings` to find orphaned inputs. |
| `InvestmentScreenRepository` | Joins screen headers to `investment-rankings`, reads the latest ranking header, and reads ordinal-sorted `investment-ranking-inputs` separately so full rankings cannot exceed MongoDB's document limit inside a lookup. |
| `InvestmentEvaluationRepository` | Joins `investment-screens` to `investment-evaluations` for capture context and work lists. One query reads matching adjusted `price-observations`, checks coverage and fetch timestamps, and trims candles while retaining the original candle count for truncation checks. |
| `DataQualityRepository` | Aggregates `dataset-availability` and joins excluded `securities` plus the latest `investment-rankings` header for the quality report. |

The composition root wires repositories into their owners. Job execution, command scheduling and provider-budget coordination remain infrastructure concerns handled by `ActionDispatcher`, `ActionExecutor`, `CommandScheduler` and `ProviderAccess`. `ProviderAccess` owns persisted request budgets. `DatasetObserver` records dataset availability inside `MarketDataClient` through `DataQualityRepository`; `DataQualityService` uses the same repository to aggregate availability, excluded securities and the latest published ranking into its report.

### Storage assumptions

Persistence entities use the current schema; there are no legacy decoders, startup backfills or migrations. Price analytics provenance (`basis`) is optional: missing or null values mean unknown provenance, while fresh calculations populate it. Monthly evaluation documents track both pending and settled captures in `investment-evaluations`; reads do not create captures or jobs. Each capture stores its dispatched history requests in an internal `requestedHistory` list, updated in batches with `$addToSet`/`$each` after dispatch and preserved across report saves. Resuming that capture does not recreate pruned history jobs, so job retention does not depend on evaluations. `InvestmentRankingRepository` owns `investment-rankings` and `investment-ranking-inputs`, publishing metadata only after all entries are stored.

Price observations keep one current snapshot per ticker and adjustment under a stable `ticker:adjustment` Mongo ID; `data.id` preserves provider provenance. Routine analytics persist only the short unadjusted quote cache, fetched before adjusted history so provider deferrals can resume. Full adjusted histories are persisted only for explicit evaluation captures. Financial history uses ticker `_id` values and retains only the latest canonical snapshot per ticker without raw provider payloads or a separate snapshot ID. Provider, observation timestamp, payload hash and mapping version preserve financial provenance, including in scorecard evidence; scorecards have no `financialSnapshotId` field. Price `data.fetchedAt` and financial `observedAt` are BSON dates with millisecond precision, while API timestamps remain ISO strings. Conditional replacements reject older or tied timestamps, with one retry without upsert on duplicate-key races. Unreferenced rankings are retained for three days; screens pin their rankings. Existing snapshot data must use the current BSON-date schema before starting the updated application; there is no startup migration. README describes startup and data refill after resetting the snapshot collections, including recovery considerations for pending evaluations. Removed financial ID fields in existing documents are ignored without a migration.

Ranking input rows contain a full scorecard plus compact screen-selection fields (exchange, country, industry, sector, old price score and exclusion explanation), rather than copying complete stock records or financial snapshots. Scoring still runs over full candidates before publication.

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
*   **Mongo Queries:** Build pipelines with the `Aggregate` builder methods (`lookup`, `project`, `set`, `facet`, and other available operations); do not construct `new Aggregate` from raw stages.
*   **Functional Programming:** The codebase strictly adheres to functional programming principles using Cats and Cats Effect. Avoid side effects outside of `IO` monads.
*   **Testing:** Unit and integration tests are located in the `test` directory of the `core` module. Tests often use embedded Mongo for integration testing.
*   **CI/CD:** GitHub Actions are configured in `.github/workflows` for CI (testing) and Docker publishing.

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

## Development Conventions

*   **Code Style:** The project uses `scalafmt` for code formatting. Always run `sbt scalafmtCheck` (or `scalafmtAll`) before committing.
*   **Functional Programming:** The codebase strictly adheres to functional programming principles using Cats and Cats Effect. Avoid side effects outside of `IO` monads.
*   **Testing:** Unit and integration tests are located in the `test` directory of the `core` module. Tests often use embedded Mongo for integration testing.
*   **CI/CD:** GitHub Actions are configured in `.github/workflows` for CI (testing) and Docker publishing.

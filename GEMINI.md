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

## Development Conventions

*   **Code Style:** The project uses `scalafmt` for code formatting. Always run `sbt scalafmtCheck` (or `scalafmtAll`) before committing.
*   **Functional Programming:** The codebase strictly adheres to functional programming principles using Cats and Cats Effect. Avoid side effects outside of `IO` monads.
*   **Testing:** Unit and integration tests are located in the `test` directory of the `core` module. Tests often use embedded Mongo for integration testing.
*   **CI/CD:** GitHub Actions are configured in `.github/workflows` for CI (testing) and Docker publishing.

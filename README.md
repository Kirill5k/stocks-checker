# Stocks Checker

A Scala service for collecting and analyzing stock market data.

## Storage

Routine price analytics save their calculated results and a small, 14-day unadjusted quote cache. Quotes are fetched first so a job deferred by provider request spacing can resume without downloading the large adjusted history again. Full adjusted histories are cached only for explicit portfolio evaluation captures; the analytics calculations still use the full 13-year history. A repeated forced analytics refresh may therefore require another provider request.

The price cache keeps one current snapshot per ticker and adjustment, replacing the whole vintage rather than merging prices adjusted at different times. Its stable Mongo ID is `ticker:adjustment`; the provider snapshot ID remains in `data.id` for provenance. Older snapshots are not a permanent audit archive. Financial history keeps only the latest canonical snapshot per ticker, including its payload hash, mapping version and validation issues, without a duplicate raw provider response.

Ranking inputs store the full scorecard and its evidence alongside just the exchange, country, industry, sector, old price score and exclusion explanation needed by screens. They do not duplicate complete stock records, company profiles or financial snapshots. Unreferenced rankings and their input rows expire after three days, allowing a day beyond their freshness window for screen creation; saved screens retain their rankings.

### Recovering an already-full cluster

The October 2026 collection report showed 420 MB of price observations, 57.7 MB of financial history, 30.8 MB of ranking inputs and no saved screens or evaluations. For that state, [scripts/cleanup-storage.js](scripts/cleanup-storage.js) can discard the reconstructible price cache and rankings, retain the latest financial snapshot per ticker and remove raw payloads. It preserves saved analytics, financial metrics, commands and jobs. It refuses to run if screens or evaluations exist because their histories and rankings must be preserved. Rankings become available again after the next scheduled research run.

Stop the application before applying cleanup, then start the updated version. Cleanup is separate from application startup and is required to remove existing snapshot copies and free space; changing future writes alone does not recover a full cluster. The new price cache uses stable IDs and ranking rows use compact inputs, so clear their old data before starting this version. If screens/evaluations already exist, preserve and convert their required histories and ranking inputs instead of using this reset procedure.

Use a connection URI that selects the `stocks-checker` database. Preview first:

```bash
mongosh "$MONGODB_URI" --file scripts/cleanup-storage.js
```

Apply the displayed cleanup with the application stopped:

```bash
APPLY_STORAGE_CLEANUP=true mongosh "$MONGODB_URI" --file scripts/cleanup-storage.js
```

Atlas's free-tier quota counts uncompressed documents and indexes, so use the collection's **Data size** and **Total index size** columns, rather than compressed **Storage size**, to check the result ([MongoDB documentation](https://www.mongodb.com/docs/atlas/reference/free-shared-limitations/)).

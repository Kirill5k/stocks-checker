# Stocks Checker

A Scala service for collecting and analyzing stock market data.

`TWELVE_DATA_API_KEY` accepts comma-separated API keys. Whitespace and empty entries
are ignored; duplicate keys count once. Twelve Data rotates requests across keys
and tries another key when one is rate limited or its credentials are disabled.
The configured minute, daily, and concurrency limits apply to each distinct key.

Provider budgets persist in MongoDB using stable credential fingerprints, so
reordering keys preserves usage and cooldowns. New per-key budgets inherit any
legacy provider-wide budget on first use; this can conservatively reduce a newly
added key's allowance until the next UTC day. After fixing credentials, restart
the application to clear credential pauses and resume paused jobs; persisted
usage and cooldowns remain in effect.

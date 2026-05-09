# Monitoring DB connection adapter (E6.b)

Thin adapter that bridges the existing nemetonshiny env-var convention
(\`NEMETON_DB_HOST\` / \`\_PORT\` / \`\_NAME\` / \`\_USER\` /
\`\_PASSWORD\`, plus the Clever Cloud \`POSTGRESQL_ADDON\_\*\` fallbacks
used by \`service_db.R\`) to the URL-based API exposed by
\`nemeton::db_connect()\`. Calling code does not need to know which
style is configured.

Resolution order:

1\. \`NEMETON_DB_URL\` — passed straight through to nemeton. 2.
\`POSTGRESQL_ADDON\_\*\` then \`NEMETON_DB\_\*\` parts — assembled into
a postgresql URL (credentials URL-encoded so passwords with \`@\`,
\`:\`, \`/\` … don't break the parse). 3. None of the above set — return
NULL so the Monitoring tab can show a configuration hint instead of
crashing.

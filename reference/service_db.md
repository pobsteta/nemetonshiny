# Database Service for PostgreSQL/PostGIS

Connection and operations for the PostgreSQL/PostGIS database (ADR-002).
The database stores projects, parcels, indicators, and users for
multi-user access and spatial queries.

Connection is optional: if no database credentials are configured, the
app falls back to local parquet storage.

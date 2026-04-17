# Sync local project to database

Uploads a local project (parquet files) to the PostGIS database.

## Usage

``` r
db_sync_project(project_id, con = NULL)
```

## Arguments

- project_id:

  Character. Local project ID.

- con:

  DBI connection. If NULL, creates a new one.

## Value

Logical. TRUE if successful.

# Initialize database schema

Creates the nemeton schema, tables, and indexes if they don't exist.
Reads from inst/sql/schema.sql.

## Usage

``` r
db_init_schema(con = NULL)
```

## Arguments

- con:

  DBI connection object. If NULL, creates a new connection.

## Value

Logical. TRUE if successful.

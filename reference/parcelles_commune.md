# Cadastral parcels of a commune

Cadastral parcels of a commune

## Usage

``` r
parcelles_commune(insee, ids = NULL)
```

## Arguments

- insee:

  INSEE code of the commune (5 characters).

- ids:

  Optional character vector of parcel ids to keep.

## Value

An sf object with `id`, `section`, `numero`, `contenance` (m2),
`commune`, `code_insee`. When `ids` is given, an error lists the ids not
found. Read-only (network access to the cadastre services).

## See also

Other api_hors_interface:
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
[`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

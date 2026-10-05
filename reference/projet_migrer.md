# Apply a project's pending migrations

What opening the project in the application does: set aside indicators
computed under an older indicator direction (renamed
`data/indicators.perime-v<n>-<date>.parquet`, never deleted; the project
goes back to draft and must be recomputed) and create the management
units when they are missing (one unit per parcel; unreadable unit files
are moved to `data/ug_sauvegarde_<date>/` first).

## Usage

``` r
projet_migrer(id)
```

## Arguments

- id:

  Project id.

## Value

The new
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
invisibly.

## See also

Other api_hors_interface:
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
[`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

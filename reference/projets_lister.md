# List the projects

List the projects

## Usage

``` r
projets_lister()
```

## Value

A data.frame, one row per project, most recently updated first: `id`,
`nom`, `statut`, `maj`, `ndp`, `ugf`, `indicateurs` (computed indicators
present on disk), `sens_a_jour` (`FALSE` when the indicators predate the
current indicator direction, see
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md)).
Read-only.

## See also

Other api_hors_interface:
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
[`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md),
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md)

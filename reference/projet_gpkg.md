# GeoPackage of a project's results

GeoPackage of a project's results

## Usage

``` r
projet_gpkg(id, fichier)
```

## Arguments

- id:

  Project id.

- fichier:

  Path of the `.gpkg` to write (overwritten; its directory is created).

## Value

The path of the GeoPackage: one feature per management unit with the
indicators and the 12 family scores. Only `fichier` is written.

## See also

Other api_hors_interface:
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
[`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

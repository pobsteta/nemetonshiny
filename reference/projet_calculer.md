# Compute a project's indicators

Runs the full computation synchronously (it can take from minutes to
more than an hour). Progress is also written to
`data/compute_progress.json`.

## Usage

``` r
projet_calculer(id, indicateurs = "all", progression = NULL)
```

## Arguments

- id:

  Project id.

- indicateurs:

  `"all"` or a character vector of indicator codes.

- progression:

  Optional function called with the progress state (a list with
  `status`, `progress`, `progress_max`, `current_task`, ...).

## Value

The computation result (list with `success`), invisibly.

## Errors

Class `nemetonshiny_projet_ancien` for a project created before version
1.0.0, `nemetonshiny_calcul_echec` when the computation fails.

## See also

Other api_hors_interface:
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
[`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

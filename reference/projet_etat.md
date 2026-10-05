# State of a project

Everything needed to decide what to do with a project, without loading
it.

## Usage

``` r
projet_etat(id)
```

## Arguments

- id:

  Project id (the name of its directory).

## Value

A list:

- `id`, `nom`, `statut`, `chemin`, `ndp`, `schema_version`;

- `sens_vu`, `sens_courant`: indicator direction version the indicators
  were computed under / expected by this version of the application;

- `indicateurs`: computed indicators present on disk;

- `ugf`: readable management units (UGF) present;

- `indicateurs_perimes`: indicators computed under an older direction
  (they would be set aside by a migration);

- `migration_ugf`: management units missing or unreadable (a migration
  would create one unit per parcel);

- `migration_necessaire`: `TRUE` when
  [`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md)
  would refuse the project and
  [`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md)
  would change it;

- `archives`: set-aside indicator files
  (`metadata$indicateurs_perimes`).

Read-only.

## See also

Other api_hors_interface:
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
[`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
[`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md),
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

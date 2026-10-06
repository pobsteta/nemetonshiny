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

- `id`, `nom`, `statut`, `chemin`, `ndp`;

- `format_projet`, `format_ok`: project format, and whether this version
  of the application reads it (`FALSE` for a project created before
  1.0.0: it is not taken over and must be recreated);

- `indicateurs`: computed indicators present on disk;

- `ugf`: readable management units (UGF) present (otherwise the default
  layout, one unit per parcel, is created at the first opening);

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
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

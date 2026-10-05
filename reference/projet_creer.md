# Create a project

Creates the project and initialises it as the application does when it
opens a new project (management units: one per parcel; current indicator
direction), so it can be computed right away with
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md).

## Usage

``` r
projet_creer(
  nom,
  parcelles,
  description = "",
  proprietaire = "",
  profil_groupes = NULL
)
```

## Arguments

- nom:

  Project name (100 characters max).

- parcelles:

  sf object of cadastral parcels, as returned by
  [`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md).

- description, proprietaire:

  Optional text.

- profil_groupes:

  Optional management-unit group profile (`"onf"`, `"crpf"`, ...);
  default from the configuration.

## Value

The project id.

## See also

Other api_hors_interface:
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md),
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

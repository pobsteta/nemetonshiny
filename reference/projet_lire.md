# Read a project's results, without side effect

Builds the project exactly as the Synthesis tab sees it (indicators per
management unit, R5 from the linked monitoring zone, R6/R7 from
reGeneration, family scores through the core), **without** the
migrations that opening the project in the application runs. Nothing is
written.

## Usage

``` r
projet_lire(id, langue = "fr")
```

## Arguments

- id:

  Project id.

- langue:

  `"fr"` or `"en"`, for the family labels of `synthese`.

## Value

A list:

- `projet`: the project object (as consumed by the export functions);

- `indicateurs`: sf, one row per management unit (or `NULL` when not
  computed);

- `familles`: sf with the 12 `famille_*` columns (or `NULL`);

- `synthese`: global score, NDP and the 12 family scores, as plain
  values (same figures as the Synthesis tab).

## Errors

Class `nemetonshiny_projet_perime` when a migration would be needed (see
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md)
and
[`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md));
class `nemetonshiny_projet_introuvable` for an unknown id.

## See also

Other api_hors_interface:
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
[`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md),
[`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md),
[`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

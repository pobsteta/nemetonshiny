# Headless API

Functions to run a Nemeton diagnostic without the Shiny interface: list
and inspect projects, read their results, create and compute a project,
export a report or a GeoPackage.

Read functions
([`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md))
**never write** into a project. In particular,
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md)
does not run the format migrations that opening a project in the
application runs: when one would be needed, it fails with an error of
class `nemetonshiny_projet_perime` instead, and
[`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md)
applies it explicitly.

Projects live in the projects directory: `run_app(project_dir = )` in
the application; outside it, the `NEMETON_PROJECT_DIR` environment
variable, or the default directory.

## Errors

Errors are classed, so callers can react without parsing messages:

- `nemetonshiny_projet_introuvable`: unknown or invalid project id;

- `nemetonshiny_projet_perime`: a migration would be needed to read the
  project (the condition carries the
  [`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md)
  result in `$etat`);

- `nemetonshiny_sans_indicateurs`: the operation needs computed
  indicators;

- `nemetonshiny_calcul_echec`: the computation failed. All of them also
  inherit `nemetonshiny_erreur`.

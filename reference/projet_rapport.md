# PDF report of a project

PDF report of a project

## Usage

``` r
projet_rapport(
  id,
  fichier,
  langue = "fr",
  synthese = NULL,
  familles = NULL,
  sources = NULL
)
```

## Arguments

- id:

  Project id.

- fichier:

  Path of the PDF to write (its directory is created).

- langue:

  `"fr"` or `"en"`.

- synthese:

  Optional synthesis comment: one character string, Markdown allowed,
  with optional footnote references `[^1]`, `[^2]`... resolved against
  `sources`.

- familles:

  Optional family comments: a named list of character strings, names
  among the family codes `C`, `B`, `W`, `A`, `F`, `L`, `T`, `R`, `S`,
  `P`, `E`, `N`. Markdown allowed; footnote references `[^n]` are
  resolved against `sources` and renumbered per family. Empty comments
  are dropped.

- sources:

  Optional Markdown list of footnote definitions
  (`[^1]: author, title, p. N. <url>`), as produced by the documentary
  sources of the AI perspectives. Without it, references stay literal.

## Value

The path of the PDF. Quarto is used when installed, otherwise a simpler
PDF. Only `fichier` is written.

## See also

Other api_hors_interface:
[`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
[`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
[`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
[`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
[`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md),
[`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
[`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md),
[`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)

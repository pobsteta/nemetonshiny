# nemetonshiny

<!-- badges: start -->
[![R-CMD-check](https://github.com/pobsteta/nemetonshiny/actions/workflows/r.yml/badge.svg)](https://github.com/pobsteta/nemetonshiny/actions/workflows/r.yml)
[![Version](https://img.shields.io/github/v/release/pobsteta/nemetonshiny?logo=github&label=version&color=blue&sort=semver)](https://github.com/pobsteta/nemetonshiny/releases/latest)
[![pkgdown](https://github.com/pobsteta/nemetonshiny/actions/workflows/pkgdown.yaml/badge.svg)](https://pobsteta.github.io/nemetonshiny/)
[![codecov](https://codecov.io/gh/pobsteta/nemetonshiny/graph/badge.svg)](https://codecov.io/gh/pobsteta/nemetonshiny)
[![License: GPL v3+](https://img.shields.io/badge/License-GPL%20v3%2B-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

Application Shiny/golem de la plateforme d'analyse systémique forestière **Nemeton**.

`nemetonshiny` fournit l'interface. La logique métier (indicateurs, familles, NDP, FORDEAD, reGénération) est portée par le paquet [`nemeton`](https://github.com/pobsteta/nemeton) (>= 0.207.0), la desserte et l'accessibilité par [`foretaccess`](https://github.com/pobsteta/foretaccess) (>= 2.4.0).

## Fonctionnalités

- **Carte interactive** : sélection de parcelles cadastrales (Leaflet, API cadastre IGN)
- **Projets** : création, sauvegarde et restauration des diagnostics, découpage en unités de gestion (UGF)
- **Indicateurs Nemeton** en 12 familles : radar, scores et détail par famille
- **Suivi sanitaire** : surveillance rapide Sentinel-2, diagnostic FORDEAD, RECONFORT
- **Terrain** : accessibilité, desserte, plans d'échantillonnage QField, aller-retour Marculus
- **Plan d'actions** et **reGénération** (vulnérabilité climatique par UGF)
- **Perspectives IA** par profil d'expert (21 profils, `ellmer` : Mistral, Anthropic, OpenAI)
- **Exports** : rapports PDF (Quarto) et GeoPackage
- **Français et anglais**

## Prérequis

- **R** >= 4.1.0 (la CI teste la dernière version de R sous Ubuntu)
- Bibliothèques système : GDAL, GEOS, PROJ, udunits2, SQLite, OpenSSL, libcurl, libxml2, fontconfig, harfbuzz, fribidi, freetype, libpng, libtiff, libjpeg
  (sous Debian/Ubuntu : voir la liste exacte dans [`.github/workflows/r.yml`](.github/workflows/r.yml))
- **Chaîne Rust** (`rustc`, `cargo`, via [rustup](https://rustup.rs)) : `foretaccess` embarque un noyau compilé en Rust
- Pour les rapports PDF : [Quarto](https://quarto.org) et une distribution LaTeX (`xelatex`)

## Installation

```r
install.packages("pak")
# Deux dépendances du cœur hébergées sur GitHub
pak::pak(c("github::cran/dissUtils", "jbferet/spinR"))
# L'application ; les champs Remotes tirent les dernières releases de
# nemeton et foretaccess (« @*release », pak >= 0.11.1)
pak::pak("pobsteta/nemetonshiny")
```

## Utilisation

```r
nemetonshiny::run_app()
```

| Paramètre     | Rôle                                                                 | Défaut |
|---------------|----------------------------------------------------------------------|--------|
| `language`    | Langue de l'interface, `"fr"` ou `"en"`                              | langue du système |
| `project_dir` | Dossier des projets                                                  | `~/.local/share/nemeton/projects` sous Linux (`~/.nemeton/projects` sans `rappdirs`) |
| `max_parcels` | Nombre maximal de parcelles sélectionnables                          | `30` |
| `tour`        | Lancer la visite guidée au premier lancement                         | `TRUE` |
| `options`     | Options Shiny (`port`, `host`, `launch.browser`...)                   | `list()` ; navigateur ouvert seulement en session interactive |

Sur un serveur :

```r
nemetonshiny::run_app(tour = FALSE, options = list(port = 3838, host = "0.0.0.0"))
```

La configuration (base PostGIS, authentification OAuth, clés LLM et Theia, notifications) passe par des variables d'environnement, décrites avec le format des projets dans le [contrat public](CONTRAT.md).

### Docker

```bash
docker build -t nemetonshiny .
docker run -p 3838:3838 -v nemeton-projets:/data nemetonshiny
```

L'image tourne sous un utilisateur non root ; les projets vivent dans le volume `/data`. Le `docker-compose.yml` démarre en plus un Keycloak **de développement** (`start-dev`, realm `keycloak/realm-nemeton-dev.json` ; secret client et mots de passe à définir dans un `.env`, cf. `.env.example`, sans valeur par défaut) : il n'est pas fait pour la production.

## Développement

```r
pkgload::load_all(); nemetonshiny::run_app()   # lancer depuis les sources
devtools::test()                                # 117 fichiers de tests
```

Les tests de bout en bout (`shinytest2`) demandent Chrome. Les conventions du projet (i18n, modules, services, release) sont dans [`CLAUDE.md`](CLAUDE.md).

## Licence

[GPL-3 ou ultérieure](LICENSE.md). nemetonshiny importe le cœur `nemeton`, sous GPL-3 ; il était distribué sous EUPL v1.2 jusqu'au 2026-07-01 (texte conservé dans [LICENSE-EUPL.md](LICENSE-EUPL.md)).

Les donnees sont soumises a des conditions specifiques decrites dans [LICENSE-DATA.md](LICENSE-DATA.md).

# nemetonShiny

Application Shiny/golem pour la plateforme d'analyse systemique forestiere **Nemeton**.

`nemetonShiny` fournit l'interface utilisateur interactive. La logique metier (indicateurs, calculs, referentiels) est portee par le package [`nemeton`](https://github.com/pobsteta/nemeton) (>= 0.14.1).

## Fonctionnalites

- **Carte interactive** -- selection de parcelles cadastrales via Leaflet et l'API cadastre IGN
- **Gestion de projets** -- creation, sauvegarde et restauration de diagnostics forestiers
- **29 indicateurs Nemeton** -- calcul automatique sur les parcelles selectionnees
- **12 familles d'indicateurs** -- visualisation radar, scores et detail par famille
- **Synthese LLM** -- analyse contextuelle par 16 profils d'experts (ellmer / Mistral)
- **Export PDF & GeoPackage** -- rapports Quarto et donnees geospatiales
- **Internationalisation** -- francais et anglais (293 cles de traduction)
- **Accessibilite** -- conformite WCAG 2.1 AA

## Prerequis

- **R** >= 4.1.0
- **nemeton** >= 0.14.1 (package core)
- Librairies systeme : GDAL, GEOS, PROJ, libudunits2

### Dependances R principales

`shiny`, `bslib`, `leaflet`, `sf`, `terra`, `DT`, `promises`, `ellmer`, `httr2`, `yaml`, `jsonlite`.

Voir le fichier [`DESCRIPTION`](DESCRIPTION) pour la liste complete.

## Installation

```r
# Installer le package core nemeton
remotes::install_github("pobsteta/nemeton")

# Installer nemetonShiny
remotes::install_github("pobsteta/nemeton_shiny")
```

## Utilisation

### Lancement direct

```r
nemetonShiny::run_app(language = "fr")
```

Options disponibles :

| Parametre  | Description                   | Defaut |
|------------|-------------------------------|--------|
| `language` | Langue (`"fr"` ou `"en"`)     | `"fr"` |
| `options`  | Options Shiny (port, host...) | `list()`|

### Lancement avec Docker

```bash
docker compose up -d
```

Le `docker-compose.yml` demarre :
- **Keycloak** (port 8080) -- authentification OAuth2/OIDC
- **nemetonShiny** (port 3838) -- application Shiny

Variables d'environnement configurables dans `docker-compose.yml` :
- `NEMETON_LANG` -- langue (defaut : `fr`)
- `NEMETON_PAYS` -- pays (defaut : `FR`)
- `MISTRAL_API_KEY` -- cle API Mistral pour la synthese LLM

## Architecture

Le projet suit l'architecture [golem](https://thinkr-open.github.io/golem/) :

```
R/
  app_ui.R / app_server.R   -- UI et serveur principaux
  mod_*.R                    -- modules Shiny (auth, home, map, project,
                                family, synthesis, search, progress)
  service_*.R                -- services (cadastre, communes, compute,
                                db, export, project)
  llm_prompts.R              -- gestion des prompts LLM
  utils_i18n.R               -- internationalisation
  utils_theme.R              -- theme et styles

inst/
  app/i18n/                  -- traductions (fr.json, en.json)
  experts/                   -- 16 profils d'experts LLM (YAML)
  quarto/                    -- template de rapport PDF
  sql/                       -- schema et migrations
```

## Tests

```r
devtools::test()
```

20 fichiers de tests couvrant l'ensemble des modules et services.

## Licence

[EUPL v1.2](LICENSE-EUPL.md)

Les donnees sont soumises a des conditions specifiques decrites dans [LICENSE-DATA.md](LICENSE-DATA.md).

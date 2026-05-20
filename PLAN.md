# PLAN — Intégration des sources Theia / DATA TERRA (nemeton v0.40.0)

## Objectif

Câbler dans `nemetonshiny` la consommation des sources Theia exposées
par `nemeton` v0.40.0 pour que les 12 familles d’indicateurs se
calculent en NDP 0 à partir de données publiques Theia. Priorité :
débloquer la famille Production (P1 Volume, P2 Productivité, P3 Qualité)
et E1 (Bois-énergie), aujourd’hui en échec faute de CHM / d’inventaire
fournis aux fonctions `indicateur_*()` de `nemeton`.

## Rappel des règles (CLAUDE.md)

- Aucune logique métier dans l’app : on appelle les fonctions exportées
  de `nemeton`, on ne recalcule rien. La seule transformation tolérée
  est la conversion d’unités explicitement demandée (FORMSpoT décimètres
  → mètres).
- Tout texte UI passe par `i18n$t()` (FR/EN), nouvelles clés dans
  `R/utils_i18n.R`, accents en `\uXXXX`.
- Tests des modules via `testServer()`.

## Étapes

### A. DESCRIPTION

`Imports: nemeton (>= 0.40.0)`

`Remotes: pobsteta/nemeton@v0.40.0`

### B. Service Theia (`R/service_theia.R`, nouveau)

`theia_python_ready()` — vérifie `reticulate` + modules Python
`teledetection` / `pystac_client`.

`theia_api_key_configured()` — vérifie `TLD_ACCESS_KEY` /
`TLD_SECRET_KEY` ou `~/.config/teledetection/.apikey`.

`theia_save_api_key()` — persiste la clé (fichier `.apikey` + env vars
de session).

`theia_status()` — agrège l’état du pré-requis (python_ok, key_ok,
ready) pour l’UI et la gestion d’erreur.

`theia_compute_year()` — année cible FORMSpoT (option ou N-1).

`download_chm_theia()` — `load_theia_source("formspot", aoi, year)` puis
`/ 10` (décimètres → mètres), `sanitize_chm()` avec les masques
disponibles.

`download_theia_layers()` — charge fapar / snow / soil_moisture.

`theia_source_provenance()` — provenance / licence / consumed_by via
`get_data_source()`.

### C. Service de calcul (`R/service_compute.R`)

Étape CHM Theia dans `download_layers_for_parcels()` : FORMSpoT utilisé
quand LiDAR HD absent et que Theia est prêt, avant Open-Canopy.

`compute_single_indicator()` : passage de `species_field = "species"`,
`fapar` (C2), `snow` / `soil_moisture` (R3) quand la fonction `nemeton`
les accepte. `fvc` (A1) et `texture` (F1/F2) restent sur leur chemin
legacy (OSO / SoilGrids) tant que les assets Theia correspondants ne
sont pas confirmés côté `nemeton`.

Enrichissement BD Forêt (`species`/`age`) étendu à P1, P3, E1
(auparavant P2 seul) — débloque la famille Production.

### D. UI configuration Theia (`R/mod_theia_config.R`, nouveau)

Lien navbar (engrenage) ouvrant une modale.

Saisie clé API THEIA + statut du pré-requis Python/reticulate.

Affichage provenance / licence des sources Theia.

### E. Gestion d’erreur

CHM Theia indisponible (Python/clé manquants) → dégradation propre,
message i18n, le reste du calcul continue.

### F. Tests & i18n

Nouvelles clés i18n FR/EN dans `R/utils_i18n.R`.

`tests/testthat/test-service_theia.R`

`tests/testthat/test-mod_theia_config.R`

### G. Release

Bump `0.37.0` → `0.38.0` (feat: intégration Theia), NEWS.md.

## Journal

- 2026-05-20 — Câblage initial Theia / DATA TERRA, cycle dev 0.38.0.

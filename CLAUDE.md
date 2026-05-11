# CLAUDE.md — Nemetonshiny Development Reference

## Identité du projet

`nemetonshiny` est l’**application Shiny/golem** de la plateforme
Nemeton (analyse forestière systémique, 31 indicateurs / 12 familles /
radar / perspectives IA par profil). Le repo cœur métier est `nemeton`
(paquet R séparé) ; ici on **n’écrit pas de logique métier**, on appelle
les fonctions exportées de `nemeton` (\>= 0.20.1.9004) et on présente
les résultats à l’utilisateur.

L’extraction `nemeton` ↔︎ `nemetonshiny` a eu lieu en v0.15.0 (ADR-009).
Depuis, l’i18n, les profils experts, les modules Shiny, l’OAuth2, le LLM
et les services applicatifs (cadastre, communes, projet, monitoring,
sampling, export) vivent ici.

## Convention NMT (Néméton Naming Convention)

Toutes les clés techniques suivent cette norme :

- **snake_case français sans accent** : `famille_biodiversite`,
  `monitoring_ingest_starting`, `health_validation_status`
- **Translittération** : é→e, è→e, ê→e, à→a, ô→o, î→i, ù→u, ç→c
- **Codes courts en majuscules** : B, C, W, A, F, L, T, R, S, P, E, N,
  B1, C2
- **Maximum 30 caractères** par clé i18n
- **Pas d’abréviation** sauf codes établis

Le code legacy en anglais (`mod_home`, `service_compute`, `mod_family`)
reste tel quel — on ne renomme pas ce qui fonctionne. Les **clés i18n
nouvelles** doivent suivre la convention.

## Architecture (packages, ADR-009)

    nemeton                → Cœur métier : indicateurs, familles, NDP, normalisation, FORDEAD, monitoring DB.
    nemetonshiny (ce repo) → Application Shiny/golem : UI, modules, i18n, profils experts, LLM, OAuth2.
    tree_sat_nemeton       → Classification d'essences par Sentinel-1/2 (NDP 0).
    maestro_nemeton        → Classification d'essences par MAESTRO ViT (ortho+MNT, NDP 1+).
    opencanopynemeton      → Canopy Height Models (CHM) consommés par nemeton (spec 005).
    platform_nemeton       → Documentation plateforme, ADR, glossaire.

**Règle de dépendance** : les flèches vont **toujours** vers `nemeton`.
`nemetonshiny` importe `nemeton` (120+ fonctions exportées, commit
`720a433`) ; jamais l’inverse.

## Les 12 familles d’indicateurs

| Code | Famille | Indicateurs |
|----|----|----|
| B | Biodiversité | B1 (protection), B2 (structure), B3 (connectivité) |
| C | Carbone & Vitalité | C1 (biomasse), C2 (NDVI) |
| W | Eau & Régulation | W1 (réseau hydro), W2 (zones humides), W3 (TWI) |
| A | Air & Microclimat | A1 (couverture arborée), A2 (qualité air) |
| F | Fertilité des sols | F1 (fertilité), F2 (érosion) |
| L | Paysage | L1 (sylvosphère), L2 (fragmentation) |
| T | Dynamique temporelle | T1 (ancienneté), T2 (changement) |
| R | Risques & Résilience | R1 (feu), R2 (tempête), R3 (sécheresse), R4 (abroutissement), R5 (dépérissement, FORDEAD-conditionné — spec 008) |
| S | Social & Usages | S1 (routes), S2 (bâti), S3 (population) |
| P | Production & Économie | P1 (volume bois), P2 (station), P3 (qualité bois) |
| E | Énergie & Climat | E1 (bois-énergie), E2 (évitement carbone) |
| N | Naturalité | N1 (distance infra), N2 (continuité), N3 (composite) |

Détails et formules : voir `nemeton/CLAUDE.md`. Côté app, les noms de
famille / indicateurs sont **lus depuis `nemeton::INDICATOR_FAMILIES`**
— ne pas dupliquer la liste.

## Système NDP (Niveau De Précision) — ADR-011

Le NDP mesure la **qualité des données d’entrée**, pas le nombre de
familles calculées (les 12 sont toujours calculées). Niveaux 0..4,
pondération Fibonacci, confiance φ. Implémentation dans
`nemeton/R/ndp.R`. Les widgets HTML (`ndp_badge()`,
`ndp_progress_bar()`) ont été déplacés dans ce repo (commit `64ba7b1`
côté nemeton) et vivent dans `R/ndp.R`.

L’app actuelle est en **NDP 0** (sources publiques uniquement). Le score
global affiché par `mod_synthesis` provient de
[`nemeton::compute_general_index()`](https://pobsteta.github.io/nemeton/reference/compute_general_index.html)
— pas d’un [`mean()`](https://rdrr.io/r/base/mean.html) local.

## Profils experts (cible : 15, livrés : 13)

Les profils sont des fichiers YAML bilingues FR/EN dans
`inst/experts/*.yml`. Chacun définit un acteur de la filière forêt-bois
et son focus (familles prioritaires, ton, objectifs). Le module
`R/llm_prompts.R` les charge et construit les prompts envoyés au LLM
(Mistral / Anthropic / OpenAI via `ellmer`).

| Profil                   | Clé YAML              | Familles prioritaires |
|--------------------------|-----------------------|-----------------------|
| Propriétaire privé       | `proprietaire_prive`  | P, E, R               |
| Propriétaire public      | `proprietaire_public` | B, N, S               |
| Gestionnaire ONF         | `gestionnaire_onf`    | P, B, C, R            |
| Gestionnaire coopérative | `gestionnaire_coop`   | P, E, L               |
| Gestionnaire expert      | `gestionnaire_expert` | Toutes                |
| Technicien terrain       | `technicien`          | C, W, F, B            |
| Naturaliste              | `naturaliste`         | B, N, W, R            |
| Élu local                | `elu_local`           | S, L, R               |
| Élu régional             | `elu_regional`        | P, E, C, S            |
| Chasseur                 | `chasseur`            | R4, B, N              |
| Industrie bois           | `industrie_bois`      | P, E                  |
| Bûcheron / ETF           | `bucheron`            | P, S, F               |
| Chercheur                | `chercheur`           | Toutes                |
| Citoyen                  | `citoyen`             | S, L, A               |
| Investisseur             | `investisseur`        | C, P, E               |

Le profil `generalist.yml` est le fallback générique. Le YAML
`_example.yml.template` documente le format pour ajouter un nouveau
profil.

## 7 Bounded Contexts (DDD)

1.  **Inventaire** (`contexte_inventaire`) — collecte/validation données
    terrain et satellite (`mod_home`, `mod_search`, `mod_field_ingest`)
2.  **Analyse systémique** (`contexte_analyse`) — calcul des 31
    indicateurs via `nemeton`, agrégation 12 familles, radar, Fibonacci
    (`mod_synthesis`, `mod_family`, `service_compute`)
3.  **Cartographie** (`contexte_cartographie`) — Leaflet, parcelles,
    UGF, LiDAR, satellite (`mod_map`, `mod_ug`)
4.  **Santé** (`contexte_sante`) — surveillance rapide (NDVI/NBR
    rolling-window) + diagnostic FORDEAD (CRSWIR + harmonique via
    reticulate), R5, workflow QGIS/QField de validation. Voir spec 008
    et ADR-013. (`mod_monitoring`, `service_monitoring`,
    `service_monitoring_db`)
5.  **Aide à la décision** (`contexte_aide_decision`) — perspectives IA,
    profils experts (`R/llm_prompts.R`, `inst/experts/`)
6.  **Utilisateurs** (`contexte_utilisateurs`) — auth, profils, droits,
    partage (`mod_auth` via `shinyOAuth`)
7.  **Interopérabilité** (`contexte_interoperabilite`) — export IFN,
    GroundForest, QField/QGIS, OGC (`service_export`, `mod_sampling`)

## ADR (Architecture Decision Records)

Les ADR vivent dans `platform_nemeton/docs/`. Rappel des décisions
structurantes pour l’app :

| ADR | Décision |
|----|----|
| 001 | R/Shiny (golem), migration Plumber+Vue.js si \>50 users simultanés |
| 002 | GeoPackage (terrain) + PostGIS (plateforme) + S3 (rasters/LiDAR en COPC) |
| 003 | OVHcloud (principal) + Scaleway GPU L4 (ponctuel) |
| 004 | Mistral API (souveraineté FR), migration self-hosted possible |
| 005 | OAuth2/OIDC via AgentConnect → Keycloak fédéré pour l’Europe |
| 006 | EUPL v1.2 (plateforme) + MIT (packages R) + CC-BY 4.0 (données) |
| 007 | Pipeline NDP : TreeSatAI (NDP 0) → PureForest (NDP 1) → local (NDP 2+) |
| 008 | OGC, ETRS89/EPSG:3035 paneuropéen, INSPIRE, sources par pays |
| 009 | 4+ packages (nemeton cœur, nemetonshiny app, opencanopy, tree_sat, maestro) |
| 010 | Docker Compose + GitHub Actions CI/CD, 12-factor app |
| 011 | Nombre d’or : pondération Fibonacci, confiance φ, suite 1-1-2-3-5 |
| 012 | Extensions PG futures : TimescaleDB (monitoring continu) + pgvector (RAG perspectives IA) |
| 013 | **Suivi sanitaire** : FORDEAD via reticulate (CRSWIR + harmonique, GPL-3) en méthode officielle, hybridé avec rolling-window E6.a, 5 garde-fous applicatifs G1-G5 issus du rapport ONF/DSF 2024 |

## Walking Skeleton — Épaississements

`nemetonshiny` n’a **pas** son propre PLAN.md : la séquence des
épaississements (E1, E2, …) est partagée avec `nemeton` dans
**`nemeton/PLAN.md`** à la racine du repo voisin. Cette source unique de
vérité indique pour chaque épaississement quel package porte la
livraison (cœur ou app).

À chaque release `nemetonshiny`, on met à jour ce PLAN.md commun (cocher
la case + ajouter une entrée datée au journal) — cf. *Consignes de
release*, étape 8.

## Internationalisation (i18n) — cœur de cette app

L’i18n vit ici depuis v0.15.0. **Tout texte affiché passe par i18n** ;
jamais de littéral français/anglais dans un module.

- **Source unique** : liste `TRANSLATIONS` dans `R/utils_i18n.R` (~360
  clés, FR/EN). Une helper `export_translations_json()` permet un export
  JSON ponctuel pour les traducteurs.
- **API** : `i18n <- get_i18n(lang)` puis `i18n$t("clé")`.
  `i18n$language` retourne `"fr"` ou `"en"`.
- **Encodage** : caractères accentués obligatoirement en `\uXXXX` (ex.
  `synchronisé`, `Répertoire`). Cohérence avec le repo, et évite les
  surprises Windows / locale.
- **Prompts LLM** : bilingues, dans `inst/experts/*.yml` (sections
  `prompt_fr` et `prompt_en`).
- **Extension européenne** : DE, ES, IT prévus ultérieurement. Le format
  `list(fr=..., en=...)` se généralise sans modification structurelle
  (`list(fr=..., en=..., de=..., ...)`).

L’implémentation est **maison** (pas `shiny.i18n`) : refresh automatique
sans rebuild, support dynamique du switch langue.

## Authentification (E4)

- `mod_auth.R` consomme `shinyOAuth` (`Suggests`).
- Provider attendu : Keycloak (ADR-005), avec fédération AgentConnect
  côté ProConnect en France et eIDAS Europe.
- `keycloak/` à la racine du repo contient le compose dev local et les
  realms.
- Mode anonyme : la session démarre en `profil_citoyen` jusqu’à login.

## LLM (E3)

- `R/llm_prompts.R` charge les YAML `inst/experts/*.yml` et construit
  les prompts par profil sélectionné.
- Backend via `ellmer` — supporte Mistral (souverain FR, ADR-004),
  Anthropic (Claude), OpenAI.
- L’API key se lit dans une variable d’environnement (`MISTRAL_API_KEY`,
  `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`) — jamais en dur, jamais
  commitée.

## Stack technique

- **R \>= 4.1.0**
- **golem** pour la structuration Shiny
- **bslib** (Bootstrap 5) pour le layout, **bsicons** pour les icônes
- **leaflet** + **leaflet.extras** + **maptiles** pour la cartographie
- **plotly** pour le radar et les séries temporelles ; `fmsb` était
  utilisé avant la promotion de plotly en `Imports` (v0.21.0)
- **sf**, **terra**, **lwgeom** pour le spatial
- **promises** + **future** + **later** pour l’asynchrone
  ([`shiny::ExtendedTask`](https://rdrr.io/pkg/shiny/man/ExtendedTask.html),
  `future_promise`)
- **ellmer** pour l’intégration LLM multi-provider
- **shinyOAuth** pour l’authentification (Suggests, ADR-005)
- **DT** pour les tables, **htmltools** + **htmlwidgets** pour les tags
  HTML custom
- **DBI** + **RPostgres** pour PostgreSQL/PostGIS (Suggests)
- **arrow** + **geoarrow** pour Parquet/GeoArrow (Suggests)
- **i18n maison** dans `R/utils_i18n.R`
- **testthat** edition 3 (+ **shinytest2** prévu pour les tests E2E)
- **cicerone** pour les tours guidés (Suggests)
- **quarto** pour les exports rapport (Suggests)

`Remotes: pobsteta/nemeton` dans `DESCRIPTION` — installation locale en
dev via `pak::local_install("/home/pascal/dev/nemeton")` ou
`pkgload::load_all("/home/pascal/dev/nemeton")`.

## Commandes de référence

``` bash
# Lancer l'application en dev
Rscript -e 'pkgload::load_all("."); nemetonshiny::run_app()'

# Lancer tous les tests
Rscript -e 'devtools::test()'

# Lancer un test spécifique
Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-mod_monitoring.R")'

# Régénérer la documentation roxygen
Rscript -e 'devtools::document()'

# Vérifier le package
Rscript -e 'devtools::check()'

# Couverture de code
Rscript -e 'cat(covr::percent_coverage(covr::package_coverage(quiet=TRUE)))'

# Exporter les traductions JSON pour les traducteurs
Rscript -e 'pkgload::load_all("."); cat(nemetonshiny:::export_translations_json())'
```

## Conventions de code

### Modules Shiny (`mod_*.R`)

- Pattern **golem** : un module = `mod_xxx_ui(id)` +
  `mod_xxx_server(id, app_state, ...)`.
- `app_state` est un `reactiveValues` partagé (current_project,
  project_id, project_status, current_user, …).
- Les modules **n’appellent jamais directement** une fonction métier de
  calcul d’indicateur — ils passent par `R/service_*.R`.
- Pas de `observe()` orphelin : utiliser `observeEvent()` ou
  `bindEvent()` pour scoper le déclenchement.
- Sortir l’i18n du module sous forme de reactive (`i18n_r()`) si la
  langue peut changer pendant la session.

### Services (`service_*.R`)

- Logique applicative non-Shiny : appel des fonctions `nemeton::*`,
  accès DB, IO disque, cache.
- Une fonction = une responsabilité ; testable hors session Shiny.
- `R/service_compute.R` : `ExtendedTask` + `future_promise` pour les
  calculs longs (calcul indicateurs, ingestion, export).
- `R/service_monitoring.R` / `service_monitoring_db.R` : adaptateurs
  pour le contexte santé (E6).
- `R/service_db.R` : connexion PostGIS, résolution
  `NEMETON_DB_URL > POSTGRESQL_ADDON_* > NEMETON_DB_HOST/...`.

### Fonctions R

- Documentation **roxygen en anglais** (cohérence avec `nemeton`).
- Commentaires inline en français OK.
- Nouveaux helpers : préfixés `.` si privés au fichier
  (`.resolve_db_config`, `.coerce_ndp_level`).

### i18n — règles strictes

- **Aucun littéral français/anglais** dans une UI. Toujours
  `i18n$t("clé")`.
- Une nouvelle clé i18n = une entrée dans `TRANSLATIONS`
  (`R/utils_i18n.R`), dans la section thématique appropriée (Project,
  Monitoring, Sampling, …).
- Encodage `\uXXXX` pour les caractères accentués.
- Si un texte combine i18n + variables, utiliser
  `sprintf(i18n$t("foo_with_count"), n)` avec `%d` / `%s` dans la
  traduction.

### Tests

- Framework : **testthat** edition 3.
- Nommage : `tests/testthat/test-{module|service}.R`.
- `testServer()` pour les modules Shiny (contribue à covr).
- `shinytest2::AppDriver` pour les tests E2E (ne contribue pas à covr) —
  `on.exit(app$stop())` obligatoire.
- Pour mocker une fonction métier :
  `testthat::local_mocked_bindings(nemeton_fonction = stub, .package = "nemeton")`.
- Variables DB de test : `NEMETON_DB_URL_TEST` (gitignore via
  `.Renviron`).

### Données de test

- `withr::with_envvar(c(...))` pour neutraliser les vars d’env DB /
  OAuth dans les tests.
- [`withr::with_tempdir()`](https://withr.r-lib.org/reference/with_tempfile.html)
  pour les fichiers temporaires (sauvegardes projet, exports).
- Fixtures projet : `tests/testthat/fixtures/`.

## Fichiers clés

    R/app_ui.R                    → UI principale (navbarPage, onglets)
    R/app_server.R                → Server principal, app_state global
    R/run_app.R                   → Point d'entrée golem
    R/app_config.R                → Options app (project_dir, theme, ...)

    R/mod_home.R                  → Sélection commune/cadastre, carte, chargement projet
    R/mod_search.R                → Recherche commune (BAN)
    R/mod_map.R                   → Carte Leaflet partagée
    R/mod_project.R               → Métadonnées projet (nom, owner, profil groupes, dossier)
    R/mod_ug.R                    → Unités de Gestion Forestière (UGF)
    R/mod_synthesis.R             → Score global, radar, perspective IA
    R/mod_family.R                → Vue détaillée par famille
    R/mod_sampling.R              → Plan d'échantillonnage terrain (GRTS, TSP, QGIS export)
    R/mod_field_ingest.R          → Ingestion GPKG terrain (E5.b) + validation sanitaire (E6.c.5)
    R/mod_monitoring.R            → Suivi sanitaire — surveillance rapide + diagnostic FORDEAD (E6)
    R/mod_progress.R              → Toasts/notifications de progression unifiées
    R/mod_auth.R                  → OAuth2/OIDC via shinyOAuth (E4)

    R/service_cadastre.R          → API cadastre IGN
    R/service_communes.R          → BAN, INSEE, départements
    R/service_project.R           → CRUD projet (disque + DB), métadonnées
    R/service_compute.R           → Calcul async des 31 indicateurs (ExtendedTask)
    R/service_db.R                → Connexion PostGIS (résolution env vars)
    R/service_export.R            → Export Quarto, PDF, IFN, GroundForest
    R/service_monitoring.R        → ExtendedTask pour ingest Sentinel-2 + run FORDEAD
    R/service_monitoring_db.R     → Adapter env→URL pour nemeton::db_connect (TimescaleDB)

    R/llm_prompts.R               → Profils experts → prompts LLM (ellmer)
    R/utils_i18n.R                → TRANSLATIONS FR/EN (~360 clés)
    R/utils_theme.R               → Thème bslib, palettes
    R/ndp.R                       → Widgets NDP (badge, progress bar)
    R/config_groupes.R            → Profils groupes UGF (ONF/CRPF/OFB/Generic)
    R/domain_ug.R                 → Helpers domaine UGF
    R/migrate.R                   → Migrations métadonnées projet entre versions
    R/zzz.R                       → .onLoad / .onAttach

    inst/app/                     → Assets statiques (www/css/js/img)
    inst/config/                  → Fichiers de config (golem)
    inst/experts/                 → 13 profils experts YAML + template + generalist
    inst/extdata/                 → Données embarquées (référentiels, schémas)
    inst/quarto/                  → Templates Quarto pour exports rapport
    inst/sql/                     → Migrations PostGIS (schema, indexes)

    keycloak/                     → Compose dev + realms pour auth locale
    .Renviron                     → Vars locales (NEMETON_DB_URL, OAuth secrets) — gitignore

# Consignes de release pour ce projet

À chaque push qui modifie le code fonctionnel (hors doc pure, hors CI),
Claude doit :

1.  Déterminer le type de changement selon Conventional Commits (feat: /
    fix: / BREAKING CHANGE:) → bump semver correspondant (minor / patch
    / major).

2.  Mettre à jour le numéro de version dans :

    - DESCRIPTION (champ Version) \[projet R\]
    - NEWS.md (ajouter une entrée datée)
    - CITATION.cff si présent

3.  Créer un tag git annoté : git tag -a vX.Y.Z -m “Release X.Y.Z”

4.  Pousser le tag : git push origin vX.Y.Z

5.  Créer la release GitHub via gh : gh release create vX.Y.Z
    –generate-notes

6.  Vérifier que les badges du README pointent vers la bonne version et
    la dernière release (badges shields.io, R-CMD-check, codecov…).

7.  Si le CHANGELOG.md existe, ajouter la section \[X.Y.Z\] - YYYY-MM-DD
    avec les catégories Added / Changed / Fixed / Removed.

8.  Mettre à jour le **`PLAN.md` du repo `nemeton`** (source unique de
    vérité partagée) : cocher la case du sous-chantier livré, ajouter
    une entrée datée au journal qui indique explicitement le commit
    `nemetonshiny@SHA` et le cycle dev. Ne jamais clore un chantier dans
    `PLAN.md` sans qu’une release correspondante ait été poussée.

## Cycle dev vs release stable

Le repo suit un cycle dev `X.Y.Z.9000+` qui s’accumule entre les
releases stables. Chaque commit fonctionnel pousse le quatrième segment
(`.9001`, `.9002`, …) sans tag ; quand un chantier est complet
(épaississement clos, hardening fini, etc.), on bumpe en `vX.Y.Z` stable
et on crée le tag + release GitHub. Le PLAN.md indique pour chaque
entrée le cycle dev concerné (ex. `0.21.0.9000` → `0.21.0.9001`).

## Règles de cohérence

- La version dans DESCRIPTION, le tag git et la release GitHub doivent
  être strictement identiques.
- Ne jamais pousser un tag sans avoir d’abord mis à jour DESCRIPTION et
  NEWS.md.
- Vérifier que la page de documentation (pkgdown) est aussi à jour de la
  version et de ses tags.
- Toujours demander confirmation avant un bump majeur.
- Quand un changement implique aussi `nemeton`, faire les deux releases
  dans l’ordre cœur → app (l’app dépend du cœur, jamais l’inverse).

## Workflow de push (branche dev → main)

Le workflow est en **deux temps** et doit être suivi systématiquement.

### Temps 1 — Push sur la branche dev (automatique)

Par défaut Claude pousse **toujours** sur la branche de développement
imposée par la session (ex. `claude/fix-action-plan-data-9wgpI`). Ce
push **ne nécessite pas** de confirmation supplémentaire — c’est le
fonctionnement nominal.

Lors d’un commit fonctionnel sur la branche dev, Claude bumpe le **cycle
dev** (`X.Y.Z.9000+` → `X.Y.Z.9001+`) conformément à la section *Cycle
dev vs release stable*, met à jour `NEWS.md` et `DESCRIPTION`
localement, commit, puis `git push -u origin <branche-dev>`. Pas de tag,
pas de release GitHub à cette étape.

### Temps 2 — Merge vers `main` + release (sur autorisation)

**Après chaque push réussi sur la branche dev**, Claude **doit
systématiquement demander** à l’utilisateur via une question dédiée s’il
faut merger sur `main` et déclencher la release. Le récapitulatif
présenté doit contenir, en suivant les règles déjà décrites dans
*Consignes de release* et *Règles de cohérence* :

1.  La **branche source** (ex. `claude/fix-action-plan-data-9wgpI`) et
    le ou les commits concernés (SHA + sujet).
2.  Le **bump de version stable** proposé (`X.Y.Z` → `X.Y.Z+1` patch /
    `X.Y+1.0` minor / `X+1.0.0` major) déduit du type Conventional
    Commit (`fix:` / `feat:` / `BREAKING CHANGE:`). Pour un bump
    **majeur**, demander une confirmation supplémentaire explicite
    (cf. *Règles de cohérence*).
3.  Les **fichiers de version** à mettre à jour : `DESCRIPTION`,
    `NEWS.md` (résumé de l’entrée datée), `CITATION.cff` si présent,
    `CHANGELOG.md` si présent (section `[X.Y.Z] - YYYY-MM-DD` avec
    Added/Changed/Fixed/Removed).
4.  Le **tag git annoté** prévu (`git tag -a vX.Y.Z -m "Release X.Y.Z"`)
    et son push (`git push origin vX.Y.Z`).
5.  La **release GitHub** prévue
    (`gh release create vX.Y.Z --generate-notes`).
6.  L’impact sur le **`PLAN.md` du repo `nemeton`** : case à cocher +
    entrée datée du journal mentionnant `nemetonshiny@SHA` et le cycle
    dev (cf. *Consignes de release* étape 8).
7.  Toute release couplée côté `nemeton` requise par l’ordre **cœur →
    app** (l’app dépend du cœur, jamais l’inverse).
8.  Vérification que les **badges du README** et la **doc pkgdown**
    pointent vers la nouvelle version.

Tant que l’utilisateur n’a pas explicitement autorisé ce temps 2, Claude
**ne fait pas** : `git checkout main`, `git merge`,
`git push origin main`, `git tag -a vX.Y.Z`, `git push origin vX.Y.Z`,
ni `gh release create`.

Une autorisation vaut **pour la seule release récapitulée** : chaque
nouveau cycle (nouveau push dev → nouvelle question de merge) demande
une nouvelle confirmation.

## Règles strictes

1.  **Aucune logique métier** dans `nemetonshiny` (indicateurs,
    familles, NDP). Tout passe par `nemeton`.
2.  Aucune logique métier dans `R/mod_*.R` non plus — les modules
    consomment `R/service_*.R`.
3.  **i18n obligatoire** : `i18n$t("clé")` partout, jamais de littéral.
4.  **Encodage `\uXXXX`** pour les caractères accentués dans les sources
    R (`utils_i18n.R` notamment).
5.  Chaque nouvelle fonction exportée a un test dans `tests/testthat/`.
6.  Les rasters et le LiDAR ne sont JAMAIS stockés en DB (ADR-002) —
    seulement les agrégats.
7.  Pas de dépendance inverse : `nemeton` n’importe JAMAIS
    `nemetonshiny`.
8.  Pas de secret en dur : OAuth, API LLM, DB → variables
    d’environnement (`.Renviron` gitignore).
9.  Pas de [`print()`](https://rdrr.io/r/base/print.html) /
    [`cat()`](https://rdrr.io/r/base/cat.html) /
    [`message()`](https://rdrr.io/r/base/message.html) en code de prod :
    passer par `cli::cli_*` ou
    [`shiny::showNotification`](https://rdrr.io/pkg/shiny/man/showNotification.html)
    selon contexte.
10. Quand je travaille sur une tâche longue, maintenir le `PLAN.md` du
    repo `nemeton` à jour à chaque étape terminée (chantier en cours +
    journal).
11. **Push branche dev = automatique ; merge `main` + tag + release =
    autorisation explicite**. Après chaque push réussi sur la branche
    dev, Claude doit systématiquement demander s’il faut merger sur
    `main` en présentant le récapitulatif décrit dans *Workflow de push
    (branche dev → main)*. Sans autorisation explicite, jamais de
    `git push origin main`, jamais de tag, jamais de
    `gh release create`, jamais de merge.

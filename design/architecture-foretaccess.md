---
stepsCompleted: ['step-01-init', 'step-02-context', 'step-03-starter', 'step-04-decisions', 'step-05-patterns', 'step-06-structure', 'step-07-validation', 'step-08-complete']
inputDocuments: ['design/prd-foretaccess.md', 'CLAUDE.md']
workflowType: 'architecture'
project_name: 'nemetonshiny'
user_name: 'Pascal'
date: '2026-07-22'
scope: 'ForêtAccess / Terrain accessible (accessibilité + desserte)'
lastStep: 8
status: 'complete'
completedAt: '2026-07-22'
---

# Architecture Decision Document — ForêtAccess (Terrain accessible)

_Ce document se construit collaborativement, étape par étape. Les sections sont
ajoutées au fil des décisions d'architecture. Périmètre : la capacité « Terrain
accessible » de nemetonshiny (accessibilité d'exploitation + création de desserte),
traçable au PRD `design/prd-foretaccess.md` (FR1–26 / NFR)._

## Project Context Analysis

### Requirements Overview

**Functional Requirements** — 26 FR / 8 domaines. Architecturalement : **deux
capacités utilisateur** (accessibilité, desserte) sur un **substrat commun**
(acquisition IGN / OSM, `preprocess`, worker async, carte, cache, export). Les
FR22–26 (Growth) partagent le même substrat — extensibilité par **ajout de moteurs**,
pas de refonte.

**Non-Functional Requirements** — pilotent les choix : perf < 5 min → **async +
cache** ; sécurité → **OAuth + verrou lecture seule** ; scalabilité → **cache disque,
pas de rasters en DB** ; intégration → **adaptateur `foretaccess`** ; fiabilité →
**erreurs structurées + garde-fous mémoire**.

**Scale & Complexity** — domaine web / géospatial ; complexité **moyenne-haute** ;
~5 composants (`mod_accessibility`, `mod_desserte`, `service_accessibility`,
`service_desserte`, `service_foretaccess_io`) + substrat i18n / thème / carte.

- Primary domain : web / géospatial (scientifique)
- Complexity level : moyenne-haute (dans une app globale *high*)
- Estimated architectural components : ~5 (2 modules + 3 services) + substrat partagé

### Technical Constraints & Dependencies

Règles 1/2 (aucune logique métier dans l'app) ; dépendance **app → foretaccess /
nemeton** via `@*release` (jamais l'inverse) ; ADR-002 (rasters / LiDAR jamais en DB) ;
ADR-005 (OAuth2/OIDC) ; i18n maison `\uXXXX` ; structure golem. Toolchain Rust (CI)
pour le noyau ForêtAccess.

### Cross-Cutting Concerns Identified

- Cycle de vie worker `future` + libération mémoire.
- Sérialisation inter-process (sf **par fichier**, rasters **par chemin**).
- Cache par projet **et par emprise** (invalidation sur changement de tampon).
- Budget performance < 5 min (glouton desserte en dépassement — dette cœur).
- Dispatch S4 `terra` (`%in%` closure retombe sur `base` → primitives / `values()`).
- Signalement provenance / qualité (badges DFCI, `connexe`).
- i18n FR/EN.
- Couplage à la cadence des releases cœur (`@*release`).

## Starter Template Evaluation

### Primary Technology Domain

Web app **R/Shiny géospatial** — **brownfield** : fondation déjà en place.

### Established Foundation (brownfield — pas de nouveau starter)

Le « starter » équivalent est **`golem`** (packaging Shiny), **déjà initialisé**. L'ajout
ForêtAccess se **greffe** sur la structure existante (`mod_*` / `service_*`) — aucune
commande d'initialisation, aucun nouveau socle à évaluer. *(Recherche web de starter non
applicable : projet existant, versions pinnées dans `DESCRIPTION`.)*

### Fondation technique établie (source : CLAUDE.md)

- **Langage / runtime** : R ≥ 4.1.0 ; package **golem**.
- **UI** : **bslib** (Bootstrap 5) + bsicons.
- **Cartographie / spatial** : **leaflet** (+ leaflet.extras, maptiles) ; **sf / terra /
  lwgeom** ; **happign** (WMS IGN).
- **Asynchrone** : **promises + future + later** (`ExtendedTask`, `future_promise`).
- **Viz / tables** : plotly ; DT ; htmltools / htmlwidgets.
- **Métier** : **foretaccess** (`@*release`) ; **nemeton**.
- **i18n** : maison (`\uXXXX`, FR/EN).
- **Tests** : testthat ed. 3 (+ shinytest2 E2E).
- **Déploiement** : Docker Compose + GitHub Actions (ADR-010) ; **toolchain Rust** en CI
  pour le noyau ForêtAccess.

### Rationale

Réutiliser la fondation golem **garantit la cohérence** (mêmes patrons de modules /
services, i18n, thème, async). L'ajout ForêtAccess suit les conventions existantes —
**pas de story d'initialisation** de projet.

## Core Architectural Decisions

_Vérification web des versions : non applicable (brownfield, dépendances pinnées dans
`DESCRIPTION`). Décisions héritées à ne pas re-trancher : PostGIS sans rasters en DB
(ADR-002), OAuth2/OIDC Keycloak / AgentConnect (ADR-005), Docker Compose + GH Actions
(ADR-010), Shiny réactif (golem), async future / promises._

### Decision Priority Analysis

**Critical (bloquent l'implémentation)** : D1 (modèle d'exécution), D2 (frontière de
sérialisation), D3 (cache), D4 (intégration métier).
**Important (façonnent l'archi)** : D5 (rendu carte), D6 (extensibilité moteurs),
D7 (contrat d'erreur), D8 (contrôle d'accès).
**Différées (post-MVP)** : moteurs lourds (câble / Steiner / optimiseurs) *gated* sur la
performance cœur.

### Data Architecture

- **D3 — Cache disque par projet** : `cache/accessibility`, `cache/desserte`, clé = moteur
  + tampon / emprise ; **sidecar RDS** pour les scalaires de badge ; **aucun raster en DB**
  (ADR-002). Rechargé au montage de l'onglet. *Rationale* : NFR-SC2, survivre au
  redémarrage (NFR-R2), éviter le recalcul (glouton 11,5 min). *Affecte* : FR12, NFR-P3.

### Authentication & Security

- **D8 — Garde `deny_if_readonly`** sur les déclencheurs de run (verrou projet) ; OAuth2/OIDC
  hérité. *Rationale* : intégrité multi-utilisateurs. *Affecte* : FR20 / 21, NFR-S1 / S3.

### API & Communication Patterns

- **D2 — Frontière de sérialisation** : AOI passée au worker **par fichier** (GeoPackage),
  rasters renvoyés **par chemin** (écrits worker-side). *Rationale* : `SpatRaster` / `sf`
  portent des pointeurs externes non sérialisables inter-process (contrainte dure).
  *Affecte* : D1.
- **D4 — Intégration métier** : consommer **uniquement les fonctions exportées de
  `foretaccess`** (`preprocess`, `surface_cout_construction`, `reseau_desserte`, moteurs)
  via `@*release` ; **aucune logique métier** app (règles 1/2) ; IO mutualisées dans
  `service_foretaccess_io.R` ; acquisition IGN (`happign` WMS, override MNT HIGHRES) + OSM.
  *Affecte* : NFR-I1 / I2.
- **D7 — Contrat d'erreur** : retours structurés `list(status="error", reason=<clé i18n>,
  detail=…)` → toast i18n ; jamais d'exception franchissant la frontière worker ;
  best-effort `tryCatch`. *Affecte* : NFR-R1.

### Frontend Architecture

- **D1 — Modèle d'exécution asynchrone** : `ExtendedTask` + `future_promise` (multisession),
  retour immédiat + chrono, libération mémoire `on.exit`. *Rationale* : cible < 5 min + UI
  non bloquante (NFR-P1 / P2), parité FAST / FORDEAD / RECONFORT. *Alternatives rejetées* :
  synchrone (gèle l'UI), callr / mirai (future déjà en stack).
- **D5 — Rendu carte** : Leaflet + **map panes dédiés** (zIndex fixe) + `leafletProxy` pour
  les overlays (stables au changement de fond) ; rasters catégoriels colorés par **table
  sémantique classe → couleur** (pas positionnelle) ; `hors_foret` masqué à NA (transparence)
  via **primitives `terra`** (`values()` / `is.na()`, pas `%in%` — piège dispatch S4).
  *Affecte* : FR7–10.
- **D6 — Extensibilité des moteurs** : moteurs déclarés en **liste nommée**
  (`ACCESSIBILITY_ENGINES` / `DESSERTE_ENGINES`) pilotant checkbox / radio + dispatch worker ;
  ajouter un moteur Growth = 1 ligne une fois la perf cœur levée ; **opt-in** pour les moteurs
  « calcul long ». *Affecte* : FR22–26.

### Infrastructure & Deployment

Hérité (ADR-010) : Docker Compose + GitHub Actions ; **toolchain Rust en CI** pour le noyau
ForêtAccess. Pas de nouvelle décision.

### Decision Impact Analysis

**Séquence d'implémentation** : D2 / D4 (frontière + adaptateur) → D1 (worker) → D3 (cache)
→ D5 (carte) → D6 / D7 / D8 (extensibilité / erreurs / accès).
**Dépendances croisées** : D1 dépend de D2 ; D3 dépend de D2 (rasters par chemin) ; D6
s'appuie sur D4 (dispatch vers `foretaccess`).

## Implementation Patterns & Consistency Rules

_Codifient CLAUDE.md + les patterns établis des onglets Accessibilité / Desserte._

### Naming Patterns

- **Clés i18n** (convention NMT) : snake_case FR **sans accent**, **préfixe par capacité**
  (`acc_*`, `dess_*`, `desserte_*`), accents en **`\uXXXX`**, ≤ 30 caractères.
- **Fonctions** : helpers privés préfixés `.` (`.acc_mask_hors_foret`) ; roxygen **en
  anglais**, commentaires inline FR OK.
- **Fichiers** : `mod_<nom>.R` + `service_<nom>.R` ; tests `test-<mod|service>.R`.
- **Moteurs** : vecteur nommé `<CAP>_ENGINES` (`ACCESSIBILITY_ENGINES`, `DESSERTE_ENGINES`).
- **Cache** : `cache/<capacité>/`.

### Structure Patterns

- **Module** = `mod_xxx_ui(id)` + `mod_xxx_server(id, app_state, …)` ; **aucune logique
  métier** dans un module (règle 2).
- **Service** = non-Shiny, **une responsabilité**, testable hors session ; appels
  `foretaccess::*` uniquement.
- **IO partagées** : `service_foretaccess_io.R`.
- **Tests** : `tests/testthat/` ; `testServer` pour les modules ; `foretaccess` **mocké**
  pour les services (données `toy`).

### Format Patterns

- **Contrat de retour worker** : `list(status = "success"|"error", reason = <clé i18n>,
  detail = …, <payload>)`.
- **Rasters** : catégoriels `value` / `classe` ; `hors_foret` **masqué à NA** ; couleurs
  par **table sémantique**.
- **CRS** : **EPSG:2154** (Lambert-93) sur tout le pipeline.
- **Unités** : mètres en interne, **km** en input UI.

### Communication & Process Patterns

- **Async** : `ExtendedTask` + `bind_task_button` ; **retour immédiat** (toast + chrono
  `.running_notif_content`), bouton grisé pendant le run ; **opt-in** « calcul long ».
- **Erreurs** : retours structurés → toast i18n ; `tryCatch` best-effort ; **jamais
  d'exception** franchissant la frontière worker.
- **Accès** : `deny_if_readonly` au déclenchement d'un run.
- **Carte** : overlays via `leafletProxy` dans un **pane dédié** ; respecter la décoche
  `input$map_groups`.

### Enforcement Guidelines — All AI Agents MUST

- **i18n partout** (aucun littéral FR/EN), accents en `\uXXXX`.
- **Aucune logique métier** dans l'app (règles 1/2) — passer par `foretaccess` / `nemeton`.
- **EPSG:2154** ; contrat d'erreur structuré ; couleurs de boutons **sémantiques**.
- **terra** : primitives / `values()` — **jamais `rast %in% codes`** (dispatch S4 →
  `base::%in%`, source du bug `hors_foret` opaque).

### Anti-Patterns (à éviter)

- Littéral FR/EN dans l'UI · `rast %in% codes` sur un `SpatRaster` · calcul métier
  (`mean()`, indicateurs) dans l'app · calcul lourd **synchrone** dans un `observeEvent`
  (gèle l'UI) · passage d'un `sf` / `SpatRaster` **vivant** au worker · deux verts /
  couleur de bouton non sémantique.

## Project Structure & Boundaries

### Complete Project Directory Structure (périmètre ForêtAccess)

```
nemetonshiny/
├── R/
│   ├── app_ui.R                    # nav_panels « Accessibilité » + « Desserte »
│   ├── app_server.R                # mod_accessibility_server + mod_desserte_server
│   ├── mod_accessibility.R         # UI + serveur accessibilité (FR1–4,7–10,17,19)
│   ├── service_accessibility.R     # run_accessibility(), cache, export (worker-side)
│   ├── mod_desserte.R              # UI + serveur desserte (FR5–6,7–10,18)
│   ├── service_desserte.R          # run_desserte(), cache, export (worker-side)
│   ├── service_foretaccess_io.R    # IO partagées (.resolve_project_aoi_2154, MNT HIGHRES)
│   ├── utils_i18n.R                # clés acc_* / dess_* / desserte_* (FR/EN, \uXXXX)
│   ├── utils_theme.R               # couleurs sémantiques de boutons
│   └── ndp.R                       # widgets NDP (transverse)
├── tests/testthat/
│   ├── test-mod_accessibility.R    # testServer + rendu carte/masque
│   ├── test-service_accessibility.R
│   ├── test-mod_desserte.R
│   └── test-service_desserte.R     # pipeline end-to-end sur données toy
├── inst/app/www/                   # assets partagés (css/js)
└── DESCRIPTION                     # Imports: foretaccess (>= 1.5.0), Remotes @*release

# Runtime (hors dépôt, par projet) :
<project_dir>/cache/accessibility/  # acc_<engine>.tif, accessibilite.gpkg, emprise_<m>m/
<project_dir>/cache/desserte/       # reseau_<engine>.tif + .rds, desserte.gpkg, emprise_<m>m/
```

### Architectural Boundaries

- **Frontière UI** : `mod_*_ui` / `nav_panel` (app_ui) — présentation seule.
- **Frontière logique app** : `service_*.R` (non-Shiny, testable hors session) — orchestration.
- **Frontière métier (dure, règles 1/2)** : `foretaccess::*` (paquet externe, `@*release`) —
  **tout calcul**.
- **Frontière worker** : `future_promise` — AOI par fichier, rasters par chemin (D2).
- **Frontière données** : **cache disque par projet** (pas de rasters en DB, ADR-002) ;
  acquisition IGN (WMS) / OSM.

### Requirements to Structure Mapping

- **Accessibilité** (FR1–4, 17, 19) → `mod_accessibility.R` + `service_accessibility.R`.
- **Desserte** (FR5–6, 18) → `mod_desserte.R` + `service_desserte.R`.
- **Cartographie** (FR7–10) → observers `renderLeaflet` + `leafletProxy` des deux modules.
- **Exécution & cache** (FR11–14) → `run_*()` + helpers cache des services ; `ExtendedTask`
  des modules.
- **Interop & export** (FR15–16) → `export_*_geopackage()` des services.
- **Accès** (FR20–21) → `deny_if_readonly()` au déclenchement des runs.
- **Growth** (FR22–26) → étendre `ACCESSIBILITY_ENGINES` / `DESSERTE_ENGINES` + câbler le moteur.
- **Acquisition partagée** → `service_foretaccess_io.R`.

### Integration Points & Data Flow

**Flux** : événement UI → `ExtendedTask` → worker `future` → `run_*()` → acquisition (IO
IGN / OSM) → `foretaccess::preprocess` + moteur → écriture raster + GPKG dans le cache →
retour **chemins + scalaires** → process principal relit → `leafletProxy` (carte) + badges.
**Externes** : IGN (`happign` WMS, MNT HIGHRES), OSM (via `foretaccess::acquire_*`), export
**GeoPackage** (QGIS / QField).

## Architecture Validation Results

### Coherence Validation ✅

Décisions D1–D8 **cohérentes** (architecture réellement implémentée, non hypothétique). Les
patterns codifient CLAUDE.md ; la structure mappe aux fichiers réels. **Aucune contradiction**
(versions pinnées `DESCRIPTION`, stack unique golem).

### Requirements Coverage Validation ✅ (réserves Growth)

- **FR1–21 (MVP)** : supportés, **implémentés et testés** (`test-*_accessibility`,
  `test-*_desserte`).
- **FR22–26 (Growth)** : **supportés architecturalement** (extensibilité D6) mais **gated**
  sur la performance cœur.
- **NFR** : P1–P3 **tenus** ; **P4 (glouton < 5 min) NON tenu** → dette cœur (mitigée
  opt-in / cache) ; S1–S3, SC1–SC2, R1–R2 **couverts** ; A2 **partiel** (WCAG en backlog).

### Implementation Readiness Validation ✅

Décisions documentées + tracées aux FR / NFR ; patterns **déjà appliqués** (exécutoires) ;
structure complète (arbre réel) ; exemples concrets (code livré).

### Gap Analysis Results

- **Critiques** : **aucun** — le MVP fonctionne.
- **Importants (côté cœur, pas archi app)** : NFR-P4 (perf glouton) ; sémantique
  `connexe = FALSE`. **Mitigés côté app** (opt-in + cache), à lever côté `foretaccess`
  (briefs perf / connexité).
- **Nice-to-have** : audit **WCAG** ; volume P1 (`nemeton`) pour le **typage** (couplage
  cœur) ; **surcoût eau**.

### Architecture Completeness Checklist

**✅ Requirements Analysis** — contexte analysé, échelle évaluée, contraintes identifiées,
préoccupations transverses mappées.
**✅ Architectural Decisions** — décisions critiques documentées, stack spécifiée, intégration
définie, performance adressée.
**✅ Implementation Patterns** — nommage, structure, communication, process documentés.
**✅ Project Structure** — arbre complet, frontières établies, points d'intégration mappés,
mapping FR → structure complet.

### Architecture Readiness Assessment

**Overall Status : READY FOR IMPLEMENTATION** (MVP livré v0.109 → v0.112).
**Confidence : HIGH** pour le MVP (implémenté + testé) ; **MEDIUM** pour le Growth (bloqué
perf cœur).
**Forces** : séparation stricte des couches (règles 1/2), async éprouvé, cache, traçabilité
FR → fichiers.
**Enhancements futurs** : perf des moteurs lourds, typage réseau, multi-pays.

### Implementation Handoff

**Agents IA** : suivre les décisions D1–D8 + patterns + frontières à la lettre.
**Première priorité** : Growth déjà spécifié — **attendre la perf cœur**, puis étendre
`<CAP>_ENGINES` (1 ligne) + câbler le moteur.

## Architecture Completion Summary

### Workflow Completion

**Architecture Decision Workflow : COMPLETED ✅**
**Étapes complétées : 8**
**Date : 2026-07-22**
**Document : `design/architecture-foretaccess.md`**

### Final Architecture Deliverables

- **8 décisions d'architecture** (D1–D8) documentées + tracées aux FR / NFR.
- **Patterns d'implémentation** (nommage, structure, format, communication, anti-patterns).
- **Structure de projet** complète (arbre réel, frontières, mapping FR → fichiers).
- **Validation** confirmant cohérence + couverture (MVP livré, Growth gated perf cœur).

### Implementation Handoff

**Pour les agents IA** : ce document + le PRD (`design/prd-foretaccess.md`) sont le guide
complet de la capacité ForêtAccess. Suivre les décisions D1–D8, les patterns et les
frontières à la lettre.
**Première priorité** : le MVP est livré ; le Growth (câble, Steiner / optimiseurs, tracé
manuel, typage, surcoût eau) attend la **performance cœur** — puis extension de
`<CAP>_ENGINES`.

### Quality Assurance Checklist

**✅ Architecture Coherence** — décisions compatibles, stack unique, patterns alignés,
structure cohérente.
**✅ Requirements Coverage** — FR1–21 implémentés, FR22–26 supportés (gated), NFR adressés
(sauf P4 = dette cœur).
**✅ Implementation Readiness** — décisions actionnables (déjà appliquées), patterns
exécutoires, structure sans ambiguïté.

---

**Architecture Status : READY FOR IMPLEMENTATION ✅**
**Document Maintenance** : mettre à jour cette architecture à chaque décision technique
majeure durant l'implémentation.

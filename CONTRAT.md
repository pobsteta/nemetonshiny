# Contrat public de nemetonshiny

Ce document fixe ce que nemetonshiny **s'engage à garder stable** à partir de la
version 1.0.0. Toute rupture de ce contrat est une version **majeure** ; tout
ajout compatible, une version mineure ou corrective. Ce qui n'y figure pas
(fonctions internes, organisation des caches, messages de log) peut changer
sans préavis.

## 1. Point d'entrée

L'application se lance par `run_app()` :

```r
run_app(language = NULL, project_dir = NULL, max_parcels = 30L,
        tour = TRUE, options = list(), ...)
```

| Argument      | Contrat |
|---------------|---------|
| `language`    | `"fr"` ou `"en"` ; `NULL` = langue du système |
| `project_dir` | dossier des projets ; défaut : `rappdirs::user_data_dir("nemeton")/projects`, ou `~/.nemeton/projects` sans `rappdirs` |
| `max_parcels` | entier positif, nombre maximal de parcelles sélectionnables |
| `tour`        | lancement automatique de la visite guidée |
| `options`     | options Shiny (`port`, `host`, `launch.browser`…) fusionnées sur les défauts ; le navigateur ne s'ouvre qu'en session interactive |
| `...`         | transmis à `shiny::shinyApp()` |

## 1 bis. API hors interface (depuis 0.154.0)

Pour piloter un diagnostic sans l'application (scripts, assistants). Chaque
fonction a sa page d'aide (`?api_hors_interface`).

| Fonction | Rôle | Écrit |
|----------|------|-------|
| `projets_lister()` | projets du dossier, avec leur format (`format_ok`) | non |
| `projet_etat(id)` | métadonnées, format du projet, indicateurs et UGF présents | non |
| `projet_lire(id, langue)` | indicateurs par UGF, scores de famille et synthèse (mêmes chiffres que l'onglet Synthèse) | **non** |
| `parcelles_commune(insee, ids)` | parcelles cadastrales d'une commune | non |
| `projet_creer(nom, parcelles, ...)` | crée et initialise un projet (UGF par défaut) ; renvoie l'id | oui |
| `projet_calculer(id, indicateurs, progression)` | calcul synchrone des indicateurs | oui |
| `projet_rapport(id, fichier, langue, synthese, familles, sources)` | rapport PDF | le fichier demandé seulement |
| `projet_gpkg(id, fichier)` | GeoPackage des résultats par UGF | le fichier demandé seulement |

Garanties :

- une fonction marquée « non » n'écrit **rien** dans le projet ;
- un projet créé avant la 1.0.0 n'est pas repris : lire, calculer ou exporter
  échoue avec une erreur de classe `nemetonshiny_projet_ancien` (champ `etat`
  = `projet_etat()`) ; il faut le recréer ;
- les erreurs sont classées et héritent toutes de `nemetonshiny_erreur` :
  `nemetonshiny_projet_introuvable`, `nemetonshiny_projet_ancien`,
  `nemetonshiny_sans_indicateurs`, `nemetonshiny_calcul_echec`,
  `nemetonshiny_parcelles_introuvables` ;
- commentaires du rapport : `synthese` est une chaîne (Markdown, notes `[^n]`
  facultatives) ; `familles` une liste nommée par code de famille (`C`, `B`,
  `W`, `A`, `F`, `L`, `T`, `R`, `S`, `P`, `E`, `N`) ; `sources` la liste
  Markdown des définitions de notes (`[^1]: auteur, titre, p. N. <url>`).
  Sans `sources`, les appels de note restent littéraux.

### Liens profonds et serveur MCP

- `?project=<id>&tab=<onglet>` ouvre l'application sur un projet et un onglet
  (`selection`, `synthesis`, `action_plan`, `terrain`, `monitoring`,
  `regeneration`, `famille_*`) ; une valeur inconnue est ignorée.
- **Signal « page prête »** (depuis 0.156.1) : quand l'application a été ouverte
  par une autre page (`window.open`, assistant VICTOR), elle lui envoie **une
  fois** `window.opener.postMessage({source: "nemetonshiny", type, project,
  tab}, <origine>)` : `type = "ready"` une fois le lien appliqué (projet chargé,
  onglet sélectionné) et Shiny au repos depuis 1 s sans sortie recalculée
  (plafond 2 min) ; `type = "invalid"` aussitôt si le lien est refusé. URL nue :
  `ready` au premier repos. Aucun envoi sans `window.opener`. Origine cible :
  `NEMETON_VICTOR_ORIGIN` (défaut `http://127.0.0.1:8788`, vide = aucun envoi,
  jamais `"*"`).
- `inst/mcp/server.R` expose l'API ci-dessus comme serveur MCP (stdio) ; ses
  outils et leurs réponses JSON sont décrits dans `inst/mcp/README.md`.

## 2. Variables d'environnement

Lues au démarrage ou à l'usage. Les secrets ne sont **jamais** écrits dans le
code ni dans les journaux.

### Base de données (PostGIS, optionnelle)

Résolution, de la plus prioritaire à la moins prioritaire :

1. `NEMETON_DB_URL` (`postgresql://user:motdepasse@hôte:port/base`)
2. `POSTGRESQL_ADDON_HOST`, `_PORT`, `_DB`, `_USER`, `_PASSWORD` (Clever Cloud)
3. `NEMETON_DB_HOST`, `NEMETON_DB_PORT`, `NEMETON_DB_NAME`, `NEMETON_DB_USER`, `NEMETON_DB_PASSWORD`

| Variable | Rôle |
|----------|------|
| `NEMETON_DB_CONNECT_TIMEOUT` | délai de connexion, en secondes |
| `NEMETON_DB_LOCAL` | `1` : ignorer la base distante (poste de développement) |
| `NEMETON_KNOWLEDGE_DB_URL` | base du corpus documentaire (RAG) |
| `NEMETON_CORPUS_ROOT` | racine des fichiers locaux du corpus RAG : un `local_path` hors de cette racine est refusé (cœur ≥ 0.210.0) ; l'option `nemeton.corpus_root` l'emporte |

Sans base, l'application fonctionne sur disque seul : pas de verrou d'édition
multi-utilisateurs, suivi sanitaire en SQLite local.

### Authentification (OAuth2 / OIDC, optionnelle)

| Variable | Rôle |
|----------|------|
| `NEMETON_OAUTH_PROVIDER` | `keycloak`, `google`, `github`, `microsoft` ou `oidc` |
| `NEMETON_OAUTH_CLIENT_ID`, `NEMETON_OAUTH_CLIENT_SECRET` | identifiants du client |
| `NEMETON_OAUTH_REDIRECT_URI` | URL de retour (défaut `http://127.0.0.1:3838`) |
| `NEMETON_OAUTH_SCOPES` | portées, séparées par des virgules (défaut `openid,profile,email`) |
| `NEMETON_KEYCLOAK_URL` | URL du realm Keycloak |
| `NEMETON_OAUTH_ISSUER` | émetteur, pour un fournisseur `oidc` générique |

**Règles d'accès** (depuis 0.152.4) :

- sans OAuth configuré, l'application est en **mode anonyme** : éditeur et
  administrateur (poste mono-utilisateur) ;
- OAuth configuré mais indisponible : **lecture seule** ;
- avec OAuth, les droits viennent des rôles du realm publiés dans `userinfo`
  (claim `realm_access.roles`) : `gestionnaire`, `editeur`, `proprietaire`,
  `owner`, `editor`, `manager` ou `admin` pour éditer ; `admin`, `proprietaire`
  ou `owner` pour administrer (clés du serveur, corpus RAG). Sans rôle
  Nemeton : lecture seule.

`NEMETON_AUTH_DEV_ROLES` (rôles injectés en mode anonyme) est réservé au
développement et aux tests.

**Pas d'isolation entre utilisateurs** (limite assumée de la 1.0) : les rôles
disent *ce qu'on peut faire*, pas *sur quels projets*. Tous les projets d'une
instance vivent dans le même répertoire (`NEMETON_PROJECT_DIR`) et, s'il est
configuré, dans le même schéma PostGIS ; `projects.owner_id` n'est pas
renseigné. Tout utilisateur authentifié voit, ouvre et (avec un rôle d'édition)
modifie ou resynchronise tous les projets. Le verrou d'édition empêche deux
éditions simultanées, il ne protège pas un projet de ses autres lecteurs. Une
instance correspond donc à **un collectif de confiance** (une équipe, un
service) ; des collectifs qui ne doivent pas se voir ont chacun leur instance,
avec leur répertoire et leur base. Le cloisonnement par propriétaire et par
partage est prévu après la 1.0.

### Services externes

| Variable | Rôle |
|----------|------|
| `MISTRAL_API_KEY` (ou `NEMETON_MISTRAL_API_KEY`), `ANTHROPIC_API_KEY`, `OPENAI_API_KEY` | clés des perspectives IA, selon le fournisseur configuré |
| `TLD_ACCESS_KEY`, `TLD_SECRET_KEY` | clé Theia / DATA TERRA (FORMSpoT, FORMS-T, Sentinel-2) ; repli sur `~/.config/teledetection/.apikey` |
| `CDSAPI_KEY` / `ECMWFR_CDS_KEY` | Copernicus Climate Data Store (reGénération) |
| `NEMETON_NTFY_URL`, `NEMETON_NTFY_TOPIC`, `NEMETON_NTFY_TOKEN` | notifications ntfy de fin de calcul ; utiliser un topic imprévisible ou un serveur privé |

### Calcul

| Variable | Rôle |
|----------|------|
| `NEMETON_PARALLEL_WORKERS` | taille du pool de workers asynchrones |
| `NEMETON_MEMORY_MAX` | plafond mémoire des calculs (transmis au cœur) |
| `NEMETON_TOPO_TARGET_RES` | résolution cible des dérivés topographiques |
| `NEMETON_TOUR` | `0`/`false` : pas de visite guidée automatique |
| `NEMETON_PROJECT_DIR` | dossier des projets par défaut (API hors interface ; `run_app(project_dir =)` l'emporte) |
| `NEMETON_VICTOR_ORIGIN` | origine de la page autorisée à recevoir le signal « page prête » (défaut `http://127.0.0.1:8788` ; vide = aucun envoi) |
| `NEMETON_APP_PORT` | port de l'application dans les liens construits par le serveur MCP (`inst/mcp/`, défaut 3838) |

Les autres variables (`NEMETON_PERF_TRACE`, `NEMETON_PIXEL_MAP_DEBUG`,
`NEMETON_S2_CACHE_DEBUG`, `NEMETON_*_SKIP_GUARD`, `NEMETONSHINY_DISABLE_*`,
`NEMETON_SCRATCH_DIR`) servent au diagnostic et **ne sont pas contractuelles**.

## 3. Format d'un projet

Un projet est un dossier `<project_dir>/<id>/`, l'identifiant étant le nom du
dossier (`AAAAMMJJ_HHMMSS_xxxx`).

| Fichier | Contenu |
|---------|---------|
| `metadata.json` | métadonnées : `id`, `name`, `description`, `owner`, `status`, `format_projet` (**1**), paramètres du projet |
| `data/parcels.gpkg` | parcelles cadastrales (référence) ; `parcels.parquet` en copie de lecture rapide |
| `data/tenements.gpkg`, `data/ugs.json` | découpage en unités de gestion (UGF) |
| `data/indicators.parquet` | indicateurs calculés, par UGF |
| `data/indicators.perime-<date>.parquet` | indicateurs invalidés, mis de côté (deux générations au plus, listées dans `metadata.json` → `indicateurs_perimes`) |
| `data/action_plan.json` | plan d'actions (`version` **1**, `annee_base`, actions, audit) |
| `data/comments.json`, `data/regen_comments.json` | commentaires |
| `data/samples.gpkg` | plans d'échantillonnage et de validation |
| `cache/` | couches téléchargées et résultats intermédiaires : **non contractuel**, supprimable |

Garanties :

- **la 1.0.0 repart de zéro** : un projet créé par une version 0.x (sans
  `format_projet`) n'est pas repris. L'application le signale « antérieur à la
  1.0 » et ne propose que sa suppression ; l'API le refuse
  (`nemetonshiny_projet_ancien`). Il faut le recréer ;
- un projet créé par une version 1.x s'ouvre dans toute version 1.y
  ultérieure : un changement de format après la 1.0.0 s'accompagne d'une
  migration automatique (marqueur `format_projet`) ;
- rien n'est détruit en silence : des données illisibles sont mises de côté
  (`data/ug_sauvegarde_<date>/`, `action_plan.illisible-<date>.json`) et des
  indicateurs invalidés sont **renommés**, jamais supprimés
  (`indicators.perime-<date>.parquet`) ;
- les écritures sont atomiques : un arrêt brutal ne laisse pas de fichier
  tronqué.

## 4. Schéma PostGIS

- `inst/sql/schema.sql` est appliqué par l'application (idempotent) et décrit
  à lui seul le schéma complet de la 1.0.0.
- **La 1.0.0 repart d'une base neuve** : les anciennes migrations ont été
  intégrées à `schema.sql` et retirées. Une base créée avant la 1.0.0 est à
  recréer, comme les tables du cœur (`nemeton::db_migrate()` refuse une base
  antérieure à `nemeton` 1.0.0, erreur `nemeton_legacy_schema`) ; la base de
  suivi sanitaire aussi.
- Après la 1.0.0, un changement de schéma est toujours un nouveau fichier
  `inst/sql/migration_00N_*.sql`, idempotent, jamais la modification d'un
  fichier publié.

## 5. Profils d'experts

Les profils des perspectives IA sont des fichiers YAML bilingues dans
`inst/experts/`. Leur format est décrit par `inst/experts/_example.yml.template`
et reste stable en 1.x ; ajouter un profil est une évolution mineure.

## 6 bis. API pour nemetonmcp

`nemetonmcp` (le serveur MCP de VICTOR et d'AIGORA) lit **35 objets internes**
de nemetonshiny par `getFromNamespace(nom, "nemetonshiny")`. Ils ne sont pas
exportés (ADR 0001, option B) mais **font partie du contrat** : les renommer,
les retirer ou changer leurs arguments est un changement cassant (section
« Changements cassants » de `NEWS.md`, et un brief à `nemetonmcp`).
`tests/testthat/test-contrat_nemetonmcp.R` verrouille la présence et les
arguments (`formals()`, dans l'ordre) de chaque objet ; il vérifie aussi que
cette section et la liste lue dans le code de `nemetonmcp` sont identiques.
Un objet absent de cette section n'est **pas** garanti.

Plan d'actions et commentaires :

| Objet | Arguments / contenu |
|-------|---------------------|
| `load_action_plan()` | `project_id` |
| `save_action_plan()` | `project_id`, `plan` ; rend `TRUE` si écrit |
| `add_action_to_plan()` | `plan`, `action`, `ug_ids`, `user` |
| `update_action_in_plan()` | `plan`, `action_id`, `updates`, `ug_ids`, `user` |
| `delete_action_from_plan()` | `plan`, `action_id`, `user` |
| `get_action_by_id()` | `plan`, `action_id` |
| `find_action_index()` | `plan`, `action_id` ; `NA` si absente |
| `action_plan_annee_base()` | `plan` |
| `load_ug_comments()` | `project_id` |
| `ACTION_PLAN_TYPES` | vecteur de types d'action |
| `ACTION_PLAN_STATUTS` | vecteur de statuts |
| `ACTION_PLAN_PRIORITES` | vecteur de priorités |
| `ACTION_PLAN_FAMILY_CODES` | les 12 codes de famille |
| `ACTION_PLAN_TRANSITIONS` | liste des transitions de statut permises |

Projet :

| Objet | Arguments / contenu |
|-------|---------------------|
| `load_project()` | `project_id`, `build_indicators_sf` |
| `load_parcels()` | `project_id` |
| `get_project_path()` | `project_id` |
| `get_projects_root()` | aucun |
| `ug_build_sf()` | `projet` |
| `lock_status()` | `pid` ; `NULL` si aucun verrou |

Communes :

| Objet | Arguments / contenu |
|-------|---------------------|
| `get_commune_geometry()` | `code_insee` |
| `save_commune_geometry()` | `project_id`, `commune_geom` |
| `get_departments()` | aucun |

Serveur MCP (`inst/mcp/`, 10 outils : `lister_projets`, `resume_projet`,
`lancer_calcul`, `etat_calcul`, `annuler_calcul`, `generer_rapport`,
`exporter_gpkg`, `appliquer_ugf`, `croiser_onf`, `url_app`) :

| Objet | Arguments / contenu |
|-------|---------------------|
| `mcp_tools()` | aucun ; liste des outils `ellmer::tool()` |
| `mcp_etat_calcul()` | `projet` |
| `mcp_exporter_gpkg()` | `projet` |
| `.mcp_resolve_project()` | `projet` ; id, ou erreur `nemetonshiny_projet_ambigu` avec `candidats` |
| `.mcp_vers_outil()` | `x` ; la chaîne rendue par un `mcp_*` : si c'est une erreur (attribut `mcp_erreur`), rend un `ellmer::ContentToolResult` en erreur (`isError`), sinon `x` tel quel. À appeler sur le résultat de tout outil que `nemetonmcp` substitue à ceux de `mcp_tools()` (aujourd'hui `etat_calcul`, `exporter_gpkg`) |
| `.mcp_garde_ecriture()` | `id`, `call` ; refuse d'écrire dans un projet occupé : `nemetonshiny_calcul_en_cours`, `nemetonshiny_projet_verrouille`, `nemetonshiny_projet_ancien` ; rend l'état du projet |
| `.mcp_jeton_emettre()` | `outil`, `projet` (id résolu), `lien`, `apercu` ; premier temps d'une écriture (ADR 0002) : enregistre sur disque (un fichier `<jeton>.rds` par jeton dans `NEMETON_MCP_JETONS_DIR`, défaut `tools::R_user_dir("nemetonshiny", "data")/mcp/jetons`, hors du dossier projet ; écriture atomique) un jeton à usage unique valable 10 min, tiré hors du générateur de R (`/dev/urandom`, insensible à `set.seed()`), lié à `outil` + `projet` + `lien` (comparé par `identical()`), puis lève `nemetonshiny_confirmation_requise` portant `projet`, `projet_nom` (nom résolu, cité dans le message), `jeton`, `apercu`, `expire_a` (ISO 8601 UTC). N'écrit rien. À appeler après les gardes et les contrôles métier |
| `.mcp_jeton_consommer()` | `jeton`, `outil`, `projet`, `lien`, `garde` (défaut `.mcp_garde_ecriture`) ; second temps : retire le jeton du registre dès sa présentation, même s'il a été émis par un autre processus serveur (la suppression du fichier fait foi : deux serveurs ne le consomment pas tous deux) (même refusé ; exception : les erreurs levées avant l'appel, projet introuvable ou ambigu, métadonnées ONF illisibles, ne le consomment pas), rejoue `garde(projet)`, puis lève `nemetonshiny_jeton_invalide` si le jeton est inconnu, périmé, ou délivré pour un autre outil, projet ou `lien`. `lien` n'est évalué qu'après la garde. Rend `TRUE` (invisible) si l'écriture peut suivre |

Divers :

| Objet | Arguments / contenu |
|-------|---------------------|
| `get_expert_profiles()` | aucun |
| `marculus_export_bundle()` | `project_id`, `file`, `essences` |
| `MARCULUS_CONTEXT_ACTION_TYPES` | vecteur de types d'action Marculus |
| `INDICATOR_FAMILIES` | liste des 12 familles, nommée par code |

## 7. Politique de compatibilité

- **Majeure** (2.0.0) : rupture d'un élément de ce document (argument de
  `run_app()` retiré ou changé, variable d'environnement renommée, format de
  projet non relisible, migration SQL non rétrocompatible).
- **Mineure** : ajout compatible (nouvel argument facultatif, nouvelle
  variable, nouveau fichier de projet, nouvel onglet).
- **Corrective** : correction sans changement de ce contrat.
- Un élément appelé à disparaître est d'abord annoncé comme déprécié dans
  `NEWS.md` pendant au moins une version mineure.
- Le cœur `nemeton` suit sa propre numérotation ; le plancher
  `Imports: nemeton (>= X.Y.Z)` indique la version minimale requise.

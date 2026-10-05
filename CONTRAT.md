# Contrat public de nemetonshiny

Ce document fixe ce que nemetonshiny **s’engage à garder stable** à
partir de la version 1.0.0. Toute rupture de ce contrat est une version
**majeure** ; tout ajout compatible, une version mineure ou corrective.
Ce qui n’y figure pas (fonctions internes, organisation des caches,
messages de log) peut changer sans préavis.

## 1. Point d’entrée

L’application se lance par
[`run_app()`](https://pobsteta.github.io/nemetonshiny/reference/run_app.md)
:

``` r

run_app(language = NULL, project_dir = NULL, max_parcels = 30L,
        tour = TRUE, options = list(), ...)
```

| Argument | Contrat |
|----|----|
| `language` | `"fr"` ou `"en"` ; `NULL` = langue du système |
| `project_dir` | dossier des projets ; défaut : `rappdirs::user_data_dir("nemeton")/projects`, ou `~/.nemeton/projects` sans `rappdirs` |
| `max_parcels` | entier positif, nombre maximal de parcelles sélectionnables |
| `tour` | lancement automatique de la visite guidée |
| `options` | options Shiny (`port`, `host`, `launch.browser`…) fusionnées sur les défauts ; le navigateur ne s’ouvre qu’en session interactive |
| `...` | transmis à [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html) |

## 1 bis. API hors interface (depuis 0.154.0)

Pour piloter un diagnostic sans l’application (scripts, assistants).
Chaque fonction a sa page d’aide
([`?api_hors_interface`](https://pobsteta.github.io/nemetonshiny/reference/api_hors_interface.md)).

| Fonction | Rôle | Écrit |
|----|----|----|
| [`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md) | projets du dossier, avec leur état de sens | non |
| `projet_etat(id)` | métadonnées, sens vu / courant, indicateurs et UGF présents, migration nécessaire | non |
| `projet_lire(id, langue)` | indicateurs par UGF, scores de famille et synthèse (mêmes chiffres que l’onglet Synthèse) | **non** |
| `projet_migrer(id)` | applique les migrations qu’appliquerait l’ouverture dans l’application | oui |
| `parcelles_commune(insee, ids)` | parcelles cadastrales d’une commune | non |
| `projet_creer(nom, parcelles, ...)` | crée et initialise un projet (UGF, sens courant) ; renvoie l’id | oui |
| `projet_calculer(id, indicateurs, progression)` | calcul synchrone des indicateurs | oui |
| `projet_rapport(id, fichier, langue, synthese, familles, sources)` | rapport PDF | le fichier demandé seulement |
| `projet_gpkg(id, fichier)` | GeoPackage des résultats par UGF | le fichier demandé seulement |

Garanties :

- une fonction marquée « non » n’écrit **rien** dans le projet ; en
  particulier
  [`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md)
  n’exécute aucune migration : si l’une serait nécessaire, elle échoue
  avec une erreur de classe `nemetonshiny_projet_perime` (champ `etat` =
  [`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md)),
  et seule
  [`projet_migrer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_migrer.md)
  l’applique ;
- les erreurs sont classées et héritent toutes de `nemetonshiny_erreur`
  : `nemetonshiny_projet_introuvable`, `nemetonshiny_projet_perime`,
  `nemetonshiny_sans_indicateurs`, `nemetonshiny_calcul_echec`,
  `nemetonshiny_parcelles_introuvables` ;
- commentaires du rapport : `synthese` est une chaîne (Markdown, notes
  `[^n]` facultatives) ; `familles` une liste nommée par code de famille
  (`C`, `B`, `W`, `A`, `F`, `L`, `T`, `R`, `S`, `P`, `E`, `N`) ;
  `sources` la liste Markdown des définitions de notes
  (`[^1]: auteur, titre, p. N. <url>`). Sans `sources`, les appels de
  note restent littéraux.

### Liens profonds et serveur MCP

- `?project=<id>&tab=<onglet>` ouvre l’application sur un projet et un
  onglet (`selection`, `synthesis`, `action_plan`, `terrain`,
  `monitoring`, `regeneration`, `famille_*`) ; une valeur inconnue est
  ignorée.
- `inst/mcp/server.R` expose l’API ci-dessus comme serveur MCP (stdio) ;
  ses outils et leurs réponses JSON sont décrits dans
  `inst/mcp/README.md`.

## 2. Variables d’environnement

Lues au démarrage ou à l’usage. Les secrets ne sont **jamais** écrits
dans le code ni dans les journaux.

### Base de données (PostGIS, optionnelle)

Résolution, de la plus prioritaire à la moins prioritaire :

1.  `NEMETON_DB_URL` (`postgresql://user:motdepasse@hôte:port/base`)
2.  `POSTGRESQL_ADDON_HOST`, `_PORT`, `_DB`, `_USER`, `_PASSWORD`
    (Clever Cloud)
3.  `NEMETON_DB_HOST`, `NEMETON_DB_PORT`, `NEMETON_DB_NAME`,
    `NEMETON_DB_USER`, `NEMETON_DB_PASSWORD`

| Variable | Rôle |
|----|----|
| `NEMETON_DB_CONNECT_TIMEOUT` | délai de connexion, en secondes |
| `NEMETON_DB_LOCAL` | `1` : ignorer la base distante (poste de développement) |
| `NEMETON_KNOWLEDGE_DB_URL` | base du corpus documentaire (RAG) |
| `NEMETON_CORPUS_ROOT` | racine des fichiers locaux du corpus RAG : un `local_path` hors de cette racine est refusé (cœur ≥ 0.210.0) ; l’option `nemeton.corpus_root` l’emporte |

Sans base, l’application fonctionne sur disque seul : pas de verrou
d’édition multi-utilisateurs, suivi sanitaire en SQLite local.

### Authentification (OAuth2 / OIDC, optionnelle)

| Variable | Rôle |
|----|----|
| `NEMETON_OAUTH_PROVIDER` | `keycloak`, `google`, `github`, `microsoft` ou `oidc` |
| `NEMETON_OAUTH_CLIENT_ID`, `NEMETON_OAUTH_CLIENT_SECRET` | identifiants du client |
| `NEMETON_OAUTH_REDIRECT_URI` | URL de retour (défaut `http://127.0.0.1:3838`) |
| `NEMETON_OAUTH_SCOPES` | portées, séparées par des virgules (défaut `openid,profile,email`) |
| `NEMETON_KEYCLOAK_URL` | URL du realm Keycloak |
| `NEMETON_OAUTH_ISSUER` | émetteur, pour un fournisseur `oidc` générique |

**Règles d’accès** (depuis 0.152.4) :

- sans OAuth configuré, l’application est en **mode anonyme** : éditeur
  et administrateur (poste mono-utilisateur) ;
- OAuth configuré mais indisponible : **lecture seule** ;
- avec OAuth, les droits viennent des rôles du realm publiés dans
  `userinfo` (claim `realm_access.roles`) : `gestionnaire`, `editeur`,
  `proprietaire`, `owner`, `editor`, `manager` ou `admin` pour éditer ;
  `admin`, `proprietaire` ou `owner` pour administrer (clés du serveur,
  corpus RAG). Sans rôle Nemeton : lecture seule.

`NEMETON_AUTH_DEV_ROLES` (rôles injectés en mode anonyme) est réservé au
développement et aux tests.

### Services externes

| Variable | Rôle |
|----|----|
| `MISTRAL_API_KEY` (ou `NEMETON_MISTRAL_API_KEY`), `ANTHROPIC_API_KEY`, `OPENAI_API_KEY` | clés des perspectives IA, selon le fournisseur configuré |
| `TLD_ACCESS_KEY`, `TLD_SECRET_KEY` | clé Theia / DATA TERRA (FORMSpoT, FORMS-T, Sentinel-2) ; repli sur `~/.config/teledetection/.apikey` |
| `CDSAPI_KEY` / `ECMWFR_CDS_KEY` | Copernicus Climate Data Store (reGénération) |
| `NEMETON_NTFY_URL`, `NEMETON_NTFY_TOPIC`, `NEMETON_NTFY_TOKEN` | notifications ntfy de fin de calcul ; utiliser un topic imprévisible ou un serveur privé |

### Calcul

| Variable | Rôle |
|----|----|
| `NEMETON_PARALLEL_WORKERS` | taille du pool de workers asynchrones |
| `NEMETON_MEMORY_MAX` | plafond mémoire des calculs (transmis au cœur) |
| `NEMETON_TOPO_TARGET_RES` | résolution cible des dérivés topographiques |
| `NEMETON_TOUR` | `0`/`false` : pas de visite guidée automatique |
| `NEMETON_PROJECT_DIR` | dossier des projets par défaut (API hors interface ; `run_app(project_dir =)` l’emporte) |
| `NEMETON_APP_PORT` | port de l’application dans les liens construits par le serveur MCP (`inst/mcp/`, défaut 3838) |

Les autres variables (`NEMETON_PERF_TRACE`, `NEMETON_PIXEL_MAP_DEBUG`,
`NEMETON_S2_CACHE_DEBUG`, `NEMETON_*_SKIP_GUARD`,
`NEMETONSHINY_DISABLE_*`, `NEMETON_SCRATCH_DIR`) servent au diagnostic
et **ne sont pas contractuelles**.

## 3. Format d’un projet

Un projet est un dossier `<project_dir>/<id>/`, l’identifiant étant le
nom du dossier (`AAAAMMJJ_HHMMSS_xxxx`).

| Fichier | Contenu |
|----|----|
| `metadata.json` | métadonnées : `id`, `name`, `description`, `owner`, `status`, `schema_version` (**2.1**), `indicator_sense_version` (**3**), paramètres du projet |
| `data/parcels.gpkg` | parcelles cadastrales (référence) ; `parcels.parquet` en copie de lecture rapide |
| `data/tenements.gpkg`, `data/ugs.json` | découpage en unités de gestion (UGF) |
| `data/indicators.parquet` | indicateurs calculés, par UGF |
| `data/indicators.perime-v<n>-<date>.parquet` | indicateurs invalidés, mis de côté (deux générations au plus, listées dans `metadata.json` → `indicateurs_perimes`) |
| `data/action_plan.json` | plan d’actions (`version` **1**, `annee_base`, actions, audit) |
| `data/comments.json`, `data/regen_comments.json` | commentaires |
| `data/samples.gpkg` | plans d’échantillonnage et de validation |
| `cache/` | couches téléchargées et résultats intermédiaires : **non contractuel**, supprimable |

Garanties :

- un projet créé par une version 1.x s’ouvre dans toute version 1.y
  ultérieure ;
- les migrations de format sont **automatiques** à l’ouverture, ne
  s’exécutent qu’une fois (marqueurs `schema_version`,
  `indicator_sense_version`, `annee_base`) et **ne détruisent rien** :
  des données illisibles sont mises de côté
  (`data/ug_sauvegarde_<date>/`, `action_plan.illisible-<date>.json`) ;
- un changement de sens ou d’échelle d’un indicateur dans le cœur
  invalide les indicateurs calculés avant (recalcul demandé à
  l’utilisateur), sans toucher aux autres données ; les indicateurs
  invalidés sont **renommés**, jamais supprimés
  (`indicators.perime-v<n>-<date>.parquet`), ce qui permet de comparer
  avant / après ;
- les écritures sont atomiques : un arrêt brutal ne laisse pas de
  fichier tronqué.

## 4. Schéma PostGIS

- `inst/sql/schema.sql` est appliqué par l’application (idempotent).
- Les fichiers `inst/sql/migration_00N_*.sql` s’appliquent **à la
  main**, dans l’ordre, une seule fois. Ils sont idempotents, sauf
  `migration_001`, historique et **destructive**, à ne jamais rejouer.
- Une nouvelle migration est toujours un nouveau fichier, jamais la
  modification d’un fichier publié.

## 5. Profils d’experts

Les profils des perspectives IA sont des fichiers YAML bilingues dans
`inst/experts/`. Leur format est décrit par
`inst/experts/_example.yml.template` et reste stable en 1.x ; ajouter un
profil est une évolution mineure.

## 6. Politique de compatibilité

- **Majeure** (2.0.0) : rupture d’un élément de ce document (argument de
  [`run_app()`](https://pobsteta.github.io/nemetonshiny/reference/run_app.md)
  retiré ou changé, variable d’environnement renommée, format de projet
  non relisible, migration SQL non rétrocompatible).
- **Mineure** : ajout compatible (nouvel argument facultatif, nouvelle
  variable, nouveau fichier de projet, nouvel onglet).
- **Corrective** : correction sans changement de ce contrat.
- Un élément appelé à disparaître est d’abord annoncé comme déprécié
  dans `NEWS.md` pendant au moins une version mineure.
- Le cœur `nemeton` suit sa propre numérotation ; le plancher
  `Imports: nemeton (>= X.Y.Z)` indique la version minimale requise.

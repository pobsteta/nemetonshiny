# Changelog

## nemetonshiny 0.26.3 (2026-05-13)

#### Suivi sanitaire — propagation des `NEMETON_*` env vars vers le worker async

- `fix(monitoring)` —
  [`future::multisession`](https://future.futureverse.org/reference/multisession.html)
  workers sur Windows sont des processus `Rscript.exe` séparés qui
  n’héritent pas systématiquement des variables d’environnement settées
  dans la session principale après leur spawn. Conséquence pratique :
  `Sys.setenv(NEMETON_S2_CACHE_DEBUG = "TRUE")` en console R ne se
  voyait pas dans le worker → les lignes `[s2_cache HH:MM:SS] ...`
  émises par `nemeton` restaient muettes même quand le cache
  fonctionnait.

  Correctif : `run_ingestion_async()` et `run_fordead_async()`
  snapshottent à l’invoke les `NEMETON_*` env vars settées dans le
  parent (`.capture_worker_envvars()`), `future` les pickle comme
  globals automatiquement, et le worker les replay via
  `.apply_worker_envvars()` en tout début de `future_promise()`.

  Couvre : `NEMETON_S2_CACHE_DEBUG`, `NEMETON_DB_URL`,
  `NEMETON_DB_LOCAL`, `NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD`.

- `test` — `test-service_monitoring_wiring.R` couvre les deux helpers
  (`.capture_worker_envvars` skip les valeurs vides ;
  `.apply_worker_envvars` no-op sur NULL/empty + setenv correct).

## nemetonshiny 0.26.2 (2026-05-13)

#### Dépendance nemeton — pin à v0.21.9 (fix writeRaster .tif.tmp)

- `fix(deps)` — pin du cœur `nemeton` bumpé à `>= 0.21.9`. Cette version
  corrige le bug d’écriture du cache S2 où
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)
  était appelé sur un chemin `.tif.tmp` (pattern d’écriture atomique)
  sans argument `filetype`, et terra refusait avec *“cannot guess file
  type from filename”*. Toutes les bandes étaient FETCH + CROP avec
  succès puis perdues à l’étape WRITE → le cache disque restait vide
  même après le fix S4 de v0.21.8.

  Symptôme côté UI v0.26.1 : le run consommait 4-5 min par scène
  (FETCH + CROP), atteignait 26/26, mais
  `<project>/cache/layers/sentinel2/` restait vide.

  - `Imports: nemeton (>= 0.21.9)`
  - `Remotes: pobsteta/nemeton@v0.21.9`

## nemetonshiny 0.26.1 (2026-05-13)

#### Dépendance nemeton — pin à v0.21.8 (fix S4→double extraction)

- `fix(deps)` — pin du cœur `nemeton` bumpé à `>= 0.21.8`. Cette version
  corrige un bug d’extraction où chaque scène Sentinel-2 était skippée
  avec le message *“cannot coerce type ‘S4’ to vector of type ‘double’”*
  lors d’un run `skip_cached = FALSE` (introduit en v0.21.4 avec le
  wiring `cache_dir`). Le bug se manifestait dans le bloc per-scène : un
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  (classe S4) était traité comme un vecteur numérique au lieu d’être
  extrait via
  [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
  /
  [`terra::values()`](https://rspatial.github.io/terra/reference/values.html).

  Conséquence côté UI v0.26.0 : cocher “Réamorcer le cache COG” donnait
  bien `skip_cached = FALSE` mais toutes les scènes étaient skippées →
  ni la DB ni le cache disque ne se remplissaient.

  - `Imports: nemeton (>= 0.21.8)`
  - `Remotes: pobsteta/nemeton@v0.21.8`

## nemetonshiny 0.26.0 (2026-05-13)

#### Suivi sanitaire — exposition de `skip_cached` dans l’UI

- `feat(monitoring)` — checkbox **“Réamorcer le cache COG”** ajoutée
  sous le bouton d’ingestion en mode quick. Cochée → l’app appelle
  `nemeton::ingest_sentinel2_timeseries(..., skip_cached = FALSE)` ce
  qui force la ré-extraction plot-par-plot et donc le retéléchargement
  des bandes, qui se persistent dans
  `<project>/cache/layers/sentinel2/`. Décochée (défaut) → comportement
  v0.25.0 inchangé (`skip_cached = TRUE`).

  Contexte : v0.25.0 a finalisé le wiring `cache_dir` +
  `progress_callback`, mais le défaut `skip_cached = TRUE` côté nemeton
  court-circuite l’extraction quand la DB est déjà peuplée → le cache
  disque restait vide tant que la table `obs_pixel` contenait des lignes
  pour la zone + fenêtre demandées.

  Note : les INSERT sont `ON CONFLICT DO NOTHING` côté core, donc cocher
  la case n’écrase rien en DB.

- `feat(i18n)` — clés `monitoring_reprime_cache_label` +
  `monitoring_reprime_cache_help` (FR + EN, accents en `\uXXXX`).

## nemetonshiny 0.25.0 (2026-05-13)

#### Suivi sanitaire — routage complet des événements `progress_callback` Sentinel-2

Cette release **complète le wiring** de
`nemeton::ingest_sentinel2_timeseries(cache_dir = , progress_callback = )`
côté UI. Le wiring de base (paramètres passés à la lambda
`ExtendedTask`, fichier JSON poll-é, toast dédupé) était déjà en place
depuis v0.24.11/v0.24.12 ; cette version ajoute le routage explicite sur
trois events qui étaient capturés par le fallback générique :

- `feat(monitoring)` — routage explicite des événements
  `progress_callback` introduits par `nemeton@v0.21.4+` :

  - `s2:cache_lookup` → toast persistant “Cache DB : N en cache, M à
    traiter” (réutilise le même `id` que la barre de progression pour
    éviter l’empilement)
  - `s2:band_fetch_failed` → toast warning non-persistant (6 s) avec
    `band` + `error_message`, sur un `id` distinct pour ne pas masquer
    la progression scène
  - `s2:pc_token_refreshed` → toast info éphémère (3 s) signalant la
    rotation du token SAS Planetary Computer

- `feat(monitoring)` — indicateur **“Cache COG actif”** sous le bouton
  d’ingestion (mode quick) : affiche le chemin absolu du répertoire
  `<project>/cache/layers/sentinel2/` où nemeton persiste les bandes
  cropées en COG. Le tooltip natif duplique le chemin pour copie rapide.

- `chore(deps)` — pin `nemeton` bumpé à `>= 0.21.7` pour aligner sur les
  versions qui exposent stablement la signature `progress_callback` +
  `cache_dir`.

#### Priming du cache COG

Note importante pour l’utilisateur : si la DB de monitoring est déjà
peuplée par un run précédent, le défaut `skip_cached = TRUE` côté
[`nemeton::ingest_sentinel2_timeseries()`](https://pobsteta.github.io/nemeton/reference/ingest_sentinel2_timeseries.html)
court-circuite l’extraction plot-par-plot → le cache disque sous
`<project>/cache/layers/sentinel2/` **reste vide** même avec ce patch.
C’est attendu. Pour amorcer le cache disque, il faut lancer au moins une
fois avec `skip_cached = FALSE` (paramètre core, non encore exposé dans
l’UI ; les INSERT sont `ON CONFLICT DO NOTHING`, la DB reste intacte).

#### i18n — nouvelles clés (FR + EN)

- `monitoring_ingest_cache_lookup_fmt` — “Cache DB : %d en cache, %d à
  traiter” / “DB cache: %d cached, %d to process”
- `monitoring_ingest_band_failed_fmt` — “Échec bande %s : %s” / “Band %s
  failed: %s”
- `monitoring_ingest_token_refreshed` — “Token SAS Planetary Computer
  rafraîchi” / “Planetary Computer SAS token refreshed”
- `monitoring_cache_active_fmt` — “Cache COG actif : %s” / “COG cache
  active: %s”

#### Tests

- `tests/testthat/test-service_monitoring_wiring.R` — vérifie que
  `run_ingestion_async()` transmet bien `cache_dir` et un
  `progress_callback` non-NULL à
  [`nemeton::ingest_sentinel2_timeseries()`](https://pobsteta.github.io/nemeton/reference/ingest_sentinel2_timeseries.html)
  (mock via `local_mocked_bindings`).

## nemetonshiny 0.24.14 (2026-05-13)

#### Dépendance nemeton — pin re-synchronisé sur v0.21.5

- `chore(deps)` — pin du cœur `nemeton` aligné sur la dernière version
  stable installée en local (`>= 0.21.5`). Aucun changement fonctionnel
  côté app : `nemeton` v0.21.4 / v0.21.5 sont des bumps patch côté cœur
  (FORDEAD hardening + clarifications de signatures internes), mais
  `DESCRIPTION` restait à `>= 0.21.3`, ce qui provoquait des warnings
  `pak::pkg_install("nemetonshiny")` sur les machines neuves (résolution
  OK mais désynchro pin / version effectivement installée).

  - `Imports: nemeton (>= 0.21.5)`
  - `Remotes: pobsteta/nemeton@v0.21.5`
  - `CITATION.cff` bumpé à `0.24.14`

## nemetonshiny 0.24.13 (2026-05-13)

Release stable consolidant deux correctifs hardening sur le suivi
sanitaire (cycle dev `0.24.12.9001` → `0.24.12.9002`).

#### Suivi sanitaire — cache S2 aligné sur la convention `<project>/cache/layers/`

- `fix(monitoring)` — la v0.24.11 posait le cache des bandes Sentinel-2
  sous `<project>/data/s2_cache/`, ce qui violait la convention NMT déjà
  en place pour les autres rasters (`<project>/cache/layers/lidar_mnh/`,
  `lidar_mnt/`, `lidar_nuage/`, `opencanopy/`, `bdforet.gpkg`, etc. —
  cf. `mod_sampling.R::cache_raster()`).

  Correctif : `.resolve_s2_cache_dir(project)` renvoie désormais
  `<project>/cache/layers/sentinel2/`. Layout attendu :

      <project>/cache/layers/sentinel2/
        S2A_MSIL2A_20240515.../
          B04.tif
          B08.tif
          B12.tif

  **Migration manuelle** des anciens projets : si tu vois un dossier
  `<project>/data/s2_cache/` héritant de la v0.24.11, déplace-le à la
  main vers `<project>/cache/layers/sentinel2/` pour récupérer le cache
  existant. Sinon il sera juste ignoré et nemeton ré-téléchargera les
  bandes au prochain run.

## nemetonshiny 0.24.12.9001 (2026-05-12)

#### Suivi sanitaire — dédup des toasts success / warning / error

- `fix(monitoring)` — les toasts finaux de l’ingestion et de FORDEAD
  s’empilaient à chaque re-clic au lieu de se remplacer (visible sur un
  504 de Planetary Computer répété : 2-3 toasts identiques “Aucune scène
  Sentinel-2 trouvée…”).

  Cause : les
  [`shiny::showNotification()`](https://rdrr.io/pkg/shiny/man/showNotification.html)
  finaux étaient appelés sans argument `id`. Sans id, Shiny crée à
  chaque fois une nouvelle notification.

  Correctif : `id = session$ns(...)` ajouté sur tous les toasts
  terminaux du module monitoring :

  - `ingest_zero` (0 scènes trouvées)
  - `ingest_success` (ingestion réussie)
  - `ingest_warns` (warnings non bloquants)
  - `ingest_error` (worker exception)
  - `fordead_success`, `fordead_error` (idem côté FORDEAD)

  Le toast persistant `ingest_progress` / `fordead_progress` avait déjà
  son id ; la dédup ne concernait que les terminaux.

## nemetonshiny 0.24.12 (2026-05-12)

#### Suivi sanitaire — bump effectif du pin nemeton à v0.21.3

- `fix(deps)` — toast d’erreur **“Échec du téléchargement : argument
  inutilisé (cache_dir = cache_dir)”** au clic FAST après installation
  de v0.24.11.

  Cause : la v0.24.11 a livré le code applicatif qui appelle
  `nemeton::ingest_sentinel2_timeseries(..., cache_dir = ...)` mais j’ai
  oublié de bumper le **pin** dans `DESCRIPTION`. `Imports` et `Remotes`
  pointaient encore vers `nemeton@v0.21.2` qui ignore `cache_dir`, donc
  pak/install installait la vieille nemeton et R levait “argument
  inutilisé”.

  Correctif : `Imports: nemeton (>= 0.21.3)` et
  `Remotes: pobsteta/nemeton@v0.21.3`. Aucun changement applicatif.

  À refaire côté utilisateur après pull :

  ``` r

  pak::pak("pobsteta/nemetonshiny@claude/fix-remaining-errors-xbROB")
  # ou en local : devtools::install_local(".", force = TRUE)
  ```

## nemetonshiny 0.24.11 (2026-05-12)

#### Suivi sanitaire — cache local Sentinel-2 + events band-level

- `feat(monitoring)` — branche le `cache_dir` introduit par
  `nemeton@v0.21.3+` sur `ingest_sentinel2_timeseries()`. Le worker pose
  désormais les bandes Sentinel-2 sous `<project>/data/s2_cache/`. Les
  bandes déjà téléchargées sont réutilisées au prochain run pour la même
  scène — gain massif sur un re-run après un échec STAC ou une extension
  de fenêtre temporelle.

  Helper `.resolve_s2_cache_dir(project)` côté `mod_monitoring.R` :
  retourne `NULL` quand aucun projet n’est ouvert (nemeton retombe sur
  son chemin legacy in-memory), sinon crée le sous-dossier au besoin et
  passe le chemin normalisé au worker.

- `feat(monitoring)` — abonnement aux nouveaux events progress
  `s2:band_cached` / `s2:band_fetched`. Chaque bande génère une ligne
  `cli_alert_info` dédiée dans la console R :

      ℹ Tuile Sentinel-2 S2A_MSIL2A_20260508T103651_R008_T31TFN_20260508T191011 (5/26) — 2026-05-08, 2.9% nuages, source=pc
        ⤷ Bande B04 (cache) — scène S2A_MSIL2A_20260508…
        ⤷ Bande B08 (téléchargement) — scène S2A_MSIL2A_20260508…

  Volontairement **pas** d’update du toast UI sur ces events : 2-4
  bandes par scène à sub-second feraient flickerer le toast et
  perdraient le contexte scène. Le toast reste sur l’event `s2:scene`.

  Helper `.log_band_event(ev, current_phase)` dispatché en tête de
  l’observer après détection de
  `current %in% c("s2:band_cached", "s2:band_fetched")`.

## nemetonshiny 0.24.10 (2026-05-12)

#### Suivi sanitaire — 3 fixes UX critiques

- `fix(monitoring)` — **spam “Database schema up to date” en boucle**
  dans la console :
  [`nemeton::db_migrate()`](https://pobsteta.github.io/nemeton/reference/db_migrate.html)
  emet un
  [`cli::cli_alert_info`](https://cli.r-lib.org/reference/cli_alert.html)
  à chaque connexion ré-ouverte. Avec les reactives multiples du module
  (validity, zones, alerts, probe…), on tape 30-50 lignes identiques par
  interaction.

  Correctif : `withCallingHandlers(message = ...)` autour de
  `db_migrate()` qui muffle uniquement les messages contenant “up to
  date” / “already migrated”. Les “Applied migration X” du premier run
  et les warnings/erreurs restent visibles.

- `feat(monitoring)` — **0 scènes trouvées masquait un timeout Planetary
  Computer** (HTTP 504). Le worker affichait juste
  `Téléchargement terminé : 0 scène(s)`, sans dire que le backend STAC
  avait timeout.

  Correctif : `withCallingHandlers(warning = ...)` dans le worker
  capture les warnings nemeton (`STAC backend "pc" failed: HTTP 504...`)
  et les remonte dans `result$warnings`. Côté result observer :

  - Si `n_scenes == 0` : toast warning rouge avec les warnings capturés
    (ou un hint générique “Élargis la fenêtre temporelle ou tolère plus
    de nuages”).
  - Si succès mais warnings non bloquants : toast secondaire
    “Avertissement(s) du backend : …” pour ne pas les perdre.

- `fix(monitoring)` — **toast affichait “Tuile Sentinel-2 (scene_id
  missing) (0/159)”** entre la recherche STAC et la première tuile. Le
  log console était tout aussi opaque (`(scene_id missing)`).

  Correctif : quand `scene_id` est vide ET `completed == 0`, le toast
  affiche maintenant :

  - “Recherche des scènes Sentinel-2 disponibles…” (si total = 0)
  - “Préparation du téléchargement : N scène(s) trouvée(s)…” (si total
    \> 0)

  Et la console : “Sentinel-2 STAC search done: N scene(s) found.”

  Nouvelles clés i18n FR/EN : `monitoring_stac_search`,
  `monitoring_stac_search_with_count_fmt`, `monitoring_ingest_zero_fmt`,
  `monitoring_ingest_zero_default`, `monitoring_ingest_warns_fmt`.

#### Suivi sanitaire — “Ingestion Sentinel-2” → “Téléchargement Sentinel-2”

- `chore(monitoring)` — le terme “ingestion” est trop technique pour le
  contexte UI où l’on télécharge des scènes Sentinel-2 depuis Planetary
  Computer. Renommage en “téléchargement” (FR) / “download” (EN) sur
  **tous les textes utilisateur du contexte Sentinel-2** :

  - `monitoring_validate_zone` : “lancer l’ingestion” → “lancer le
    téléchargement”
  - `monitoring_ingest_starting` : “Ingestion Sentinel-2 en cours” →
    “Téléchargement Sentinel-2 en cours”
  - `monitoring_ingest_success` : “Ingestion terminée” → “Téléchargement
    terminé”
  - `monitoring_ingest_error` : “Échec de l’ingestion” → “Échec du
    téléchargement”
  - `monitoring_timeseries_placeholder` /
    `monitoring_alerts_placeholder` : “après la première ingestion” →
    “après le premier téléchargement”
  - Console R (`cli_alert_info`) : “Sentinel-2 ingestion starting” →
    “Sentinel-2 download starting”

  **Volontairement non renommés** : `field_ingest_*` (import des données
  terrain depuis QField/GPKG) et `health_validation_*` (import des
  validations FORDEAD). Ce sont des uploads de données utilisateur, pas
  des téléchargements distants — le terme “ingestion” reste correct dans
  ces contextes.

  Les **clés i18n** elles-mêmes (`monitoring_ingest_*`) ne sont pas
  renommées — seul le contenu FR/EN change. Aucune cassure pour les
  modules qui les consomment.

## nemetonshiny 0.24.9 (2026-05-12)

#### Suivi sanitaire — mirroring console des events de progression

- `feat(monitoring)` — la progression du Suivi sanitaire n’apparaissait
  que dans le toast UI. Aucune ligne dans la console R pour le
  développeur qui lance l’app via `Rscript -e ...`.

  Correctif : les deux observers de progression (ingestion + FORDEAD)
  écrivent désormais une ligne
  [`cli::cli_alert_info`](https://cli.r-lib.org/reference/cli_alert.html)
  (ou `cli_alert_warning` sur `scene_error` / `phase_error`) à chaque
  event, exactement une fois par tuile / phase grâce à la granularité du
  `reactivePoll(500 ms)`.

  Le format console est plus riche que le toast : on profite des champs
  supplémentaires nemeton (`obs_date`, `cloud_pct`, `source`) pour
  donner une ligne du genre :

      ℹ Tuile Sentinel-2 S2A_MSIL2A_20260508T103651_R008_T31TFN_20260508T191011 (5/26) — 2026-05-08, 2.9% nuages, source=pc

  Pour FORDEAD :

      ℹ FORDEAD phase training (1/5)

  Helpers : `.log_ingest_event()` et `.log_fordead_event()` dans la
  section Internal de `mod_monitoring.R`. Aucune utilisation de
  [`print()`](https://rdrr.io/r/base/print.html) /
  [`message()`](https://rdrr.io/r/base/message.html) /
  [`cat()`](https://rdrr.io/r/base/cat.html) (règle 9 CLAUDE.md).

#### Suivi sanitaire — toast progression aligné sur le payload nemeton

- `fix(monitoring)` — toast d’ingestion affichait **“Tuile Sentinel-2
  0/0”** pendant tout le run alors que le `scene_id` arrivait
  correctement.

  Cause : <nemeton@v0.21.2> émet le payload sous la forme
  `{current, completed, total, scene_id, obs_date, cloud_pct, source}`,
  pas `{i, n, status, ...}` comme initialement spécifié. L’observer
  cherchait `ev$i` / `ev$n` qui n’existaient pas, donc retombait
  toujours sur le défaut `0L`.

  Correctif :

  - Lecture des champs `completed` / `total` avec fallback vers `i` /
    `n` (au cas où le schéma évoluerait).
  - Idem côté FORDEAD : `current` pour la phase, fallback vers `phase` /
    `scene_id`.
  - Reformatage des i18n : compteur **entre parenthèses** en fin de
    message — `"Tuile Sentinel-2 <scene_id> (X/N)"` et
    `"FORDEAD — phase <nom> (X/N)"`.
  - Ajout d’une **roue dentée animée** devant chaque message
    (`bsicons::bs_icon("gear-fill")` + classe `.nmt-spin`, keyframe déjà
    définie dans `custom.css`). Le toast persistant ne ressemble plus à
    un message figé.

## nemetonshiny 0.24.8 (2026-05-12)

#### Suivi sanitaire — progression “X/N tuiles Sentinel-2” + phases FORDEAD

- `feat(monitoring)` — pendant l’ingestion Sentinel-2 (FAST) et le
  diagnostic FORDEAD, l’utilisateur reste sur un toast statique pendant
  plusieurs minutes sans aucune indication de progression intermédiaire.
  Le seul retour était le toast final résumé
  (`%d scènes, %d observations insérées`).

  Correctif (couplé avec `nemeton@v0.21.2` qui introduit l’argument
  `progress_callback` sur `ingest_sentinel2_timeseries()` et
  `run_fordead_dieback()`) :

  - Le worker async (`run_ingestion_async` / `run_fordead_async`)
    construit un callback qui sérialise chaque événement en JSON
    atomique (write to `.tmp` + rename) vers
    `<project>/data/ingest_progress.json` (resp.
    `fordead_progress.json`).
  - Côté main process, un `shiny::reactivePoll(500 ms)` lit le fichier,
    et un observer met à jour un toast persistant (même `id`) avec
    `"Tuile Sentinel-2 X/N : <scene_id>"` (ou simplement
    `"X/N téléchargée…"` quand le scene_id n’est pas fourni) pour
    l’ingestion, et `"FORDEAD — phase : <nom> (X/N)"` pour FORDEAD.
  - À la fin de la tâche (succès ou erreur), le toast persistant est
    retiré et le fichier `progress.json` purgé. Le toast final
    `monitoring_ingest_success` / `monitoring_health_success` reprend la
    main.

  Nouvelles clés i18n FR/EN : `monitoring_ingest_progress_fmt`,
  `monitoring_ingest_progress_named_fmt`, `monitoring_health_phase_fmt`,
  `monitoring_health_phase_simple_fmt`.

  Bump `DESCRIPTION` : `Imports: nemeton (>= 0.21.2)`,
  `Remotes: pobsteta/nemeton@v0.21.2`. ADR-009 respecté — toute la
  logique métier (savoir qu’une tuile est téléchargée, qu’une phase est
  terminée) reste dans `nemeton` ; nemetonshiny n’écoute que le canal
  callback exporté.

#### Suivi sanitaire — bouton “Lancer le diagnostic FAST” muet au clic

- `fix(monitoring)` — clic sur **“Lancer le diagnostic FAST”** (ou
  **“Lancer le diagnostic FORDEAD”** en mode santé) sans aucune réaction
  : ni toast d’ingestion, ni toast d’erreur.

  Cause : les deux boutons étaient rendus avec
  `htmltools::tagAppendAttributes(..., disabled = NA)` (commit
  `a880507`), ce qui les désactive **au niveau HTML** au premier rendu.
  Un observer côté serveur les réactivait via
  `updateActionButton(disabled = FALSE)`, mais le style `btn-primary`
  masque visuellement l’état `disabled` du Bootstrap — l’utilisateur
  voit un bouton bleu d’aspect cliquable alors que le navigateur refuse
  le clic, donc aucun `observeEvent` ne se déclenche.

  Correctif : on suit le pattern explicite déjà appliqué au bouton
  **“Enregistrer la zone”** (commenté dans le module) :

  - Suppression du wrapper `tagAppendAttributes(disabled = NA)` sur
    `run` et `run_health` → les boutons partent toujours actifs.
  - Les préconditions (zone sélectionnée, bands cochées, période valide)
    ne désactivent **plus** le bouton — elles sont validées dans
    l’`observeEvent` qui affiche un toast explicite par cause
    (`monitoring_validate_zone` / `monitoring_validate_bands` /
    `monitoring_validate_dates`).
  - `updateActionButton(disabled = is_running)` reste pour griser le
    bouton **pendant** la tâche async (protection double-clic).
  - Garde `is_running` ajouté en tête des deux `observeEvent` pour
    avaler un éventuel double-clic sans relancer la tâche.

## nemetonshiny 0.24.7 (2026-05-12)

#### Suivi sanitaire — bump nemeton 0.21.1 (fix DDL DuckDB)

- `fix(monitoring)` — bandeau **“Migration failed: Parser Error: syntax
  error at or near GENERATED”** au premier passage dans l’onglet *Suivi
  sanitaire* en mode local (DuckDB).

  Cause : le DDL des migrations de
  [`nemeton::db_migrate()`](https://pobsteta.github.io/nemeton/reference/db_migrate.html)
  utilisait `id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY`,
  syntaxe acceptée par PostgreSQL mais rejetée par DuckDB.

  Correctif (côté `nemeton` v0.21.1) : remplacement par un DDL portable
  Postgres/DuckDB (`CREATE SEQUENCE IF NOT EXISTS …` +
  `DEFAULT nextval(…)`).

  Côté `nemetonshiny` : on remonte le pin de `nemeton` à `>= 0.21.1` (et
  `Remotes: pobsteta/nemeton@v0.21.1`). Aucune modification de code
  applicatif — la correction est purement dans le paquet cœur,
  conformément à ADR-009.

## nemetonshiny 0.24.6 (2026-05-12)

#### Suivi sanitaire — worker async ne dépend plus de nemetonshiny

- `fix(monitoring)` — bandeau d’erreur **“argument inutilisé (db_url =
  db_url)”** au premier passage dans l’onglet *Suivi sanitaire* après
  installation de v0.24.5.

  Cause : la probe async passait par
  `nemetonshiny:::get_monitoring_db_connection(db_url = ...)` via
  `getFromNamespace`. Quand le worker chargeait une version obsolète de
  nemetonshiny (cache pak, dev checkout pas à jour via
  [`pkgload::load_all`](https://pkgload.r-lib.org/reference/load_all.html),
  binaire shadowé), le wrapper résolu était l’ancienne signature
  `get_monitoring_db_connection(project = NULL)` sans `db_url` → erreur
  R *“argument inutilisé”* dans le worker → bandeau warning avec le
  message brut.

  Correctif : le worker ne passe **plus jamais** par les helpers
  internes de nemetonshiny. Il appelle directement les fonctions
  exportées de `nemeton` (`db_connect()`, `db_migrate()`,
  `db_disconnect()`), qui sont l’API publique stable du paquet cœur.
  Bénéfices :

  - Plus de couplage entre la version de nemetonshiny dans le main
    process et celle dans le worker subprocess.
  - L’erreur de connexion (et de migration) est capturée directement et
    transférée proprement au main process via la valeur de retour de la
    promise.
  - Code plus court et plus simple (suppression de `.pkg_path_mon`, des
    trois `getFromNamespace`, du chargement conditionnel
    pkgload/loadNamespace).

- `chore(monitoring)` — suppression du capture
  `.pkg_path_mon <- pkgload::pkg_path()` (devenu inutile depuis que le
  worker ne charge plus nemetonshiny).

## nemetonshiny 0.24.5 (2026-05-12)

#### Suivi sanitaire — hardening du worker async contre un nemetonshiny obsolète

- `fix(monitoring)` — bandeau d’erreur **“objet
  ‘last_monitoring_db_error’ introuvable”** au premier passage dans
  l’onglet *Suivi sanitaire* après installation de v0.24.4.

  Cause : la probe async lancée dans le worker
  [`future::multisession`](https://future.futureverse.org/reference/multisession.html)
  appelait
  `utils::getFromNamespace("last_monitoring_db_error", "nemetonshiny")`.
  Si le worker charge un nemetonshiny **plus ancien** que celui de la
  session principale (cache pak, .libPaths divergent, binaire obsolète
  qui shadow le dev), la fonction est absente du namespace et
  `getFromNamespace` jette l’erreur localisée “objet ‘X’ introuvable” —
  qui remontait telle quelle dans le bandeau, sans hint pour
  l’utilisateur.

  Correctif : le worker est maintenant entièrement défensif. Tous les
  [`getFromNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
  sont enveloppés dans
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html), et si un
  helper interne est manquant, le worker retourne un message explicite
  *“Outdated nemetonshiny in worker library path. Re-install
  <pobsteta/nemetonshiny@v0.24.4> with pak::cache_clean();
  pak::pak(…).”* plutôt qu’une erreur cryptique. Le worker capture aussi
  désormais l’erreur directe de `db_connect` (`probe_err`) pour le cas
  où le helper retourne NULL sans contexte transférable entre processus.

- `chore(monitoring)` — suppression de la dépendance à
  `last_monitoring_db_error()` dans le worker (le slot de package est
  process-local, pas accessible depuis main de toute façon). La
  dépendance à `%||%` est également retirée du worker (base R 4.4+ only)
  au profit de `if/else` explicite.

## nemetonshiny 0.24.4 (2026-05-12)

#### Suivi sanitaire — connexion DB asynchrone + correctif schéma DuckDB

- `fix(monitoring)` — **table `monitoring_zone` manquante** sur les
  fichiers DuckDB ouverts après le premier (erreur
  `Catalog Error: Table with name monitoring_zone does not exist!` au
  clic sur *Enregistrer ce projet comme zone de suivi*).

  Cause : `.ensure_monitoring_schema()` mémoïsait le succès de
  `db_migrate()` dans `.nemeton_env$.monitoring_schema_initialized`,
  flag **process-level** partagé entre toutes les connexions de la
  session R. Une fois TRUE après la 1re connexion réussie, l’appel à
  `db_migrate()` était **systématiquement skippé** sur les connexions
  suivantes — y compris quand celles-ci visaient un **fichier DuckDB
  tout neuf** (changement de projet, suppression du fichier, autre
  `db_url`). Résultat : `db_connect()` créait bien le fichier (file
  I/O), mais aucune table n’y était jamais insérée → le 1er
  `INSERT INTO monitoring_zone` plantait.

  Correctif : suppression du cache wrapper.
  [`nemeton::db_migrate()`](https://pobsteta.github.io/nemeton/reference/db_migrate.html)
  est **déjà idempotent** (la table `schema_migration` traque les
  versions appliquées, une migration appliquée est sautée après un
  simple SELECT). Le coût sur une base déjà migrée est sub-ms.

- `feat(monitoring)` — **connexion DB asynchrone** (`ExtendedTask`) +
  **bandeau “loading” persistant** avec roue dentée animée
  (`.nmt-spin` + `bsicons::bs_icon("gear-fill")`). Remplace le feedback
  par toasts qui auto-dismiss en 2.5 s et étaient positionnés top-right
  (donc faciles à manquer).

  Le worker `future::plan("multisession")` ouvre
  `db_connect + db_migrate` en arrière-plan ; pendant l’exécution, le
  bandeau *Connexion à la base de suivi… — Migration du schéma en cours,
  cela peut prendre quelques secondes au premier démarrage…* reste
  affiché en place dans la zone principale. Le résultat (succès / échec
  avec message d’erreur réel) remplace ensuite le bandeau de chargement,
  sans race ni clignotement.

  Le worker ne fait qu’**éprouver la connexion** (DBI n’est pas
  sérialisable entre processus) ; les reactives métier (`validity()`,
  `alerts()`, `zones()`) continuent d’ouvrir leurs propres connexions
  sync dans le main process — fast après la migration initiale.

- `chore(monitoring)` — suppression des helpers maintenant inutiles
  `.ensure_monitoring_db_announced()` et
  `.announce_monitoring_db_ready()` (toasts top-right), remplacés par
  `.monitoring_loading_card()` (bandeau persistant in-place).

- `feat(i18n)` — nouvelle clé `monitoring_db_loading_hint` (“Migration
  du schéma en cours, cela peut prendre quelques secondes au premier
  démarrage…”).

## nemetonshiny 0.24.3 (2026-05-11)

#### Suivi sanitaire — override `NEMETON_DB_LOCAL=1` pour forcer le mode local

- `feat(monitoring)` — nouvelle variable d’environnement
  **`NEMETON_DB_LOCAL`** (truthy = `1` / `true` / `yes` / `on`, case
  insensitive). Quand elle est truthy, `.resolve_monitoring_db_url()`
  **saute** les vars `NEMETON_DB_URL` et `POSTGRESQL_ADDON_*` et utilise
  directement la base DuckDB locale du projet.

  **Cas d’usage typique** : un développeur a les credentials Clever
  Cloud (`POSTGRESQL_ADDON_*`) qui traînent dans son `~/.Renviron`
  (déposés par un build CI ou copiés depuis `clever env`). Sans
  l’override, l’app dialait la Postgres prod depuis la machine locale,
  timeout, et le bandeau restait sur “Base non configurée”. Avec
  `NEMETON_DB_LOCAL=1`, la cascade saute directement à la fallback
  DuckDB sans toucher au `.Renviron`.

  Le `cli_alert_info` mentionne désormais explicitement l’override quand
  il est actif : *“Monitoring uses local DuckDB at … (NEMETON_DB_LOCAL
  override).”*

#### Suivi sanitaire — diagnostic d’erreur visible dans le bandeau

- `feat(monitoring)` — `get_monitoring_db_connection()` capture
  désormais l’erreur réelle de
  [`nemeton::db_connect()`](https://pobsteta.github.io/nemeton/reference/db_connect.html)
  (et celle de `db_migrate()`) dans
  `.nemeton_env$.last_monitoring_db_error`. Le bandeau *Base de suivi
  non configurée* affiche ensuite cette erreur après un tiret cadratin,
  par ex. *“Renseignez NEMETON_DB_URL… — Invalid DB URL:
  C:/Users/…/monitoring.duckdb”*. Avant : `cli_warn` partait dans la
  console R (souvent invisible pour l’utilisateur Shiny) et le bandeau
  restait générique.
- `feat(monitoring)` — feature-flag : `.resolve_monitoring_db_url()`
  vérifie via `getFromNamespace(".detect_driver", "nemeton")` que la
  version installée de `nemeton` supporte bien le backend DuckDB
  (introduite en v0.21.0). Si non, message explicite *“Installed nemeton
  is too old (no DuckDB support). Re-install
  <pobsteta/nemeton@v0.21.0>.”* dans le bandeau et un `cli_warn` une
  fois par session expliquant comment réparer
  ([`pak::pak()`](https://pak.r-lib.org/reference/pak.html)
  - clear cache).
- `feat(monitoring)` — capture aussi les erreurs de `db_migrate()` (qui
  s’exécute juste après la connexion) sous forme *“Migration failed: ”*.
  Si la migration échoue, la connexion est refermée proprement et `NULL`
  est retourné (au lieu de laisser une connexion sans schéma qui
  crasherait les requêtes suivantes).

## nemetonshiny 0.24.2 (2026-05-11)

#### Suivi sanitaire — 4 fixes UX critiques

- `fix(duckdb)` — **bug Windows path** : sur Windows
  `duckdb:///C:/Users/<projet>/data/monitoring.duckdb` était parsé par
  `nemeton:::.parse_duckdb_url()` en
  `/C:/Users/<projet>/data/monitoring.duckdb` (slash en trop) → DuckDB
  refusait d’ouvrir → bandeau “Base non configurée” affiché à tort alors
  que le toast disait “Base locale prête”. Fix :
  `.resolve_monitoring_db_url()` émet désormais un **bare path** (sans
  préfixe `duckdb://`), reconnu cross-platform par
  `nemeton:::.detect_driver()` via le suffixe `.duckdb`.
- `fix(ux)` — **bouton “Enregistrer ce projet comme zone de suivi”**
  était figé en `disabled = NA` au niveau HTML, donc non cliquable même
  quand toutes les conditions étaient réunies. Supprimé : l’observer du
  clic valide déjà les préconditions et affiche une notification
  d’erreur si elles ne sont pas remplies, donc l’utilisateur reçoit
  toujours un feedback précis sans être bloqué à l’avance.
- `fix(ux)` — **toast “Création de la base DuckDB locale…”**
  apparaissait et disparaissait trop vite pour être perçu (la connexion
  DuckDB est typiquement \< 100 ms). Désormais le toast a
  `duration = 2.5` s minimum et un toast séparé “Base locale prête” est
  émis APRÈS confirmation de la connexion (au lieu d’un
  [`later::later`](https://later.r-lib.org/reference/later.html) aveugle
  qui se déclenchait même en cas d’échec).
- `fix(ux)` — la **checkbox “Inclure les classes faible et moyenne”**
  débordait du `card_header` next to *“Alertes détectées”* sur les
  viewports étroits. Déplacée sur sa propre ligne juste sous le header —
  toujours sur une seule ligne, peu importe la largeur.

## nemetonshiny 0.24.1 (2026-05-11)

#### UX Suivi sanitaire — diagnostic précis + toast + séparateur “au”

- `feat(i18n)` — le séparateur du `dateRangeInput` (sidebar *Période
  d’observation* et *Période d’entraînement* FORDEAD) était hardcodé en
  *“to”* malgré la langue FR. Nouvelle clé i18n `date_range_separator` :
  *“au”* en FR, *“to”* en EN.
- `feat(ux)` — un **toast “Tentative de connexion à la base…” ou
  “Création de la base DuckDB locale…”** s’affiche en bas à droite la
  première fois que l’utilisateur ouvre l’onglet *Suivi sanitaire*
  (suivi 1,2 s plus tard d’un toast *“Base prête”*). Avant cette
  release, l’onglet semblait figé pendant le bootstrap du schéma (qui
  prend ~1 s sur un DuckDB neuf). Le toast est idempotent (un par
  session, id stable).
- `fix(ux)` — le bandeau *Base de suivi non configurée* distingue
  maintenant **deux causes** au lieu d’une seule :
  - **aucun projet chargé** → message *“Aucun projet chargé.
    Sélectionnez ou créez un projet dans l’onglet Sélection pour activer
    le mode local (DuckDB).”*
  - **`duckdb` package manquant** → message *“Le paquet R duckdb n’est
    pas installé, le mode local n’est pas disponible. Installez avec
    install.packages(‘duckdb’) ou configurez Postgres.”* Avant : un seul
    message générique qui ne disait pas quoi faire.
- Le `register_hint` (petit texte sous *Enregistrer ce projet comme zone
  de suivi*) bénéficie du même diagnostic — il indique désormais
  l’installation de `duckdb` plutôt que le générique *“impossible
  d’enregistrer la zone”*.

## nemetonshiny 0.24.0 (2026-05-11)

#### Added — Suivi sanitaire en mode local (DuckDB)

L’onglet *Suivi sanitaire* ne nécessite plus une instance
PostgreSQL+TimescaleDB pour démarrer. Quand aucune variable
d’environnement `NEMETON_DB_*` n’est définie et qu’un projet est chargé,
la couche monitoring bascule automatiquement sur un fichier **DuckDB
local** stocké à `<project>/data/monitoring.duckdb` (à côté de
`samples.gpkg`).

- **Bascule transparente** :
  `service_monitoring_db.R::.resolve_monitoring_db_url(project)`
  inspecte les vars `NEMETON_DB_URL` / `NEMETON_DB_*` /
  `POSTGRESQL_ADDON_*` et, à défaut, émet un
  `duckdb:///<project>/data/monitoring.duckdb` exploitable par
  [`nemeton::db_connect()`](https://pobsteta.github.io/nemeton/reference/db_connect.html)
  (v0.21.0).
- **Pré-requis** : le package `duckdb (>= 0.8.0)` est ajouté en
  `Suggests`. S’il n’est pas installé, la bascule reste silencieuse et
  l’utilisateur voit l’ancien bandeau “Base non configurée”.
- **UI** : le bandeau de l’onglet *Suivi sanitaire* affiche désormais
  **trois états** au lieu de deux :
  - rouge : aucune base configurée et aucun projet chargé ;
  - **bleu (NOUVEAU)** : *“Mode local (DuckDB) — base de suivi monoposte
    stockée dans le projet”* avec un hint pour passer en Postgres
    multi-utilisateurs ;
  - vert : Postgres connecté, N zones disponibles.

#### Changed — Signatures async pour passer la DB URL au worker

`run_ingestion_async()` et `run_fordead_async()` acceptent désormais un
paramètre `db_url` dans leur `$invoke(...)`. Les observers dans
`mod_monitoring.R` pré-résolvent l’URL via
`.resolve_monitoring_db_url(app_state$current_project)` avant de lancer
le worker — nécessaire parce que les futures workers ne voient pas
`app_state` ni les vars Shiny.

`get_monitoring_db_connection()` accepte deux nouveaux paramètres
optionnels : \* `project` : pour la résolution synchrone du fallback
DuckDB (utilisé par les 9 callsites dans `mod_monitoring.R`). \*
`db_url` : pour le path asynchrone qui reçoit l’URL déjà résolue depuis
l’observer.

#### Bumped — `Remotes: pobsteta/nemeton@v0.21.0`

Pour bénéficier du backend DuckDB côté cœur
([`nemeton::db_connect`](https://pobsteta.github.io/nemeton/reference/db_connect.html)
détecte le scheme `duckdb://` et applique les migrations dans
`inst/db/migrations/duckdb/`).

## nemetonshiny 0.23.17 (2026-05-11)

#### Plan d’actions — fond satellite Esri.WorldImagery

- `feat(report)` — les cartes UGF du PDF *Plan d’actions* passent
  d’`OpenTopoMap` (relief topographique) à **`Esri.WorldImagery`**
  (imagerie satellite gratuite, sans clé API). Permet de voir la canopée
  réelle / l’occupation du sol derrière chaque parcelle plutôt que des
  courbes de niveau abstraites.
- Le rapport *Synthèse* reste sur `OpenTopoMap` (différent domaine
  d’usage : score par famille d’indicateurs, le relief est plus parlant
  qu’une photo aérienne).
- Le fallback géométrie-seule (déclenché si les tuiles satellite sont
  inaccessibles côté serveur) est inchangé.

## nemetonshiny 0.23.16 (2026-05-11)

#### Export PDF Plan d’actions — retiré + verbose quarto

- `fix(report)` — l’export PDF échouait encore avec une erreur générique
  `"Error running quarto CLI from R"` même quand toutes les UGF
  tombaient en fallback géométrie-seule (tuiles OSM inaccessibles côté
  serveur, ce qui est légitime et géré). Cause probable : la déclaration
  `\small` placée en v0.23.13 dans
  `inst/quarto/action_plan_template.qmd` entre `\begin{longtable}{...}`
  et `\toprule` — booktabs + longtable peuvent mal traiter une
  déclaration de taille à cet endroit sur certaines toolchains xelatex
  et faire planter tout le rendu. Désormais le template **ne wrappe plus
  le longtable** dans un changement de taille ; il utilise la taille par
  défaut qui compile de manière fiable. Les `{` `}` parasites corrigés
  en v0.23.13 ne reviennent pas (puisqu’il n’y a plus de wrapper à mal
  sortir).
- `debug(report)` —
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)
  passe en `quiet = FALSE` côté Plan d’actions (aligné avec la Synthèse
  qui était déjà verbose). La sortie quarto / xelatex est désormais
  streamée dans la console R, ce qui permet de diagnostiquer une vraie
  erreur LaTeX (package manquant, `\includegraphics` malformé, etc.) au
  lieu du seul “Error running quarto CLI from R” générique.

## nemetonshiny 0.23.15 (2026-05-11)

#### Plan d’actions — cartes UGF alignées sur le rapport Synthèse + libellés boutons

- `fix(export)` — porte la recette éprouvée de `generate_family_maps()`
  (utilisée par le rapport PDF de l’onglet *Synthèse* qui rend ses
  cartes correctement) sur `render_ug_map()` de l’export PDF Plan
  d’actions :
  - provider **OpenTopoMap** (au lieu d’OpenStreetMap) ;
  - **zoom explicite** calculé à partir de la bbox de la parcelle
    (`auto_zoom <- min(17, max(13, round(17 - log2(extent_size * 100))))`)
    au lieu de `zoom = NULL` ;
  - dimensions PNG **800×600 res=150** (au lieu de 1200×800 res=180)
    pour rester aligné avec la sortie qui passe déjà en production sur
    la Synthèse.

  Les autres garde-fous v0.23.14 (fermeture explicite du device avant la
  validation taille, fallback géométrie seule sans tuiles, sanity check
  `file.size < 100 → NA_character_`) sont conservés.
- `ui(action_plan)` — libellés des boutons d’export uniformisés avec le
  reste de l’app :
  - “Exporter GeoPackage” → “Télécharger le GeoPackage” (FR) / “Download
    GeoPackage” (EN)
  - “Exporter PDF” → “Télécharger le PDF” (FR) / “Download PDF” (EN)

  Cohérent avec les libellés `download_pdf` (rapport Synthèse) et
  `download_gpkg` (export projet) déjà existants.

## nemetonshiny 0.23.14 (2026-05-11)

#### Export PDF Plan d’actions — toast persistant + résilience renderUGmap

- `fix(export)` — le toast *“Génération PDF…”* (roue dentée animée)
  **s’auto-dismissait après 8 s** côté JS. Pour un export Quarto +
  xelatex typique (15–30 s avec maps), le spinner disparaissait bien
  avant que la boîte de dialogue *Enregistrer le PDF* du navigateur
  n’apparaisse → l’utilisateur voyait un écran vide puis une boîte de
  dialogue sortie de nulle part. Désormais `nemetonShowDownloadToast` :
  - n’a plus d’auto-dismiss (`duration: null`) ;
  - le serveur envoie un `nemetonHideDownloadToast` (custom message
    Shiny) à la fin de `content` (via `on.exit` → s’exécute même si
    l’export échoue), donc le spinner disparaît **synchroniquement**
    avec l’envoi du fichier par le serveur ;
  - filet de sécurité 120 s (au lieu de 8 s) en cas de perte du message
    custom.
- `fix(export)` — le PDF généré faisait **1 KB et ne s’ouvrait pas**.
  Cause : `generate_action_plan_pdf()` levait une erreur (typiquement la
  fallback `render_ug_map` v0.23.13 passait `bg` / `xlim` / `ylim` à
  `plot.sf` qui ne les supporte pas uniformément selon la version de
  `sf` → PNG corrompu ou 0 byte → `\includegraphics` plantait xelatex →
  `quarto_render` échouait). Le tryCatch upstream écrivait alors
  `"PDF generation failed"` (22 octets) dans le fichier — d’où le PDF
  cassé livré à l’utilisateur. Désormais :
  - **fallback `render_ug_map` minimal** : appel `plot.sf` sans `bg` /
    `xlim` / `ylim`, plot.sf auto-fit la bbox de la géométrie ;
  - **fermeture explicite du device PNG** avant la fin de
    `render_ug_map` (au lieu de `on.exit`) pour garantir que le fichier
    est complètement flushé avant la validation ;
  - **validation taille PNG** : si le fichier produit fait moins de 100
    octets ou n’existe pas, `render_ug_map` renvoie `NA_character_` et
    le template skip `\includegraphics` proprement (au lieu d’envoyer un
    fichier corrompu à xelatex).
- `fix(export)` — la notification d’erreur PDF est désormais **sticky**
  (`duration = NULL` au lieu de `duration = 8`). L’erreur réelle
  (`conditionMessage(e)`) reste affichée jusqu’à fermeture manuelle, ce
  qui permet de diagnostiquer la cause sans avoir à inspecter les logs
  serveur. Le message verbatim est aussi loggué via
  [`cli::cli_warn`](https://cli.r-lib.org/reference/cli_abort.html).
- `fix(export)` — quand la génération échoue, le fichier livré contient
  maintenant le texte *“PDF generation failed. See the in-app error
  toast for the underlying cause.”* (au lieu d’un cryptique *“PDF
  generation failed”*) pour orienter vers la vraie diagnostic.

## nemetonshiny 0.23.13 (2026-05-11)

#### Rapport PDF Plan d’actions — accolades parasites + cartes UGF restaurées

- `fix(report)` — **`{` et `}` apparaissaient en littéral** dans le PDF,
  au-dessus et au-dessous du tableau d’actions de chaque UGF. Cause : le
  template écrivait `cat("{\\small\n")` puis le longtable puis
  `cat("}\n\n")`. Pandoc voyait `{` / `}` seuls sur leur propre
  paragraphe (séparés par des newlines), ne les reconnaissait pas comme
  bloc LaTeX brut (raw LaTeX = `\begin{env}…\end{env}` ou backticked
  `\`\`\`{=latex}\`\`\``fences) et les passait verbatim au PDF. Désormais``est placé **à l'intérieur** de l'environnement`longtable`(Pandoc préserve son contenu comme raw LaTeX) ; la déclaration scope automatiquement jusqu'à`\end{longtable}\`.
- `fix(report)` — **cartes des parcelles absentes** du PDF.
  `render_ug_map()` retournait `NA_character_` dès la moindre erreur de
  [`maptiles::get_tiles()`](https://rdrr.io/pkg/maptiles/man/get_tiles.html)
  (réseau coupé sur le serveur, rate-limit OSM, package `maptiles` non
  installé) ; le template skippait alors silencieusement le bloc
  `\includegraphics`. Désormais un **fallback sans tuiles OSM** dessine
  la géométrie de la parcelle sur fond gris clair, donc le rapport
  contient toujours une carte par UGF — même réseau coupé.
  [`cli::cli_alert_info`](https://cli.r-lib.org/reference/cli_alert.html)
  documente la raison du fallback dans les logs R.

## nemetonshiny 0.23.12 (2026-05-11)

#### Terrain — fixes défensifs init (réactivité UI)

- `fix(samples)` — l’observer `leafletProxy()` du *Terrain* est
  désormais **gardé derrière `req(input$map_zoom)`**. Avant le premier
  rendu de la carte Sampling (typiquement quand l’utilisateur travaille
  dans un autre onglet — *Plan d’actions* par ex.), l’observer tirait
  sur chaque changement de `sampling_rv$plots` /
  `sampling_rv$observations` / `app_state$language` et empilait des
  messages `leaflet-calls` dans la file de flush différée pour une carte
  qui n’existait pas encore côté client. Désormais l’observer
  court-circuite tant que `input$map_zoom` est NULL ; il reprend son
  comportement nominal dès que la carte est ouverte au moins une fois.
- `perf(samples)` — `.restore_samples` ouvre désormais `samples.gpkg`
  **une seule fois** via
  [`sf::st_layers()`](https://r-spatial.github.io/sf/reference/st_layers.html)
  pour scanner les layers disponibles, puis lit directement les layers
  présents via
  [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html).
  L’ancien chemin appelait `load_samples(layer = ...)` deux fois
  (plots + observations), ce qui ouvrait le GPKG quatre fois au total
  (chaque `load_samples` faisait son propre `st_layers` + `st_read`).
  Sur un projet où `current_project` est régulièrement réassigné, cette
  demi-pile d’I/O disparaît.

Ces deux mesures sont défensives. Si la sluggishness ressentie sur
l’onglet *Plan d’actions* ne disparaît pas, il faut probablement
chercher du côté de `ug_build_sf()` / `output$kanban_board` (renderUI
sur grandes listes d’actions) — voir issue à venir.

## nemetonshiny 0.23.11 (2026-05-11)

#### Terrain — légende plots restaurée au rendu initial

- `fix(samples)` — **la légende Base/Over/Observation n’apparaissait
  plus** sur la carte *Terrain* depuis v0.23.10. La cause : la légende
  avait été déplacée entièrement dans un observer `leafletProxy()`
  séparé de `renderLeaflet`. Quand l’observer proxy se déclenche
  **avant** que l’élément carte ne soit monté sur le client (cas
  fréquent au premier flush — l’ordre entre outputs et observers n’est
  pas garanti), les messages `leaflet-calls` arrivent à un client sans
  carte et **sont perdus**. Du coup, plus aucune légende.
- La légende initiale est désormais redessinée **dans `renderLeaflet`**
  avec `addLegend(colors = …, labels = …)` (toujours sans `colorFactor`,
  donc l’ordre saumon ↔︎ vert reste correct — pas de régression v0.23.9 →
  v0.23.10). Lecture de `sampling_rv$observations` en `isolate()` pour
  ne PAS forcer un full-redraw de la carte quand seules les observations
  changent : la mise à jour dynamique reste portée par l’observer
  `leafletProxy()` (qui réémet `removeControl + addLegend` avec
  `layerId = "plots-legend"`).
- Net : légende correcte au premier render (cas le plus fréquent côté
  utilisateur) ; mise à jour live conservée au clic *Envoyer vers
  Terrain* (pas de flicker tuiles, pan/zoom préservé).

## nemetonshiny 0.23.10 (2026-05-11)

#### Envoyer vers Terrain — auto-refresh carte + légende correcte

- `fix(samples)` — **légende inversée** :
  [`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)
  réordonne son `domain` par ordre **alphabétique** ; pour
  `c("Base","Over","Observation")` il triait en
  `c("Base","Observation","Over")` et mappait la palette
  positionnellement → *Observation* héritait du saumon `#ff7f0e` (place
  2 = position d’*Over*) et *Over* héritait du vert `#2ca02c` (place 3).
  Les pastilles légende contredisaient les marqueurs sur la carte.
  Désormais la légende est construite avec
  `addLegend(colors = …, labels = …)` qui préserve strictement l’ordre
  passé.
- `fix(samples)` — **la carte ne se mettait pas à jour automatiquement**
  après *Envoyer vers Terrain* : l’output `renderLeaflet` est suspendu
  (`suspendWhenHidden = TRUE`) pendant que l’utilisateur reste sur
  l’onglet *Plan d’actions* ; les changements de
  `sampling_rv$observations` n’étaient observés qu’au retour sur
  l’onglet *Terrain* — et encore, redessinaient la carte entière
  (flicker tuiles, perte du pan/zoom). Désormais les points
  d’observation et la légende sont gérés par un observer dédié
  `leafletProxy()` qui synchronise **uniquement** le groupe
  `Observations` et le contrôle légende (`layerId = "plots-legend"`).
  Conséquence :
  - la carte se met à jour **immédiatement** au clic *Envoyer vers
    Terrain*, même si l’onglet est masqué (le proxy met les opérations
    en file d’attente côté serveur et les rejoue dès que le client est
    connecté) ;
  - les tuiles ne sont plus rechargées ;
  - le pan/zoom courant est conservé ;
  - la légende reflète strictement les marqueurs présents.
- `refactor(samples)` — `renderLeaflet` ne dépend plus de
  `sampling_rv$observations` ; le groupe `Observations` est préinscrit
  dans le `addLayersControl` pour exposer le toggle dès le premier
  render, et c’est le proxy qui peuple le groupe. Le groupe peut être
  vide tant qu’aucun point n’a été envoyé.

## nemetonshiny 0.23.9 (2026-05-11)

#### Envoyer vers Terrain — coexistence calibration + observations

- `fix(samples)` — bug critique : cliquer **Envoyer vers Terrain** dans
  l’onglet *Plan d’actions* **détruisait silencieusement les placettes
  de calibration Base/Over** générées dans l’onglet *Terrain*.
  `save_samples()` faisait `unlink(samples.gpkg)` puis réécrivait le
  fichier avec uniquement les points d’observation. Désormais :
  - `save_samples(project_id, plots, layer = "plots"|"observations")`
    écrit **un layer nommé** dans `samples.gpkg` avec
    `append = FALSE, delete_layer = TRUE` — il remplace uniquement le
    layer cible et préserve les autres ;
  - les placettes de calibration (`mod_sampling`) restent dans le layer
    `plots` (par défaut) ; les points d’observation (`mod_action_plan`)
    sont écrits dans le layer `observations` ;
  - `samples_count` / `samples_generated_at` (metadata projet) ne sont
    mis à jour **que** pour le layer `plots` — l’envoi d’observations ne
    perturbe plus la comptabilité du plan d’échantillonnage ;
  - `load_samples(project_id, layer = "plots"|"observations")` lit un
    layer spécifique et renvoie `NULL` si le layer est absent
    (silencieusement).
- `feat(samples)` — la **carte de l’onglet Terrain** affiche désormais
  les **deux familles de points en simultané** :
  - Base / Over (bleu / orange, parcours TSP, icônes orienteering
    Départ/Arrivée) — inchangés ;
  - Observations issues du plan d’actions (vert `#2ca02c`, groupe
    leaflet dédié `Observations` toggle-able dans le layer control,
    popup *plot_id — observation (UGF, an)*).
- `feat(samples)` — **légende dynamique** : ne liste que les familles
  effectivement présentes dans `plots` et `observations`. Plus de
  `addLegend(values = c("Base","Over"))` en dur. Nouvelle clé i18n
  `sampling_legend_plots_title` (FR *Placettes* / EN *Plots*).
- `feat(samples)` — `mod_sampling` réagit à `samples_refresh` : quand
  `mod_action_plan` bumpe ce signal après *Envoyer vers Terrain*, le
  layer `observations` est rechargé sans changer de projet.
- Couverture : test régression
  `save_samples 'observations' layer coexists with 'plots' layer`
  (couche calibration préservée par un save observations successif, et
  inversement ; `samples_count` reste sur `plots` ; couche inconnue →
  NULL silencieux).

## nemetonshiny 0.23.8 (2026-05-11)

#### Plan d’actions — toast PDF unifié

- `fix(action_plan)` — l’export PDF avait deux toasts qui se
  superposaient au clic sur **Exporter PDF** : un toast serveur
  (`showNotification` avec icône `spinner` et libellé *« Génération du
  PDF en cours… »*, dans `downloadHandler`) et le nouveau toast client
  roue dentée v0.23.7 (helper JS `nemetonShowDownloadToast`). Le serveur
  est retiré : on garde uniquement le toast client, cohérent avec
  **Exporter GeoPackage**. Clé i18n orpheline
  `action_plan_pdf_generating` supprimée.

## nemetonshiny 0.23.7 (2026-05-11)

#### Plan d’actions — Exports GPKG / PDF

- `feat(action_plan)` — export GeoPackage : nouvelle colonne
  **`annee_civile`** ajoutée à la couche `actions`, dérivée de
  `current_year + annee_cible - 1L`. Le champ `annee_cible` du schéma
  reste un offset relatif 1..HORIZON (utilisé par le LLM et les calculs
  internes du plan) ; la colonne `annee_civile` matérialise l’année
  calendaire au moment de l’export pour les consommateurs QGIS/terrain.
  Ancre = année courante au clic d’export (pas de stockage). Placée
  juste après `annee_cible` dans la couche `actions`. Test de régression
  `export_action_plan_gpkg writes actions + ugf layers` étendu pour
  vérifier la présence + la valeur + l’ordre des colonnes.
- `feat(action_plan)` — clic sur **Exporter GeoPackage** ou **Exporter
  PDF** affiche maintenant un **toast roue dentée** en bas à droite
  pendant la phase d’export. `downloadButton` ne fournit aucun événement
  côté serveur (l’ouverture de la boîte de dialogue de téléchargement
  est strictement client), donc le toast est déclenché par un `onclick`
  JS qui appelle un helper `nemetonShowDownloadToast()` ajouté dans
  `custom.js`. Le toast utilise `Shiny.notifications.show` (même slot
  visuel que
  [`shiny::showNotification`](https://rdrr.io/pkg/shiny/man/showNotification.html))
  avec `duration: null, closeButton: false`, et s’auto-dismisse après
  8 s. Idempotent sur re-clic (le timer est réinitialisé). Deux
  nouvelles clés i18n FR/EN : `action_plan_export_running_gpkg`,
  `action_plan_export_running_pdf`.
- `feat(action_plan)` — **rapport PDF Quarto refondu**. Page de garde
  dédiée (titre, sous-titre, encadré bleu avec date / horizon / nombre
  d’actions / nombre d’UGF), header courant *fancyhdr*, synthèse globale
  en `tcolorbox` avec bilan coloré vert/rouge. Pour chaque UGF :
  - une **carte OSM** centrée sur la parcelle est pré-rendue côté R via
    `maptiles::get_tiles(provider = "OpenStreetMap")`
    - `plot_tiles()` + tracé du polygone (bleu translucide, bordure
      pleine). PNG 1200x800 sauvegardé dans le temp_dir et inclus via
      `\includegraphics`. Dégradation silencieuse si `maptiles`
      indisponible ou si la requête tuile échoue (le PDF est rendu sans
      la carte plutôt que de planter) ;
  - une **carte de synthèse UGF** (surface, nb d’actions, coût, revenu,
    bilan coloré) en `tcolorbox` ;
  - le **tableau des actions** passe en `longtable` (continue sur
    plusieurs pages si besoin), avec priorité en couleur (rouge / orange
    / vert) ;
  - une **zone “Commentaires” large** en `tcolorbox` *breakable* sous le
    tableau, qui restitue le `commentaire` de chaque action non-vide
    avec l’année cible, le type et le statut coloré.

  `generate_action_plan_pdf()` enrichit le payload `data` passé au qmd :
  `commentaire` par action, `map_png` (chemin ou NA) par UGF, et
  factorise la création du `temp_dir` pour qu’il porte à la fois les PNG
  et le qmd rendu.

## nemetonshiny 0.23.6 (2026-05-11)

#### Plan d’actions — chat IA : surface / volume / coûts conservés

- `fix(action_plan)` — quand l’utilisateur cliquait sur **Affiner le
  plan avec l’IA**, les lignes proposées par le LLM arrivaient avec les
  colonnes `surface_ha`, `volume_m3`, `nb_tiges`, `rdi`, `cout_eur` et
  `revenu_eur` **vides**. Deux causes côté prompt :
  - `build_action_plan_chat_prompt()` se contentait de référencer
    `build_action_plan_prompt` par son nom de fonction R sans embarquer
    le schéma JSON, donc le LLM ignorait l’existence du bloc `quantite`
    ;
  - `mod_action_plan` envoyait au LLM un **résumé** des actions
    courantes (juste `id, ug_id, type, annee_cible, statut, priorite`),
    ce qui empêchait le LLM de réémettre les quantités existantes lors
    d’une modification. Le schéma JSON et le rappel économique sont
    maintenant factorisés dans `.action_plan_json_schema()` et
    `.action_plan_econ_hint()`, partagés par les deux prompts. Le prompt
    de chat embarque désormais le schéma complet, ajoute des règles
    explicites (« remplir tout le bloc `quantite` », « réémettre
    l’action entière sur update »), et reçoit le **JSON complet** du
    plan courant. Couverture testthat :
    `build_action_plan_chat_prompt embeds the full JSON schema + econ hint`.

## nemetonshiny 0.23.5 (2026-05-09)

#### Plan d’actions — chat IA : scope + 0e9crasement

- `feat(action_plan)` — le **chat IA gagne deux contrôles** juste sous
  l’historique pour piloter chaque tour de conversation : un radio
  **Toutes les UGF / Sélection courante** (`chat_scope`) et une checkbox
  **Écraser le plan existant** (`chat_overwrite`). Mêmes sémantiques que
  dans le modal “Générer les actions (IA)” :
  - `scope = "selected"` restreint le `ctx$ug_ids` envoyé au prompt aux
    seules UGF cochées sur la carte (sinon garde-fou `action_plan_no_ug`
    si rien n’est sélectionné) ;
  - `overwrite = TRUE` au moment de l’apply supprime les actions
    existantes des UGF ciblées avant le `bulk_upsert_actions()`. Le
    modal de confirmation qui s’ouvre quand le LLM renvoie un bloc
    `actions` JSON affiche désormais une **bannière `text-warning`**
    quand l’overwrite est coché, listant le nombre d’UGF impactées
    (`action_plan_chat_apply_overwrite_warn_fmt`). Les UGF cibles sont
    stashées dans `rv_state$pending_chat_target_ugs` au moment de
    l’envoi pour rester cohérentes entre le tour qui propose les actions
    et celui qui les applique. Nouvelle clé i18n
    `action_plan_chat_scope_sel`.

#### Bascule de langue FR ↔︎ EN

- `fix(language)` — basculer FR↔︎EN dans le sélecteur de la navbar
  **fonctionne enfin** : la page se recharge automatiquement en EN (ou
  FR) et **ne reverse plus** sur la langue de démarrage. Deux bugs
  combinés :
  - `app_server.R` écrivait dans `nemeton.app_language` alors que
    `app_ui` lit `getOption("nemeton.app_options")$language` (clé
    totalement différente) — donc la préférence ne survivait pas au
    reload. Désormais on persiste dans *la bonne* option
    `nemeton.app_options$language`.
  - L’observer affichait un toast *“Rechargez la page pour appliquer”*
    sans recharger automatiquement. Remplacé par un appel direct
    `session$reload()` pour que le rebuild d’`app_ui` se fasse sans
    intervention de l’utilisateur. Garde anti-init : si la nouvelle
    valeur est identique à `app_state$language`, l’observer retourne tôt
    — pas de reload involontaire au démarrage de session.
- `i18n` : clé orpheline `language_changed` (le toast manuel) retirée.

## nemetonshiny 0.23.4 (2026-05-09)

#### Plan d’actions — chat IA en français

- `i18n(action_plan)` — les libellés de rôle dans l’historique du chat
  passent par i18n : `msg$role` brut (“user” / “assistant”) n’apparaît
  plus tel quel dans l’UI. `output$chat_history_ui` traduit au moment du
  rendu en **“Vous”** / **“Assistant”** (FR) ou **“You”** /
  **“Assistant”** (EN). Le modèle de données conserve les clés anglaises
  (cohérent avec la convention LLM, le prompt builder downstream reste
  intact) — la traduction est purement cosmétique au display, et bascule
  dynamiquement avec le switch de langue. Deux nouvelles clés i18n :
  `action_plan_chat_role_user`, `action_plan_chat_role_assistant`.

## nemetonshiny 0.23.3 (2026-05-09)

#### Plan d’actions — toast “L’IA réfléchit…”

- `feat(action_plan)` — clic sur **Envoyer** dans le chat IA fait
  apparaître un **toast en bas à droite** avec une **roue dentée
  tournante** et le label *“L’IA réfléchit…”*. Le toast reste affiché
  tant que la réponse du LLM n’est pas reçue : `duration = NULL`,
  `closeButton = FALSE`, type `"default"` (neutre). Suppression garantie
  via `on.exit(shiny::removeNotification(thinking_id))` dans toutes les
  branches de retour de l’observer (succès, erreur LLM, parse échoué).
  L’engrenage utilise la classe Font Awesome `fa-spin` sur l’icône
  `shiny::icon("gear")`. Nouvelle clé i18n `action_plan_chat_thinking`
  (FR *“L’IA réfléchit…”* / EN *“AI is thinking…”*).

## nemetonshiny 0.23.2 (2026-05-09)

#### Plan d’actions — chat dans la sidebar droite + auto-scroll

- `feat(action_plan)` — le **chat IA** rejoint la sidebar droite **sous
  la carte “Tableau des actions”**, plutôt que dans une sidebar gauche
  dédiée. Disposition haut → bas de la sidebar droite : *Tableau des
  actions* (Sélection / IA / Manuel / Exports) puis *Affiner le plan
  avec l’IA* (historique scrollable + textarea + boutons Effacer /
  Envoyer). Le double
  [`bslib::layout_sidebar`](https://rstudio.github.io/bslib/reference/sidebar.html)
  imbriqué introduit en v0.23.1 disparaît, on revient à un seul
  `layout_sidebar` avec les deux cartes empilées dans la sidebar droite
  — meilleure densité d’information sur les écrans portables.
- `feat(action_plan)` — la zone d’historique du chat **scrolle
  automatiquement vers le bas** à chaque mise à jour. La div
  `.chat-history` reçoit un `id` stable (`ns("chat_history")`) ;
  `output$chat_history_ui` injecte à la fin de chaque render un
  `tags$script` qui fait
  `setTimeout(function(){ el.scrollTop = el.scrollHeight; }, 0)` — le
  `setTimeout(0)` diffère au prochain tick pour que le DOM soit à jour
  quand `scrollHeight` est lu. Le dernier message reste visible sans
  intervention manuelle.

#### Plan d’actions — libellé bouton

- `i18n(action_plan)` — bouton **“Générer (IA)”** renommé **“Générer les
  actions (IA)”** (FR) / **“Generate actions (AI)”** (EN), pour
  clarifier que la cible est bien la génération du plan d’actions et pas
  autre chose (ex. : un rapport).

## nemetonshiny 0.23.1 (2026-05-09)

#### Plan d’actions — chat IA en sidebar gauche

- `feat(action_plan)` — le **chat IA** quitte la modal et s’installe
  dans une **sidebar gauche persistante** de 350 px. Carte collapsible
  (header `bg-info` avec icône `chat-dots`), historique scrollable
  (max-height 50 vh, min-height 160 px, fond gris clair),
  `textAreaInput` 3 lignes resize=vertical, boutons *Effacer* /
  *Envoyer* en flex. La conversation reste visible pendant que
  l’utilisateur navigue map / table / Kanban — auparavant un bouton
  “Ouvrir le chat” déclenchait un modal qui se fermait à chaque
  interaction.
- `refactor(action_plan)` — le bouton **“Ouvrir le chat”** dans la
  section IA de la sidebar droite est retiré (doublon avec le panel
  persistant). L’observer `input$open_chat` (~30 LOC qui faisait
  `showModal()`) supprimé. Layout passe à un double
  [`bslib::layout_sidebar`](https://rstudio.github.io/bslib/reference/sidebar.html)
  imbriqué : sidebar gauche (chat) → sidebar droite (action panel) →
  contenu principal. Les deux sidebars se collapsent indépendamment via
  le bouton bslib en bordure (utile sur écran portable).

#### Plan d’actions — sync carte ↔︎ tableau

- `fix(action_plan)` — clic sur une parcelle dans la **carte**
  sélectionne maintenant **toutes les lignes correspondantes** dans le
  tableau. Le handler `input$map_shape_click` (le toggle qui mettait à
  jour `selected_ug_rv` et la couche orange Selection) ne propageait pas
  la sélection à `DT` ; un appel
  `DT::selectRows(proxy, which(df$ug_id %in% cur))` est ajouté. Le sens
  table → carte (déjà fonctionnel via l’observer
  `input$action_table_rows_selected`) reste inchangé. Pas de boucle
  réactive : `reactiveVal` dedupe par
  [`identical()`](https://rdrr.io/r/base/identical.html) donc le
  round-trip map → selected_ug_rv → table → reverse-observer →
  selected_ug_rv s’arrête au 2e pas.

#### Removed

- i18n: `action_plan_open_chat`, `action_plan_chat_input_label`
  (orphelines après la refonte du chat en sidebar).

## nemetonshiny 0.23.0 (2026-05-09)

#### Plan d’actions — Kanban libre + édition par double-clic

- `feat(action_plan)` — **drag-and-drop libre entre toutes les colonnes
  du Kanban**. La sémantique du DAG (proposée → validée → planifiée →
  réalisée → abandonnée) qui empêchait certaines transitions disparaît :
  un utilisateur peut désormais déplacer n’importe quelle fiche vers
  n’importe quelle colonne. Le service `update_action_in_plan()`
  n’enforce plus le DAG ; il valide uniquement que le nouveau statut
  figure dans `ACTION_PLAN_TRANSITIONS`. La fonction
  `is_valid_status_transition()` reste exportée comme documentation du
  flux naturel mais ne gate plus les writes.
- `feat(action_plan)` — **double-clic sur une fiche Kanban ouvre une
  modal d’édition** pré-remplie avec les valeurs courantes (statut,
  priorité, année calendaire, commentaire). Le commentaire est éditable
  en `textAreaInput` 6 lignes, c’est le cas d’usage principal de la
  modal — l’édition inline du tableau DT est single-line et restait peu
  pratique pour des commentaires longs. Le handler `dblclick` est
  délégué au niveau du board (un seul listener pour toutes les cartes),
  avec cleanup entre re-renders pour éviter les fuites.
- `refactor(action_plan)` — le bouton **“Déplacer”** dans la dropdown de
  chaque carte Kanban est supprimé : avec le drag-drop libre il faisait
  doublon. ~50 lignes d’observer dispatcher `kanban_move_*` retirées en
  conséquence. La constante `KANBAN_STATUSES` (uniquement utilisée par
  la dropdown) retirée également.
- `feat(action_plan)` — **tri chronologique des cartes par colonne** :
  dans chaque statut Kanban, les fiches sont triées par
  `annee_realisation` ascendante (NAs en queue), pour qu’une colonne se
  lise du plus tôt au plus tard du haut vers le bas.
- `feat(action_plan)` — **commentaire affiché sur chaque carte Kanban**.
  Une div `.kanban-card-comment` (small text-muted, mt-1, word-break)
  apparaît sous le bloc type/année/UGF si le commentaire est non-vide ;
  rien si vide pour préserver la hauteur minimale.

#### Plan d’actions — fiche d’ajout d’action

- `fix(action_plan)` — la dropdown **UGF** dans la modal “Ajouter une
  action” affichait le `ug_id` brut (ex. `ugf_42`) au lieu du libellé
  humain. Construction d’un `ug_choices` via `setNames(ids, labels)` à
  partir de `sf$label` mappé sur `sf$ug_id`, trié par label. Fallback
  sur les IDs si `ug_sf_4326()` est indisponible.
- `fix(action_plan)` — le champ **Année cible** de la même modal
  affichait l’offset interne (1, 2, 3 …) au lieu d’une année calendaire.
  Le `numericInput` montre désormais l’année réelle (default =
  `base_year + 1`, min = `base_year + 1`, max = `base_year + horizon`) ;
  la conversion en offset (`year - base_year`) se fait au moment du save
  dans l’observer `add_run`.

#### Plan d’actions — UX du tableau

- `ui(action_plan)` — **total des actions affiché en bas à droite** du
  tableau DT (auparavant à gauche). Le `dom` DT passe à un layout custom
  `<"top"f>rt<"d-flex … dt-bottom-row"<"d-flex gap-3 align-items-center"lp>i>`
  ; règles CSS scoped sur `.dt-bottom-row` neutralisent les `float`/
  `clear` par défaut de `dataTables_info`/`_length`/`_paginate` et
  alignent l’info à droite via `text-align: right`.

## nemetonshiny 0.22.4 (2026-05-09)

#### Plan d’actions — UX polish

- `ui(action_plan)` — le sélecteur **“Afficher 5/10/25/50/All”** passe
  **sous le tableau** (à côté de l’info “*TOTAL* action(s)” et de la
  pagination Préc./Suiv.). Le `dom` DT passe de `"lfrtip"` à `"frtilp"`
  : la barre de recherche reste seule au-dessus du tableau, ce qui aère
  l’en-tête de la card.
- `ui(action_plan)` — **figeage strict** des colonnes UGF + Année lors
  du scroll horizontal. `DISPLAY_COLS` réordonné pour placer les
  colonnes cachées (`id`, `ug_id`, `annee_cible`) en queue ;
  `fixedColumns: leftColumns` passe de 5 à 2 — l’extension
  `FixedColumns` de DT compte toutes les colonnes du DOM (y compris
  `visible:FALSE`), donc seul le décompte sur les colonnes visibles
  évite les artefacts d’en-têtes clones. `colname_map` et
  `hidden_targets` ajustés en conséquence (cibles 13-15).
- `i18n(action_plan)` — titres de légende de la **carte des actions**
  traduits. Les littéraux `"annee"` / `"type"` / `"priority"` passés à
  `leaflet::addLegend(title = …)` sont remplacés par
  `i18n$t("action_plan_col_annee" | "_type" | "_priorite")` ;
  ré-utilisation des clés des en-têtes de colonnes pour rester cohérent
  avec le tableau. Affiche désormais “Année” / “Type” / “Priorité” en FR
  (et “Year” / “Type” / “Priority” en EN), avec switch dynamique au
  changement de langue.

#### Auth — fix démarrage anonyme avec dev roles

- `fix(auth)` — `mod_auth_server()` plantait au démarrage en mode
  anonyme dès que `NEMETON_AUTH_DEV_ROLES` était défini : l’appel
  `cli::cli_alert_info("…dev roles: {.val {auth_state$user_roles}}.")`
  faisait évaluer `{auth_state$user_roles}` par `glue` hors d’un
  `reactive()/observe()`, ce que `reactiveValues` interdit (“Can’t
  access reactive value ‘user_roles’ outside of reactive consumer”). La
  valeur parsée est désormais capturée dans une locale `parsed_roles`
  avant l’assignation à `auth_state$user_roles` ; le message `cli`
  interpole la locale, plus le reactiveValues. Régression introduite par
  [\#41](https://github.com/pobsteta/nemetonshiny/issues/41) (v0.22.3).

## nemetonshiny 0.22.3 (2026-05-09)

#### Plan d’actions — UX polish

- `feat(action_plan)` — global DT search box now uses **regex with OR
  semantics**
  ([\#35](https://github.com/pobsteta/nemetonshiny/issues/35)). Typing
  `eclaircie|plantation` in the search box returns rows matching either
  term. Search is case-insensitive (`caseInsensitive = TRUE`) so
  accent-less typing keeps working.
- `feat(action_plan)` — new **Surface totale** badge in the totals strip
  above the action table
  ([\#36](https://github.com/pobsteta/nemetonshiny/issues/36), plus
  reorder in this release). Sums `surface_ha` over the rows currently
  visible in the DT, formatted with two decimals + `ha`, rendered in
  `text-primary`. The badge sits **after Bilan** so the monetary totals
  (Coût / Revenu / Bilan) read first and the surface tally is the
  trailing metric. The `pill()` helper now accepts optional `unit` and
  `digits` arguments (defaulting to `"EUR"` / `0`) for backward-compat
  with the three monetary pills.
- `refactor(action_plan)` — DT table paginated at **5 rows per page**
  (was 50 with a 60 vh internal scroll). New `lengthMenu` lets users
  expand to 10 / 25 / 50 / All on demand; `dom` switched from `"frtip"`
  to `"lfrtip"` so the length selector sits left of the global search.
  Removed `scrollY` + `scrollCollapse` so the table card now contracts
  around the visible rows instead of padding to 60 vh. `scrollX = TRUE`
  and the two pinned columns (UGF + Année) are unchanged.
- `ui(action_plan)` — sidebar title now carries an icon and reads
  “Tableau des actions”
  ([\#37](https://github.com/pobsteta/nemetonshiny/issues/37)) for a
  tighter visual link to the table card it controls.
- `ui(action_plan)` — bulk-status block (the *Statut Kanban* section
  that duplicated the per-card Kanban moves) dropped in favour of a
  **collapsible action card**
  ([\#38](https://github.com/pobsteta/nemetonshiny/issues/38)) so the
  sidebar stays scannable.
- `ui(action_plan)` — right action panel resized to **350 px** with
  **dual collapse** behavior
  ([\#39](https://github.com/pobsteta/nemetonshiny/issues/39)): the
  bslib sidebar itself can collapse, and the action card inside it has
  its own collapse toggle.

#### Plan d’actions — role-based permissions (Lot 6 S15)

- `feat(action_plan)` — **role-based write permissions**
  ([\#40](https://github.com/pobsteta/nemetonshiny/issues/40)). New
  helper `can_edit_action_plan(auth_state)` centralises the role
  convention used across the tab: roles `proprietaire`, `editeur`,
  `admin`, `manager`, `owner`, `editor` are allowed to mutate the plan;
  anonymous sessions default to *editor* to preserve the current dev
  experience; the `lecteur` role (and any other unrecognised role) is
  read-only. `app_state$auth` now exposes the auth reactive so the
  action plan module can subscribe to it. Read-only sessions see a
  banner above the action table and 8 server-side mutation observers
  (add / edit / delete / bulk status / IA generation / Kanban drop /
  GeoPackage write / PDF write) bounce back with a toast. The Kanban
  drop handler also rolls back the optimistic UI move when the user
  lacks edit rights. 5 unit tests on `can_edit_action_plan()`. Closes
  [\#22](https://github.com/pobsteta/nemetonshiny/issues/22), refs
  [\#7](https://github.com/pobsteta/nemetonshiny/issues/7).

#### Auth

- `feat(auth)` — **`NEMETON_AUTH_DEV_ROLES` env override**
  ([\#41](https://github.com/pobsteta/nemetonshiny/issues/41)). In
  anonymous mode (no OAuth client configured), `auth_state$user_roles`
  is now seeded from the comma-separated env var
  `NEMETON_AUTH_DEV_ROLES` (e.g. `NEMETON_AUTH_DEV_ROLES=lecteur` to
  test the read-only banner). Lets developers exercise the role-based
  mutation guards landed in PR
  [\#40](https://github.com/pobsteta/nemetonshiny/issues/40) without
  standing up a Keycloak realm. Refs
  [\#7](https://github.com/pobsteta/nemetonshiny/issues/7).

#### i18n

- New key `action_plan_total_surface` (FR: *Surface totale* / EN: *Total
  area*).
- New keys for the Lot 6 read-only banner + permission-denied toast
  (FR/EN), bundled in PR
  [\#40](https://github.com/pobsteta/nemetonshiny/issues/40).

## nemetonshiny 0.22.2 (2026-05-06)

#### Plan d’actions — table & Kanban polish

- `refactor(action_plan)` — DT table trimmed and stabilised. Removed
  columns *Type libre*, *Objectifs*, *RDI*, and *Source* — they were
  rarely used, made horizontal scroll worse, and the underlying fields
  are still editable through the row-level form. Per-column filter row
  dropped (`filter = "top"` → `filter = "none"`); the global search box
  is the single filter exposed. Rows now have a uniform height: the
  datatable carries `class = "compact stripe hover nowrap"` and a scoped
  `.dt-truncate` rule
  (`max-width: 220px; overflow: hidden; text-overflow: ellipsis`) keeps
  long commentaire / labels on a single line. The two pinned left
  columns (UGF + Année) are unchanged.
- `feat(action_plan)` — Kanban board layout reorganised. The four active
  workflow stages (*Proposée*, *Validée*, *Planifiée*, *Réalisée*) sit
  side by side as a 4-column grid; *Abandonnée* is rendered full-width
  below as a separate, less prominent archive lane. Empty columns now
  reserve a 60 px drop zone so cards can be dragged into them.
- `feat(action_plan)` — **Drag-and-drop on the Kanban board**. Cards can
  be moved between columns by dragging. SortableJS 1.15.6 is vendored
  under `inst/app/www/js/Sortable-1.15.6.min.js` (MIT, ~45 KB), wired up
  by a small init script (`action_plan_kanban.js`) that re-binds on
  every renderUI tick to avoid stale instances. On drop, the JS pushes
  `input$kanban_drop = list(action_id, target_status, source_status, nonce)`
  to the server, where a new observer validates the transition through
  the existing `is_valid_status_transition()` rules:
  - **Allowed transition** → `update_action_in_plan()` +
    `save_action_plan()`, then `plan_rv()` is bumped, which triggers a
    renderUI re-run that confirms the move.
  - **Refused transition** (e.g. trying to drag a *Réalisée* card back
    to *Proposée*) → a warning toast surfaces with the offending pair,
    and `kanban_render_token` is bumped to re-render the board, which
    puts the card back where the data says it belongs. The previous
    per-card *Déplacer* dropdown is preserved — both paths share the
    same validator + persistence code.

#### i18n

- New key `action_plan_kanban_drop_invalid_fmt` (FR/EN) for the
  refused-transition toast.

#### Tests

- `tests/testthat/test-mod_action_plan.R` — three new test_thats:
  vendored asset existence (`Sortable-1.15.6.min.js` +
  `action_plan_kanban.js`), validator coverage for legal vs refused
  drag-drop transitions, and presence of the new i18n key in both
  locales.

## nemetonshiny 0.22.1 (2026-05-06)

#### Bug fixes

- `fix(action_plan)` — empty action plans no longer crash the reactive
  chain. `actions_df_all()` was assigning a length-1 `NA_character_` to
  `df$ug_label` on a 0-row data.frame, which R rejects with
  *“replacement has 1 row, data has 0”*. The 0-row branch now returns
  early with explicit empty columns. Surfaced on a fresh project with no
  actions yet (regression introduced in v0.22.0 with the new Plan
  d’actions tab).

#### Improvements

- `feat(db)` — app schema (`nemeton.projects` and friends) is now
  initialized automatically on first connection through
  `get_db_connection()`. The existing idempotent
  [`db_init_schema()`](https://pobsteta.github.io/nemetonshiny/reference/db_init_schema.md)
  used to be exported but never invoked, so a freshly provisioned
  database surfaced *“relation "nemeton.projects" does not exist”* on
  the first project save. Memoized once per R session via
  `.nemeton_env`.
- `feat(monitoring)` — monitoring-DB migrations (`monitoring_zone`,
  `alert`, `obs_pixel`, …) are now applied automatically on first
  connection through `get_monitoring_db_connection()` by calling
  [`nemeton::db_migrate()`](https://pobsteta.github.io/nemeton/reference/db_migrate.html).
  The Monitoring tab no longer warns *“relation "monitoring_zone" does
  not exist”* on a fresh TimescaleDB. Memoized once per R session via
  `.nemeton_env`.

## nemetonshiny 0.22.0 (2026-05-06)

#### New feature — “Plan d’actions” tab

A full new tab dedicated to building, visualising and exporting
multi-year forest action plans, delivered across PRs
[\#23](https://github.com/pobsteta/nemetonshiny/issues/23)..#34.

- **Lot 1+2 (S1..S6,
  [\#23](https://github.com/pobsteta/nemetonshiny/issues/23))** —
  scaffold of the tab, interactive Leaflet map + DT table with two-way
  sync, per-UGF action rows.
- **Lot 3 (S7+S8,
  [\#25](https://github.com/pobsteta/nemetonshiny/issues/25))** —
  LLM-powered plan generation through a new `planificateur` expert
  profile, Kanban board view, and an audit modal exposing the prompt,
  model, latency and token counts for each generation.
- **Lot 4 (S9..S11,
  [\#30](https://github.com/pobsteta/nemetonshiny/issues/30))** —
  cumulative balance sparkline per UGF, Gantt timeline of scheduled
  actions, and bridges from each row to the Terrain tab (jump to the
  matching plot).
- **Lot 5 (S12+S13,
  [\#32](https://github.com/pobsteta/nemetonshiny/issues/32))** —
  GeoPackage export of the full plan and per-UGF PDF export through a
  new Quarto template `inst/quarto/action_plan_template.qmd`.
- **Bilan column + steered LLM
  ([\#29](https://github.com/pobsteta/nemetonshiny/issues/29))** —
  derive a per-UGF balance column (`revenu_eur - cout_eur`, cumulative)
  and steer the LLM prompt toward solutions that keep the cumulative
  balance positive over the planning horizon.
- **`revenu_eur` field
  ([\#28](https://github.com/pobsteta/nemetonshiny/issues/28))** — new
  revenue column alongside `cout_eur`, propagated through all views,
  exports and prompts.
- **UI polish** — 50/50 layout with native DT filters and frozen UGF
  label column
  ([\#26](https://github.com/pobsteta/nemetonshiny/issues/26)), map
  auto-refit on bbox change with calendar year display
  ([\#27](https://github.com/pobsteta/nemetonshiny/issues/27)),
  categorical year legend with UGF label in popup
  ([\#31](https://github.com/pobsteta/nemetonshiny/issues/31)), Kanban
  board with sticky DT header
  ([\#33](https://github.com/pobsteta/nemetonshiny/issues/33)),
  right-hand bslib action sidebar grouping all toolbar buttons by intent
  ([\#34](https://github.com/pobsteta/nemetonshiny/issues/34), refs
  [\#7](https://github.com/pobsteta/nemetonshiny/issues/7)).
- **Robustness
  ([\#24](https://github.com/pobsteta/nemetonshiny/issues/24))** — guard
  map color palettes when the plan is empty so the tab never crashes on
  a project with no actions.

#### Other improvements

- `feat(project)` — the *Informations projet* card now shows the storage
  directory of the loaded project, so users can locate the GeoPackage
  and metadata files without leaving the app.
- `feat(home)` — the PostGIS sync toast now reports the actual target as
  `dbname@host:port`, so users immediately know which database their
  commune cache is going to.
- `feat(home)` — load the active project into the Monitoring zone picker
  so users do not have to redraw the AOI when switching tabs.
- `feat(monitoring)` — the “Enregistrer comme zone” button now exposes a
  tooltip explaining why it is disabled (no AOI drawn, no project
  loaded, etc.).
- `i18n(monitoring)` — clarify FAST naming in the quick-mode labels.
- `refactor(monitoring)` — rename the health-export “QField” labels to
  “QGIS” in the UI to match what is actually produced (.qgz project
  file).
- `fix(db)` — `service_db.R` now honors `NEMETON_DB_URL` priority over
  the legacy `POSTGRESQL_ADDON_*` Clever Cloud variables, so local
  overrides are respected.
- `fix(sampling)` — surface silent `save_samples()` failures with a
  user-visible toast instead of swallowing the error.

#### Internal

- New module `R/mod_action_plan.R` (~1816 LOC) and service layer
  `R/service_action_plan.R` (~686 LOC).
- New expert profile `inst/experts/planificateur.yml`.
- New Quarto template `inst/quarto/action_plan_template.qmd`.
- New tests `test-action_plan_prompts.R`, `test-mod_action_plan.R`,
  `test-service_action_plan.R` (~500 LOC of testthat).
- `R/utils_i18n.R` — +263 LOC of new keys (NMT convention) for the Plan
  d’actions tab in FR/EN.

## nemetonshiny 0.21.0 (2026-04-30)

#### New feature — Forest health monitoring (E6.c.5, spec 008)

The Monitoring tab is now a two-mode forest health workstation.

- **Mode 1 — Surveillance rapide** (existing E6.b NDVI/NBR rolling
  window, kept as-is) detects recent shocks (cuts, windthrows, fires) in
  seconds.
- **Mode 2 — Diagnostic sanitaire (FORDEAD)** wraps
  [`nemeton::run_fordead_dieback()`](https://pobsteta.github.io/nemeton/reference/run_fordead_dieback.html)
  (CRSWIR + harmonic model via reticulate, GPL-3 isolated to the Python
  frontier) in a
  [`shiny::ExtendedTask`](https://rdrr.io/pkg/shiny/man/ExtendedTask.html).
  Detects progressive dieback (bark beetle, drought) on conifers in
  minutes-to-hours. Both pipelines write to the same `alert` table.
- **G1 — class filter**. By default the leaflet shows only
  `confidence_class` 3-forte and 4-sol-nu (\>70% true positives per
  ONF/DSF 2024). A “Inclure faible/moyenne” toggle adds the 1-2 classes
  and surfaces a `border-warning` banner citing the up-to-50%
  false-positive rate.
- **G2 — disturbance classification**. Alerts go through
  [`nemeton::classify_disturbance()`](https://pobsteta.github.io/nemeton/reference/classify_disturbance.html)
  server-side, so the popup carries a `disturbance_type` to separate
  progressive dieback from mechanical intervention.
- **G3 — validity banners + confirmation modal**.
  [`nemeton::check_fordead_validity()`](https://pobsteta.github.io/nemeton/reference/check_fordead_validity.html)
  is called on the current zone whenever the user enters health mode.
  Two `border-warning` banners fire when the AOI overlaps the 5
  validated departments (88, 39, 01, 73, 74) under 50%, or when épicéa +
  sapin pectiné drops under 70%. Launching FORDEAD on an out-of-domain
  area pops a modal citing the ONF/DSF caveat; “Run anyway” forwards to
  the task.
- **G4 — QField field-validation workflow**. A new card in health mode
  lets the user pick *n* plots (default 30) and a sampling method (GRTS
  / random) and download a `.qgz`. Re-uploading the filled GPKG via the
  new “Validation sanitaire” sub-tab in *Données terrain* runs
  [`nemeton::ingest_health_validation()`](https://pobsteta.github.io/nemeton/reference/ingest_health_validation.html),
  reports counts (confirmed / false-positive / unmatched), and updates
  `validation_status` + `validation_cause` per alert.
- **G5 — R5 dieback indicator**. The radar’s R-family picks up R5
  automatically through `nemeton::INDICATOR_FAMILIES$R` (no UI change
  needed; the cœur computes it via
  [`nemeton::indicateur_r5_deperissement()`](https://pobsteta.github.io/nemeton/reference/indicateur_r5_deperissement.html)).
- **Plotly time series**: in health mode shows the alert distribution by
  `confidence_class`. Quick-mode time series (NDVI/NBR per plot) ships
  with E6.b phase 3.
- **Persistence**: each FORDEAD launch writes `monitoring_mode`,
  `monitoring_threshold_anomaly`, `monitoring_vegetation_index`,
  `monitoring_dates_training`, plus the validity intersection
  percentages, to `metadata.json`. The module restores these inputs
  whenever a project is reopened.
- **i18n**: ~30 new keys (`monitoring_mode_*`, `monitoring_warning_*`,
  `monitoring_class_*`, `monitoring_qfield_*`, `health_validation_*`,
  `r5_*`). The tab itself is renamed *Suivi sanitaire* / *Forest health
  monitoring*.
- **Dependencies**: `plotly` promoted to Imports;
  `nemeton (>= 0.20.1.9004)` (FORDEAD pipeline + helpers).

## nemetonshiny 0.20.0 (2026-04-24)

#### New feature — LiDAR HD integration (E5.d)

- **LiDAR HD MNH as preferred CHM source**. The download path now tries
  `download_ign_lidar_hd(product = "mnh")` via `happign` first — a
  direct airborne measurement (~0.5 m vertical accuracy, NDP 2
  precision). Open-Canopy ML remains the fallback when LiDAR HD tiles
  are missing for the AOI.
- **LiDAR HD MNT promoted to the `dem` slot** (1 m vs 25 m BD ALTI) so
  W3 (TWI), R1 (feu), R2 (tempête), R3 (sécheresse) and the erosion risk
  all run at LiDAR HD resolution.
- **NDP 1 “Observation” auto-lifts** whenever any LiDAR HD product (MNH
  or MNT) is cached for the AOI, via
  `attr(compute_unit, "has_lidar_hd")` consumed by
  [`nemeton::detect_ndp()`](https://pobsteta.github.io/nemeton/reference/detect_ndp.html).
- **Stratified GRTS kicks in on the sampling plan**. Two new reactives
  (`chm_raster`, `mnt_raster`) load the cached CHM / MNT with the same
  LiDAR-first / fallback order and pass them to
  [`nemeton::create_sampling_plan()`](https://pobsteta.github.io/nemeton/reference/create_sampling_plan.html).
  The core upgrades from LPM2 to stratified GRTS whenever CHM + MNT + BD
  Forêt are all available. The draw method is surfaced in the generation
  toast.
- **New “Hauteur LiDAR HD” badge** on the Synthesis tab
  (`augmented_height_lidar_*` i18n keys) — green, distinct from the cyan
  “Hauteur ML” used for Open-Canopy.
- `chm_phase:lidar_hd_download` progress key translated so the compute
  status line reads “Téléchargement CHM LiDAR HD (IGN)…” instead of the
  raw key.

#### New feature — Sampling polish

- **`forest_mask` passed to the sampling plan**: reuse the project’s
  cached BD Forêt v2 polygons (filtered to true forest) so points
  falling in water, fields or roads are filtered by the
  `min_forest_cover = 0.7` constraint. Fixes the Couchey lake scenario.
- **Map zoom fixed to the UGF extent**, not BD Forêt’s (which is fetched
  with a buffer and was dominating the auto-fit).
- **Immediate toast on Générer les placettes** with a spinning gear,
  matching the Projet chargé / Retry pattern. Dispatched on the root
  session.
- **Tooltip on the Source du CV radio** explicitly states that the
  choice controls the CV value (Cochran), not the draw method (GRTS /
  LPM2 / random).
- **Sampling method note rewritten** to describe the full pipeline:
  candidates on a regular 50 m grid, filtered by the forest mask, then
  GRTS → LPM2 → random depending on what is provided.

#### Fixed

- Duplicate PostGIS-sync toast at compute completion — removed the
  second occurrence in `mod_progress`; only the `mod_home` one fires
  now.
- Immediate toast when clicking *Réessayer* on the compute-error card,
  dispatched on the root session.

#### Dependencies

- Bumped `nemeton` minimum to `>= 0.19.5` (for `height_lidar` augmented
  flag and TSP tour integration).

## nemetonshiny 0.19.0 (2026-04-24)

#### New feature — Sampling UX polish

- **Tooltips** on six sidebar inputs of the Export terrain sub-tab
  (target error, alpha risk, over-sample ratio, CV position, seed,
  region). Each tooltip explains the statistical or biological meaning
  of the parameter.
- **TSP legend on the map**: when a sampling plan with ≥ 2 Base plots is
  drawn, a legend panel appears at the bottom-left with three inline-SVG
  glyphs — dashed magenta line (*Ordre de visite*), open triangle
  (*Départ*), double concentric circle (*Arrivée*) — matching the
  markers and route on the map.
- **Retry toast**: clicking *Réessayer* now fires an immediate
  notification with a spinning arrow-clockwise icon (“Projet
  réinitialisé — prêt à relancer le calcul.”), dispatched on the root
  session so it lands in the top-level toast stack.

#### Fixed

- **Duplicate PostGIS-sync toast** on compute completion: the same
  notification used to fire both from `mod_home` and `mod_progress` with
  slightly different wording (“la base PostGIS” vs “la base de données
  PostGIS”). Kept the `mod_home` one (orchestrator), dropped the
  `mod_progress` one.
- **Package documentation icon in RStudio’s Packages pane**: added `URL`
  and `BugReports` fields to `DESCRIPTION` so the icon is now rendered
  alongside the globe and uninstall icons.

#### Docs

- README: synced counters to the real state (31 indicators, 13 expert
  profiles, 504 i18n translation keys). Previously 29 / 16 / 293.
- i18n: `sampling_tt_region` tooltip says *QGIS*, not *QField* (the
  species dropdown is defined in the QGIS project descriptor).

## nemetonshiny 0.18.0 (2026-04-24)

#### New feature — Sample size from target error + BD Forêt v2 CV (E5.c)

- **`R/mod_sampling.R`** — the sidebar accordion in the Export terrain
  sub-tab gains a *Mode de dimensionnement* radio (*Taille fixe* /
  *Erreur cible*). In *Erreur cible* mode the user picks a relative
  error (default 10 %), an alpha risk (default 5 %), an over-sample
  ratio (default 20 %) and either a manual CV or an automatic CV derived
  from BD Forêt v2 via
  [`nemeton::cv_from_bdforet()`](https://pobsteta.github.io/nemeton/reference/cv_from_bdforet.html).
  The computed sample size is shown live under the inputs, along with
  diagnostics on the BD Forêt v2 coverage and any ambiguous / unmapped
  TFV codes.
- BD Forêt v2 is read from the project cache populated during the first
  compute run (`<project>/cache/layers/bdforet.gpkg`). When the cache is
  absent, the UI points the user at the manual mode via an explicit
  warning.
- The TFV column is auto-detected (`TFV`, `tfv`, `code_tfv`) to cope
  with different WFS layouts.
- The existing *Taille fixe* path is preserved via a `conditionalPanel`;
  `create_sampling_plan()` is called with `n_base` / `n_over` computed
  upstream depending on the mode.
- Bumped nemeton dependency to `>= 0.19.0.9000` (the dev version
  introducing `compute_sample_size()`, `cv_from_bdforet()` and the
  editable CV typology CSVs).
- 19 new FR/EN i18n keys (`sampling_sizing_mode`, `sampling_mode_*`,
  `sampling_target_error_label`, `sampling_alpha_label`,
  `sampling_over_ratio_label`, `sampling_cv_source_*`,
  `sampling_cv_position*`, `sampling_cv_compute`,
  `sampling_cv_bdforet_hint`, `sampling_cv_bdforet_missing`,
  `sampling_cv_computed`, `sampling_cv_ambiguous`,
  `sampling_cv_unmapped`, `sampling_n_computed*`).
- Tests: 6 new testServer assertions covering the Cochran sizing path
  (manual CV) and the bail-out when CV is zero. Full suite 5145 / 0
  failure.

#### New feature — Field ingest (E5.b — QField return path)

- **`R/mod_field_ingest.R`** — new “Ingestion terrain” tab that closes
  the terrain → plateforme loop. A field agent drops the GeoPackage
  returned by QField; the module runs
  [`nemeton::import_qfield_gpkg()`](https://pobsteta.github.io/nemeton/reference/import_qgis_gpkg.html) +
  `validate_field_data()`, renders a validation report (counts, errors,
  warnings), and previews the placettes / arbres on the project map.
- **NDP bump on attach**: clicking *Rattacher au projet* calls
  `aggregate_plot_metrics()` + `attach_field_data_to_units()` on the
  project’s UGF sf, tags it via `tag_field_data_sources()`, runs
  `detect_ndp()` along the alternative field path (NDP 2 with plots
  only, NDP 3 from 10 trees/plot on average), persists the GPKG to
  `<project>/data/field_data.gpkg` and updates project metadata so the
  bumped NDP is picked up by every downstream module (synthesis badge,
  family tabs). Before/after NDP badges make the change visible to the
  user.
- **MVP scope**: this iteration persists the field data and bumps the
  NDP, but does not rerun `compute_all_indicators()`. The indicators
  consuming field aggregates (P1, P2, B2, C1, R2) are picked up on the
  next compute triggered from the Home tab.
- i18n: 22 new FR/EN keys (`tab_field_ingest`, `field_ingest_*`,
  `field_ingest_ndp_before` / `_after`, report headers).
- Tests: `tests/testthat/test-mod_field_ingest.R` — 24 assertions
  covering UI controls, reactive NULL state, the validate flow on a
  real-ish GPKG (placettes + arbres) and the attach flow with mocked
  persistence (GPKG written to the project dir + metadata update
  recorded).

#### Sampling module now uses the library-level GRTS pipeline

- **`R/mod_sampling.R`** — replace the temporary
  `sf::st_sample(..., type = "random")` draw with
  [`nemeton::create_sampling_plan()`](https://pobsteta.github.io/nemeton/reference/create_sampling_plan.html),
  which delivers GRTS stratification when CHM/DEM/BD Forêt layers are
  provided and falls back to spatially-balanced LPM2 or plain random
  otherwise. The notification now appends the draw method (`GRTS`,
  `LPM2`, `RANDOM`) so users can see which path was taken.
- i18n: `sampling_method_note` rewritten to describe the new behaviour.

#### New feature — Field sampling / QField export (E5.a)

- **`R/mod_sampling.R`** — new “Terrain” tab: given the current
  project’s study area (union of `indicators_sf` polygons), the user
  sets `n_base` / `n_over` / seed / biogeographic region, clicks
  *Générer*, and previews the sample plots on a leaflet map. A
  *Télécharger le projet QField (.qgz)* button produces a QField-ready
  project via
  [`nemeton::create_qfield_project()`](https://pobsteta.github.io/nemeton/reference/create_qgis_project.html)
  (placettes + empty arbres layer + pre-configured forms).
- First iteration uses a spatial random draw
  ([`sf::st_sample`](https://r-spatial.github.io/sf/reference/st_sample.html)).
  The full stratified GRTS + TSP pipeline from the 09-sampling tutorial
  will be lifted to
  [`nemeton::create_sampling_plan()`](https://pobsteta.github.io/nemeton/reference/create_sampling_plan.html)
  in a follow-up.
- `DESCRIPTION` now requires `nemeton (>= 0.18.0.9000)` for
  `create_qfield_project()`.
- i18n: 14 new FR/EN keys (`tab_sampling`, `sampling_*`, `qfield_*`).
- Tests: `tests/testthat/test-mod_sampling.R` — 23 assertions covering
  UI controls, reactive draw, empty-state handling and a round-trip .qgz
  built from the module’s generated plots.

#### Changes — F1 soil fertility

- **F1 now uses the absolute SoilGrids CEC scoring path** from the core
  package. `compute_single_indicator()` passes `source = "soilgrids"` to
  `indicateur_f1_fertilite()`, which streams the 250 m CEC topsoil
  raster on demand via
  [`nemeton::load_raster_source()`](https://pobsteta.github.io/nemeton/reference/load_raster_source.html)
  and applies
  [`nemeton::cec_to_fertility_score()`](https://pobsteta.github.io/nemeton/reference/cec_to_fertility_score.html)
  (absolute 0-100). Scores are now comparable across projects instead of
  being min-maxed per AOI.
- **Removed the duplicated `download_soilgrids_cec()`** and its entry in
  `DATA_SOURCES$rasters$soil`. The core package owns the download path
  (ADR-009), so the app no longer stages a SoilGrids layer in
  `layers$rasters$soil`. One less pre-compute step surfaces in the
  progress UI.
- **Bumped the `nemeton` dependency** to `>= 0.17.0.9000` (the dev
  version introducing `load_raster_source()`, `source = "soilgrids"`,
  and the UTS → fertility crosswalk).

## nemetonshiny 0.16.0

First release targeting the v0.17.0 nemeton core. End-to-end integration
of the Open-Canopy CHM pipeline, live per-step progress feedback, and a
consolidated i18n layer.

#### New Features — CHM / Open-Canopy

- **Auto-detected Open-Canopy CHM** — the UI no longer forces the user
  to pick “CHM: none / Open-Canopy” before every run. The pipeline fires
  automatically when the `opencanopy` package is installed, unless the
  user opts out via `options(nemetonshiny.chm = "none")` or
  `NEMETONSHINY_DISABLE_CHM=1`. Each synthesis view gets two provenance
  badges:
  - ⚡ **Hauteur ML** — CHM was consumed by height-aware indicators (C1,
    B2, R2, P2).
  - 📋 **Inventaire estimé ML** — P1 / P3 / E1 ran, meaning `dbh` /
    `density` were synthesised from the CHM via
    [`nemeton::ensure_inventory_fields()`](https://pobsteta.github.io/nemeton/reference/ensure_inventory_fields.html)
    (Charru 2012 self-thinning).
- **BD Forêt enrichment for P2** — UGFs are enriched with `species` /
  `age` from BD Forêt V2 once up-front via
  [`nemeton::enrich_parcels_bdforet()`](https://pobsteta.github.io/nemeton/reference/enrich_parcels_bdforet.html)
  when `indicateur_p2_station` is scheduled, so the CHM mode can run
  instead of falling back on the legacy `fertility` / `climate` path
  that never had its inputs.

#### New Features — progress UX

- **Live step-by-step status** replaces the “frozen on Inférence CHM”
  ~8-minute silence on large AOIs. The task toast now paints:
  - “Étape 1/5 : chargement de l’AOI…”
  - “Étape 2/5 : téléchargement ortho IGN…” + “Téléchargement ortho IGN
    RVB : tuile 5/28…” per WMS tile
  - “Étape 3/5 : configuration Python + téléchargement modèle…”
  - “Étape 4/5 : inférence du modèle pvtv2…” + “Inférence CHM : tuile
    2/3…” per inference tile
  - “Étape 5/5 : export des résultats…”
- **Initialisation spinner** — the toast paints ⚙ + “Initialisation des
  calculs…” the moment the user clicks “Lancer les calculs” or
  “Réessayer”, so the 1-3 s gap before the async worker writes its first
  progress event is no longer silent.
- **Task translator unified** — `mod_progress.R` no longer ships its own
  partial `translate_task()`; it delegates to the canonical
  `translate_task_message()` in `utils_i18n.R`, so every new task prefix
  is routed to its label in one place.

#### New Features — i18n

- **Single source of truth** — the `TRANSLATIONS` list in
  `R/utils_i18n.R` is now the only runtime dictionary. The stale
  `inst/app/i18n/{fr,en}.json` files (339 keys, 19 behind the R list)
  have been removed and the unused `shiny.i18n` suggested dependency
  dropped. `export_translations_json()` remains available for one-way R
  → JSON exports to external translators.

#### Bug Fixes

- **`download_chm_opencanopy()`** — unwraps the bare `SpatRaster` / `sf`
  returned by `download_{raster,vector}_source()` instead of chasing a
  `$object` attribute that didn’t exist. The previous code called `[[`
  on a `SpatRaster` looking for a layer named “object” and triggered a
  terra `[subset] invalid name(s)` error that aborted the whole CHM
  pipeline and forced P2 back into legacy mode.
- **Open-Canopy pipeline resume** — the retry and recompute paths now
  reset the project to “draft” and wait for the user to re- launch the
  run, instead of silently firing a new `compute_task` invocation. One
  entry point is the confirmation modal.
- **Resume from a legacy progress file** — `translate_task_message()`
  now maps the pre-`e74bdcc` literal `"download:source_chm_opencanopy"`
  to the new `"chm_inference_opencanopy"` label so re-opening an older
  project no longer spams “Translation key not found:
  source_chm_opencanopy”.
- **Tests** — `NEMETONSHINY_DISABLE_CHM=1` is now scoped to the test run
  via `withr::local_envvar(.local_envir = testthat::teardown_env())` in
  a dedicated `setup-chm.R`, so `devtools::test()` in an interactive
  session no longer leaves the CHM pipeline silently disabled.

#### Breaking changes

- None. The CHM toggle that was previously visible on the compute button
  disappeared, but the underlying metadata is still written (now
  reflecting the *outcome* of the auto-detected run, not the user’s a
  priori choice), so the synthesis badge keeps working on old projects.

## nemetonshiny 0.15.1

See git history.

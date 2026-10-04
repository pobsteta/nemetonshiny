# BRIEF — Piloter nemetonshiny depuis VICTOR (voix) et AIGORA (Claude Code)

> **Statut** : ouvert, 2026-10-04. A.1 livré (0.153.0.9001) ; API publique sous-jacente livrée (0.153.0.9002).
> **Dépôts concernés** : `nemetonshiny` (lot A, principal), `aigora` (lot B),
> `victor` (lot C). **`nemeton` (cœur) : aucun changement de code** (§2).
> **Émetteur** : session `nemetonshiny`, sur la base de 0.153.0 (main) / cycle 0.153.0.9xxx.
> **Nature** : nouvelle porte d'entrée **sans interface** vers les services
> applicatifs existants. Aucune logique métier nouvelle, aucun changement de
> l'API publique R (`run_app()` reste le seul export).

---

## 1. Le besoin

Pouvoir dire, à la voix ou dans un terminal :

- « Victor, où en est le projet Dabo ? » → score global, 12 familles, niveau de précision (NDP)
  affichés sur l'écran VICTOR (`display_report`) ;
- « Lance le calcul des indicateurs de Couchey » → calcul lancé en tâche de fond,
  suivi possible ensuite (« où en est le calcul ? ») ;
- « Fais-moi le rapport PDF de Dabo » / « exporte-le en GeoPackage » ;
- « Ouvre Nemeton sur la synthèse de Dabo » → l'app s'ouvre dans le navigateur
  sur le bon projet et le bon onglet.

Aujourd'hui c'est impossible : `nemetonshiny` n'exporte que `run_app()`, les
services sont internes (`@noRd`), l'app ne lit aucun paramètre d'URL, et une
tâche VICTOR est coupée à 600 s alors qu'un calcul complet peut durer plus d'une
heure (R1 feu sur MNT LiDAR).

### Chaîne cible

```
VICTOR (voix, Haiku)
  └─ delegate_to_claude(espace = "nemeton")
       └─ claude -p  dans ~/dev/aigora/aigora-nemeton   (lot B)
            └─ skill `nemeton`  +  serveur MCP `nemeton`  (lot A)
                 └─ services nemetonshiny (service_project / service_compute / service_export)
                      └─ nemeton (cœur, inchangé)
  └─ display_report(...)  ← données structurées renvoyées par la tâche
  └─ open_url("http://127.0.0.1:3838/?project=…&tab=synthesis")
```

---

## 2. Impact sur le cœur `nemeton` : aucun

Toutes les fonctions cœur nécessaires sont déjà dans un tag de release (donc
tirées par `Remotes: @*release`) et déjà consommées par l'app :

| Besoin | Fonction cœur | Déjà utilisée dans |
|---|---|---|
| Calcul plafonné en mémoire, process enfant + log | `nemeton::run_memory_capped()` (≥ 0.195.0 pour `log_path`) | `.compute_run_capped()` (`R/service_compute.R`) |
| Score global pondéré Fibonacci | `nemeton::compute_general_index()` | `mod_synthesis.R`, `service_export.R` |
| Niveau NDP | `nemeton::detect_ndp()` | `service_export.R` |
| Scores par famille | `create_family_index()` | `mod_synthesis.R` |
| Verrou multi-utilisateurs | `nemeton::project_lock_*()` (≥ 0.148.0) | `R/service_lock.R` |
| Libellés familles | `nemeton::INDICATOR_FAMILIES` | partout |

**Pas de bump du plancher `Imports: nemeton (>= …)`.** Seule action cœur : à la
release, l'entrée datée de `nemeton/PLAN.md`, à faire depuis une **instance
dédiée au cœur** (règle stricte 12).

---

## 3. Lot A — `nemetonshiny` (cette session)

### A.1 Extraire la synthèse dans un service (refactor préalable)

Le calcul des scores de famille vit aujourd'hui **dans un `reactive()`** de
`mod_synthesis.R` (l. 136-165 : `add_r5_to_indicators` →
`add_regen_r_indicators` → `create_family_index`) et le score global dans un
`renderUI` (l. 245-255). Le serveur MCP ne doit pas dupliquer ce code.

- Nouveau `R/service_synthesis.R` :
  - `project_family_scores(project)` → sf une ligne par UGF (corps actuel du reactive) ;
  - `project_synthesis_summary(project)` → liste simple, sérialisable JSON :
    `list(project_id, name, status, ndp_level, global_score, families = data.frame(code, libelle, score), n_ugf, n_parcels, computed_at)`.
    Libellés lus depuis `nemeton::INDICATOR_FAMILIES` (FR/EN selon `language`).
- `mod_synthesis.R` consomme ces deux fonctions (comportement identique ;
  tests existants de `test-mod_synthesis.R` doivent rester verts sans retouche).

### A.2 Outils MCP — `R/service_mcp.R` (interne) + lanceur `inst/mcp/server.R`

> **Mise à jour 2026-10-04** : l'API hors interface exportée
> (`?api_hors_interface`, 0.153.0.9002, brief aigora-nemeton
> « api-hors-interface-sans-effet-de-bord ») est livrée. Les outils MCP sont
> donc de **fines enveloppes de cette API publique** (`projets_lister`,
> `projet_etat`, `projet_lire()$synthese`, `projet_calculer`, `projet_rapport`,
> `projet_gpkg`), et non plus des appels aux services internes. La lecture
> sans effet de bord et les erreurs classées (`nemetonshiny_projet_perime`…)
> viennent de cette API.

Implémentation des outils en **fonctions R internes et testables** (préfixe
`mcp_`), déclarées comme `ellmer::tool()` (ellmer est déjà en `Imports`).
Le lanceur est minimal :

```r
# inst/mcp/server.R
mcptools::mcp_server(tools = nemetonshiny:::mcp_tools())
```

`mcptools` passe en **`Suggests`** (installé localement : 1.0.3). Un
`requireNamespace("mcptools")` explicite avec message clair si absent.

| Outil | Entrée | Sortie (JSON) | Service sous-jacent |
|---|---|---|---|
| `lister_projets` | `limite` (déf. 20) | id, nom, statut, maj, état de sens | `projets_lister()` |
| `resume_projet` | `projet` (id **ou** nom approché) | `projet_lire()$synthese` ; projet périmé → message clair, rien modifié | API |
| `lancer_calcul` | `projet` | `job_id`, `pid`, chemin du log | §A.3 |
| `etat_calcul` | `projet` | statut (`en_cours`/`termine`/`echec`/`annule`), indicateurs faits / total, dernier indicateur, âge de la progression, erreur | `get_computation_progress()`, `read_progress_state()`, `progress_state_age_sec()` |
| `annuler_calcul` | `projet` | ok | `cancel_computation()` |
| `generer_rapport` | `projet`, `langue`, commentaires | chemin du PDF | `projet_rapport()` |
| `exporter_gpkg` | `projet` | chemin du `.gpkg` | `projet_gpkg()` |
| `url_app` | `projet`, `onglet` | URL `http://127.0.0.1:<port>/?project=…&tab=…` | §A.4 |

Règles communes :

- **Résolution du projet** : id exact, sinon correspondance insensible à la casse
  et aux accents sur le nom ; plusieurs correspondances → erreur listant les
  candidats (VICTOR posera la question). Jamais de choix silencieux.
- **Sorties courtes et structurées** : pas de sf ni de géométrie dans les
  réponses, nombres arrondis, chemins absolus. Les données sont faites pour
  remplir `display_report` (chiffres clés et graphique en barres des 12 familles).
- **Lecture seule par défaut.** Seuls `lancer_calcul`, `annuler_calcul`,
  `generer_rapport`, `exporter_gpkg` écrivent, et uniquement dans le dossier
  du projet.
- **Aucun outil de suppression, de création de projet, ni d'exécution de code
  arbitraire** dans ce lot. (Création de projet = sélection de parcelles, reste
  dans l'app.)
- Journalisation par `cli::cli_*` sur **stderr** uniquement : stdout est le
  canal du protocole MCP (stdio) ; un `cat()` ou `print()` le corrompt.

### A.3 Calcul détaché (le point délicat)

Contraintes :

1. Une tâche VICTOR = un `claude -p` éphémère → le serveur MCP **meurt** à la
   fin de la tâche. Le calcul doit lui survivre.
2. Mémoire : même plafond que l'app (règle cœur depuis 0.183.0) ; ne pas
   réintroduire de fraction locale (cf. commentaire de `.compute_run_capped()`).
3. Pas de travail lourd synchrone dans le process MCP.

Mise en œuvre :

- `mcp_lancer_calcul()` lance un `Rscript` **détaché** (`processx::process$new(…, cleanup = FALSE, supervise = FALSE)`,
  ou `callr::r_bg(…, supervise = FALSE)` ; `callr` est déjà en `Suggests`) qui
  appelle `.compute_run_capped(project_id, app_opts)`. On réutilise **exactement** le chemin de l'app :
  plafond mémoire, log enfant, `use_file_progress = TRUE`.
- Un fichier `data/compute_job.json` (`job_id`, `pid`, `started_at`, `source = "mcp"`, `log_path`)
  permet à `etat_calcul` de distinguer « en cours » de « mort sans finir » :
  pid absent + progression non terminée = échec, avec la fin du log (cf. mémoire
  « Run mort sans erreur = SIGKILL »).
- **Garde anti-doublon** : refuser si un job vivant existe déjà pour ce projet,
  ou si `compute_progress.json` a été écrit il y a moins de N s par l'app.
- **Verrou** : prendre `lock_acquire_or_null(pid, hid, label = "mcp")` pour la
  durée du calcul ; si le projet est verrouillé par un autre détenteur (app
  ouverte en édition), refuser avec le libellé du détenteur. Sans base de données
  (`lock_no_db`) : continuer, comme l'app.
- `app_opts` : reconstruits depuis `get_app_options()` / variables d'env
  (`project_dir`, langue), pas depuis une session Shiny.

### A.4 Liens profonds dans l'app

Dans `app_server.R` : à l'ouverture de session, lire
`session$clientData$url_search` (`shiny::parseQueryString`) :

- `project=<id>` → même chemin que l'ouverture d'un projet récent dans `mod_home` ;
- `tab=<cle>` parmi une liste blanche (`synthesis`, `family`, `map`, `monitoring`, …) → `bslib::nav_select()`.

Valeur inconnue → ignorée sans erreur (`showNotification` i18n discrète). Clés
i18n nouvelles en snake_case français (ex. `lien_projet_introuvable`), FR/EN,
`\uXXXX`.

Lancement de l'app « serveur » pour VICTOR (documenté, pas codé) :
`Rscript -e 'nemetonshiny::run_app(tour = FALSE, options = list(port = 3838, launch.browser = FALSE))'`,
idéalement en service systemd utilisateur `nemetonshiny.service` (modèle :
`victor/installer-demarrage.sh`).

### A.5 Tests (testthat 3)

- `test-service_synthesis.R` : équivalence `project_synthesis_summary()` ↔ ce que
  rendait `mod_synthesis` sur la fixture projet (score global, 12 familles).
- `test-service_mcp.R` :
  - chaque outil, appelé en direct (sans protocole), sur fixture `withr::with_tempdir()` ;
  - résolution de nom : exact, approché, ambigu (erreur avec candidats), inconnu ;
  - `lancer_calcul` avec `local_mocked_bindings` du lanceur de process : écrit
    `compute_job.json`, refuse le doublon, refuse si verrou tenu par un tiers ;
  - `etat_calcul` : en cours / terminé / pid mort sans fin → `echec` ;
  - **aucune écriture sur stdout** (`expect_silent` + capture de stdout vide).
- Test d'intégration MCP réel (`skip_if_not_installed("mcptools")`, `skip_on_cran`) :
  lancer `inst/mcp/server.R` en sous-process, envoyer `tools/list` et vérifier les 8 noms.
- `test-app_server.R` : `?project=…&tab=synthesis` sélectionne l'onglet ; valeur
  inconnue ignorée.

Garde des tests lisant `R/` : cf. mémoire (sauter si pas de sources `.R`, covr).

### A.6 Documentation

- `inst/mcp/README.md` : enregistrement dans Claude Code
  (`claude mcp add nemeton -- Rscript -e 'source(system.file("mcp/server.R", package = "nemetonshiny"))'`),
  liste des outils, sécurité.
- NEWS.md, CHANGELOG.md.

### A.7 Version

Lot cohérent de plusieurs features (service synthèse + serveur MCP + liens
profonds) → **MINOR** à la release stable (`0.154.0`), en passant par le cycle
dev `0.153.0.900x`. L'API R publique n'est pas modifiée.

---

## 4. Lot B — `aigora` (instance dédiée, pas cette session)

1. **Espace dédié** `~/dev/aigora/aigora-nemeton` (copie d'`aigora-dev` allégée) :
   **ne jamais** utiliser le dépôt de dev `~/dev/nemetonshiny` comme espace, car
   VICTOR tourne en `bypassPermissions`.
2. Skill `.claude/skills/nemeton/SKILL.md` :
   - quand l'utiliser (projet forestier, indicateurs, familles, rapport, Nemeton) ;
   - toujours `resume_projet` avant de commenter des chiffres, jamais de mémoire ;
   - calcul : `lancer_calcul` puis **rendre la main** (ne pas attendre), suivi par `etat_calcul` ;
   - terminer chaque réponse par un bloc JSON structuré (kpis, chart bar 12
     familles, table) pour que VICTOR remplisse `display_report` ;
   - données de projet = confidentielles : pas de copie dans `memory/` ni kChat.
3. `R/aigora.R` : `aigora_nemeton_mcp(TRUE/FALSE)` sur le modèle de
   `aigora_qgis_mcp()` (vérifie `nemetonshiny` + `mcptools`, enregistre en scope local).
4. `.claude/settings.json` de l'espace : autoriser `mcp__nemeton__lister_projets`,
   `resume_projet`, `etat_calcul`, `url_app` ; laisser les outils qui écrivent
   soumis à autorisation pour un usage interactif.
5. Optionnel : enchaîner avec les skills `qgis` / `qfieldcloud` existants
   (plan d'échantillonnage → QFieldCloud → ingestion terrain) dans un lot ultérieur,
   qui demandera alors des outils MCP `plan_echantillonnage` / `ingerer_terrain`.

## 5. Lot C — `victor` (instance dédiée, pas cette session)

1. `.env` : déclarer l'espace (aujourd'hui seul `VICTOR_WORKDIR=~/Documents` est posé) :
   ```
   VICTOR_ESPACES=nemeton:~/dev/aigora/aigora-nemeton;dev:~/dev/aigora/aigora-dev;business:~/dev/aigora/aigora-business
   VICTOR_ESPACES_DESC=nemeton:projets forestiers Nemeton, indicateurs, familles, rapports;dev:code R et Python, QGIS;business:mails kSuite, agenda, Odoo
   ```
2. `INSTRUCTIONS` (server.py) : une phrase. Pour un calcul Nemeton, ne pas
   attendre la fin ; proposer de redemander l'état plus tard.
3. Optionnel : outil `open_nemeton(projet, onglet)` qui vérifie que l'app répond
   sur `127.0.0.1:3838` (sinon la démarre via `systemctl --user start nemetonshiny`)
   puis appelle `open_url`. Sinon le couple `url_app` (MCP) + `open_url` suffit.
4. Ne pas augmenter `VICTOR_TASK_TIMEOUT` pour les calculs : le détachement (§A.3)
   rend la tâche courte.

---

## 6. Hors périmètre

- Exécuter du code R arbitraire dans la session de l'app (`btw` groupe `run`) :
  l'app bloque la console et `app_state` n'est pas dans l'environnement global.
- Création de projet ou sélection de parcelles à la voix.
- Exposition réseau : tout reste sur `127.0.0.1`.
- Perspectives IA par profil via MCP (déjà servies par l'app, LLM Mistral) :
  éventuel lot ultérieur.

## 7. Critères d'acceptation

1. `claude mcp list` dans l'espace AIGORA montre `nemeton` connecté, 8 outils.
2. « Résume Dabo » renvoie le même score global et les mêmes 12 familles que
   l'onglet Synthèse de l'app (écart 0).
3. « Lance le calcul de Couchey » rend la main en moins de 10 s ; le calcul
   continue après la fin de la tâche `claude -p` ; `etat_calcul` suit jusqu'à
   `termine` ; un `kill -9` de l'enfant donne `echec` avec la fin du log.
4. Deuxième `lancer_calcul` pendant le premier : refus explicite.
5. Projet ouvert en édition dans l'app : `lancer_calcul` refusé avec le nom du détenteur.
6. `http://127.0.0.1:3838/?project=<id>&tab=synthesis` ouvre la synthèse du projet.
7. `devtools::check()` sans warning ; couverture des nouveaux fichiers ≥ celle du paquet.
8. Rien de modifié dans `nemeton` hormis l'entrée de `PLAN.md`, faite par une instance cœur.

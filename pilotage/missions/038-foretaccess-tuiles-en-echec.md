# Mission 038 — foretaccess 2.6.2 : plancher et alerte « tuiles en échec » dans le rapport

> Créée par l'orchestrateur le 2026-11-21 (« lis et code le brief »). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-21-foretaccess-2.6.2-tuiles-en-echec.md` — **le lire**.
> foretaccess v2.6.2 publiée (tag `v2.6.2`, installée localement).

## Mission

**En une phrase :** quand foretaccess perd une tuile IGN après ses 3 tentatives, l'étape réussit
toujours mais son rapport (pipeline et notification de l'onglet) le dit, avec le message complet.

## Constat (orchestrateur, lu sur `main` le 2026-11-21, `b518ee1c`)

- foretaccess 2.6.2 émet un `warning` (`rlang_warning`, `cli::cli_warn()`) « N tuile(s) WFS sur M
  en echec pour "<couche>" » + ligne « ! … incomplete … » + « ✖ Derniere erreur : … ». Les appels de
  l'app sont dans des `tryCatch(error = …)` : l'avertissement se perd.
- Appels IGN à couvrir (`git grep -n "foretaccess::acquire_" -- R/`) :
  - `R/service_desserte.R:390`, `:393`, `:469`, `:624`, `:1146` (`acquire_mnt`, WMS), `:1158`, `:1170` ;
  - `R/service_accessibility.R:380` (`acquire_mnt`), `:392`, `:765`.
  - Hors périmètre : `acquire_desserte_osm` et `acquire_dfci` (OSM, pas de tuiles IGN).
  - Vérifier avec `foretaccess` 2.6.2 installé (`foretaccess:::` source) quels `acquire_*` passent
    par le mécanisme de tuiles et émettent cet avertissement ; ne couvrir que ceux-là.
- Plusieurs de ces appels tournent dans un worker `future` (ExtendedTask) : l'avertissement doit
  être capturé **dans** le service et rendu dans le résultat, pas compté sur le relais `future`.
- Le rapport sait déjà afficher le `detail` d'une erreur (mission 037 : `pipeline_error_message()`,
  `pipeline_engine_outcome()` dans `R/service_pipeline.R`).

## Travail attendu

1. `DESCRIPTION` : `foretaccess (>= 2.6.1)` → `foretaccess (>= 2.6.2)`.
2. Un helper (ex. `.capturer_alertes_tuiles(expr)` dans un fichier service adapté) :
   `withCallingHandlers(warning = …)` qui garde les avertissements dont le texte contient « tuile »
   et « WFS » (ou « WMS » si foretaccess l'émet aussi pour le WMS — vérifier), les étouffe
   (`invokeRestart("muffleWarning")`) et les rend avec la valeur ; les autres avertissements passent
   inchangés. Message complet, sans ANSI (`.strip_ansi`).
3. Les résultats des services desserte/accessibilité portent ces alertes (ex. champ
   `avertissements`). En fin de tâche, une étape réussie avec alertes : `pipeline_answer(..., "ok",
   <alertes>)` si le protocole accepte un message sur `ok` (vérifier `R/mod_pipeline.R`), sinon
   proposer la forme la plus simple et le dire dans la PR ; et une `showNotification` de type
   `warning` dans l'onglet. Clé i18n si un intitulé est ajouté (≤ 30 caractères, `\uXXXX`).
4. Une alerte ne doit pas être perdue par le cache : si le résultat est relu depuis le cache disque
   (`.load_cached_desserte` etc.), dire dans la PR ce que devient l'alerte (acceptable : elle ne
   vaut que pour le run qui l'a vue).

## Tests

Sans réseau : `local_mocked_bindings(acquire_desserte = function(...) { warning("1 tuile WFS sur 4
en echec pour \"BDTOPO_V3:troncon_de_route\"…"); <sf jouet> }, .package = "foretaccess")`. Le
service réussit et rend l'alerte ; un avertissement sans rapport n'est pas capturé ; test direct du
helper ; et, si le câblage module est testable, le message pipeline contient l'alerte. Mutation
rouge (retirer le `muffle`/la capture → le test échoue). Filtre
`desserte|accessibility|pipeline|foretaccess`, `timeout 540`. Vérifier
`grep -nP "[^\x00-\x7F]" R/*.R`.

## Versionnage

`2026.11.2.9008`, entrée NEWS (`fix`), section « Dépendances » pour le plancher.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/main`, branche `claude/038-foretaccess-tuiles`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : un run desserte normal ne montre aucune alerte (validation foretaccess :
  « Parc de la colombière », 0 alerte en 3 appels).

## Statut

Terminée le 2026-11-21 : PR #309 fusionnée (`441d03bb`, `.9008`, plancher `foretaccess (>= 2.6.2)`). Relecture : fusionnable, reprises (tests des 4 services, double `.strip_ansi`) faites. Cache et fausse alarme renvoyés à foretaccess, livrés en 2.6.3.

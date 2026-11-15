# Mission 033 — Inventaire vidéo : téléchargement incrémental de terrain.zip et part au ruban (cœur nemeton 2026.11.30)

> Créée par l'orchestrateur le 2026-11-15 sur ordre de Pascal (« publie et code les briefs
> restants »). Solde le reste de la mission 030 (points en attente du cœur 2026.11.30, publié le
> 2026-11-15) : brief de référence `/home/pascal/dev/nemeton/specs/061-inventaire-video/brief-nemetonshiny.md`,
> § 2, § 3.2 point 3, § 3.3 points 5 et 8, § 5 — **le lire en entier** (lecture seule). Doc roxygen
> du cœur : `gh api repos/pobsteta/nemeton/contents/R/<fichier>?ref=v2026.11.30` (lecture seule).

## Mission

**En une phrase :** « Récupérer » télécharge dans un dossier stable du projet en passant
`etat =` au cœur, pour qu'un `terrain.zip` inchangé ne soit pas redemandé, et le bilan
d'intégration affiche par placette la part des arbres mesurés au ruban (`dbh_ruban`).

## Constat (orchestrateur, lu sur `main` le 2026-11-15)

- `R/mod_inventaire_video.R:~444-475` (`input$recuperer`) : `dossier <- tempfile(...)` → le témoin
  `<plot_id>_terrain.envoye_le` du cœur est perdu à chaque clic ; `etat` n'est pas passé.
- `R/service_inventaire_video.R:74` `video_telecharger(url, jeton, plot_ids, dossier, fichiers = NULL)`
  ne connaît pas `etat`.
- `R/service_inventaire_video.R:434` `video_bilan(res)` : vérifier s'il rend `res$validation$dbh_ruban`
  (les alertes ont été reprises par la mission 030).
- Le tableau brut du serveur (`video_etat_serveur()`) est déjà obtenu dans « Suivre » : le garder
  (ex. `rv$srv`) pour le passer en `etat`.

## Travail attendu

1. `video_telecharger(..., etat = NULL)` transmet `etat` au cœur (seulement si non `NULL`, pour rester
   compatible).
2. « Récupérer » : dossier stable par projet, ex. `file.path(video_dossier(project_path()), "serveur")`
   (vérifier le nom réel du helper), créé au besoin ; `etat = <tableau brut du serveur>`. Sans projet
   ouvert, repli sur `tempfile()` comme aujourd'hui. Rien ne doit effacer ce dossier.
3. `video_bilan()` rend `dbh_ruban` ; l'écran de bilan affiche par placette « x arbres sur n mesurés
   au ruban » (clé `video_part_ruban`, `%d`), rien quand `n_ruban` est `NA`.
4. Plancher `Imports: nemeton (>= 2026.11.30)` (`chore(deps)`), l'argument `etat` n'existant pas
   avant.

## Tests

`etat` transmis (mock `local_mocked_bindings(.package = "nemeton")` de `groma_telecharger_resultats`),
absent quand `NULL` ; dossier stable sous le projet ; `video_bilan()` rend `dbh_ruban` ; affichage
de la part, rien si `NA`. Si le nemeton installé est ≥ 2026.11.30 (`packageVersion`), un test réel :
deux appels avec le même `terrain_envoye_le` → une seule requête `terrain.zip`
(`httr2::local_mocked_responses`), sinon `skip_if`. Mutation rouge. Filtre `inventaire_video`,
`timeout 540`.

## Versionnage

`2026.11.1.9008`, entrée NEWS (`feat`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/033-groma-incremental`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : deux clics « Récupérer » sur une placette Groma 1.6 → une seule archive téléchargée.

## Statut

Terminée le 2026-11-15 : PR #292 fusionnée (`0d9eb553`), `.9008`, plancher nemeton 2026.11.30. Reprise après relecture : serveur réinterrogé à chaque « Récupérer ». Reste : essai des deux clics sur une placette Groma 1.6.

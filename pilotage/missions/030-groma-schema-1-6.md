# Mission 030 — Inventaire vidéo : schéma Groma 1.6.0 (cœur nemeton 2026.11.29)

> Créée par l'orchestrateur le 2026-11-15 sur ordre de Pascal (« traite-les tous […] ne demande
> rien »). Solde le brief `~/dev/briefs/vers-nemetonshiny/2026-11-15-groma-schema-1-6.md`, dont la
> copie de référence est `/home/pascal/dev/nemeton/specs/061-inventaire-video/brief-nemetonshiny.md`
> (section « schéma Groma 1.6.0 », lignes 1-25) — **la lire en entier** (lecture seule), ainsi que
> la doc roxygen des fonctions au tag `v2026.11.29` du cœur
> (`git -C /home/pascal/dev/nemeton show v2026.11.29:R/<fichier>`, lecture seule).

## Mission

**En une phrase :** l'onglet Inventaire vidéo affiche la réception du terrain et les placettes
`sans_paquet` dans « Suivre », télécharge et passe `terrain.zip` à l'import dans « Intégrer »,
traduit les trois nouveaux motifs d'alerte avec leur `detail`, et propose un rayon de 15 m.

## Constat (orchestrateur, lu sur `main` le 2026-11-15)

- `R/service_inventaire_video.R` (468 l.) appelle le cœur par `.video_appeler()` :
  `groma_etat_placettes()` (`:60`), `groma_telecharger_resultats()` (`:68`),
  `importer_inventaire_video(gpkg = …)` (`:94`) ; fusion serveur / local des états (`:361-383`).
- `R/mod_inventaire_video.R:30-31` : `numericInput(ns("rayon"), value = 20, …)` → 15.
- Alertes : chercher la traduction des motifs existants (`R/service_inventaire_video.R:174` et
  suivantes, clés `video_*` de `R/utils_i18n.R`).

## Travail attendu (brief, points 1 à 5)

1. **Suivre** : afficher `terrain_envoye_le` (« terrain reçu le … », heure locale Europe/Paris,
   `NA` = rien) ; état `sans_paquet` traduit (« saisie reçue, sans vidéo ») et rangé dans l'ordre
   des états (`rang()`, `:381`).
2. **Intégrer** : `groma_telecharger_resultats()` télécharge `terrain.zip` par défaut ; passer le
   dossier en `terrain =` à `importer_inventaire_video()` ; `gpkg` (copie de QField) devient
   facultatif — l'import ne doit plus l'exiger côté app. Photos copiées sous `photos/<plot_id>/` à
   côté de `field_data_gpkg` : vérifier que le dossier du projet les accueille et que rien ne les
   efface.
3. **Alertes** : colonne `detail` nouvelle ; motifs `terrain_serveur_prefere`,
   `saisie_qfield_ignoree` (`detail` = `champ = valeur`), `centre_telephone` (`detail` =
   `precision_h_m = …`) traduits fr/en, `detail` affiché à côté.
4. **Validation** : un centre `telephone` sort en avertissement (`field = "qualite_gnss"`) ;
   nouveaux contrôles `dbh_source`, `qualite_gnss`, `source_azimut` — vérifier que l'app affiche ces
   avertissements sans les traiter comme des erreurs bloquantes.
5. **Rayon** : 15 m par défaut.
6. Plancher `Imports: nemeton (>= 2026.11.29)` (`chore(deps)`), l'argument `terrain =` n'existant
   pas avant ; garder la détection de disponibilité `:32-33` cohérente.

## Tests

Mocks des fonctions cœur (`local_mocked_bindings(.package = "nemeton")`) : `terrain_envoye_le` et
`sans_paquet` dans l'état fusionné ; `terrain =` transmis, `gpkg = NULL` accepté ; traduction des
3 motifs + `detail` ; rayon 15. Mutation rouge. Filtre `inventaire_video`, `timeout 540`.

## Lecteurs et appelants

nemetonmcp (lecture seule, `grep -rn inventaire_video /home/pascal/dev/nemetonmcp`), export
QField, `mod_field_ingest`. Groma : aucun changement demandé à Groma.

## Versionnage

Version donnée par l'orchestrateur au lancement.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/030-groma-1-6`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : une placette envoyée par Groma 1.6 avec et sans vidéo.

## Statut

Terminée le 2026-11-15 : PR #287 fusionnée (`953729be`), `.9005`, plancher nemeton 2026.11.29. Reprise : « Suivre » lit `field_data.gpkg` en lecture seule sans copie QField, repli de téléchargement ciblé, motifs sans clé sous « Autre alerte ». Reste : `etat =` incrémental et `dbh_ruban` (cœur 2026.11.30 non publié), essai d'une placette Groma 1.6.

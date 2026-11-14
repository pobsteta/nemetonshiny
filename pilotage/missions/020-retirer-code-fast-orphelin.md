# Mission 020 — Retirer le code FAST resté sans lecteur (libellés de classes)

> Créée par l'orchestrateur le 2026-11-14 sur décision de Pascal (« oui fais le »), suite de la
> relecture de la mission 018.

## Mission

**En une phrase :** Retirer la branche FAST de `.fast_class_labels`, `.classify_alert_count`,
`.alert_count_palette` et les sept clés i18n `validation_class_fast_*` /
`validation_class_unit_*`, que l'application n'appelle plus, sans rien changer à l'écran.

## Constat (orchestrateur, 2026-11-14, lu sur `origin/main` 99959307)

- Seul appel applicatif de `.fast_class_labels` : l'observateur des libellés du Plan de
  validation, `R/mod_validation_sampling.R:288`, avec `NULL` et uniquement pour FORDEAD
  (l'observateur sort pour FAST et RECONFORT, mission 018).
- `.fast_class_labels` (`R/mod_monitoring_fast_alerts.R:1243`) : branche FORDEAD statique, puis
  branche FAST (`:1256-1284` environ) qui seule appelle `.classify_alert_count` (`:1192`, appel
  `:1269`) et lit `validation_class_fast_1..4` (`:1258-1261`, `:1281`) et
  `validation_class_unit_days|trend|deficit` (`:1264-1266`).
- `.alert_count_palette` (`:1292`) : aucun appel dans `R/` (seul son commentaire `:1185` et
  `:1289`).
- Clés i18n : `R/utils_i18n.R:5565-5595` ; commentaire d'en-tête `:5560-5564` (déjà réécrit par
  018 pour dire qu'elles n'ont plus de lecteur).
- Tests qui les visent seuls : `tests/testthat/test-mod_monitoring_fast_alerts.R:60-70`
  (trend), `:72-123` (`.classify_alert_count`), `:133-151` (FAST nul / quartiles), `:153-157`
  (`.alert_count_palette`). `:125-131` teste la branche FORDEAD, qui reste.
- Le test du module ajouté par 018 (`tests/testthat/test-mod_validation_sampling.R`, libellés
  FORDEAD en anglais, ordre 4-3-2-1) doit rester vert : c'est le garde-fou « rien ne change ».

## Travail attendu

1. Remplacer `.fast_class_labels(r, source, mode, i18n)` par une fonction FORDEAD seule au nom
   juste (proposé : `.fordead_class_labels(i18n)`), qui rend les mêmes libellés ; mettre à jour
   l'appel `R/mod_validation_sampling.R:288` et réécrire tout le commentaire `:280-287` (le motif
   « match.arg … reecrirait les choix » devient faux : la raison de sauter FAST et RECONFORT est
   simplement que la fonction ne rend que des libellés FORDEAD). La fonction peut rester dans
   `R/mod_monitoring_fast_alerts.R` (pour ne pas toucher le fichier de 019), si son commentaire le
   dit.
2. Supprimer `.classify_alert_count`, `.alert_count_palette` et leurs commentaires.
3. Supprimer les 7 clés i18n et réécrire le commentaire `R/utils_i18n.R:5560-5564`.
4. Supprimer les tests qui ne visent que le code retiré ; adapter `:125-131` au nouveau nom **et
   le renforcer** : comparer les 4 libellés, en `fr` **et** en `en`, par `expect_identical` à
   `i18n$t("validation_class_fordead_k")`, plus au moins un littéral (`"4 — sol nu"`, en
   `\uXXXX`). Lister dans le rendu chaque test supprimé.
5. Avant de supprimer, vérifier par `grep` sur `R/`, `tests/`, `inst/` (y compris quarto, js,
   yml) qu'aucune autre référence n'existe, y compris les helpers appelés seulement par le code
   retiré (ex. `.fast_raster_is_empty` : le garder s'il a un autre lecteur, sinon le signaler et
   le retirer dans la même logique). Vérifier aussi qu'aucun test de cohérence i18n (clés FR/EN,
   clés lues) ne casse.
6. Ne toucher ni au Plan de validation FAST (trend), ni à `alert_mask_r`, ni à la légende
   d'Alertes FAST.
7. `grep -nP "[^\x00-\x7F]"` vide sur chaque fichier R touché.

## Coordination

La mission 019 tourne en parallèle sur `R/service_validation_sampling.R` et
`R/mod_validation_sampling.R` (lecteurs du masque FORDEAD). 020 ne touche dans
`R/mod_validation_sampling.R` que l'observateur des libellés (`:280-300`). En cas de conflit au
merge, l'orchestrateur rebase la PR arrivée en second.

## Versionnage

Cycle dev : prochain numéro libre après 019 (`2026.10.18.9013` si 019 fusionne avant, sinon
`.9012` et l'orchestrateur renumérote). Entrée NEWS « Non publié », section nettoyage, qui dit que ce code est désormais retiré. Ne
**pas** modifier l'entrée `.9011` (sa ligne « signalé et non retiré » est de l'historique).

## Tests

Commande : `Rscript -e 'devtools::test(filter = "validation|monitoring|i18n")'` ; rendre pass /
fail / skip. Deux mutations au moins : changer une clé FORDEAD dans la nouvelle fonction (doit
rougir le test unitaire et celui du module de 018), échanger l'ordre des clés. « Rien ne change à
l'écran » se prouve par le `grep` (aucun autre lecteur) plus l'identité des libellés fr/en.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`.
- Relecteur sur la fiche avant dev, puis sur la PR.
- Temps de la main : *appuyer*.

## Redite par l'agent : _à compléter_

## Statut

Terminée le 2026-11-14 : PR #271 fusionnée sur `main` (`6a9326d4`, commit de mission `3aad4011`,
fusion de main et renumérotation `208df40b` par l'orchestrateur), cycle dev `2026.10.18.9013`, non
publié. Relecture : prêt.

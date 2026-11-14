# Mission 023 — Surcouche du Plan de validation : classe saine transparente, couleurs de chaque moteur (FAST, FORDEAD, RECONFORT)

> Créée par l'orchestrateur le 2026-11-14 sur décision de Pascal (« oui identique entre FAST,
> FORDEAD et RECONFORT »), suite de la relecture de la fiche 021.

## Mission

**En une phrase :** Sur la carte du Plan de validation, la surcouche du masque d'alerte rend la
classe saine **transparente** et colore les classes d'alerte **comme la carte de santé du même
moteur**, pour FAST, FORDEAD et RECONFORT à l'identique.

## Constat (orchestrateur, 2026-11-14, lu sur `main` 33a0fc42)

- Surcouche du Plan de validation : `R/mod_validation_sampling.R:734-748`. Palette unique 0:4
  (`#2CA02C`, `#FFD27F`, `#FF9933`, `#D62728`, `#222222`) et `terra::ifel(r == 0, NA, r)` : seule
  la classe **0** est transparente.
- RECONFORT a une échelle 1/2/3 avec **1 = sain** (`.validation_classe_saine()`,
  `R/mod_validation_sampling.R:943-950`) : sa classe saine 1 s'affiche en jaune pâle, comme une
  alerte faible ; 2 et 3 prennent les couleurs FAST/FORDEAD 2 et 3 (orange, rouge) par hasard.
- Cartes de santé, référence de rendu :
  - FAST, Alertes FAST : `r_show <- terra::ifel(is.na(r) | r <= 0, NA, r)`
    (`R/mod_monitoring_fast_alerts.R:1083`), classe 0 transparente ;
  - FORDEAD : sévérité, classe 0 transparente (`R/mod_monitoring_fordead_map.R:110-114`) ;
  - RECONFORT : `.RECONFORT_CLASSIF_COLORS` (`R/mod_monitoring_reconfort_map.R:45-49`), classe 1
    transparente (`:338-345`).
- La surcouche n'est tracée que hors trend (`!is_trend`, `:734`) : FAST n'y passe que par le chemin
  « classes » (non atteignable depuis l'UI aujourd'hui, mission 017) ; la règle doit quand même
  valoir pour lui.

## Travail attendu

1. Rendre la classe saine transparente selon la source (`.validation_classe_saine()` : 0 pour
   FAST / FORDEAD, 1 pour RECONFORT) ; NA reste transparent.
2. Palette par source, **reprise** des cartes de santé (pas recopiée) : FAST et FORDEAD la palette
   0-4 de leur carte (exposer ou réutiliser la constante existante si elle existe, sinon en créer
   une partagée dans le fichier du moteur) ; RECONFORT `.RECONFORT_CLASSIF_COLORS`. Une seule
   source de vérité par moteur.
3. Ne pas toucher la palette des marqueurs (Validation vert / Témoin gris) ni le plan trend.
4. Helper testable hors Shiny (ex. `.validation_overlay_style(r, source)` rendant `r_show`, la
   palette et les niveaux) ; tests : pour chaque source, classe saine → NA, classes d'alerte →
   couleur de la carte du moteur ; au moins 3 mutations rouges (une par source).
5. `grep -nP "[^\x00-\x7F]"` vide sur chaque fichier R touché.

## Coordination

`R/mod_validation_sampling.R` est aussi touché par la mission 021 (en cours). 023 est lancée
**après la fusion de 021**, depuis le `main` à jour.

## Versionnage

Prochain numéro libre du cycle dev au moment du lancement ; entrée NEWS « Non publié », section
changements visibles.

## Tests

Machine chargée : premier plan, `timeout 540`, filtre `validation_sampling` ; rendre pass / fail
/ skip.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main` après 021.
- Relecteur sur la fiche avant dev, puis sur la PR.
- Temps de la main : *appuyer*.

## Redite par l'agent : _à compléter_

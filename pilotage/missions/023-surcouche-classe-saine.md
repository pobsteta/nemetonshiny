# Mission 023 — Surcouche du Plan de validation : classe saine transparente, couleurs de chaque moteur (FAST, FORDEAD, RECONFORT)

> Créée par l'orchestrateur le 2026-11-14 sur décision de Pascal (« oui identique entre FAST,
> FORDEAD et RECONFORT »), suite de la relecture de la fiche 021.

## Mission

**En une phrase :** Sur la carte du Plan de validation, la surcouche du masque d'alerte rend la
classe saine **transparente** et colore les classes d'alerte **comme la carte de santé du même
moteur**, pour FAST, FORDEAD et RECONFORT à l'identique.

## Constat (orchestrateur, 2026-11-14, lu sur `main` 33a0fc42)

- Surcouche du Plan de validation : `R/mod_validation_sampling.R:733-747` (`!is_trend` `:735`,
  palette `:741` ; ces lignes bougent d'environ −10 après 021 : retrouver le bloc par son
  contenu). Palette unique 0:4 (`#2CA02C`, `#FFD27F`, `#FF9933`, `#D62728`, `#222222`) = **copie
  de la palette FORDEAD**, et `terra::ifel(r == 0, NA, r)` : seule la classe **0** est
  transparente. **Aucune légende** des classes sur la surcouche (seules les légendes des marqueurs,
  `:770-790`).
- RECONFORT a une échelle 1/2/3 avec **1 = sain** (`.validation_classe_saine()`,
  `R/mod_validation_sampling.R:943-950`) : sa classe saine 1 s'affiche en jaune pâle, comme une
  alerte faible ; 2 et 3 prennent les couleurs FAST/FORDEAD 2 et 3 (orange, rouge) par hasard.
- Cartes de santé, référence de rendu — **les palettes FAST et FORDEAD diffèrent** et sont
  écrites en ligne (seul RECONFORT a une constante) :
  - FAST, Alertes FAST : classes 1-4 `#fee08b #fdae61 #f46d43 #d73027` (`:1085`, `colorBin`
    `:1095`, légende `:1108`), classe 4 = « sévère » (rouge foncé) ;
    `r_show <- terra::ifel(is.na(r) | r <= 0, NA, r)` (`R/mod_monitoring_fast_alerts.R:1083`) ;
  - FORDEAD, sévérité : classes 0-4 `#2CA02C #FFD27F #FF9933 #D62728 #222222`
    (`R/mod_monitoring_fordead_map.R:111`, dans `.fordead_layer_spec`), classe 4 = « sol nu »
    (quasi-noir), classe 0 transparente (`:113`) ;
  - RECONFORT : `.RECONFORT_CLASSIF_COLORS` (`R/mod_monitoring_reconfort_map.R:45-49`), classe 1
    transparente (`:338-345`).
- La surcouche n'est tracée que hors trend (`!is_trend`, `:734`) : FAST n'y passe que par le chemin
  « classes » (non atteignable depuis l'UI aujourd'hui, mission 017) ; la règle doit quand même
  valoir pour lui.

## Travail attendu

1. Classe saine transparente selon la source : `NA si is.na(r) | r <= saine`, comme les cartes
   (`fast_alerts:1083`, `fordead_map:113`, `reconfort_map:345`), avec
   `saine <- as.integer(.validation_classe_saine(source))` (rend `"0"` / `"1"` en texte). Pour
   RECONFORT, 0 = pas de donnée : `r == 1` seul laisserait 0 hors des niveaux 1:3.
2. Palette par source, **une seule source de vérité par moteur** :
   - créer `.FAST_ALERT_CLASS_COLORS` (nommée "1".."4") dans `R/mod_monitoring_fast_alerts.R` et
     l'utiliser en `:1085` et `:1108` ;
   - créer `.FORDEAD_SEVERITY_COLORS` (nommée "0".."4") dans `R/mod_monitoring_fordead_map.R` et
     l'utiliser en `:111` ;
   - RECONFORT : `.RECONFORT_CLASSIF_COLORS` existante.
   Conséquence visible voulue : sur le Plan de validation, FAST prend ses vraies couleurs (classe
   4 « sévère » rouge au lieu du noir « sol nu » de FORDEAD) ; FORDEAD ne change pas
   visuellement ; RECONFORT prend vert/orange/rouge de sa carte, classe 1 transparente.
2 bis. **Légende** (décision de l'orchestrateur, au nom de « identique » demandé par Pascal) :
   ajouter à la surcouche une légende des **seules classes peintes** (alertes), libellés existants
   `fast_alert_class_*`, `monitoring_fordead_class_*`, `reconfort_class_label_2/3`, comme la
   carte RECONFORT (`reconfort_map:350-358`).
3. Ne pas toucher la palette des marqueurs (Validation vert / Témoin gris) ni le plan trend.
4. Helper testable hors Shiny (ex. `.validation_overlay_style(r, source)` rendant `r_show`, la
   palette, les niveaux et la légende). Tests non vacants :
   - FORDEAD : comparer à la palette de la **carte réelle**,
     `.fordead_layer_spec(r, "severity", i18n)$pal(k)` pour k = 1..4 ;
   - FAST et RECONFORT : comparer à la constante **et** à au moins une couleur hexadécimale en
     dur par source ; comparer en `toupper()` ;
   - `r_show` vérifié par `terra::values()` sur un raster contenant toutes les classes, y compris
     0 et NA pour RECONFORT ;
   - mutations attendues rouges : (a) RECONFORT revient à `r == 0` ; (b) FAST garde la palette
     FORDEAD ; (c) FORDEAD : la classe 0 n'est plus NA ;
   - `test-monitoring_lot3.R:13` (`.validation_classe_saine`) reste intact.
5. `grep -nP "[^\x00-\x7F]"` vide sur chaque fichier R touché.

## Coordination

Ordre **021 → 022 → 023** : 021 touche `R/mod_validation_sampling.R`, 022 les trois fichiers des
cartes de santé (dont `reconfort_map:338-345`) d'où 023 tire ses constantes. 023 part du `main`
après la fusion de 022.

## Hors périmètre

- Opacité de la surcouche (0,55) inchangée, même si les cartes de santé sont à 0,75 / 0,85.
- La légende FORDEAD de la carte de santé liste encore le vert de la classe 0, non peinte
  (`fordead_map:117-118`) : signalé, non corrigé.
- Palette `colorNumeric` des marqueurs trend (`mod_validation_sampling.R:680`).

## Terrain (à vérifier par Pascal)

Un projet réel avec Plan de validation sur strate, au minimum RECONFORT (`classification`) et
FORDEAD (Couchey par exemple) : la forêt saine laisse voir le fond de carte ; les classes d'alerte
ont la même couleur que sur la carte de santé du moteur.

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

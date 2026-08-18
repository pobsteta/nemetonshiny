# Brief `nemetonshiny` — les trois derniers points ouverts (2026-08-17)

**Dépôt cible** : `nemetonshiny`. **Rien à livrer côté cœur** : les trois points
ne dépendent que de code app. Plancher requis : **`nemeton (>= 0.175.0)`**.

Ce brief consolide ce qui reste après la journée du 2026-08-16/17, où le cœur a
publié cinq releases (v0.172.0 → v0.175.0) et où l'app a câblé
`theia_source_status()`, `a5_status`, `build_index_stack()` pour C2. Il remplace,
pour ces trois points, les renvois épars : §7 du brief A5 diagnostic et
`brief-nemetonshiny-indicator-families.md`.

État vérifié en lecture seule le 2026-08-17 sur `nemetonshiny@ccb85ad0`.

---

## Point 1 — Afficher les verdicts d'applicabilité (nouveau, cœur v0.175.0)

### Ce que le cœur fournit

```r
nemeton::r5_applicabilite(units, bdforet = NULL, layers = NULL)
#> $status "eligible_fordead_out_of_calibration"  $method "fordead"
#> $in_calibration FALSE  $geo_pct 0  $n_fordead 30/30  $per_unit <data.frame>

nemeton::a5_applicabilite(units, lst = NULL, buffer_m = 500)
#> $status "no_coverage"  $n_assets 0  $n_eligible 0/30  $per_unit NULL
```

Les deux rendent des **clés stables**, à traduire côté app :

| Fonction | `status` |
|---|---|
| `r5_applicabilite` | `eligible_fordead`, `eligible_fordead_out_of_calibration`, `eligible_reconfort`, `no_species`, `not_applicable` |
| `a5_applicabilite` | `eligible`, `eligible_partial`, `no_coverage`, `no_reference`, `no_credentials`, `error` |

### Deux nuances à ne pas aplatir

**`eligible_fordead_out_of_calibration` n'est pas un refus.** La zone de
validation de R5 est celle du rapport ONF/DSF 2024 — Vosges (88), Jura (39),
Ain (01), Savoie (73), Haute-Savoie (74), 27 565 km². Hors de ces départements
un sapin pectiné reste un sapin pectiné : le calcul tourne, seules ses classes de
confiance sont extrapolées. **Aucun des trois projets locaux n'est dans la
zone** — Fordead (Ardennes) et Dabo (Moselle) y sont pourtant à 100 % de sapin.
Bloquer R5 sur ce critère reviendrait à priver l'utilisateur d'un signal
exploitable ; le message doit dire *« hors zone de validation ONF/DSF, classes de
confiance extrapolées »*, pas *« non calculable »*.

**`a5_applicabilite()` sans `lst` répond à l'échelle de l'emprise.** Une requête
STAC connaît des bounding boxes, pas des pixels : `eligible` y signifie « la
couverture existe », pas « chaque UGF est notable ». Passer le raster du cache
(`cache/layers/lst/lst_*.tif`) fait basculer sur un verdict **par unité**, seul
capable de rendre `eligible_partial`.

### Le travail

1. **Au moment de choisir les sources** (`mod_sources_config.R`) : afficher le
   verdict à côté du toggle, avec le compte (`n_eligible / n_units`,
   `n_assets`). C'est là qu'il est utile — avant, pas après.
2. **Court-circuiter le calcul** quand le statut est `not_applicable`,
   `no_species` ou `no_coverage` : inutile de lancer FORDEAD ou d'interroger le
   catalogue pour aboutir à une colonne de `NA`.
3. **Dans la vue famille** : `per_unit` permet de dire *quelles* UGF sont
   concernées quand le statut est partiel — la machinerie d'affichage des causes
   existe déjà (`mod_family.R` §`.a5_status`).

### Clés i18n (FR/EN)

`r5_appl_eligible_fordead`, `r5_appl_out_of_calibration`,
`r5_appl_eligible_reconfort`, `r5_appl_no_species`, `r5_appl_not_applicable`,
`a5_appl_eligible`, `a5_appl_partial`, `a5_appl_no_coverage`,
`a5_appl_no_reference`, `a5_appl_no_credentials`, `a5_appl_error`.

---

## Point 2 — Cesser de jeter `r5_status` (une ligne)

`R/service_r5.R:117` :

```r
out$r5_status <- NULL          # <- à retirer
```

Le cœur produit cette colonne depuis la **spec 008** (2026-04-30). Elle est la
seule à expliquer un R5 vide, et elle est supprimée juste avant d'atteindre
l'interface. Valeurs : `calculated`, `calculated_reconfort`, `skipped_no_fordead`,
`skipped_no_reconfort`, `skipped_no_method`.

Si la colonne gêne la détection de famille, la préfixer (`.r5_status`) comme
c'est déjà fait pour `.a5_status` — `create_family_index()` n'apparie que
`indicateur_*` et `^R[0-9]+(_norm)?$`, un nom préfixé d'un point ne le perturbe
pas. **Ne pas la supprimer.**

`mod_family.R` sait déjà traduire une colonne de cause (l. 92, 736, 757) : le
même chemin sert pour R5, seule la table de correspondance change.

Clés i18n : `r5_skipped_no_fordead`, `r5_skipped_no_reconfort`,
`r5_skipped_no_method`.

---

## Point 3 — Dé-forker `INDICATOR_FAMILIES` (le point bloquant)

### Pourquoi il bloque le reste

`R/app_config.R:127` déclare sa propre copie de la table des familles. Sa
famille A vaut :

```r
indicators   = c("A1", "A2", "A3", "A4")        # A5 ABSENT
column_names = c("indicateur_a1_couverture", ..., "indicateur_a4_tamponnement")
```

`mod_family.R:17` lit `get_family_config()`, donc ce fork, et construit la liste
affichée à partir de `family_config$indicators` (l. 70, 535, 541). **A5 est
calculé par `service_compute.R` puis filtré à l'affichage** : tout le travail A5
livré en v0.173.1 — y compris le statut de cause câblé aujourd'hui — reste
invisible dans l'onglet Air tant que ce fork existe.

### La racine du bug F1/F2, mesurée

```r
# famille F
indicators   = c("F1", "F2")
column_names = c("indicateur_f2_erosion", "indicateur_f1_fertilite")   # inversé
# famille L
indicators   = c("L1", "L2", "L3")
column_names = c("indicateur_l2_fragmentation", "indicateur_l1_sylvosphere", ...)
```

L'appariement code ↔ colonne se fait **par position** : `F1` pointe sur
l'érosion, `L1` sur la fragmentation. Le fork compense dans ses propres
`indicator_labels` (F1 = « Risque d'érosion »), mais la **troisième copie** — les
clés i18n `indicator_F1` (`utils_i18n.R:1475`) — dit l'inverse (« Fertilité des
sols »). Le libellé affiché dépend donc de la source lue. Dé-forker règle les
trois copies d'un coup, parce que le cœur apparie explicitement, pas par rang.

### Les patches, revérifiés le 2026-08-17

`git apply --check` contre `nemetonshiny@ccb85ad0` :

| Patch | État |
|---|---|
| `01-app_config-defork.diff` | ✅ s'applique |
| `02-menu-boucle.diff` | ✅ s'applique |
| `03-cles-famille.diff` | ❌ `R/utils_i18n.R:1220` |
| `04-libelles-indicateurs.diff` | ❌ `R/utils_i18n.R:1225` |
| `05-theme-ordre.diff` | ✅ s'applique |
| `06-test-identite.diff` | ✅ s'applique |

`03` et `04` ne s'appliquent plus parce que `utils_i18n.R` a bougé aujourd'hui —
les clés A5 / LST / C2 y ont été insérées. Leur **contenu** reste valable : ils
remplacent les copies locales de libellés et de clés de famille par une lecture
de `nemeton::indicator_families()` / `nemeton::indicator_labels()`, qui rendent
désormais les deux langues en permanence (colonnes `_fr` / `_en`, v0.171.0). À
rejouer à la main sur les blocs correspondants ; le patch `06` verrouille le
résultat par un test d'identité app ↔ cœur, à passer en dernier.

---

## Critères d'acceptation

- [ ] `DESCRIPTION` : `nemeton (>= 0.175.0)` et `foretaccess (>= 2.4.0)` — ce
      dernier est consommé par `mod_desserte.R` sans être exigé.
- [ ] `app_config.R` ne déclare plus `INDICATOR_FAMILIES` ; le test d'identité
      app ↔ cœur (patch 06) passe.
- [ ] L'onglet Air affiche **A5**, avec son statut de cause quand il est vide.
- [ ] Le libellé de la colonne d'érosion n'est plus « F1 - Fertilité des sols ».
- [ ] `r5_status` survit jusqu'à l'UI ; un R5 vide dit pourquoi.
- [ ] Le panneau des sources affiche les verdicts d'applicabilité **avant**
      calcul, et le calcul est court-circuité quand l'indicateur ne s'applique
      pas.
- [ ] Tous les textes passent par `i18n$t()`.

## Hors scope

- Toute modification du calcul des indicateurs : les cinq releases du cœur
  couvrent le versant métier.
- La recherche d'une source LST nationale pour étendre A5 hors métropoles —
  chantier cœur, déjà listé hors scope du brief 032.

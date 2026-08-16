# BRIEF `nemeton` — les scores de famille agrègent des valeurs brutes

**Dépôt cible** : `/home/pascal/dev/nemeton` (cœur). Session dédiée requise.

**Origine** : diagnostic du 2026-08-16 sur le projet **Fordead** (30 UGF,
`nemeton 0.172.0`, `nemetonshiny 0.125.0`). L'utilisateur constate un score de
famille **C = 0,90 / 100**. Reproduit à l'identique depuis
`data/indicators.parquet`.

**Ce brief est autoportant** : il couvre l'ensemble de ce qui reste à corriger
côté cœur après la release v0.172.0 (borne R1, déjà livrée). Aucun autre brief
n'est à passer pour ce sujet.

## 0. Critères d'acceptation — récapitulatif

| CA | Objet | Section | Priorité |
|----|-------|---------|----------|
| **CA-1** | `create_family_index()` normalise ce qu'il agrège | §4 | **bloquant** — 9 familles sur 12 sont fausses sans lui |
| **CA-2** | Avertir au lieu de se taire sur une colonne hors `[0, 100]` | §4 | haute |
| **CA-3** | Test de non-régression sur les valeurs Fordead | §4 | haute |
| **CA-4** | C2 calculé depuis Sentinel-2 L2A, pas depuis une ortho WMS | §5.a | **haute** — plus gros gain de justesse |
| **CA-5** | C1 : `zmean` sur la canopée, pas sur l'unité entière | §5.b | moyenne — facteur 10 à 14 |
| **CA-6** | `sufosat` : déclarer la collection STAC | §6 | moyenne — T3 est tout NA |
| **CA-7** | Vérifier `r1_feu` constant à 0,000 depuis la borne 30 m | §5.c | à investiguer |

CA-1 conditionne la lecture de tout le reste : tant qu'il n'est pas livré,
aucun score de famille n'est interprétable, et les autres correctifs ne se
verront pas.

---

## 1. Le symptôme

| Famille | Score moyen (30 UGF) | Indicateurs sous-jacents |
|---------|---------------------|--------------------------|
| S — Social | 94,03 | S1, S2, S3 |
| A — Air | 81,12 | A1, A2 |
| B — Biodiversité | 66,72 | B1–B4 |
| F — Sols | 66,06 | F1, F2 |
| N — Naturalité | 61,76 | N1–N3 |
| T — Temporel | 50,68 | T1–T3 |
| W — Eau | 48,89 | W1–W3 |
| P — Production | 38,05 | P1–P3 |
| R — Risques | 35,86 | R1–R4 |
| L — Paysage | 32,97 | L1–L3 |
| **E — Énergie** | **3,75** | E1, E2 |
| **C — Carbone** | **0,90** | C1, C2 |

C et E ne sont pas « mauvaises » : elles sont **hors échelle**.

## 2. La cause : `create_family_index()` ignore la normalisation

`normalize_indicators()` produit des colonnes suffixées **`_norm`** (31 sur ce
jeu). `create_family_index()`, lui, sélectionne ses colonnes ainsi
(`R/…` — extrait déparsé de la v0.172.0) :

```r
pattern <- paste0("^", fam, "[0-9]")
fam_indicators <- grep(pattern, indicator_cols, value = TRUE)
if (length(fam_indicators) == 0) {
  config_cols <- INDICATOR_FAMILIES[[fam]]$column_names   # <-- noms BRUTS
  fam_indicators <- intersect(config_cols, indicator_cols)
}
```

`INDICATOR_FAMILIES[["C"]]$column_names` vaut
`c("indicateur_c1_biomasse", "indicateur_c2_ndvi")` — **sans le suffixe
`_norm`**. Les deux fonctions du cœur ne composent donc pas. Vérifié :

```
colonnes produites par normalize_indicators()   : 31 en *_norm
famille_carbone AVEC les colonnes _norm présentes : 0.898
famille_carbone SANS les colonnes _norm           : 0.898
=> identique : les _norm sont IGNORÉES
```

Normaliser avant d'appeler `create_family_index()` **ne change strictement
rien**. C'est le défaut central de ce brief.

## 3. Pourquoi seules C et E s'effondrent

Le cœur couvre pourtant *tous* les indicateurs (spec 038) : sur les 41 colonnes
de `get_all_column_names()`, **18 ont une règle explicite** dans
`normalize_indicator()` et **23 sont déclarées dans
`.NORMALIZE_NATIVE_0_100`**. Aucun orphelin — l'inventaire est complet.

Mais comme la normalisation n'est jamais appliquée, seuls les 23 « natifs »
arrivent réellement sur 0–100. Les 18 autres entrent bruts dans la moyenne de
famille. Valeurs observées sur Fordead :

| Indicateur | min | méd | max | Unité brute | Effet |
|---|---|---|---|---|---|
| `c1_biomasse` | 0,002 | 0,062 | 2,388 | tC/ha (`ref_max` 150) | **C ≈ 0** |
| `c2_ndvi` | −0,223 | −0,109 | 0,093 | NDVI [−1,1] | **C ≈ 0** |
| `e1_bois_energie` | 0 | 0 | 0,566 | tep/ha/an (`ref_max` 0,3) | **E ≈ 0** |
| `e2_evitement` | 0 | 0 | 0 | tCO₂/ha/an (`ref_max` 0,75) | **E ≈ 0** |
| `s3_population` | 8 309 | 9 095 | 9 238 | habitants (`ref_max` 10 000) | **S surévaluée** (écrêtée à 100) |
| `p1_volume` | 0 | 0 | 304 | m³/ha (`ref_max` 800) | P faussée |
| `p2_station` | 10,9 | 10,9 | 18,4 | m³/ha/an (`ref_max` 15) | P faussée |
| `w1_reseau` | 17,3 | 48,2 | 78,1 | m/ha (`ref_max` 50) | W faussée |
| `w2_zones_humides` | 0 | 0 | 0,009 | % (`ref_max` 5) | W ≈ 0 sur ce terme |
| `w3_humidite` | 3,23 | 3,68 | 3,95 | TWI (règle `[2,5;4,5]`) | W faussée |
| `l3_het_spectrale` | −0,159 | −0,016 | 0,200 | indice ×100 | L faussée |
| `b4_div_spectrale` | 0,135 | 0,174 | 0,211 | indice `/log(50)` | B faussée |

**Neuf familles sur douze sont donc fausses à des degrés divers** ; C et E ne
sont que les plus visibles, parce que *tous* leurs indicateurs sont bruts.

Le cas de S mérite mention à part : `s3_population` entre à **9 095** dans une
moyenne, ce qui devrait faire exploser le score. Il faut vérifier où
l'écrêtement à 100 intervient — le score S de 94 suggère un `pmin(100, …)` en
sortie de famille, qui **masque** l'anomalie au lieu de la signaler.

## 4. Correction demandée

**CA-1 — `create_family_index()` normalise ce qu'il agrège.** Trois options,
par ordre de préférence :

1. *(recommandé)* `create_family_index()` applique `normalize_indicator()` à
   chaque colonne avant d'agréger. L'appelant n'a rien à changer, et
   `nemetonshiny` (qui appelle `create_family_index(base_sf, method = "mean")`
   en `mod_synthesis.R:159`) est corrigé sans modification.
2. `create_family_index()` **préfère** la colonne `<nom>_norm` quand elle
   existe, et retombe sur la brute sinon. Impose à l'appelant d'appeler
   `normalize_indicators()` d'abord — plus fragile, mais compatible avec un
   pipeline qui veut inspecter les valeurs normalisées.
3. Les `indicateur_*()` renvoient directement du 0–100. Rupture d'API : les
   valeurs physiques (tC/ha, m³/ha) ont une valeur d'usage propre et ne doivent
   pas disparaître du `parquet`.

**CA-2 — Un garde-fou, pas un silence.** Si `create_family_index()` reçoit une
colonne hors `[0, 100]` (négative, ou > 100) et ne la normalise pas, il doit
émettre un `cli::cli_warn()` nommant la colonne et la famille. Le mode d'échec
actuel est muet : un score de 0,90 s'affiche comme un diagnostic forestier
alors que c'est une erreur d'unité.

**CA-3 — Test de non-régression sur Fordead.** Avec les valeurs brutes du
tableau §3, `famille_carbone` doit passer de **0,90 à ~28** (moyenne des
normalisées : C1 → 9,0/100, C2 → 47,0/100 en min-max ; à recalculer avec la
règle `ref_max` : C1 = 0,062/150×100 ≈ 0,04, C2 = clamp(−0,109×100) = 0).
**Attention** : avec la règle `ref_max` du cœur, C reste très bas — voir §5,
c'est un problème distinct et il ne faut pas le masquer en choisissant une
normalisation min-max qui « remonte » artificiellement les scores.

## 5. Deux anomalies distinctes, à ne pas confondre avec §2

Elles resteront après la correction de la normalisation.

**5.a — `c2_ndvi` est négatif sur les 30 UGF, parce que sa source n'est pas un
produit de réflectance.** Le `ndvi.tif` ne vient pas de Sentinel-2 : l'app le
dérive de l'**orthophoto IRC du WMS IGN** (`download_ign_irc_ndvi`,
`service_compute.R`). L'ordre des bandes est correct — j'ai testé les six paires
possibles, `(B1 − B2)/(B1 + B2)` est bien la seule dont la distribution ressemble
à de la végétation. Le problème est la nature de la source : une ortho WMS est
une image **8 bits étirée pour l'affichage** (valeurs 9–247, compression JPEG),
pas de la réflectance calibrée. Le NDVI qu'on en tire n'a pas de sens physique :
33,7 % de pixels négatifs, médiane 0,168, seulement 32,5 % au-dessus de 0,3.

Or le projet dispose déjà de **dizaines de scènes Sentinel-2 L2A en cache**
(`cache/layers/sentinel2/`, série 2017→, utilisées par FORDEAD) — de la vraie
réflectance de surface. **CA-4 : C2 doit être calculé depuis S2 L2A (B8/B4), pas
depuis une ortho d'affichage.** Le cœur expose déjà `read_s2_band_raster()`, que
l'app utilise pour B4/L3.

Côté app, la borne basse du NDVI dérivé a été ramenée de −1 à 0 (v0.125.0.9001)
— un négatif tirait la moyenne de l'unité vers le bas *avant* d'être écrêté en
aval. C'est un correctif de propagation, pas un correctif de source.

**5.b — `c1_biomasse` vaut 0,002 à 2,4 tC/ha** là où une forêt tempérée est à
50–200. Le chemin emprunté est le MNH LiDAR :

```r
agb     <- 2.5 * (pzabove2/100) * pmax(0, zmean)^1.5
biomass <- agb * 0.47
```

Le MNH lui-même est **sain** (`lidar_mnh_mosaic.tif`, 8000 × 10000 à 0,5 m :
médiane 11,97 m, moyenne 12,25 m, 63,4 % au-dessus de 2 m, max 47 m). Mais
*dans les parcelles*, il ne l'est pas : sur la parcelle 1 (25,4 ha, 1 014 936
cellules), **47,1 % des cellules valent exactement 0,00 m** et 60,3 % sont sous
0,5 m, d'où `zmean = 1,04 m` et `pzabove2 = 11 %`. J'ai rejoué la formule à la
main : elle redonne exactement les valeurs du `parquet` (0,14 / 0,06 / 0,02
contre 0,138 / 0,056 / 0,018 stockés). **Le calcul est juste, son entrée ne
l'est pas.**

**Ce n'est pas un artefact — la contradiction avec A1 est apparente.** J'avais
d'abord opposé C1 à `indicateur_a1_couverture`, qui annonce 98,8 à 99,7 % sur
les mêmes polygones. C'est une erreur de lecture de ma part : A1 lit le **FVC
(fraction de couvert végétal, Theia s2_biophysical)**, pas le couvert *arboré*.
Une coupe rase envahie de ronces affiche un FVC proche de 100 % et une hauteur
de canopée nulle. Les deux indicateurs sont compatibles, et pour un projet de
suivi du **dépérissement** c'est même le signal attendu.

Le détail de la distribution le confirme : sur la parcelle 1, les zéros exacts
ne sont pas un plancher appliqué aux négatifs (seulement 0,7 % des cellules
sont entre −0,1 et 0, et deux cellules sous −0,5 : le négatif est donc
**conservé**). Ils forment 1 505 taches, dont une contiguë de **4,86 ha** —
signature d'un sol nu où le MNS égale exactement le MNT, faute de retour
au-dessus du sol. **89 % de la parcelle est sous 2 m.** Ces parcelles sont
réellement rases.

**En revanche, un défaut de modélisation subsiste : les zéros sont comptés deux
fois.** La formule multiplie par `pzabove2/100` *et* élève à la puissance 1,5 un
`zmean` calculé sur **toutes** les cellules, zéros compris. La même surface nue
pénalise donc le résultat deux fois. En calculant `zmean` sur les seules
cellules de canopée (> 2 m), à `pzabove2` inchangé :

| Parcelle | zmean (tout) | zmean (> 2 m) | % > 2 m | C1 actuel | C1 corrigé |
|---|---|---|---|---|---|
| 1 | 1,04 | 6,03 | 11,0 % | 0,138 | **1,91** |
| 2 | 0,80 | 5,79 | 6,6 % | 0,056 | **1,09** |
| 4 | 1,71 | 8,38 | 16,4 % | 0,433 | **4,69** |

Un facteur ~10 à 14. Cela ne « répare » pas C1 — le peuplement reste très
au-dessous d'une forêt sur pied, et c'est normal ici — mais la formule actuelle
n'est pas robuste aux peuplements hétérogènes, où elle écrase le signal des
îlots restants. **CA-5** : `zmean` doit être la hauteur moyenne *de la canopée*,
pas celle de l'unité entière, dès lors que `pzabove2` porte déjà la fraction.

**5.c — `r1_feu` vaut exactement 0,000 sur les 30 UGF** depuis la borne à 30 m
livrée en v0.172.0. **CA-7** : un indicateur déclaré natif 0–100 qui sort
constant à zéro mérite vérification. Trois pistes : `fire_exp()` produit une
exposition réellement nulle sur cette emprise ; le `hazard` rasterisé à 30 m est
vide (`terra::rasterize(bdforet, dem)` sur une grille 60× plus grossière peut
perdre des polygones étroits) ; ou l'anneau de 500 m déborde de l'emprise
utile. À départager avant de conclure que la borne est neutre sur les valeurs —
c'était le CA-4 du brief précédent (`BRIEF-nemeton-r1-feu-resolution.md`), qui
demandait que la borne change le temps, pas le classement.

## 6. Sources de données indisponibles

**`sufosat` — configuration manquante.**
```
load_theia_source("sufosat", aoi)
ERREUR: Datasource "sufosat" has no confirmed STAC collection.
ℹ Set access.stac_collection in the datasource JSON.
```
Conséquence : `indicateur_t3_coupes_rases` est **entièrement NA**, le cache
`cache/layers/sufosat/` reste vide. **CA-6** : déclarer
`access.stac_collection` dans le JSON de datasource, et vérifier que T3 sort des
valeurs sur Fordead.

**`theia_lst` — absence de couverture, comportement attendu.**
```
load_theia_source("theia_lst", aoi)
ERREUR: No "thermocity-lst" STAC item intersects the AOI.
```
ThermoCity est un produit de chaleur **urbaine** ; Fordead est une commune
forestière. `indicateur_a5_rafraichissement` est donc NA à juste titre. **Ne pas
« corriger ».** En revanche, le distinguer d'une panne serait utile : un message
« pas de couverture ThermoCity sur cette emprise » plutôt qu'une erreur.

## 7. Côté `nemetonshiny`

Si **CA-1 option 1** est retenue, **rien à faire** : l'app appelle déjà
`create_family_index()` comme unique point d'entrée et bénéficiera du correctif.

Deux points à traiter ici quoi qu'il arrive, indépendants de ce brief :
- `R/service_compute.R:3793` définit une copie morte de `normalize_indicator()`
  (jamais appelée, duplique la logique métier du cœur) — à supprimer, elle viole
  la règle « aucune logique métier dans l'app ».
- Si **CA-1 option 2** est retenue, l'app devra insérer
  `nemeton::normalize_indicators()` avant `create_family_index()` en
  `mod_synthesis.R:159`, et le brief devra le dire explicitement.

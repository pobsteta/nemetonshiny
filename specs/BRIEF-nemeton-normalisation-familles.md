# BRIEF `nemeton` — les scores de famille agrègent des valeurs brutes

**Dépôt cible** : `/home/pascal/dev/nemeton` (cœur). Session dédiée requise.

**Origine** : diagnostic du 2026-08-16 sur le projet **Fordead** (30 UGF,
`nemeton 0.172.0`, `nemetonshiny 0.125.0`). L'utilisateur constate un score de
famille **C = 0,90 / 100**. Reproduit à l'identique depuis
`data/indicators.parquet`.

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

**5.a — `c2_ndvi` est négatif sur les 30 UGF.** Un NDVI négatif sous couvert
forestier est physiquement impossible. Le `ndvi.tif` en cache (daté du
2026-07-05) contient **33,7 % de pixels négatifs**, médiane 0,168, max 0,728 —
signature d'une scène hivernale, neigeuse ou nuageuse. À tracer : quelle scène
S2 alimente `ndvi.tif`, et selon quel critère elle est choisie.

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

Deux hypothèses à départager côté cœur, je ne les ai pas tranchées :
- le plancher à 0 de `sanitize_chm()` transforme en zéros une zone de non-donnée
  ou de sous-sol, et 47 % d'une parcelle de 25 ha n'est pas un artefact
  ponctuel ;
- ou le dépérissement est réel et ces parcelles sont effectivement rases — mais
  `indicateur_a1_couverture` annonce **98,8 à 99,7 % de couvert forestier** sur
  les mêmes polygones. **A1 et C1 se contredisent** ; l'un des deux ment.

**5.c — `r1_feu` vaut exactement 0,000 sur les 30 UGF** depuis la borne à 30 m
livrée en v0.172.0. Un indicateur déclaré natif 0–100 qui sort constant à zéro
mérite vérification : soit `fire_exp()` produit une exposition nulle sur cette
emprise, soit le `hazard` rasterisé à 30 m est vide.

## 6. Sources de données indisponibles

**`sufosat` — configuration manquante.**
```
load_theia_source("sufosat", aoi)
ERREUR: Datasource "sufosat" has no confirmed STAC collection.
ℹ Set access.stac_collection in the datasource JSON.
```
Conséquence : `indicateur_t3_coupes_rases` est **entièrement NA**, le cache
`cache/layers/sufosat/` reste vide. À corriger dans le JSON de datasource.

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

# Brief nemeton — `resolve_project_chm()` ne voit pas les CHM Open-Canopy que l'app écrit

**Repo cible : `nemeton` (cœur). Session dev dédiée cœur (règle 12 côté app).**
**Émis depuis** `nemetonshiny@5fbe4b0e` (v0.142.2, 2026-08-28).
**Cœur au moment du constat** : `nemeton 0.192.1`.

## Résumé en une phrase

Le candidat `"Open-Canopy CHM"` de `resolve_project_chm()` sonde
`cache/layers/chm/`, alors que l'app dépose ses livrables Open-Canopy dans
`cache/layers/opencanopy/` : sur tout projet dont c'est la seule source de
hauteur, le résolveur renvoie `NULL` et les appelants travaillent **sans modèle
de hauteur, en silence**.

## Symptôme mesuré (projet Couchey, 75 UGF)

```r
nemeton::resolve_project_chm("<projet>", verbose = TRUE)
# i Skip "Open-Canopy CHM": directory does not exist.
# i Skip "LiDAR HD MNH": no matching file in '.../cache/layers/lidar_mnh'.
# i Skip "generic MNH cache": directory does not exist.
# i Skip "cache/layers/chm.tif": no matching file in '.../cache/layers'.
# ... (six autres candidats)
# ! No CHM found anywhere under '<projet>'.
# -> NULL
```

Or le projet **contient** un modèle de hauteur parfaitement exploitable :

| Fichier | CRS | Résolution | Dimensions | Hauteurs |
|---|---|---|---|---|
| `cache/layers/opencanopy/chm_predicted_0_2m.tif` | EPSG:2154 | 0,2 m | 14 695 × 28 481 | 0 → 32,34 m (médiane 1,06) |

## Conséquence côté app (déjà contournée, mais le défaut reste au cœur)

Sur l'onglet *Terrain accessible*, `create_sampling_plan()` tirait **sans strate
de hauteur**, sans rien signaler. L'app v0.142.2 contourne en interrogeant le
cœur puis, s'il rend `NULL`, en sondant elle-même `cache/layers/opencanopy/`
(`.project_chm()` / `.chm_exploitable()` dans `R/service_marculus.R`). Ce
contournement tient ; il pourra être retiré quand le cœur saura résoudre seul.

`service_marculus.R` (segmentation des houppiers) avait déjà rencontré et
contourné le même défaut plus tôt : c'est le **deuxième** appelant à devoir
dupliquer la connaissance d'un chemin que le résolveur devrait porter.

## Où l'app écrit, et pourquoi

`nemetonshiny::download_chm_opencanopy()` (`R/service_compute.R`) crée
`<projet>/cache/layers/opencanopy/` et y laisse :

- `chm_1_5m.tif` — **témoin**, écrit par l'app une fois le pipeline rendu ;
- `chm_predicted_1_5m.tif`, `chm_predicted_0_2m.tif` — livrables du pipeline,
  écrits au fil de l'eau ;
- `chm_vegetation_0_2m.tif` — dérivé.

## ⚠️ Piège : un candidat sans `file` ferait un VRT d'orthophotos

`.probe_raster_candidate()` prend **tous** les `.tif` du répertoire quand
`cand$file` est absent, et `.materialise_raster()` en fait un `terra::vrt()`.
Or `cache/layers/opencanopy/` ne contient pas que des CHM — voici son contenu
réel sur Couchey :

```
chm_1_5m.tif            chm_predicted_0_2m.tif   chm_predicted_1_5m.tif
chm_vegetation_0_2m.tif gndvi.tif                ndvi.tif
ndwi.tif                ortho_irc.tif            ortho_rvb.tif
savi.tif
```

Ajouter `list(label = "Open-Canopy CHM", dir = .../opencanopy)` **sans `file`**
mosaïquerait donc deux orthophotos RVB/IRC et quatre indices spectraux avec les
modèles de hauteur. Il faut **une entrée par nom de fichier**.

## ⚠️ Second point : l'ordre de priorité contredit la doc

La liste `candidates` de `resolve_project_chm()` place aujourd'hui
`"Open-Canopy CHM"` **en premier**, avant `"LiDAR HD MNH"`. La documentation de
l'app affirme l'inverse — « prefers LiDAR HD over Open-Canopy, which is both the
better source and the higher NDP » — et c'est le bon ordre (ADR-007 : LiDAR
local = NDP supérieur). Les nouvelles entrées doivent donc atterrir **après**
`"LiDAR HD MNH"`, et il vaut la peine de vérifier au passage si le rang actuel
de `cache/layers/chm` est délibéré ou hérité.

## Modification demandée

Dans `R/project_layers.R`, fonction `resolve_project_chm()`, liste
`candidates` — insérer **après** `"LiDAR HD MNH"` et `"generic MNH cache"`,
dans cet ordre de préférence (le plus fin d'abord) :

```r
list(label = "Open-Canopy CHM 0,2 m",
     dir   = file.path(project_path, "cache", "layers", "opencanopy"),
     file  = "chm_predicted_0_2m.tif"),
list(label = "Open-Canopy CHM 1,5 m",
     dir   = file.path(project_path, "cache", "layers", "opencanopy"),
     file  = "chm_predicted_1_5m.tif"),
list(label = "Open-Canopy CHM (témoin)",
     dir   = file.path(project_path, "cache", "layers", "opencanopy"),
     file  = "chm_1_5m.tif"),
```

`chm_vegetation_0_2m.tif` est **volontairement exclu** : c'est un dérivé
(végétation masquée), pas le modèle de hauteur de référence.

Le label existant `"Open-Canopy CHM"` pointant sur `cache/layers/chm` est à
conserver — c'est peut-être une convention d'un autre producteur — mais son
libellé gagnerait à ne plus dire « Open-Canopy », puisque ce n'est pas là que le
producteur Open-Canopy écrit. Suggestion : `"cache/layers/chm/"`.

## Tests attendus côté cœur

1. Un projet factice avec `cache/layers/opencanopy/chm_predicted_0_2m.tif`
   → `resolve_project_chm()` le résout, `attr(r, "nemeton_chm_layer")` le nomme.
2. **Le test qui compte** : le même répertoire contenant AUSSI `ortho_rvb.tif` et
   `ndvi.tif` → le résultat porte **une seule couche** et vient du CHM, pas un
   VRT multi-sources. Sans ce test, une régression vers un candidat sans `file`
   passerait inaperçue.
3. Un projet portant à la fois `cache/layers/lidar_mnh/` et
   `cache/layers/opencanopy/` → **LiDAR HD gagne**.

## Ce que l'app fera ensuite

Rien n'est requis côté app : `.project_chm()` interroge déjà le cœur en premier
et ne retombe sur son propre sondage que si le cœur rend `NULL`. Une fois ce
brief livré et le plancher `Imports: nemeton (>= X.Y.Z)` relevé, le repli
applicatif deviendra du code mort et pourra être retiré — ce sera l'occasion de
supprimer aussi celui de `service_marculus.R`.

## Entrée `PLAN.md` à ajouter (journal cœur)

La consigne de release app demande une entrée datée dans le `PLAN.md` du cœur,
que la session app ne peut pas écrire (règle 12) :

```markdown
### 2026-08-28 — App `nemetonshiny` v0.142.2 : chargement projet 2,5× et plan de placettes débloqué

Trois correctifs de performance sur le chemin « clic projet récent → parcelles
à l'écran » (Couchey, 75 UGF / 223 tènements) : **13,2 s → 5,1 s** (médiane de
3 mesures Chrome piloté, dispersion ±0,5 s).

- `ug_build_sf()` était appelée par **sept reactives** dans le même flush
  (`ug_sf_4326`, `units_sf` ×4 via `.resolve_project_aoi_2154`, `ugf_sf_r`,
  rendu carte `mod_ug`), leurs sorties portant `suspendWhenHidden = FALSE`.
  Mémoïsée, clé = hash du couple `(ugs, tenements)`.
- La dissolution faisait un `st_make_valid()` par UGF (75 appels, 695 ms) au
  lieu d'un seul sur les 223 tènements (97 ms) : `ug_build_sf()` passe de
  2988 ms à ~950 ms, à résultat géométriquement identique (75/75 `st_equals`).
- `mod_ug` dessinait sa carte onglet fermé — travail que leaflet jette et que
  le module redessinait déjà à l'ouverture : 2 × 370 ms retirés.

Deux bugs de l'onglet *Terrain accessible* corrigés dans la foulée :
`prep_sampling_raster()` comparait la résolution d'un MNT en EPSG:4326
(0,00025 degré) à `target_res_m = 5` mètres, d'où un facteur d'agrégation de
20 003 et un MNT réduit à **une cellule** — `create_sampling_plan()` échouait sur
« Stratification-valid candidate pool (0) is below n_base », 2108 candidats sur
2108 rejetés. Et le CHM Open-Canopy du projet était invisible pour le résolveur
cœur (cf. brief `BRIEF-nemeton-resolve-chm-opencanopy.md`, **écart ouvert**).
Couchey passe d'une erreur bloquante à 112 placettes stratifiées hauteur ×
topographie.

Suite app : 13 103 PASS, 0 FAIL. Cycle dev : `nemetonshiny@5fbe4b0e`.
```

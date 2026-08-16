# BRIEF `nemeton` — R1 feu : borner la résolution de travail de `fire_exp()`

**Dépôt cible** : `/home/pascal/dev/nemeton` (cœur). Session dédiée requise —
`nemetonshiny` ne peut rien corriger ici, tout le chemin fautif est côté cœur.

**Origine** : diagnostic du 2026-08-16 sur le projet **Fordead**. Le calcul des
31 indicateurs reste bloqué visuellement à 64 % ; le processus n'est pas bloqué,
il calcule `indicateur_r1_feu` depuis plus d'une heure.

---

## 1. Mesures

Processus plafonné (`run_memory_capped`), PID observé pendant ~75 min :

| Signal | Valeur | Lecture |
|--------|--------|---------|
| `utime` sur 21 s | +1076 ticks | **51 % d'un cœur en continu**, état `R` |
| `rchar` / `wchar` sur 21 s | **0 octet** | calcul pur en mémoire, aucune I/O |
| `VmRSS` / `VmHWM` | 2,7 / 3,1 Go | loin du plafond `NEMETON_MEMORY_MAX=10G` |
| `progress_state.json` | figé à `compute:indicateur_r1_feu` | 19/35 faits, R1 en cours |

Ce n'est ni un blocage, ni un problème mémoire : c'est un coût algorithmique.

## 2. Chaîne fautive

```
indicateur_r1_feu(units, dem = get_dem_raster(layers), ...)
  get_dem_raster()      -> prefere `lidar_mnt` au `dem`  ==> mosaique 0,50 m
  .dem_working_res(dem, target_res = .topo_target_res())  ==> 1 m (cf. ci-dessous)
  terra::rasterize(bdforet, dem)                          ==> hazard 20 M cellules
  fireexposuR::fire_exp(hazard, t_dist = 500)             ==> focal annulaire
```

**La résolution de travail est de 1 m, pas des 2 m par défaut du cœur.**
`nemetonshiny` impose `NEMETON_TOPO_TARGET_RES=1` (`APP_CONFIG$topo_target_res`,
posé dans `run_app.R:125-129`) — arbitrage produit documenté, mesuré sur Dabo,
et justifié pour R2/R3 (écart de score R3 vs référence 0,5 m : 0,81 pt à 1 m
contre 1,40 pt à 2 m). Vérifié sur le processus incriminé :
`tr '\0' '\n' < /proc/<pid>/environ` renvoie bien `NEMETON_TOPO_TARGET_RES=1`.

Sur Fordead : mosaïque `lidar_mnt` **8000 × 10000 à 0,50 m**, agrégée par
`.dem_working_res()` (`fact = floor(1 / 0,5) = 2`), soit
**4000 × 5000 = 20 M cellules à 1 m**.

`fire_exp()` construit sa fenêtre avec
`MultiscaleDTM::annulus_window(c(res, t_dist), "map", res)` puis
`terra::focal(haz, wgtwindow, fun = sum)`. **La fenêtre est exprimée en mètres
mais matérialisée en cellules** : à `res = 1 m` et `t_dist = 500 m`, elle fait
**1001 × 1001**, soit ~7,9 × 10⁵ cellules non-NA dans l'anneau. Coût ≈
2 × 10⁷ × 7,9 × 10⁵ ≈ **1,6 × 10¹³ opérations**, mono-thread.

`fireexposuR` est calibré pour du **~30 m** (Landsat). Le coût par cellule varie
en `(2·t_dist / res)²` et le nombre de cellules en `1/res²`, donc le coût total
varie en **`1/res⁴`** :

| Résolution | Fenêtre | Cellules | Coût relatif |
|-----------|---------|----------|--------------|
| 30 m | 33 × 33 ≈ 1 100 | ~22 000 | **1×** |
| 2 m | 501 × 501 ≈ 251 000 | 5 000 000 | ~52 000× |
| **1 m (réel)** | **1001 × 1001 ≈ 790 000** | **20 000 000** | **~660 000×** |

**Mesure de contrôle** : un `terra::focal()` avec une fenêtre annulaire de
`t_dist = 500` sur un raster de **400 × 400 à 2 m** — soit 1/31 du cas 2 m, et
~1/1900 du cas réel à 1 m — **n'a pas rendu la main en 300 s**. Extrapolé, R1
sur Fordead demande de l'ordre de **plusieurs dizaines d'heures de CPU**. Ce
n'est pas un calcul lent, c'est un calcul qui ne rendra pas la main.

## 3. Correction demandée

Borner la résolution de travail de R1 à **30 m**, indépendamment de
`.topo_target_res()` (qui vaut 2 m et convient aux indicateurs topographiques —
pente, TWI — mais pas à un noyau de 500 m de portée).

Deux points d'attention :

1. **Ne pas se contenter de changer `.topo_target_res()`** : il sert aussi à R2,
   R3, W2, W3, F2, S1 et S2, qui ont de bonnes raisons de travailler à 1 m sur
   un MNT LiDAR — l'app l'a mesuré et l'assume (`APP_CONFIG$topo_target_res`).
   Remonter ce réglage global pour sauver R1 dégraderait sept indicateurs
   au profit d'un seul. La borne doit être **spécifique au chemin `fireexposuR`**.
   C'est la nature de R1 qui diffère : les sept autres lisent la topographie
   *sous* l'unité, R1 convolue un voisinage de 500 m — un rayon métier, pas une
   finesse de terrain.
2. **Le repli** (`slope + species + climate`) n'a pas ce problème : son coût est
   celui d'un `terrain()` + `safe_extract()`, linéaire en cellules. Si la borne
   n'est appliquée qu'au chemin `fire_exp`, le repli peut rester au
   `dem_target_res` courant.

Proposition de signature :

```r
indicateur_r1_feu(units, dem = NULL, layers = NULL, bdforet = NULL,
                  species_field = "species", climate = NULL,
                  weights = c(slope = 1/3, species = 1/3, climate = 1/3),
                  dem_target_res = .topo_target_res(),
                  fire_exp_res = 30)   # <- nouveau, borne du chemin fireexposuR
```

et, dans le corps, avant `terra::rasterize()` :

```r
hazard_dem <- .dem_working_res(dem, target_res = max(fire_exp_res,
                                                     terra::res(dem)[1]),
                               context = "R1/fire_exp")
```

Le `max()` évite de **sur-échantillonner** un MNT déjà plus grossier que 30 m
(cas NDP 0 sans LiDAR : le `dem` public est souvent à 25 m — il ne faut pas le
ramener artificiellement à 30 m si ce n'est pas nécessaire, mais surtout ne
jamais l'affiner).

## 4. Critères d'acceptation

- **CA-1** : sur une emprise dotée d'un `lidar_mnt` 0,50 m, `indicateur_r1_feu`
  se termine en **< 60 s** (contre > 75 min aujourd'hui).
- **CA-2** : le raster de `hazard` passé à `fire_exp()` a une résolution
  **≥ 30 m**, vérifié par un test unitaire sur `terra::res()`.
- **CA-3** : un `dem` déjà à 25 m n'est **pas** ré-agrégé à 30 m (le `max()`
  protège), et un `dem` à 100 m n'est jamais affiné.
- **CA-4** : les valeurs R1 restent dans `[0, 100]` et la corrélation avec le
  résultat 2 m sur une petite emprise de référence reste élevée — la borne doit
  changer le **temps**, pas le **classement** des unités.
- **CA-5** : le `cli::cli_alert_info()` de `.dem_working_res()` dit
  explicitement le contexte `R1/fire_exp`, pour que le prochain diagnostic lise
  la résolution effective dans les logs.

## 5. Côté `nemetonshiny`

**Rien à faire** tant que la signature reste compatible : l'app appelle le
dispatcher du cœur, elle ne passe pas `dem_target_res`. Si le cœur choisit
d'exposer `fire_exp_res` comme option utilisateur plutôt que comme défaut,
prévenir — l'app devra alors ajouter l'entrée d'UI et la clé i18n.

Après release cœur, bumper ici `Imports: nemeton (>= X.Y.Z)` uniquement si
l'app doit exiger cette version comme minimum strict.

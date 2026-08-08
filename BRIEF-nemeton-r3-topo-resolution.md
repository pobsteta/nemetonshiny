# BRIEF cœur nemeton — indicateur_r3_secheresse() : explosion mémoire sur MNT LiDAR HD (OOM kill)

> Hand-off depuis la session de dev `nemetonshiny`. **À traiter dans une
> session de dev dédiée sur `/home/pascal/dev/nemeton`** (règle 12 : pas
> de modif cœur depuis la session app). Repo concerné :
> `pobsteta/nemeton` (cœur métier). Version observée : `nemeton 0.168.1`
> (release), `nemetonshiny 0.121.0`, `terra 1.9.34`. Un correctif app
> complémentaire est prévu de notre côté (plafond de résolution à la
> promotion `dem`) — voir §7. Les deux sont utiles ; celui-ci est le
> correctif de fond.

## 1. Symptôme

Le calcul des 31 indicateurs sur le projet **DABO**
(`20260801_130303_xpdk`, Vosges, 4 UGF, NDP 1, `chm_source = lidar_hd`)
tue la session RStudio.

Ce n’est pas un crash R : c’est **`systemd-oomd` qui tue tout le scope
RStudio** sur pression mémoire du user slice.

    07:25:42  début du calcul
    07:28:58  systemd-oomd: Killed .../app-gnome-rstudio-22671.scope due to memory
              pressure for user@1000.service being 64.51% > 50.00% for > 20s
    07:29:30  dernière écriture de progress_state.json
    07:35:18  systemd-oomd: Killed .../app-rstudio-22671.scope  (7 process)

Brave et ghostty sont tués dans la même fenêtre (07:29, 07:31) —
signature d’une pression mémoire globale, pas d’un défaut applicatif
isolé.

`data/progress_state.json` s’arrête **pile sur
`indicateur_r3_secheresse`** : 21/35 indicateurs `completed`,
`current_task: "compute:indicateur_r3_secheresse"`, projet resté bloqué
en `status: "computing"`.

Les autres indicateurs terrain passent : W3, R1, R2 sont tous
`completed`. **R3 est le seul à mourir.** Voir §4 pour la raison.

## 2. Reproduction minimale

Le MNT LiDAR HD mosaïqué de DABO :

    cache/layers/lidar_mnt_mosaic.tif : 12000 x 10000 px @ 0.5 m = 1.2e+08 px, Float32

Rejeu de la chaîne topographique de R3 sur ce raster réel, dans un
cgroup borné à 10 Go
(`systemd-run --user --scope -p MemoryMax=10G -p MemorySwapMax=0`), pic
RSS via `/proc/self/status:VmHWM` :

| étape R3 | pic RSS @ 0,5 m (actuel) | pic RSS @ 5 m (illustration) |
|----|----|----|
| baseline | 0,20 Go | 0,66 Go |
| `terrain(aspect)` | 2,44 Go | 0,66 Go |
| `terrain(slope)` | 3,33 Go | 0,66 Go |
| `aspect_risk` ([`cos()`](https://rdrr.io/r/base/Trig.html)) | **8,70 Go** | 0,66 Go |
| `pente_risk` (`clamp`) | 9,59 Go | 0,66 Go |
| `resample(twi → aspect)` | 9,59 Go | 0,66 Go |
| `topo_risk` | ☠️ **SIGKILL (exit 137)** | 0,66 Go |
| durée | — | **0,5 s** |

La colonne 5 m illustre l’effet d’un plafond ; **le réglage retenu est 1
m** (3,49 Go de pic, 13,7 s — cf. §3).

Le saut de 3,33 → 8,70 Go vient de
`aspect_risk <- (1 + cos((aspect - 180) * pi/180))/2` : quatre
`SpatRaster` temporaires de 120 M pixels dans une seule expression.

`nemeton:::.onLoad` pose `terraOptions(memfrac = ...)` (0.5 sur ce
poste). Sur une machine à 31 Go, terra s’autorise donc ~15,5 Go **dans
un seul process** et ne bascule jamais sur disque avant qu’oomd (seuil
50 % de pression sur le user slice) n’ait tué la session. Le garde-fou
terra est là, mais il est calibré pour un process seul, pas pour un
process qui cohabite avec RStudio + un navigateur.

## 3. Résolution retenue : `topo_res = 1 m`

**Décision : plafond à 1 m** (le MNT IGN livré est à 0,5 m — c’est donc
une division par 4 des pixels, pas un simple garde-fou théorique).

Ce que la précision fine achète réellement est borné par la façon dont
le raster est consommé :

``` r

r3_mean <- safe_extract(r3_raster, as_pure_sf(units), fun = "mean", progress = FALSE)
```

— une **moyenne par polygone UGF**. Toute la structure sub-métrique est
moyennée avant d’entrer dans le score.

Mesures sur géométrie réelle, **grille TWI alignée sur la grille du
terrain à chaque résolution** (`twi_target_res = topo_res`, donc aucun
`resample`), écart exprimé en points de score R3 /100 par rapport à la
référence 0,5 m :

| résolution | DABO (110–420 ha) | ForetAccess (0,14–3,7 ha) | temps R3 (DABO) | spill disque |
|----|----|----|----|----|
| 0,5 m (référence) | — | — | 265 s | — |
| **1 m (retenu)** | **0,81** (max 0,96) | **0,64** (max 1,03) | **32,9 s** | 329 Mo |
| 2 m | 1,40 (max 1,64) | 1,12 (max 2,02) | 10,3 s | 0 Mo |
| 5 m | 2,33 (max 2,66) | 2,10 (max 4,18) | 3,5 s | 0 Mo |

La dégradation est **monotone** : plus la grille est fine, plus on colle
à la référence. Le 1 m divise le temps par 8 par rapport au 0,5 m tout
en restant à moins d’un point de score.

### Deux réserves à assumer explicitement

**a) Le 1 m fait spiller terra — `memmax` n’est donc pas optionnel.** À
1 m, le pic mesuré est de 3,49 Go avec 329 Mo écrits en temporaires, là
où le 2 m tient entièrement en RAM (0 Mo). Le choix du 1 m **rend le
§5ter obligatoire, pas complémentaire** : sans plafond mémoire absolu,
une AOI sensiblement plus grande que DABO (3 000 ha) ramène le risque
d’OOM. Les deux correctifs forment un tout.

**b) « Plus proche du 0,5 m » n’est pas « plus juste ».** Le tableau
mesure l’écart à la référence fine en supposant que le fin est le vrai.
Sur un MNT LiDAR en forêt, le 1 m capte les cloisonnements
d’exploitation, les chablis et les fossés — de la micro-topographie qui
n’est pas l’exposition du peuplement à la sécheresse. Cet argument
physique joue *contre* le 1 m. Le 1 m est retenu comme choix de prudence
(on ne dégrade la donnée qu’au strict nécessaire pour la tenue mémoire),
pas comme optimum démontré. Un recalibrage ultérieur vers 2 m resterait
défendable.

### Correction d’une mesure antérieure de ce brief

Une version précédente annonçait le 5 m à 0,50 pt de la référence et une
dégradation **non monotone** (5 m réputé plus fidèle que 10 m). C’était
un artefact de protocole : la référence 0,5 m y utilisait le TWI à 10 m
*upsamplé*, ce qui favorisait mécaniquement les réglages proches de 10
m. Avec une référence cohérente (TWI recalculé à chaque résolution),
l’ordre est monotone et le 5 m est à 2,33 pt. **Les chiffres du tableau
ci-dessus font foi.**

### La contrainte structurante, elle, est inchangée

**`twi_target_res` doit suivre `topo_res`, pas rester figé à 10.** C’est
le point le plus important de ce brief : il ne s’agit pas seulement de «
baisser la résolution », mais de **faire coïncider les deux grilles**.
Un patch qui plafonnerait le terrain sans propager la résolution au TWI
laisserait en place un `resample` d’un TWI grossier vers une grille fine
— coûteux, et qui n’ajoute aucune information.

Mesure de l’effet de ce seul désalignement, à `topo_res = 5 m` : un TWI
resté à 10 m puis upsamplé écarte le score de **1,36 pt** contre **0,50
pt** avec les grilles alignées, soit un facteur 2,7 d’erreur ajoutée
gratuitement.

## 4. Analyse — pourquoi R3 et pas W3/R1/R2

Le cœur **a déjà le bon réflexe**, et R3 l’annule.

`get_or_compute_twi()` protège explicitement le calcul lourd :

``` r
get_or_compute_twi <- function(dem, cache_dir = NULL, twi_target_res = 10)
# -> calculate_twi_terra(dem, target_res = twi_target_res)   # ou _grass
```

Le cache TWI de DABO est bien à 10 m (`twi_1c42a9144d1c.tif`, 600 × 500
px). C’est pour ça que **W3 passe** : il consomme `get_or_compute_twi()`
et reste à 10 m.

Puis `indicateur_r3_secheresse()` jette la protection :

``` r

aspect <- terra::terrain(dem, v = "aspect", unit = "degrees")   # 0.5 m -> 120 M px
pente  <- terra::terrain(dem, v = "slope",  unit = "degrees")   # 0.5 m -> 120 M px
aspect_risk <- (1 + cos((aspect - 180) * pi/180))/2             # 4 temporaires 120 M px
pente_risk  <- terra::clamp(pente/30, lower = 0, upper = 1)

twi_raster <- get_or_compute_twi(dem, cache_dir = twi_cache_dir)   # 10 m, OK
if (!terra::compareGeom(twi_raster, aspect, stopOnError = FALSE)) {
  twi_raster <- terra::resample(twi_raster, aspect, method = "bilinear")  # <-- 10 m REMONTÉ à 0.5 m
}
...
topo_risk <- 0.4 * aspect_risk + 0.3 * pente_risk + 0.3 * twi_risk
```

Le `resample` aligne le TWI **sur la grille pleine résolution** au lieu
d’aligner l’analyse terrain sur la grille TWI. Il n’ajoute aucune
information (le TWI *est* à 10 m) et multiplie par 400 le coût de tout
ce qui suit.

Comparaison de charge entre les indicateurs terrain, qui explique
exactement qui survit :

| indicateur | rasters pleins créés | verdict sur DABO |
|----|----|----|
| W3 | 0 (via `get_or_compute_twi`, 10 m) | ✅ completed |
| R1 | 1 (`terrain(slope)`) | ✅ completed |
| R2 | 3 (`terrain` aspect/slope/TRI) | ✅ completed |
| **R3** | **~8** (aspect, slope, 4 temp. `cos`, clamp, TWI upsamplé, somme pondérée) | ☠️ **OOM** |

R1 et R2 ne meurent pas aujourd’hui, mais ils travaillent eux aussi à
0,5 m sans plafond : ils sont sur la même pente, avec moins de marge
qu’il n’y paraît (R2 crée déjà 3 rasters de 120 M px).

## 5. Correctif demandé

**Principe : une résolution de travail topographique unique, et on
n’upsample jamais.**

Proposition d’API (à ajuster selon les conventions du cœur) :

1.  Un helper interne partagé, p. ex. :

    ``` r

    .terrain_working_dem <- function(dem, target_res = 1) {
      r <- terra::res(dem)[1]
      if (is.na(r) || r >= target_res) return(dem)        # BD ALTI 25 m : inchangé
      terra::aggregate(dem, fact = max(1, round(target_res / r)), fun = "mean")
    }
    ```

    Le garde `r >= target_res` est important : sur un projet BD ALTI 25
    m, rien ne change.

2.  `indicateur_r3_secheresse()` gagne un argument **`topo_res = 1`**,
    applique `.terrain_working_dem()` **une fois** en tête de la section
    topographique, et calcule `aspect` / `pente` sur cette grille.

3.  **`topo_res` doit être propagé à `get_or_compute_twi()`** :

    ``` r

    twi_raster <- get_or_compute_twi(dem_work, cache_dir = twi_cache_dir,
                                     twi_target_res = topo_res)
    ```

    C’est la clé du §3bis. Les deux grilles coïncident alors,
    `compareGeom()` passe, et le `resample` disparaît. **Ne pas laisser
    `twi_target_res` à sa valeur par défaut de 10** : le patch serait
    alors moins fidèle que l’état actuel.

    Effet de bord à assumer : la clé de cache TWI intègre déjà
    `twi_target_res`, donc les caches 10 m existants deviennent
    orphelins et seront recalculés une fois (0,5 s — négligeable).
    Penser à ce que les anciens fichiers ne s’accumulent pas
    indéfiniment, ou l’assumer explicitement.

4.  Même traitement pour `indicateur_r1_feu()`,
    `indicateur_r2_tempete()` et le calcul `risk_erosion`, pour fermer
    la classe de bugs plutôt que le seul cas qui a explosé. Attention :
    **W3 (`indicateur_w3_humidite`) consomme aussi
    `get_or_compute_twi()`** — si son `twi_target_res` reste à 10
    pendant que R3 passe à 5, les deux indicateurs ne partagent plus le
    même cache ni la même grille. Aligner W3 sur la même valeur.

5.  Idéalement, une option paquet
    (`getOption("nemeton.topo_target_res", 1)`) pour que l’app puisse
    ajuster sans nouvelle release cœur.

Point d’attention rédactionnel :
`aspect_risk <- (1 + cos((aspect - 180) * pi/180))/2` reste coûteux en
temporaires même à 10 m. Un
[`terra::app()`](https://rspatial.github.io/terra/reference/app.html) en
une passe serait plus propre, mais à 10 m ce n’est plus un problème de
mémoire — à traiter seulement si vous le jugez utile.

## 5ter. Second correctif cœur : remplacer `memfrac` par un plafond absolu dans `.onLoad`

**Obligatoire, pas optionnel** — la résolution retenue (1 m) fait
spiller terra (329 Mo de temporaires, 3,49 Go de pic sur une AOI de 3
000 ha). Sans plafond mémoire absolu, une AOI plus grande ramène le
risque d’OOM que ce brief cherche à éliminer. Les §5 et §5ter forment un
tout : livrer le plafond de résolution seul laisserait le bug
atteignable.

`nemeton:::.onLoad` pose aujourd’hui `terraOptions(memfrac = ...)` (0,5
sur le poste de test). Sur une machine à 31 Go, terra s’autorise donc
**15,5 Go dans un seul process**, ce qui est calibré pour un process
dédié, pas pour une session R qui cohabite avec RStudio et un navigateur
— `systemd-oomd` tue à 50 % de pression sur le user slice bien avant que
terra ne songe à spiller.

Deux défauts de `memfrac` :

1.  **C’est une fraction**, donc le plafond effectif varie du simple au
    décuple selon la machine : 0,15 donne 4,6 Go sur ce poste, 1,2 Go
    sur un portable à 8 Go, 19 Go sur un serveur à 128 Go. Un
    comportement non reproductible d’un poste à l’autre.
2.  Elle ne dit rien de ce que la machine peut *réellement* céder à un
    seul process.

Comparaison des quatre configurations, chaîne R3 complète, cgroup borné
à 10 Go :

| configuration | @ 5 m : RSS / temps / disque | @ 0,5 m : RSS / temps / disque |
|----|----|----|
| actuelle (`memfrac = 0.5`) | 0,67 Go / 0,7 s / 0 Mo | ☠️ **OOM (exit 137)** |
| `todisk = TRUE` | 0,66 Go / **2,4 s** / **142 Mo** | 2,51 Go / 141,7 s / 8,6 Go |
| `memfrac = 0.15` | 0,67 Go / 0,7 s / 0 Mo | 0,97 Go / **163,7 s** / 8,5 Go |
| **`memmax = 3`** | **0,67 Go / 0,8 s / 0 Mo** | **0,88 Go / 142,4 s / 8,5 Go** |

Lecture :

- **`todisk = TRUE` est le mauvais levier.** Il est inconditionnel : au
  point de fonctionnement normal (5 m) il coûte 3,4× le temps et 142 Mo
  d’écritures pour **0,01 Go de mémoire économisée**. Il écrit sur
  disque même les rasters minuscules pour lesquels la RAM est évidemment
  le bon choix.
- **`memmax` est adaptatif** : terra ne spille que lorsqu’un raster
  dépasse le plafond. À 5 m il est donc strictement gratuit (identique à
  l’existant, 0 Mo écrit), et à 0,5 m il protège mieux que `todisk`
  (0,88 Go contre 2,51 Go) pour le même temps.
- `memmax` étant un **plafond absolu en Go**, il se comporte
  identiquement sur toutes les machines.

**Demande : dans `.onLoad`, poser un `memmax` par défaut (ordre de
grandeur 3–4 Go), surchargeable par option/variable d’environnement.**
Cela transforme un OOM kill en dégradation gracieuse (calcul plus lent
sur disque) pour *tous* les appelants de terra dans le cœur, pas
seulement R3 — et rend le plafond de résolution non critique en cas de
contournement.

Note : vérifier que le `tempdir` de terra n’est pas sur un `tmpfs`. Sur
un système où `/tmp` est monté en tmpfs, spiller « sur disque » écrit en
RAM et n’apporte **aucune** protection. (Sur le poste de test, `/tmp`
est sur ext4 — OK.)

## 5bis. Alternatives étudiées, et pourquoi elles ne remplacent pas le plafond

Deux pistes ont été évaluées avant de retenir le plafond de résolution.
Elles sont documentées ici parce qu’elles se posent naturellement et
méritent une réponse mesurée plutôt qu’un avis.

### A. Adapter la résolution à la surface des UGF — **écarté**

Trois objections, par ordre de gravité :

1.  **Cela casse la comparabilité inter-projets.** Le biais de
    résolution atteint 5,4 points (§3bis). Or `compute_general_index()`,
    le radar et le NDP supposent des scores comparables d’un projet à
    l’autre. Une méthodologie qui varie par projet injecte un biais non
    attribuable dans la grandeur même qu’on compare.
2.  **Le paramètre contraignant est la surface *minimale*, pas
    maximale.** Une seule petite parcelle suffit à imposer la résolution
    fine à tout le projet. Sur les projets réels : ForetAccess (min 0,14
    ha), Reconfort (min 0,48 ha) et Fordead (min 2,11 ha) tomberaient
    tous sur la résolution fine ; seul DABO (min 109 ha) basculerait en
    grossier. **L’adaptativité ne changerait rien pour 3 projets sur
    4.**
3.  **Cela ne borne pas la mémoire.** Le coût est en
    `étendue AOI / résolution²`, et la taille des parcelles ne contrôle
    pas l’étendue. Un projet à petit parcellaire dispersé sur une grande
    AOI resterait exposé.

Si l’adaptativité est malgré tout retenue un jour, elle doit porter sur
la surface **minimale** et la résolution effective doit être **inscrite
dans les métadonnées du projet**, pour qu’un score porte la provenance
de sa méthode.

### B. Découper les calculs en dalles avec buffer — **valide pour le terrain, piège pour le TWI**

Mesuré sur le MNT de DABO à 5 m, dalles 2×2, erreur contre le calcul en
une passe :

| opération | buffer | err. moyenne | err. max | % de l’amplitude |
|----|----|----|----|----|
| `terrain(slope)` — locale 3×3 | 50 m | 0,00000 | 0,00000 | **0 %** |
| `calculate_twi_terra()` — accumulation de flux | 50 m | 0,00803 | 4,31415 | **29,1 %** |
| idem | 250 m | 0,00076 | 1,53607 | 10,3 % |
| idem | 1000 m | 0,00000 | 0,00358 | 0,0 % |

Lecture :

- **Les opérations locales se dallent exactement.** `terrain()` est un
  focal 3×3 ; un buffer de quelques pixels suffit et l’erreur est nulle.
  (Le test à buffer nul donne aussi 0, mais c’est un artefact : les
  bords deviennent `NA` et sortent des statistiques. Seul le résultat à
  50 m est concluant.)
- **Le TWI ne se dalle pas naïvement.** L’aire contributive amont d’un
  pixel peut s’étendre bien au-delà de la dalle ; à 50 m de buffer
  l’erreur locale atteint **29 % de l’amplitude du TWI**. Il faut ~1 km
  de buffer pour converger — et sur une AOI de 6 × 5 km en 2×2 dalles,
  des dalles élargies de 1 km se recouvrent au point que l’économie
  disparaît.

**Surtout : le dallage des opérations locales est déjà fourni par
`terra`.** Son traitement par blocs gère nativement les rasters plus
grands que la RAM — la chaîne complète à 0,5 m passe en 113 s sous
`terraOptions(todisk = TRUE)`, sans OOM. Le plantage vient de `memfrac`,
qui autorise terra à tout garder en mémoire, pas d’une incapacité à
streamer. Réimplémenter un dallage à la main pour les opérations locales
dupliquerait ce que terra fait déjà, tout en ajoutant le risque de
l’appliquer par erreur au TWI.

### C. Piste complémentaire réellement utile : découper sur les parcelles, pas sur des dalles

La fraction de l’AOI effectivement exploitée est faible, puisque seuls
les polygones UGF sont extraits *in fine* :

| projet      | AOI      | parcelles       | parcelles + buffer 50 m |
|-------------|----------|-----------------|-------------------------|
| DABO        | 3 000 ha | 774 ha (25,8 %) | 873 ha (**29,1 %**)     |
| Reconfort   | 2 500 ha | 554 ha (22,2 %) | 638 ha (**25,5 %**)     |
| ForetAccess | 400 ha   | 31 ha (7,8 %)   | 47 ha (**11,7 %**)      |

Un `crop`/`mask` sur l’union des parcelles bufferisées économise **3,4×
à 8,5×**, indépendamment du choix de résolution, et sans aucun risque
sur les opérations locales. Ce n’est pas suffisant seul (DABO à 0,5 m
resterait à ~35 M px), mais c’est un second levier propre, cumulable
avec le plafond. Réserve : le TWI doit rester calculé sur une étendue
hydrologiquement cohérente — le masquer aux parcelles fausserait
l’accumulation de flux exactement comme le dallage.

## 6. Tests attendus

- Non-régression basse résolution : DEM synthétique à 25 m →
  `.terrain_working_dem()` retourne le raster **inchangé** (identité),
  et R3 rend le même score qu’avant le patch.
- Plafonnement haute résolution : DEM synthétique à 0,5 m → la grille de
  travail est bien à `topo_res = 1`, et `ncell()` est divisé par 4.
- Équivalence numérique : sur un DEM à 0,5 m,
  `|R3(topo_res = 1) - R3(topo_res = 0.5)| < 1` point /100 (marge
  mesurée : 0,96 max sur DABO, 1,03 sur ForetAccess — cf. §3).
- Tenue mémoire à 1 m : le pic reste borné par `memmax` (mesuré 3,49 Go
  avec 329 Mo de temporaires sur une AOI de 3 000 ha). Ce test n’a de
  sens qu’avec le correctif §5ter appliqué.
- **Alignement des grilles (le test qui compte)** : sur un DEM à 0,5 m,
  vérifier que `terra::compareGeom(twi_raster, aspect)` est **TRUE**
  après le patch, donc que la branche `resample` n’est jamais empruntée.
  Un mock sur
  [`terra::resample`](https://rspatial.github.io/terra/reference/resample.html)
  qui [`stop()`](https://rdrr.io/r/base/stop.html) est le moyen le plus
  direct de rendre ce test non-vacant.
- Cohérence W3/R3 : les deux doivent produire un TWI à la même
  résolution et partager la même entrée de cache.

## 7. Ce que fait l’app en attendant

`nemetonshiny` promeut le MNT LiDAR HD dans le slot `dem` sans plafond,
dans `R/service_compute.R:1672-1677` :

``` r

if (!is.null(rasters$lidar_mnt) && inherits(rasters$lidar_mnt, "SpatRaster")) {
  cli::cli_alert_success("Using LiDAR HD MNT (1 m) for terrain indicators instead of BD ALTI 25 m")
  rasters$dem <- rasters$lidar_mnt
}
```

Deux remarques :

- Le message annonce **1 m** ; la mosaïque IGN livrée est à **0,5 m**,
  soit 4× plus de pixels que ce que le commentaire suppose. (Bug de
  commentaire côté app, on le corrige chez nous.)
- Nous prévoyons de plafonner la résolution **à la promotion**, ce qui
  protège aussi les versions cœur déjà déployées.

Ces deux correctifs sont complémentaires et non exclusifs : le plafond
app protège l’existant, le plafond cœur protège tous les appelants (y
compris les usages hors app, scripts, tests). **Aucun des deux ne rend
l’autre inutile.**

Compatibilité : le correctif cœur ne change pas de signature de manière
cassante (ajout d’arguments avec défaut). Il **modifie légèrement les
scores R3** des projets à MNT LiDAR (0,96 point /100 au maximum sur
DABO, 1,03 sur ForetAccess, mesuré) et **invalide les caches TWI à 10
m** (recalcul unique, 0,5 s) — les deux à signaler dans `NEWS.md`.

Nous plafonnerons côté app à la **même valeur de 1 m**, pour que les
deux couches restent cohérentes tant que la release cœur n’est pas là.

## 8. Références

- Projet de repro :
  `~/.local/share/nemeton/projects/20260801_130303_xpdk` (DABO)
- MNT : `cache/layers/lidar_mnt_mosaic.tif` (12000 × 10000 @ 0,5 m)
- Cache TWI : `cache/layers/twi_1c42a9144d1c.tif` (600 × 500 @ 10 m)
- État bloqué : `data/progress_state.json` (`status: "computing"`,
  `current_task: "compute:indicateur_r3_secheresse"`)
- Logs : `journalctl --user --since "2026-08-07" | grep oomd`

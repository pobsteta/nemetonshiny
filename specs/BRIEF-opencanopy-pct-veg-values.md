# BRIEF `opencanopynemeton` — `pct_veg` charge deux fois le raster entier (OOM)

> **Statut** : ouvert, 2026-08-23.
> **Dépôt concerné** : `opencanopynemeton` uniquement.
> **Fichier** : `R/pipeline_aoi_to_chm.R`, lignes 1827-1828.
> **Nature** : correctif mémoire, deux lignes. Aucun changement de résultat.
> **Sévérité** : bloquant — tue le pipeline à sa **dernière** étape, après avoir
> écrit tous ses livrables.

---

## L'incident

Calcul des 31 indicateurs sur **Couchey** (23 parcelles, 535,6 ha), lancé depuis
`nemetonshiny` le 2026-08-22 à 20:35. Le processus enfant plafonné à 10 Go a été
tué par l'OOM killer à 00:12, après **3 h 20 de CPU**, avec **11 Go de cache
écrits** et **zéro indicateur calculé** (`progress_state.json` :
`current_task: "chm_phase:export"`, les 35 indicateurs à `pending`).

Journal système, à la minute près :

```
Aug 23 00:12:16  run-r11dc2ffb…scope: A process of this unit has been killed by the OOM killer.
Aug 23 00:12:17  run-r11dc2ffb…scope: Failed with result 'oom-kill'.
Aug 23 00:12:17  run-r11dc2ffb…scope: Consumed 3h 20min 50.114s CPU time.
```

Horodatage des livrables, qui désigne l'étape fautive sans ambiguïté :

| Fichier | Taille | Heure |
|---|---|---|
| `ortho_rvb.tif` / `ortho_irc.tif` | 1,94 / 2,13 Go | 21:11 / 21:13 |
| `chm_predicted_1_5m.tif` | 6,2 Mo | 23:42 |
| `chm_predicted_0_2m.tif` | 700 Mo | 23:44 |
| `ndvi` / `gndvi` / `savi` / `ndwi` | ≈ 1,55 Go chacun | 23:49 → 00:07 |
| **`chm_vegetation_0_2m.tif`** | 340 Mo | **00:12** ← le kill |

## La cause

```r
# R/pipeline_aoi_to_chm.R:1827-1828
pct_veg <- sum(values(clean_mask, na.rm = TRUE)) /
           sum(!is.na(values(clean_mask))) * 100
```

`gdalinfo` sur les rasters du run : **28 481 × 14 695 = 418 528 295 cellules**
(0,20 m).

`values()` rapatrie la couche **entière** en mémoire — et la ligne l'appelle
**deux fois**, sur deux expressions distinctes, plus la copie temporaire du
`!is.na()`. Soit **≈ 4,7 Go de vecteurs R** pour calculer un pourcentage, par-
dessus les deux `resample()` en mémoire des lignes 1817-1818 (3,1 Go chacun s'ils
ne débordent pas sur disque) et le `mask()` qui vient de s'exécuter.

`mask()` (ligne 1824) écrit pourtant **avec `filename=`** : il streame, il ne
garde rien. Le fichier de 340 Mo est bien sur le disque. C'est la ligne
*suivante*, celle qui ne sert qu'à un `message()`, qui emporte le processus.

## Le correctif

```r
pct_veg <- as.numeric(global(clean_mask, "mean", na.rm = TRUE)) * 100
```

`global()` streame par blocs et ne matérialise rien. Sur un masque logique, la
moyenne des `TRUE`/`FALSE` en ignorant les `NA` **est** la proportion cherchée —
c'est exactement `sum(TRUE) / sum(!is.na())`, à l'identique et non à peu près.

Si la formulation explicite est préférée, `freq(clean_mask)` donne le décompte
par valeur en un seul passage, également sans charger la couche.

## Ce qu'il faut vérifier en corrigeant

**Le même motif ailleurs.** `values(` apparaît probablement à d'autres endroits
du pipeline sur des rasters pleine résolution. Un `grep -n "values(" R/` avant
de refermer : ce qui a tué ce run à 0,20 m tuera le suivant à la même résolution.

**L'ordre des opérations.** Ce pourcentage est purement informatif
(`message()`). S'il devait rester coûteux, il vaudrait mieux le calculer sur le
fichier déjà écrit — ou pas du tout — plutôt qu'entre `mask()` et le retour de
la fonction, au pic de mémoire du pipeline.

## Vérification

| Contrôle | Attendu |
|---|---|
| Rejouer Couchey (cache présent, orthos et indices réutilisés) | passe l'étape `chm_vegetation` sous le plafond de 10 Go |
| `pct_veg` affiché | identique à la valeur qu'aurait donnée l'ancienne formule |
| Pic mémoire de l'étape | de l'ordre du bloc terra, plus du raster entier |

## Contexte côté app

Aucun changement applicatif n'est requis ni possible : `nemetonshiny` ne fait
qu'appeler le pipeline. Deux choses ont malgré tout été faites côté app en
v0.133.1, pour que l'incident soit *lisible* la prochaine fois :

- le message d'échec ne dit plus « exit -15 » mais nomme le plafond mémoire et
  le remède (`.compute_error_message()`) ;
- le plafond lui-même n'est plus décidé par l'app (brief
  `2026-08-22-plafond-memoire.md`, cœur `nemeton 0.183.0`).

Un brief frère part vers `nemeton` :
`specs/BRIEF-nemeton-oom-sigterm-scope.md`, sur la raison pour laquelle un OOM
sous cgroup se présente en `-15` et non en `-9`.

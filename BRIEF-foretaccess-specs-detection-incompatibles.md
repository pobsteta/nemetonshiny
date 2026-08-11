# BRIEF cœur — `detecter_desserte(specs =)` n'accepte pas ce que `dsr_calibrer_specs()` produit

> Hand-off depuis la session de dev `nemetonshiny`.
> **Concerne deux repos** : `pobsteta/foretaccess` et `pobsteta/dessertR`. L'écart est à leur frontière ; à vous de décider lequel bouge.
> Versions : `foretaccess 2.0.1`, `dessertR 1.3.0`, `nemetonshiny 0.121.15`.
> Mesuré sur le projet ForetAccess (`20260717_101641_wsfi`), 30 parcelles / 31 ha.

## 1. Le symptôme

`detecter_desserte()` rend **0 tronçon** sur une AOI où le signal est pourtant mesurable, et `dessertR` dit lui-même pourquoi :

```
! Bornes mal adaptees a la donnee : "rugosite (86 %)" sature a une extremite.
i Des bornes calibrees sur un AUTRE massif ne se transportent pas : elles sont
  dans l'unite du canal.
i Recalibrer avec `dsr_calibrer_specs()` sur ces donnees, ou utiliser
  `bornes = FALSE`.
```

Nous avons suivi la consigne. Elle ne peut pas être appliquée.

## 2. Ce que nous avons d'abord corrigé chez nous

Avant de conclure à un problème cœur, nous avons trouvé et corrigé **notre** défaut : `run_desserte_detection()` passait le MNT **RGE ALTI 5 m** utilisé par les autres étapes, alors que `detecter_desserte()` cherche une signature de **micro-relief** et défaute à `dtm_res = 1`.

Chiffré avec `dsr_calibrer_specs()`, qui donne l'AUC de chaque canal :

| MNT fourni | canaux retenus | meilleure AUC |
|---|---:|---:|
| RGE ALTI 5 m | **0 / 7** | 0,56 |
| **LiDAR HD 0,5 m** | **5 / 7** | **0,77** (rugosité) |

À 5 m aucun canal ne faisait mieux que le hasard. Corrigé en `nemetonshiny 0.121.15` : le service prend la mosaïque LiDAR du projet quand elle existe.

**Le signal est donc bien là — et la détection rend toujours 0** (729 s sur ForetAccess). Ce qui reste n'est plus chez nous.

## 3. L'écart, précisément

`dessertR::dsr_calibrer_specs()` calibre correctement sur nos données : 66 s, 5 canaux retenus. Mais son `$specs` et celui qu'attend `foretaccess::detecter_desserte()` **n'ont pas la même forme** :

```r
dsr_calibrer_specs(couches, reference = desserte)$specs
#> List of 5 : rugosite, openness_pos, vesselness, pente, slrm      (liste PLATE de canaux)

foretaccess::specs_desserte_calibrees()
#> List of 3 : geomorpho, surface, c_vessel                          (3 groupes IMBRIQUÉS)
```

La documentation de chaque côté est cohérente avec elle-même :

- `dsr_calibrer_specs()` annonce un `$specs` « directement utilisable comme argument `specs` de `dsr_conductivite()` » ;
- `detecter_desserte(specs =)` renvoie à `specs_desserte_calibrees()`.

Ce sont **deux contrats différents pour un même mot**. Le conseil émis par l'avertissement — recalibrer avec `dsr_calibrer_specs()` — ne mène nulle part pour un appelant de `detecter_desserte()`.

## 4. Ce que nous demandons

Un chemin, quel qu'il soit, pour qu'une calibration locale atteigne la détection. Trois pistes, par ordre de préférence de notre point de vue d'appelant :

1. **`detecter_desserte()` accepte la forme `dsr_calibrer_specs()$specs`**, en plus de la sienne — détection de forme, ou argument distinct (`specs_canaux =`).
2. **Un convertisseur exposé**, du type `foretaccess::specs_depuis_calibration(cal)`, qui rende la forme `geomorpho`/`surface`/`c_vessel` à partir de la sortie dessertR. Charge à vous de dire ce qui se perd : la calibration ne produit pas de canal `surface`, qui vient du nuage.
3. **`detecter_desserte()` calibre lui-même** quand on lui passe `specs = "auto"`, puisqu'il a déjà le MNT et la référence sous la main — c'est-à-dire tout ce dont `dsr_calibrer_specs()` a besoin.

La piste 3 nous paraît la plus juste : l'appelant n'a aucune raison de savoir qu'il existe deux vocabulaires de specs.

**Et corriger le message**, dans tous les cas. Conseiller `dsr_calibrer_specs()` à un appelant de `detecter_desserte()` l'envoie dans une impasse — nous y avons passé du temps.

## 5. Question ouverte : `specs = NULL` suffit-il ?

`detecter_desserte()` documente que `specs = NULL` « restaure les specs de dessertR, dont les bornes sont dérivées par **quantiles de l'emprise** — le `seuil` cesse alors d'être comparable d'un site à l'autre ».

Pour une exploration mono-projet, ce compromis nous conviendrait. **Mais nous n'avons pas pu le vérifier** : chaque essai de détection coûte 12 minutes sur cette AOI, et nous ne voulions pas conclure sur une hypothèse non mesurée.

Si `specs = NULL` rend un résultat non vide là où le défaut rend 0, dites-le : c'est un contournement immédiat pour l'app, et cela circonscrit le §4 à un problème de confort plutôt que de blocage.

## 6. Ce que nous n'affirmons pas

Nous **ne savons pas** si la détection rendrait quelque chose avec des bornes correctes sur cette AOI. Il est possible qu'il n'y ait réellement aucune route non cartographiée sur ces 31 ha de forêt privée — c'est même plausible. Notre constat se limite à ceci : **la voie que l'avertissement recommande est fermée**, donc nous ne pouvons pas trancher entre « bornes inadaptées » et « rien à trouver ».

## 7. Références

- App : `nemetonshiny/R/service_desserte.R`, `run_desserte_detection()` (choix du MNT, appel à `detecter_desserte`)
- Cœur : `foretaccess::detecter_desserte()` (`specs`), `foretaccess::specs_desserte_calibrees()`
- dessertR : `dsr_calibrer_specs()` (`$specs`, `$diagnostic`), `dsr_layers_dtm()`, `dsr_conductivite()`
- Données : `~/.local/share/nemeton/projects/20260717_101641_wsfi`, MNT `cache/layers/lidar_mnt_mosaic.tif` (0,5 m), référence `cache/desserte/emprise_1000m/layers/desserte/desserte.gpkg` (3 299 tronçons)

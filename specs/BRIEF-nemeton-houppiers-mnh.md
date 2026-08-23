# BRIEF `nemeton` — segmentation des houppiers sur MNH (couche `houppier`)

> **Statut** : ouvert, 2026-08-23.
> **Dépôt concerné** : `nemeton` (cœur métier). Rien à écrire côté app avant
> qu'une release cœur expose la fonction.
> **Demandeur** : `nemetonshiny` v0.135.0, export Marculus (lot A livré sans
> cette couche).
> **Spécification aval** : `marculus/docs/specs/couche-houppier-mnh.md` et
> `couches-gpkg.md` — le contrat de sortie y est figé, il n'est pas à réinventer.

---

## Pourquoi le cœur et pas l'app

L'app sait déjà écrire le GeoPackage que lit Marculus : elle produit `parcelle`
et `desserte` depuis la v0.135.0. Il lui manque `houppier`, qui pré-remplit la
**hauteur** d'une tige au martelage par un simple point-dans-polygone sur la
position GNSS.

Produire cette couche demande une segmentation de couronnes — détection des
apex, délimitation, hauteur maximale par houppier. C'est un **calcul
forestier**, donc de la logique métier : règle 1 du `CLAUDE.md` de l'app, elle
appartient à `nemeton`. L'app appellera la fonction exportée et écrira la
couche ; elle ne calculera rien.

## Ce qui est demandé

Une fonction exportée, dans l'esprit des autres entrées « couche » du cœur :

```r
segment_houppiers(chm, aoi = NULL, ws = 5, hmin = 5, algorithme = "dalponte")
```

| Paramètre | Rôle |
|---|---|
| `chm` | MNH (`SpatRaster`) ou chemin. Le projet en cache déjà un : `cache/layers/opencanopy/chm_predicted_0_2m.tif` |
| `aoi` | Emprise de découpe (`sf`), typiquement l'UGF du chantier |
| `ws` | Fenêtre de recherche des maxima locaux, en mètres |
| `hmin` | Hauteur minimale d'un apex retenu |
| `algorithme` | `dalponte` / `silva` / `watershed` — au choix du cœur |

**Sortie** : un `sf` de POLYGON, une entité par houppier, avec au minimum une
colonne **`h_max`** — réel, en **mètres**, hauteur de l'apex.

`h_max` est le nom canonique côté Marculus. Les alias `hmax`, `hauteur_max`,
`hauteur`, `height` sont acceptés par l'app téléphone, mais autant écrire le
nom canonique.

## Les contraintes qui viennent de l'aval, et qui comptent

Elles ne sont pas décoratives : chacune correspond à un comportement déjà
implémenté côté téléphone.

- **Hauteurs hors de 1–70 m rejetées.** Un `h_max` à 0 (houppier vide) ou en
  centimètres n'écrira rien plutôt qu'une absurdité au journal de martelage.
  Autant ne pas les produire.
- **Houppiers superposés** (enveloppes convexes qui se chevauchent) : le
  téléphone retient le **plus haut**, celui dont l'apex domine physiquement
  l'opérateur. La segmentation n'a donc pas à garantir une partition — mais
  elle doit savoir que le recouvrement sera arbitré par la hauteur.
- **Aucun repli sur le houppier le plus proche.** Une position dans aucun
  polygone n'écrit rien : trouée, bord de peuplement, tige dominée. Ne pas
  chercher à couvrir tout l'espace par des polygones étirés — ce serait deviner
  l'arbre d'à côté.

## Le point de vigilance mémoire

Le MNH d'un chantier réel est gros : sur Couchey, `chm_predicted_0_2m.tif` fait
**28 481 × 14 695 = 418 528 295 cellules** à 0,20 m. C'est exactement le raster
qui a fait tomber le pipeline `opencanopynemeton` le 2026-08-22, sur deux
`values()` innocents (`specs/BRIEF-opencanopy-pct-veg-values.md`).

La segmentation à cette résolution n'a par ailleurs pas de sens sylvicole : un
houppier fait 3 à 10 m de diamètre. **Ré-échantillonner à 0,5 ou 1 m avant de
segmenter** divise le coût par 6 à 25 sans rien perdre — et c'est une décision
du cœur, pas un réglage à laisser à l'appelant.

Le calcul du `h_max` doit se faire par **zonale en streaming**
(`terra::zonal()`, `exactextractr::exact_extract()`), jamais par un
`values()`/`extract()` global.

## L'outillage est déjà là

`lasR 0.21.0` est une dépendance de l'app **et** du cœur, et expose la chaîne
complète : `chm`, `local_maximum`, `region_growing`, `hulls`. `lidR` est
également installé sur la station (`locate_trees` + `segment_trees` +
`crown_metrics`, l'exemple de la spec Marculus). Le choix entre les deux
appartient au cœur ; `lasR` a l'avantage d'être déjà déclaré et de streamer.

## Ce que l'app fera ensuite

Une fois la fonction publiée dans un **tag de release** (`@*release` ne tire que
les tags) :

1. plancher `Imports: nemeton (>= X.Y.Z)` relevé ;
2. `service_marculus.R` ajoute la couche `houppier` au GeoPackage, sous ce nom
   exact — **c'est un contrat, pas une convention** : une couche de houppiers
   nommée autrement devient une couche de *parcelles* côté téléphone, et chaque
   houppier devient candidat au rattachement spatial des tiges, remplissant le
   journal de parcelles fantômes ;
3. le calcul passera par le chemin plafonné (`run_memory_capped`), comme les
   autres travaux lourds.

## Vérification proposée

| Contrôle | Attendu |
|---|---|
| Sortie sur une UGF de Couchey | un `sf` POLYGON, `h_max` réel en mètres |
| Distribution de `h_max` | dans 1–70 m ; aucun 0, aucune valeur en centimètres |
| Densité | de l'ordre de la densité de tiges dominantes du peuplement, pas du nombre de pixels |
| Pic mémoire | compatible avec le plafond (50 % de `MemTotal` depuis v0.183.0) sur le MNH 0,20 m de Couchey |
| Emprise | découpée sur l'`aoi`, pas sur la dalle entière |

# BRIEF `nemeton` — FAST : masques qui s'écrasent entre eux, et un stack d'indice jamais mis en cache

> **Statut** : ouvert, 2026-09-23.
> **Émis par** : session `nemetonshiny` (cycle `0.143.28.9001`).
> **Dépôt concerné** : `nemeton` — `R/fast_alert_mask.R`, `R/pixel-map.R`.
> **Base relue** : `nemeton@0.197.0.9000` (lecture seule), release `v0.197.0`.
> **Urgence** : aucune. Rien n'est bloquant côté app : le recalcul à chaque
> entrée dans l'onglet « Suivi sanitaire » est corrigé côté app (mémoire de
> signature dans `mod_monitoring_fast_alerts` et `mod_monitoring_pixel_map`).
> Ce brief traite ce qui reste **au cœur**.

---

## 0. D'où ça vient

L'utilisateur voyait « Suivi sanitaire » tout recalculer à chaque visite de
l'onglet. Il y avait deux causes côté app, corrigées. Les mesures faites pour
les établir ont fait apparaître deux défauts au cœur, que l'app ne peut
corriger qu'en partie.

Mesures sur le projet réel `20260624_073705_armn` (zone 5, 327 scènes S2 en
cache, cache chaud, `Rscript` hors Shiny) :

| Appel | Durée |
|---|---|
| `compute_fast_alert_mask()` count/NDVI, 1ᵉʳ appel | 1,44 s |
| `compute_fast_alert_mask()` count, rolling, trend, appels suivants | 0,11 à 0,15 s |
| `build_index_stack(index = "NDVI")`, 3 appels identiques | **8,75 / 9,20 / 9,37 s** |

---

## 1. Défaut A — deux masques différents tombent sur le même fichier

### Constat

`compute_fast_alert_mask()` nomme le masque avec un horodatage **à la
seconde** :

```r
ts <- format(Sys.time(), "%Y%m%dT%H%M%S")
out_path <- file.path(zone_dir, sprintf("fast_alert_%s.tif", ts))
terra::writeRaster(mask, out_path, overwrite = TRUE, ...)
```

Deux appels dans la même seconde écrivent donc **le même chemin**, et le
second écrase le premier grâce à `overwrite = TRUE`. C'est reproduit dans la
série de mesures ci-dessus. Ce sont bien des masques différents, pas un même
masque réécrit :

```
rolling  NDVI  0.12 s  -> fast_alert_20260923T094317.tif
trend    NDMI  0.11 s  -> fast_alert_20260923T094317.tif   ← même fichier
```

Le cache chaud (0,12 s) rend la collision **facile** : plusieurs appels
tiennent dans une seconde.

### Pourquoi c'est un défaut et pas un détail

1. **Le raster rendu pointe sur le fichier.** L'app fait
   `terra::rast(mask_path)` : ce `SpatRaster` est adossé au disque. Si le
   fichier est réécrit ensuite, toute lecture ultérieure (clic sur la carte →
   classe de sévérité du pixel, repeinture) lit **l'autre masque**. Dans
   l'app, le pré-chauffage de la tendance (NDMI puis NDRE à la suite, en
   arrière-plan) et un changement rapide d'indice suffisent à le provoquer.
2. **`read_fast_alert_mask()` rend « le plus récent »**, sans savoir de quel
   indice, mode ou seuil il s'agit. Le nom ne dit rien des paramètres, donc
   « le dernier masque » désigne celui du dernier appel, quel qu'il soit. Ce
   n'est pas un problème pour l'app aujourd'hui : son échantillonnage de
   validation lit un autre répertoire (`fast_sampling/`). Mais ce
   comportement est piégeux pour tout autre lecteur.

### Correctif proposé

L'une des deux options, au choix du cœur. La seconde règle aussi le défaut C :

- **Minimal** : un nom unique garanti — horodatage à la milliseconde plus un
  suffixe aléatoire, ou `tempfile(pattern = "fast_alert_", tmpdir = zone_dir,
  fileext = ".tif")`. Pas d'`overwrite = TRUE` sur un nom censé être neuf.
- **Mieux** : nommer le masque **par son contenu**, comme le raster continu
  (le cache D6) : un hash de (zone, index, mode, seuil, dates, `window_days`,
  months, `min_years`, alpha, `breaks`, polygone de masque, couverture S2
  réelle). Deux appels identiques rendent alors le même fichier **sans le
  réécrire**, et deux appels différents ne peuvent pas se télescoper.

Conserver `.fast_alert_mask_gc()` (LRU, `keep = 20`) dans les deux cas.

**Test attendu** : deux appels consécutifs avec des indices différents
doivent donner deux chemins différents, et le premier fichier doit garder
son contenu (valeurs relues après le second appel).

---

## 2. Défaut B — `build_index_stack()` refait tout à chaque appel

### Constat

`build_index_stack(cache_dir, scenes_df, index)` relit les bandes de chaque
scène (`read_s2_band_raster()`), calcule l'indice, rééchantillonne B11/B12 à
10 m pour NDMI/NBR, puis empile le tout. Rien n'est persisté. Sur 327 scènes,
cela prend **~9 s**, et à l'identique à chaque appel.

L'app l'appelle pour la « Carte FAST » (le curseur de dates). Elle ne le
recalcule plus en revenant sur l'onglet, mais elle paie toujours ces 9 s :
- au **premier affichage** de chaque session (redémarrage de l'app,
  changement de projet) ;
- à **chaque changement d'indice** (NDVI ↔ NBR ↔ NDMI ↔ NDRE), un retour à
  un indice déjà vu compris.

L'appel est synchrone, donc la session Shiny est gelée pendant ce temps.

### Correctif proposé

Un cache disque adressé par contenu, sur le modèle du raster continu du
défaut A. Signature suggérée, rétrocompatible (défauts = comportement
actuel) :

```r
build_index_stack(cache_dir, scenes_df, index, mask_polygon = NULL,
                  parallel = FALSE,
                  cache_result = FALSE, result_cache_dir = NULL)
```

- **Clé** = hash de (index, `scene_id` triés, `mask_polygon` s'il est
  fourni). Pour que les scènes réingérées invalident le cache, ajouter à la
  clé la taille et la date de modification (mtime) de leurs fichiers de
  bandes.
- **Stockage** : un GeoTIFF multicouche (ou COG), avec les noms de couches et
  `terra::time()` rétablis à la relecture, ainsi que l'attribut `"index"`.
  Vérifier que la relecture rend un objet **identique** à celui du calcul
  (noms, dates, `attr(, "index")`, valeurs NA).
- **GC** : un LRU comme `.fast_raster_gc()`.

Côté app, l'appel passera `cache_result = TRUE, result_cache_dir =
<project>/cache/layers/index_stack` une fois la version publiée, avec un
plancher `Imports: nemeton (>= X.Y.Z)`.

**Test attendu** : deux appels identiques, le second sans relecture de bandes
(mocker `read_s2_band_raster` et compter ses appels) et avec un résultat
identique ; une scène ajoutée ou modifiée invalide le cache.

### Question en passant

L'app appelle `build_index_stack()` **sans** `parallel = TRUE`. Est-ce
recommandé dans un processus Shiny, où il n'y a pas de `future::plan()`
multisession posé en permanence ? Si oui, l'app peut l'activer dès
aujourd'hui, sans attendre le cache.

---

## 3. Hors périmètre

- **Le recalcul à chaque entrée dans l'onglet** : réglé côté app, rien à
  faire au cœur.
- **Le commentaire de `.compute_fast_mask()` côté app** : il affirmait que le
  masque était réutilisé sans recalcul. Il est corrigé côté app. Si le défaut
  A est corrigé par la seconde option (nom par contenu), le commentaire
  devra de nouveau être mis à jour — l'app s'en charge à la montée de
  plancher.

## 4. Retour attendu vers l'app

- Numéro de la release cœur, et option retenue pour le défaut A.
- Pour le défaut B : la signature finale et le répertoire de cache conseillé.
- La réponse à la question `parallel`.
- Une ligne dans la table « Écarts ouverts » du `PLAN.md` (livré par le cœur,
  en attente chez `nemetonshiny`) tant que l'app n'a pas consommé la nouvelle
  version.

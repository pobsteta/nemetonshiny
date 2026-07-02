# Brief `nemetonshiny` — Forêt ancienne → N2 continuité (spec 031)

**Cœur requis** : `nemeton (>= 0.113.0)` — fonction exportée
`build_foret_ancienne_mask()`.
**Objectif app** : permettre à l'utilisateur de fournir une **source
historique de forêt ancienne** ; l'app la convertit via le cœur et la passe à
`indicateur_n2_continuite()` pour un score de continuité **réel** (au lieu du
défaut 50 / de la seule couverture actuelle).

> **Rappel métier** : la forêt ancienne = état boisé continu depuis une date
> de référence historique (Cassini XVIIIᵉ, cartes d'état-major XIXᵉ, couche
> IGN « forêt ancienne »). **Ce n'est pas** un produit satellite récent, et
> **Corona-4B n'est pas une source** (0 couverture France — vérifié spec 031).
> La source est **fournie par l'utilisateur**.

---

## 1. Ce que le cœur fournit (aucune logique métier côté app)

```r
# raster historique classé (ex. Cassini scanné/classifié) :
fa <- nemeton::build_foret_ancienne_mask(
  source       = hist_raster,     # terra SpatRaster
  forest_class = c(1L, 4L),       # valeur(s) = forêt   (OU threshold=)
  min_area_m2  = 5000,            # optionnel : retire les taches < 0,5 ha
  crs          = 2154
)

# OU couche vecteur déjà digitalisée (IGN forêt ancienne, Cassini vectorisé) :
fa <- nemeton::build_foret_ancienne_mask(source = hist_sf, min_area_m2 = 5000)

# -> `fa` : sf polygones, colonne `foret_ancienne = TRUE`
# On la passe telle quelle à N2 :
units <- nemeton::indicateur_n2_continuite(units, foret_ancienne = fa)
```

Contrat : `build_foret_ancienne_mask()` accepte **sf/sfc** (vecteur) **ou**
**SpatRaster** (raster). Pour un raster, `forest_class` (appartenance) **ou**
`threshold` (valeur ≥) sélectionne la forêt ; à défaut `valeur > 0`. Sortie =
sf `foret_ancienne = TRUE` (0 ligne si rien trouvé).

## 2. UI à ajouter (module données / famille N)

Un bloc **optionnel** « Forêt ancienne (source historique) » :

1. **Upload / sélection de source** :
   - fichier **raster** (`.tif`/`.tiff` GeoTIFF/COG) **ou**
   - fichier **vecteur** (`.gpkg`/`.shp`/`.geojson`).
   - (option) sélection d'une couche déjà connue du projet.
2. **Paramètres** (affichés selon le type détecté) :
   - raster → `forest_class` (multi-sélection des valeurs de classe présentes,
     lues via `terra::unique()`) **ou** `threshold` (numérique) ;
   - commun → `min_area_m2` (défaut p. ex. 5000, slider) ;
   - CRS : auto (lu de la source), pas d'UI.
3. **Aide contextuelle** i18n : rappeler les sources acceptées
   (Cassini / état-major / IGN forêt ancienne) et que **Corona n'est pas
   utilisable**.

## 3. Server / calcul

- Au calcul du projet, **si** une source forêt ancienne est fournie :
  `fa <- nemeton::build_foret_ancienne_mask(source, forest_class|threshold,
  min_area_m2, crs = <crs projet>)`, puis
  `indicateur_n2_continuite(units, foret_ancienne = fa)`.
- **Si absente** : appeler N2 sans `foret_ancienne` (comportement actuel
  inchangé — pas de régression).
- **Aucune** logique métier dans l'app : uniquement l'appel aux fonctions
  cœur (règle CLAUDE.md).
- **Cache** : persister la couche `fa` construite sous
  `<project>/cache/layers/foret_ancienne/` (comme les autres couches) pour ne
  pas re-polygoniser à chaque run ; clé = hash(source + params).

## 4. i18n (clés FR/EN à ajouter dans `utils_i18n.R`)

- `foret_ancienne_section` : « Forêt ancienne (continuité N2) » / « Ancient
  forest (N2 continuity) »
- `foret_ancienne_source` : « Source historique (raster classé ou vecteur) » /
  « Historical source (classified raster or vector) »
- `foret_ancienne_forest_class` / `foret_ancienne_threshold` /
  `foret_ancienne_min_area`
- `foret_ancienne_hint` : « Cartes de Cassini, d'état-major, ou couche IGN
  forêt ancienne. Corona-4B non couvert en France. » (+ EN)

## 5. Critères d'acceptation (app)

- [ ] Sans source fournie : N2 se calcule comme aujourd'hui (aucune
      régression).
- [ ] Avec un raster classé + `forest_class` : le score N2 des UGF recouvertes
      de forêt ancienne monte dans la plage 60-100.
- [ ] Avec un vecteur : idem, la couche est validée/reprojetée automatiquement.
- [ ] La couche construite est mise en cache et réutilisée au run suivant.
- [ ] Tous les textes passent par `i18n$t()`.

## 6. Hors scope

- Fourniture d'une source historique nationale (nemeton n'en embarque pas ;
  c'est une donnée utilisateur).
- Extraction automatique du couvert depuis une image panchromatique brute
  (Corona/état-major non géoréférencé) — chantier ML distinct si un jour
  pertinent.

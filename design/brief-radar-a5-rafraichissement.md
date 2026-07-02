# Brief `nemetonshiny` — Radar A5 « Rafraîchissement urbain » (LST, spec 032)

**Cœur requis** : `nemeton (>= 0.114.0)` — A5 est livré en **0.114.0**.
⚠️ **Bump de dépendance nécessaire** : `nemetonshiny` main est à
`nemeton (>= 0.113.0)` → passer à `>= 0.114.0` dans `DESCRIPTION` (fait sur la
branche de terrain, cf. §0).
**Objectif app** : câbler l'indicateur **A5** (`indicateur_a5_rafraichissement`)
pour qu'il apparaisse sur le **radar de la famille A** (à côté de A1-A4),
alimenté par la **température de surface (LST)** Theia (`theia_lst` /
`thermocity-lst`).

> **Rappels métier** :
> - A5 = **fraîcheur relative de surface**, indicateur **à sens direct**
>   (haut = plus frais que l'entour = bon). **AUCUNE inversion** — contrairement
>   à T3/R5. L'app ne fait rien de spécial : la normalisation positive se fait
>   au cœur (`create_family_index`).
> - **Couverture URBAINE seulement** : la LST Thermocity couvre quelques
>   métropoles (Marseille, Montpellier, Paris, Strasbourg…), **pas les massifs
>   forestiers**. Sur un projet rural, A5 = **NA** (indicateur orienté arbre en
>   ville). À dire clairement dans l'UI (tooltip).

---

## 0. Dépendance (déjà fait sur la branche de terrain)

`DESCRIPTION` : `nemeton (>= 0.113.0)` → **`nemeton (>= 0.114.0)`**.

## 1. Ce que le cœur fournit

```r
lst <- nemeton::load_theia_source("theia_lst", aoi, asset = "LST")  # SpatRaster kelvin

out <- nemeton::indicateur_a5_rafraichissement(
  units,
  lst          = lst,
  reference    = NULL,   # NULL → référence locale = médiane LST d'un anneau
  buffer_m     = 500,    # rayon de l'anneau de référence
  delta_scale  = 5       # écart T° (K/°C) mappé sur toute l'amplitude du score
)
# out est l'`sf` `units` AUGMENTÉ de $A5 (0-100, haut=frais) et $A5_delta (réf − unité)
```

- Retour = **`sf`** avec `$A5` et `$A5_delta` (≠ T3 qui renvoie un vecteur).
  **Aucun traitement spécial requis** : `compute_single_indicator()` détecte le
  `sf` et appelle `nemeton::extract_indicator_value(result, indicator)`, qui
  fait `short = "A5"` et renvoie `result$A5` **exactement** (le `A5_delta` est
  ignoré). Même mécanique que A1/A2.
- `lst = NULL` → `A5 = NA` par unité (source-conditionnel, comme A3/A4).
- LST en **kelvin** (float32, nodata `-32768` filtré au cœur) ; K ou °C
  fonctionnent (l'indice est un écart).

## 2. Acquisition + cache — `build_lst_layer()`

Nouveau helper dans `service_compute.R`, sur le modèle de
`build_sufosat_layer` / `build_foret_ancienne_layer` :

```r
build_lst_layer <- function(cfg, project_path, aoi, crs = 2154) {
  # cache <project>/cache/layers/lst/lst.tif, clé = bbox AOI
  # 1. si cache présent → terra::rast() ; sinon
  # 2. nemeton::theia_configure_s3(); nemeton::load_theia_source("theia_lst", aoi, "LST")
  # 3. crop AOI, écrire le cache, retourner le SpatRaster (ou NULL)
  # Échec / hors couverture urbaine → NULL (A5 restera NA, pas de régression)
}
```

## 3. Wiring dans `service_compute.R`

**Staging** (bloc miroir de foret_ancienne / sufosat, ~l. 854) — ranger la LST
là où `resolve_raster_layer()` la trouvera (`layers$rasters$lst`) :

```r
lst_cfg <- projet_for_ug$metadata$lst_urbain
if (!is.null(lst_cfg) && isTRUE(lst_cfg$enabled) &&
    "indicateur_a5_rafraichissement" %in% effective_indicators) {
  state$current_task <- "lst_urbain"; report_progress(state)
  lst_r <- build_lst_layer(lst_cfg, project_path, aoi = compute_unit,
                           crs = sf::st_crs(compute_unit)$epsg %||% 2154)
  if (!is.null(lst_r)) layers$rasters$lst <- lst_r
}
```

**Dispatcher** (`compute_single_indicator`, l. 3701) — ajouter une branche qui
injecte l'arg `lst` quand la fonction l'accepte, comme `dem`/`ndvi` :

```r
if ("lst" %in% func_args) {
  lst <- resolve_raster_layer(layers, "lst")
  if (!is.null(lst)) args$lst <- lst
}
```

Rien d'autre côté dispatcher : le `sf` retourné est déjà géré par le chemin
générique (`extract_indicator_value`). Ajouter
`indicateur_a5_rafraichissement` à **`list_available_indicators()`**, **gaté**
sur `metadata$lst_urbain$enabled`.

## 4. UI (`mod_project.R`) — toggle Theia, pas d'upload

Bloc optionnel « **Rafraîchissement urbain (LST)** » :
- **Toggle** « Activer » — gaté sur Theia configuré ; désactivé + tooltip sinon.
- **Paramètre** optionnel : `buffer_m` (rayon de l'anneau de référence, défaut
  500 m). `delta_scale`/`reference` : garder les défauts cœur.
- Écrit dans `projet$metadata$lst_urbain = list(enabled=, buffer_m=)`.
- Aide i18n : « Fraîcheur de surface (LST) de l'arbre en ville vs son
  environnement. **Disponible en zone urbaine / métropole uniquement** (hors
  couverture, l'indicateur reste vide). Sens direct : plus frais = meilleur. »

## 5. Radar — automatique (sens direct, ne rien inverser)

Une fois la colonne `indicateur_a5_rafraichissement` présente sur `units`,
`nemeton::create_family_index()` la rattache **automatiquement** à la famille A
(le cœur a déjà `A5` dans sa config). Sens **positif** → la normalisation passe
la valeur telle quelle (pas d'inversion). L'axe A du radar passe de 4 à 5
sous-indicateurs **sans code radar dédié**.

- Le libellé/tooltip radar de A5 vient de la config cœur
  (`INDICATOR_FAMILIES$A$indicator_labels$A5` : « Rafraîchissement urbain » /
  « Urban cooling ») — réutiliser la même source que A1-A4.

## 6. Critères d'acceptation (app)

- [ ] `DESCRIPTION` exige `nemeton (>= 0.114.0)`.
- [ ] Sans `metadata$lst_urbain$enabled` : famille A inchangée (A1-A4), aucune
      requête Theia, aucune régression.
- [ ] Projet **rural** (hors couverture LST) : A5 = NA proprement, l'axe reste
      vide sans casser le radar.
- [ ] Projet **urbain** (métropole) avec LST : l'axe A5 apparaît, **score haut**
      là où l'arbre est plus frais que son environnement (sens direct).
- [ ] La couche LST est mise en cache et réutilisée ; `buffer_m` propagé.
- [ ] Aucun code d'inversion de A5 côté app.
- [ ] Tous les textes passent par `i18n$t()`.

## 7. Hors scope

- Carte de chaleur LST (affichage raster) — le radar A5 d'abord.
- Recherche d'une source LST **nationale** (ECOSTRESS brut / Copernicus LST)
  pour étendre A5 hors métropoles — chantier cœur ultérieur.

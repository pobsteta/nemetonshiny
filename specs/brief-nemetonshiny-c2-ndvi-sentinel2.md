# Brief `nemetonshiny` — C2 : calculer le NDVI depuis Sentinel-2, pas depuis une ortho WMS

> **✅ CLOS — livré en `nemetonshiny` v0.126.1.** `build_s2_ndvi_layer()` compose
> les scènes Sentinel-2 L2A du cache ; l'ortho IRC WMS n'est plus qu'un repli.
> Mesuré sur les 30 UGF de Fordead : NDVI médian **−0,109 → 0,584**, aucune UGF
> négative contre 20 avant, et `famille_carbone` **0,90 → 29,5**. La dette §5
> (copie morte de `normalize_indicator()`) était déjà soldée en v0.125.1.
> Ne pas rouvrir.


**Dépôt cible** : `nemetonshiny`. Aucun développement côté cœur : tout ce qui
est nécessaire est déjà exporté par `nemeton`.

**Origine** : brief `specs/BRIEF-nemeton-normalisation-familles.md` §5.a
(diagnostic du 2026-08-16, projet Fordead). Traité ici parce que le point est
**entièrement app** : le cœur reçoit un raster NDVI, il ne choisit pas sa source.

---

## 1. Le problème

`indicateur_c2_ndvi` sort **négatif sur les 30 UGF** de Fordead (médiane
−0,109). Après normalisation, C2 vaut donc `0` et tire `famille_carbone` à
0,90 / 100.

La cause n'est ni le calcul, ni l'ordre des bandes — les six paires possibles
ont été testées, `(B1 − B2)/(B1 + B2)` est bien la seule dont la distribution
ressemble à de la végétation. La cause est **la nature de la source** :
`download_ign_irc_ndvi()` (`service_compute.R`) dérive le NDVI de l'**orthophoto
IRC du WMS IGN**, c'est-à-dire d'une image **8 bits étirée pour l'affichage**
(valeurs 9–247, compression JPEG). Ce n'est pas de la réflectance calibrée : le
NDVI qu'on en tire n'a pas de sens physique. Mesuré : 33,7 % de pixels négatifs,
médiane 0,168, seulement 32,5 % au-dessus de 0,3.

## 2. La source correcte est déjà dans le projet

Chaque projet suivi dispose de **dizaines de scènes Sentinel-2 L2A en cache**
(`cache/layers/sentinel2/`, série 2017→, alimentée pour FORDEAD) — de la
réflectance de surface, corrigée atmosphériquement.

Le cœur expose déjà tout le nécessaire, sans rien à ajouter :

```r
# une scène, deux bandes
b8 <- nemeton::read_s2_band_raster(cache_dir, scene_id, "B08")
b4 <- nemeton::read_s2_band_raster(cache_dir, scene_id, "B04")

# ou, directement, la pile d'indices sur une sélection de scènes
ndvi <- nemeton::build_index_stack(cache_dir, scenes_df, index = "NDVI")
```

`build_index_stack()` est déjà utilisée par l'app pour B4 / L3 : le chemin est
connu et éprouvé.

## 3. Le travail

1. Dans `service_compute.R`, alimenter `layers$rasters$ndvi` depuis le cache S2
   quand il existe, et **ne retomber sur `download_ign_irc_ndvi()` que s'il est
   vide**. L'ortho WMS reste un repli acceptable pour un projet sans série S2 —
   elle n'est pas *fausse*, elle est *non calibrée*.
2. **Choisir la scène**, et le dire : une composite de saison de végétation
   (médiane des scènes peu nuageuses de juin à septembre) est plus robuste
   qu'une date unique. Le choix doit être visible dans le journal de calcul —
   un NDVI n'a de sens que rapporté à sa date.
3. Journaliser la source retenue (`"C2 : NDVI Sentinel-2 L2A (n scènes,
   AAAA-MM-JJ → AAAA-MM-JJ)"` vs `"C2 : NDVI dérivé de l'ortho IRC WMS (non
   calibré)"`), pour que la lecture du score reste possible a posteriori.
4. Clé i18n pour l'avertissement « NDVI non calibré » quand le repli WMS est
   utilisé.

## 4. Critères d'acceptation

- [ ] Projet avec cache S2 (Fordead) : C2 est calculé depuis L2A, la médiane
      redevient **positive** et cohérente avec un couvert forestier.
- [ ] Projet sans cache S2 : repli WMS inchangé, avec avertissement explicite.
- [ ] La source et la fenêtre temporelle retenues apparaissent dans le journal.
- [ ] Aucun calcul d'indice dans l'app : passer par
      `nemeton::build_index_stack()`. La règle « aucune logique métier dans
      l'app » s'applique ici aussi.

## 5. Dette adjacente à solder au passage

`R/service_compute.R:3793` définit une **copie morte de `normalize_indicator()`**
— jamais appelée, dupliquant la logique métier du cœur. À supprimer : elle viole
la règle « aucune logique métier dans l'app », et une copie morte finit toujours
par diverger puis par être appelée.

## 6. Hors scope

- Le seuil bas du NDVI dérivé (ramené de −1 à 0 en v0.125.0.9001) : correctif de
  propagation déjà livré, indépendant de la source.
- Le reste du brief `BRIEF-nemeton-normalisation-familles.md` : traité côté cœur
  en v0.174.0.

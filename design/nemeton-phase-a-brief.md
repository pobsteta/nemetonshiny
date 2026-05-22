# Brief session `nemeton` — Phase A : échantillonnage de validation

> **À coller dans une nouvelle instance de dev sur `/home/pascal/dev/nemeton`.**
> Repo cible : `nemeton` (cœur) uniquement.
> Spec de référence : `nemetonshiny/design/validation-sampling.md` (décisions §13).
> Cette phase A doit être **livrée et releasée** avant la phase B (app).

## Objectif

Fournir au cœur `nemeton` les fonctions permettant à `nemetonshiny` de
générer un **plan d'échantillonnage de validation sanitaire** : des placettes
terrain concentrées sur les zones de dépérissement détectées par FORDEAD/FAST
(échantillonnage GRTS pondéré par l'intensité), plus des placettes témoins en
zone saine.

Contexte : aujourd'hui les alertes FAST/FORDEAD sont projetées sur les
placettes systémiques (réparties sur tous les peuplements) → un foyer de
dépérissement sans placette à proximité passe inaperçu (« 0 alertes » alors
que le `dieback_mask` montre du dépérissement). Cf. spec complète.

## Existant cœur (rappel)

- `read_fordead_dieback_mask(con, zone_id, run_id = NULL, cache_dir)` → renvoie
  le `SpatRaster` catégoriel **0-4** (0 sain, 1 faible, 2 moyenne, 3 forte,
  4 sol-nu ; NA hors masque forestier). Persisté par `run_fordead_dieback()`
  sous `<project>/cache/layers/fordead/zone_<id>/dieback_mask_<ts>.tif`.
- `create_sampling_plan(zone, n_base, n_over, target_error, cv, alpha,
  over_ratio, chm, slope, forest_mask, mnt, grid_step, plot_radius,
  min_forest_cover, max_slope, min_per_stratum, seed)` → GRTS stratifié,
  renvoie un `sf` POINT.
- FAST : détection rolling-window NDVI/NBR ; `list_fast_alerts_for_zone()`
  renvoie des alertes **par placette** (buckets critical/warning/info selon le
  ratio NDVI/NBR). **Pas** de raster d'alerte continu persisté à ce jour.

## Livrables (4)

### A1 — `fordead_alert_mask()`

```r
fordead_alert_mask(alert_raster,
                   classes  = c(3L, 4L),
                   buffer_m = 0)
```

- `alert_raster` : `SpatRaster` catégoriel 0-4 (dieback_mask FORDEAD ou raster
  FAST de A4).
- `classes` : classes considérées « zone d'alerte ». Défaut **3-4** (décision 6).
- `buffer_m` : dilatation optionnelle (mètres) autour des pixels d'alerte ;
  défaut 0.
- **Retour** : `SpatRaster` où les cellules d'alerte **conservent leur valeur
  de classe** (3, 4…) et les autres sont `NA`. Ce raster sert à la fois de
  masque (NA = exclu) **et** de raster de priorité (la valeur = intensité).
  Les cellules ajoutées par le tampon (non alerte à l'origine) prennent
  `min(classes)` (samplables, priorité basse).
- Cas limite : aucune cellule dans `classes` → renvoyer un raster tout-NA
  (la détection du « masque vide » est faite par A3).

### A2 — GRTS pondéré par un raster de priorité

Décision 3 : l'échantillonnage doit pouvoir **pondérer** l'allocation des
points par l'intensité (plus de placettes sur les classes sévères), et non
seulement restreindre le domaine.

- Ajouter un paramètre **`priority_raster`** (`SpatRaster`, optionnel) à
  `create_sampling_plan()` — ou implémenter la pondération directement dans
  A3. Choix laissé au cœur ; recommandation : l'ajouter à `create_sampling_plan()`
  et faire déléguer A3, pour réutilisabilité.
- Sémantique : les valeurs du `priority_raster` deviennent des **probabilités
  d'inclusion inégales** (GRTS unequal-probability, p. ex. via l'`aux`/
  `caty` de `spsurvey::grts()`). Une cellule de classe 4 doit avoir une
  proba d'inclusion plus élevée qu'une cellule de classe 3.
- `priority_raster = NULL` → comportement actuel inchangé (équiprobable).
- **Ne pas** détourner `forest_mask` pour cet usage (décision 3).

### A3 — `create_validation_sampling_plan()`

Entrée publique unique, encapsule toute la méthodologie (décision 5 : les
témoins sont générés ici, pas côté app).

```r
create_validation_sampling_plan(
  zone,                                  # sf — AOI de la zone de suivi
  alert_raster,                          # SpatRaster catégoriel 0-4
  n_validation = 20L,                    # placettes cibles en zone d'alerte
  n_control    = 5L,                     # placettes témoins en zone saine
  classes      = c(3L, 4L),              # classes « alerte » (décision 6)
  buffer_m     = 0,
  source       = c("FORDEAD", "FAST"),   # tag de provenance
  seed         = NULL
)
```

Comportement :

1. **Placettes de validation** : GRTS **pondéré** (A2) sur
   `fordead_alert_mask(alert_raster, classes, buffer_m)`, `n = n_validation`,
   inclusion pondérée par l'intensité.
2. **Placettes témoins** : GRTS équiprobable sur la zone de **classe 0 (sain)**
   de `alert_raster`, `n = n_control`.
3. `visit_order` : ordre de tournée (TSP) sur l'ensemble des placettes
   (même logique que `create_sampling_plan`).

**Retour** : `sf` POINT (CRS de `zone`, attendu EPSG:2154) avec :

| Colonne | Type | Note |
|---|---|---|
| `plot_id` | chr | identifiant placette |
| `type` | chr | `"Validation"` \| `"Temoin"` |
| `alert_class` | int | valeur du raster sous la placette (0-4 ; 0 pour témoin) |
| `visit_order` | int | ordre de tournée |
| `source` | chr | echo de `source` |
| `classes` | chr | echo, p. ex. `"3,4"` |
| `seed` | int | echo (NA si non fourni) |
| `geometry` | POINT | |

> La provenance applicative (`zone_id`, `fordead_run_id` / `mask_timestamp`,
> `generated_at`) est ajoutée **côté app** (Phase B) — le cœur ne la connaît
> pas.

Cas limites :

- **Masque d'alerte vide** (aucune cellule dans `classes`) : lever une erreur
  typée — `cli::cli_abort(..., class = "nemeton_empty_alert_mask")` — pour que
  l'app affiche un message propre (« zone saine, rien à valider ») au lieu de
  produire un plan dégénéré.
- `n_validation` / `n_control` sont des **cibles**, pas des garanties (GRTS) —
  comme `create_sampling_plan` ; l'app ne codera pas en dur les comptes.
- `seed` → résultat reproductible.

### A4 — Raster d'alerte FAST + lecteur

Décision 4 : FAST est traité de front avec FORDEAD.

- La détection FAST (rolling-window NDVI/NBR) doit **persister un raster
  d'alerte continu** par pixel, sur le modèle du `dieback_mask` FORDEAD :
  `<project>/cache/layers/fast/zone_<id>/fast_alert_<ts>.tif`.
- **Échelle de classes alignée sur FORDEAD** (catégoriel 0-4) si possible, en
  mappant les buckets FAST existants (critical / warning / info, fondés sur le
  ratio NDVI/NBR) sur 0-4 — pour que `fordead_alert_mask()` et
  `create_validation_sampling_plan()` consomment FAST et FORDEAD sans
  distinction.
- Exposer un lecteur **`read_fast_alert_raster(con, zone_id, run_id = NULL,
  cache_dir)`** — strict miroir de `read_fordead_dieback_mask()`.

> Le mapping buckets FAST → 0-4 et l'intégration au pipeline FAST sont à la
> main du cœur (l'app n'a pas la visibilité sur les internes FAST). Si une
> échelle 0-4 stricte n'a pas de sens pour FAST, documenter l'échelle retenue
> et garder `fordead_alert_mask()` paramétrable en conséquence.

## Contrat attendu par l'app (Phase B)

`nemetonshiny` appellera, côté `mod_monitoring` :

```r
mask  <- nemeton::read_fordead_dieback_mask(con, zone_id, cache_dir = cd)
# ou nemeton::read_fast_alert_raster(con, zone_id, cache_dir = cd)
plan  <- nemeton::create_validation_sampling_plan(
  zone, alert_raster = mask, n_validation = ..., n_control = ...,
  classes = ..., source = "FORDEAD", seed = ...)
```

puis ajoutera ses colonnes de provenance et persistera `plan` dans la couche
`validation_plots` de `samples.gpkg`.

## Transverse

- **Exports** : `fordead_alert_mask`, `create_validation_sampling_plan`,
  `read_fast_alert_raster` exportés (`@export` + NAMESPACE). `priority_raster`
  = nouveau paramètre d'une fonction déjà exportée.
- **Tests** (`testthat` e3) :
  - `fordead_alert_mask()` : classes retenues, tampon, raster tout-NA.
  - GRTS pondéré : une zone de classe 4 reçoit en moyenne plus de points
    qu'une zone de classe 3 de même surface (test statistique sur seed fixe).
  - `create_validation_sampling_plan()` : types `Validation`/`Temoin`
    présents, témoins en `alert_class == 0`, `priority_raster` respecté,
    masque vide → erreur `nemeton_empty_alert_mask`, reproductibilité `seed`.
  - `read_fast_alert_raster()` : lecture du dernier raster, `run_id`, absence.
- **NEWS.md** (`nemeton`) : entrée datée.
- **PLAN.md** (`nemeton`) : cocher / journaliser le sous-chantier (échantillonnage
  de validation, phase A) avec le commit `nemeton@SHA` et le cycle dev.
- **Versionnage** : cycle dev `nemeton` puis release stable `vX.Y.Z`. Une fois
  releasée, `nemetonshiny` consommera ces fonctions via `Remotes: @*release`
  (pas de bump `Remotes:`) ; le plancher `Imports: nemeton (>= X.Y.Z)` de l'app
  sera relevé en Phase B quand le code app appellera ces fonctions.

## Points à confirmer côté cœur

1. `priority_raster` sur `create_sampling_plan()` vs pondération interne à A3 —
   choix d'implémentation cœur.
2. Échelle de classes du raster FAST : 0-4 strict aligné FORDEAD, ou échelle
   propre documentée.
3. Mécanique GRTS unequal-probability disponible dans la version `spsurvey`
   utilisée (`aux_var` / `caty_var`).
4. `visit_order` : une tournée unique validation+témoins, ou deux tournées
   séparées.

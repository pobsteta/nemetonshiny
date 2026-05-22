# Design note — Échantillonnage de validation sanitaire piloté par raster d'alerte

> Statut : **proposition** · Cycle : post-v0.40.0 · Contexte DDD : `contexte_sante` ↔ `contexte_interoperabilite`
> ADR liés : ADR-013 (suivi sanitaire), ADR-011 (NDP/φ), ADR-007 (pipeline NDP) · Spec liée : spec 008 (FORDEAD)
> Repos concernés : `nemeton` (cœur) **et** `nemetonshiny` (app)

## 1. Contexte & problème

FAST (surveillance rapide NDVI/NBR rolling-window) et FORDEAD (diagnostic
CRSWIR + harmonique) font de la détection **au pixel** Sentinel-2 — couverture
spatiale **continue** de l'AOI.

Mais les alertes restituées à l'utilisateur sont **projetées sur les
placettes** de suivi terrain :

- `mod_monitoring_fast_alerts` — rend une carte des **placettes** dont le ratio
  NDVI/NBR franchit un seuil ;
- FORDEAD — `nemeton::list_alerts_for_zone()` insère une alerte **par placette**
  intersectant un pixel de dépérissement.

Or ces placettes proviennent de `mod_sampling` : un échantillonnage GRTS conçu
pour l'**analyse systémique des 31 indicateurs**, réparti sur *l'ensemble des
peuplements*. Les réutiliser comme capteurs de santé revient à
**échantillonner le dépérissement au hasard**.

### Symptôme observé

Run FORDEAD réel (zone 2, mai 2026) : le `dieback_mask` a été persisté, mais
**« 0 alertes insérées »** — parce qu'**aucune placette systémique n'est tombée
dans un pixel de classe 3 (forte) ou 4 (sol-nu)**. Le raster « voit » le
dépérissement ; le système d'alertes, lui, ne regarde que les placettes
existantes. Information spatiale perdue.

## 2. Objectif

Découpler la **détection** (raster, exhaustive) de l'**échantillonnage**
(placettes, ciblé), et introduire un **plan d'échantillonnage de validation**
généré *à partir du raster d'alerte*, concentré sur les zones de détection —
pour le contrôle terrain (workflow QField/QGIS, garde-fous G1-G5 de l'ADR-013).

## 3. Workflow cible — échantillonnage à deux phases

Échantillonnage **adaptatif à deux phases**, standard du protocole ONF/DSF :

```
Phase 1 — DÉTECTION exhaustive
  Sentinel-2 → FAST / FORDEAD → raster d'alerte (couverture continue)
                                  • FORDEAD : dieback_mask catégoriel 0-4
                                  • FAST    : raster d'alerte continu (à créer)

Phase 2 — VALIDATION ciblée
  raster d'alerte → masque des zones d'alerte → GRTS contraint au masque
                  → plan de validation terrain (placettes alerte + témoins)
                  → export QField → contrôle au sol → mod_field_ingest (E6.c.5)
```

## 4. Deux types de plan — ne pas confondre

| | Plan **systémique** (existant) | Plan de **validation sanitaire** (nouveau) |
|---|---|---|
| Objectif | Analyse 31 indicateurs / 12 familles | Valider/caractériser le dépérissement détecté |
| Domaine | Tous les peuplements | Zones d'alerte uniquement (+ témoins) |
| Pilotage | GRTS sur l'AOI complète | GRTS contraint au masque d'alerte |
| Module | `mod_sampling` | nouveau parcours (cf. §7) |
| Déclencheur | Création de projet | Run FAST/FORDEAD abouti |

Les deux **coexistent** : le plan de validation ne remplace pas le plan
systémique.

## 5. Découpage cœur ↔ app

Règle 1 (aucune logique métier dans l'app) → la logique d'échantillonnage et la
définition « qu'est-ce qu'une zone d'alerte » vivent dans `nemeton`. L'app
orchestre, persiste, exporte, présente.

| Élément | Repo |
|---|---|
| Définition du masque d'alerte (classes, tampon) | `nemeton` |
| GRTS contraint / pondéré par le raster | `nemeton` |
| Raster d'alerte continu FAST | `nemeton` |
| Entrée dédiée « plan de validation » | `nemeton` |
| Parcours UI, persistance du plan, export QField, carte | `nemetonshiny` |

> ⚠️ Le travail cœur exige une **session de dev dédiée au dépôt `nemeton`**
> (règle 12 — une session = un repo). Cette note sert de cahier des charges.

## 6. API cœur `nemeton` à fournir

### 6.1 Masque des zones d'alerte

Helper qui dérive un masque exploitable depuis le raster d'alerte :

```r
fordead_alert_mask(dieback_mask,
                   classes  = c(3L, 4L),   # forte + sol-nu (défaut G1)
                   buffer_m = 0)           # tampon optionnel autour des foyers
# → SpatRaster binaire (1 = zone d'alerte, NA ailleurs)
```

Pourquoi cœur : le choix des classes retenues et du tampon est une décision
méthodologique (cf. seuils ONF/DSF), pas une simple manipulation `terra`.

### 6.2 Échantillonnage contraint / pondéré

Deux options — **décision cœur** :

- **Option A (minimale)** : documenter/bénir le paramètre **`forest_mask`**
  déjà présent sur `create_sampling_plan()` pour accepter un masque d'alerte
  comme domaine de candidats. Disponible immédiatement, mais sémantiquement un
  détournement (`forest_mask` = « où est la forêt »).
- **Option B (propre)** : ajouter un paramètre explicite **`priority_raster`**
  → GRTS **pondéré** par l'intensité (placer plus de placettes là où la classe
  de dépérissement est élevée, en exploitant les valeurs 0-4 plutôt qu'un
  masque binaire).

Recommandation : viser B, livrer A en transition.

### 6.3 Entrée dédiée « plan de validation »

Encapsule la méthodologie (placettes alerte + témoins) en une fonction :

```r
create_validation_sampling_plan(
  zone,                 # AOI
  alert_raster,         # dieback_mask FORDEAD ou raster FAST
  n_validation = 20L,   # placettes dans les zones d'alerte
  n_control    = 5L,    # placettes témoins en zone saine (classe 0)
  classes      = c(3L, 4L),
  seed         = NULL
)
# → sf POINT : plot_id, type ∈ {Validation, Temoin}, alert_class (0-4),
#   visit_order, geometry
```

### 6.4 Raster d'alerte FAST

FAST n'expose aujourd'hui que des alertes par placette. Pour la symétrie avec
FORDEAD, le cœur doit **persister un raster d'alerte FAST continu** (sévérité
rolling-window par pixel) et un lecteur `read_fast_alert_raster()`.

## 7. Travail app `nemetonshiny`

### 7.1 Parcours utilisateur

Action **« Générer un plan de validation terrain »** dans l'onglet **Suivi
sanitaire**, sur les sous-onglets **Carte FORDEAD** / **Carte FAST** — visible
quand un raster d'alerte existe pour la zone.

1. Clic → modale : `n_validation`, `n_control`, classes retenues (3-4 / 2-4),
   `seed`.
2. App lit le raster (`read_fordead_dieback_mask()` /
   `read_fast_alert_raster()`), appelle `nemeton::create_validation_sampling_plan()`.
3. Le plan s'affiche **superposé au raster d'alerte** sur la carte.
4. Persistance + export QField.

### 7.2 Persistance & provenance

Le plan de validation est un jeu distinct du plan systémique. Options de
stockage (décision §10) : couche dédiée dans `samples.gpkg`, fichier
`validation_samples_<ts>.gpkg`, ou table DB.

Provenance obligatoire (traçabilité, NDP/φ) : `zone_id`,
`fordead_run_id` / `mask_timestamp`, `source` ∈ {FORDEAD, FAST},
`generated_at`, `classes`, `seed`.

### 7.3 Réutilisation de l'existant

- `read_fordead_dieback_mask()` — déjà câblé (`mod_monitoring_fordead_map`).
- Export QField — `mod_sampling` le fait déjà ; migrer
  `create_qfield_project()` → `create_qgis_project()` (déprécié) au passage.
- Ingestion du retour terrain → `mod_field_ingest` (validation sanitaire
  E6.c.5) consomme déjà un GPKG terrain : le plan de validation doit produire
  un GPKG compatible.

## 8. Modèle de données

Plan de validation = `sf` POINT :

| Colonne | Type | Note |
|---|---|---|
| `plot_id` | chr | identifiant placette |
| `type` | chr | `Validation` \| `Temoin` |
| `alert_class` | int | classe 0-4 du pixel sous la placette |
| `visit_order` | int | ordre de tournée (TSP) |
| `geometry` | POINT | CRS 2154 |

+ métadonnées de provenance (§7.2) en attributs de couche / sidecar JSON.

## 9. i18n

Nouvelles clés `TRANSLATIONS` (FR/EN, section Monitoring/Sampling) : titre de
l'action, libellés de la modale, types `Validation`/`Temoin`, messages
d'erreur (« aucun raster d'alerte pour cette zone »), légende carte.

## 10. Garde-fous méthodologiques

- **Placettes témoins obligatoires** : un plan « 100 % zone d'alerte » ne
  permet pas de confirmer les vrais négatifs. `n_control > 0` par défaut,
  réparties en classe 0 (sain) — alimente les garde-fous G1-G5.
- **Domaine candidat vide** : si le masque d'alerte est vide (zone saine —
  cas réel « 0 alertes »), l'app doit le dire clairement et ne pas générer un
  plan dégénéré.
- **NDP/φ** : le plan de validation hérite du NDP du run de détection ;
  conserver la provenance pour le calcul de confiance.

## 11. Tests

- Cœur : `fordead_alert_mask()` (classes, tampon, masque vide),
  `create_validation_sampling_plan()` (comptes cibles, témoins en classe 0,
  masque vide → erreur explicite).
- App : action de génération (mock cœur), persistance + relecture,
  provenance, parcours modale, export QField. `testServer` pour le module.
- Comme pour le plan systémique (cf. v0.39.1), **ne pas coder en dur** les
  comptes exacts — assertions sur le contrat (plan non vide, types,
  provenance), pas sur l'arithmétique GRTS du cœur.

## 12. Phasage proposé

| Phase | Repo | Contenu |
|---|---|---|
| **A** | `nemeton` | `fordead_alert_mask()`, `create_validation_sampling_plan()`, GRTS contraint (option A puis B), raster FAST + `read_fast_alert_raster()` |
| **B** | `nemetonshiny` | Action « Plan de validation » (Carte FORDEAD d'abord), génération, carte superposée, persistance + provenance |
| **C** | `nemetonshiny` | Export QField du plan de validation, raccord `mod_field_ingest` (E6.c.5), extension à FAST |

Ordre imposé cœur → app (règle 11) : Phase A livrée et release `nemeton`
publiée **avant** la Phase B.

## 13. Décisions ouvertes

1. **Emplacement de l'action** : `mod_monitoring` (Carte FORDEAD/FAST) — *a
   priori* — ou nouveau mode dans `mod_sampling` ?
2. **Stockage du plan de validation** : couche dans `samples.gpkg`, fichier
   `validation_samples_<ts>.gpkg`, ou table DB monitoring ?
3. **GRTS** : option A (`forest_mask`) suffisante, ou viser d'emblée
   l'option B (`priority_raster` pondéré) ?
4. **FAST** : traiter FORDEAD d'abord et FAST en Phase C, ou les deux de
   front ?
5. **Témoins** : générés dans le même appel cœur, ou étape app séparée ?
6. **Classes d'alerte par défaut** : 3-4 (forte + sol-nu) ou 2-4 (inclure
   moyenne) ? Aligner sur le garde-fou G1.

---

*Document de travail — à valider avant ouverture de la session de dev
`nemeton` (Phase A).*

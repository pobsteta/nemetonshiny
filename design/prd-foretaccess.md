---
stepsCompleted: ['step-01-init', 'step-02-discovery', 'step-03-success', 'step-04-journeys', 'step-05-domain', 'step-06-innovation', 'step-07-project-type', 'step-08-scoping', 'step-09-functional', 'step-10-nonfunctional', 'step-11-polish', 'step-12-complete']
status: complete
inputDocuments: ['CLAUDE.md']
workflowType: 'prd'
documentCounts:
  briefsFound: 11
  briefsLoaded: 0
  research: 0
  brainstorming: 0
  projectDocs: 1
project: nemetonshiny
classification:
  projectType: web_app
  domain: scientific
  complexity: high
  projectContext: brownfield
scope: 'ForêtAccess / Terrain accessible (accessibilité + desserte + évolutions)'
---

# Product Requirements Document - nemetonshiny

**Author:** Pascal
**Date:** 2026-07-21

## Overview

Ce PRD spécifie la capacité **« Terrain accessible » (ForêtAccess)** de nemetonshiny :
cartographie de l'accessibilité d'exploitation et conception de desserte forestière, à
partir de **données publiques**, pour un **gestionnaire ONF / coopérative**. Le **MVP
est livré** (v0.109 → v0.112) ; les évolutions (câble-mât, optimiseurs, tracé manuel,
typage) sont conditionnées à un travail de **performance côté cœur `foretaccess`**.

## Success Criteria

### User Success

**Utilisateur principal : gestionnaire ONF / coopérative forestière.** À partir de
données publiques (NDP 0) et sans quitter l'application, par projet, il obtient :

- une **carte d'accessibilité d'exploitation** (débusqueur, porteur, camion DFCI)
  avec classes lisibles et provenance des sources (badge DFCI OSM/heuristique) ;
- un **réseau de desserte proposé** reliant ses parcelles au réseau existant, avec
  bilan (parcelles desservies, coût de construction, connexité) ;
- des **exports GeoPackage** réutilisables en SIG (QGIS / QField).

Moment « aha » : voir en un run où l'exploitation est possible et par où faire
passer une piste, sans commander d'étude externe.

### Business Success

La capacité **« Terrain accessible »** devient un différenciateur de la plateforme
Nemeton : elle transforme une expertise coûteuse (type Sylvaccess / bureau
d'études) en fonction intégrée. Mesure d'adoption : nombre de projets ayant lancé
au moins un run accessibilité ou desserte.

### Technical Success

- Sorties **cohérentes avec la référence Sylvaccess / ForêtAccess** (classes, coûts).
- Calculs **asynchrones sans gel de l'UI** (worker `future`, retour immédiat, cache
  projet, garde-fous mémoire).
- Intégration `foretaccess` via `@*release`, **aucune logique métier côté app**
  (règles 1/2).
- **Temps de calcul cible : < 5 min par run** sur une AOI de gestion type.

### Measurable Outcomes

- **Accessibilité terrestre** : run complet **< 5 min** (atteint : ordre de la
  minute sur l'AOI de test).
- **Desserte glouton** : **100 % des parcelles desservies** (atteint : 30/30 mesuré
  sur Chastel-Nouvel).
- **Temps desserte glouton : cible < 5 min NON ATTEINTE** aujourd'hui (~11,5 min
  mesuré sur 30 parcelles) → **dette de performance côté `foretaccess`** (brief cœur).
- **Connexité du réseau créé : à garantir** (aujourd'hui `connexe = FALSE` observé
  → dette de qualité côté `foretaccess`, sémantique à clarifier).

Le périmètre phasé (MVP / Growth / Vision) est détaillé dans la section
*Project Scoping & Phased Development*.

## User Journeys

### 1. Gestionnaire ONF/coopérative — parcours nominal (utilisateur principal)

**Camille**, gestionnaire en coopérative, doit planifier l'exploitation d'un massif
de 30 parcelles. Aujourd'hui elle commande une étude d'accessibilité externe
(semaines, coût).

- **Ouverture** : elle charge son projet, va dans *Terrain accessible ›
  Accessibilité*, coche les 3 moteurs terrestres, lance.
- **Montée** : retour immédiat (toast + chrono), le worker tourne ; en quelques
  minutes la carte se peint — classes lisibles vert→rouge, badge DFCI « réseau OSM ».
- **Climax** : elle bascule sur *Desserte*, lance le glouton ; le bilan affiche
  **30/30 desservies**, coût total, tracé du réseau proposé.
- **Résolution** : elle exporte le GeoPackage, l'ouvre dans QGIS pour son plan de
  gestion. Étude externe évitée.

### 2. Gestionnaire — cas limite (chemin d'erreur / calcul long)

Même Camille, sur un massif **sans desserte taguée DFCI** et plus étendu.

- Le camion DFCI affiche un **badge d'avertissement jaune** (sources estimées par
  heuristique) : elle sait que la couche est approximative.
- Le run desserte annonce **« calcul long »** ; elle le lance, ferme l'onglet,
  revient plus tard : le **cache** a restauré le réseau sans relancer.
- Elle voit `connexe = FALSE` : signal que le réseau proposé demande une revue
  humaine avant travaux.

### 3. ETF / bûcheron — usage terrain (utilisateur secondaire)

**Marc**, entrepreneur de travaux forestiers, reçoit le GeoPackage de Camille.

- Il l'ouvre dans **QField** sur tablette, superpose les classes d'accessibilité et
  le réseau proposé à sa position GPS.
- **Climax** : sur le terrain, il voit quelles zones sont « parcourables » vs « non
  accessibles » et par où passe la piste projetée — sans carte papier ni étude.
- Résolution : il adapte son chantier à la réalité du relief connue *avant*
  d'engager les engins.

### 4. Co-gestionnaire — lecture seule (accès concurrent)

**Sofia**, collègue de Camille, ouvre le **même projet déjà verrouillé** en édition.

- Elle voit les résultats accessibilité/desserte **en lecture** (cache rechargé),
  mais toute tentative de lancer un run est **bloquée** (`deny_if_readonly`) avec un
  message clair.
- Résolution : elle consulte sans risque d'écraser le travail de Camille ; le verrou
  garantit la cohérence multi-utilisateurs.

### Journey Requirements Summary

Ces parcours révèlent les capacités requises :

- **Sélection de moteurs + lancement async** (worker `future`, retour immédiat,
  chrono) — J1, J2.
- **Rendu carte lisible** (classes sémantiques, provenance des sources, badges de
  bilan) — J1, J2.
- **Cache projet + opt-in « calcul long »** (survivre à la fermeture d'onglet) — J2.
- **Export GeoPackage interopérable** (QGIS / QField) — J1, J3.
- **Garde-fou perf < 5 min** (aujourd'hui non tenu par le glouton) — J2 (dette cœur).
- **Signalement qualité** (badge DFCI heuristique, `connexe = FALSE`) — J2.
- **Verrou d'édition / lecture seule** (`deny_if_readonly`) — J4.

## Domain-Specific Requirements

### Compliance & Regulatory

- **Licences** (ADR-006) : EUPL v1.2 (plateforme) + MIT (paquets R) + CC-BY 4.0
  (données). ForêtAccess = réimplémentation de Sylvaccess (INRAE) : respecter les
  licences amont (noyau Rust + FORDEAD GPL-3 côté santé).
- **Souveraineté & données publiques** : capacité en **NDP 0** — sources publiques
  uniquement (IGN RGE ALTI HIGHRES, BD TOPO V3, BD Forêt V2, OSM `ref:FR:DFCI`).
  Provenance tracée et affichée (badge DFCI).
- **Interopérabilité normative** (ADR-008) : OGC, **ETRS89 / EPSG:3035** en cible
  paneuropéenne, INSPIRE, sources par pays.

### Technical Constraints

- **Reproductibilité scientifique** : sorties cohérentes avec la référence
  Sylvaccess / ForêtAccess. `Remotes: @*release` casse la reproductibilité pure dans
  le temps → `renv::snapshot()` côté projet utilisateur.
- **Stockage** (ADR-002) : rasters / LiDAR **jamais en DB** — seulement agrégats ;
  artefacts en **cache disque projet**.
- **Performance** : cible **< 5 min/run** ; asynchrone `future` / `promises`,
  garde-fous mémoire worker.
- **Sécurité** (ADR-005 / règle 8) : OAuth2/OIDC (AgentConnect/ProConnect, eIDAS) ;
  **aucun secret en dur** (variables d'environnement).

### Integration Requirements

- `foretaccess (>= 1.5.0)` via `@*release` ; acquisition IGN (`happign` WMS) + OSM.
- **Toolchain Rust** embarquée (Dockerfile / CI) pour le noyau ForêtAccess.
- Interop SIG : **GeoPackage** (QGIS / QField) ; passerelles IFN / GroundForest
  (contexte Interopérabilité).

### Risk Mitigations

- **Régression release cœur** (`@*release` tire la dernière release) → ne publier
  `nemeton` / `foretaccess` qu'avec **CI verte** + patch cœur rapide.
- **Sorties « optimistes » / non connexes** → **signalement UI** (badges heuristique
  DFCI, `connexe = FALSE`) + revue humaine avant travaux.
- **Perf hors cible < 5 min** → opt-in « calcul long » + cache + **brief perf cœur**.
- **Reproductibilité** dans le temps → `renv::snapshot()`.

## Innovation & Novel Patterns

### Detected Innovation Areas

Innovation **d'intégration et de livraison**, non de rupture : Sylvaccess
(algorithmes établis, INRAE) rendu **interactif, par projet, sur données publiques
(NDP 0)**, dans une plateforme systémique (31 indicateurs, NDP, pondération nombre
d'or). **Noyau Rust** (`foretaccess`) intégré à un pipeline **R/Shiny asynchrone**
(worker `future` + cache, toolchain Rust en CI).

### Market Context & Competitive Landscape

Alternatives : Sylvaccess (outil desktop / recherche, INRAE), bureaux d'études
(prestation sur devis, délais). Différenciateur : **intégré, souverain FR, sur
données publiques, sans étude externe**, et articulé aux autres familles
d'indicateurs de la plateforme.

### Validation Approach

Cohérence des sorties avec la référence Sylvaccess / ForêtAccess ; validation
**terrain** (aller-retour QField / QGIS) prévue en vision.

### Risk Mitigation

Innovation à **faible risque** (algorithmes éprouvés) ; le risque réel est le
**passage à l'échelle / la performance** des moteurs lourds (déjà tracé — briefs
cœur, cible < 5 min).

## Web App Specific Requirements

### Project-Type Overview

Application **Shiny/golem** mono-page, rendue serveur, orientée session ;
`navbarPage` avec sous-onglets dynamiques ; calculs longs en **asynchrone**
(`future` / `promises`, `ExtendedTask`).

### Technical Architecture Considerations

- **SPA/MPA** : SPA de fait (Shiny réactif, une page, onglets ; pas de routing
  multi-pages).
- **Browser matrix** : navigateurs modernes (Bootstrap 5 / bslib) ; pas d'IE.
- **Temps réel** : oui — worker `future` + `future_promise`, progression **live**
  (toasts + chrono), mises à jour carte via `leafletProxy` ; transport WebSocket Shiny.
- **Performance targets** : **< 5 min/run**, **UI non bloquante** (retour immédiat),
  cache disque projet, garde-fous mémoire worker.
- **Responsive design** : layout `bslib` (sidebars / cartes) **orienté desktop pro**
  (cartographie, non mobile-first) ; le terrain est couvert par **QField** (export GPKG).
- **Accessibility level** : i18n **FR/EN**, couleurs de boutons **sémantiques**
  (hiérarchie d'action) ; **WCAG non audité** formellement → piste d'amélioration.

### SEO Strategy

**N/A** — application **authentifiée** (OAuth2/OIDC), non destinée à l'indexation.

### Implementation Considerations

Structure golem (`mod_*` / `service_*`), i18n maison (`\uXXXX`), Leaflet + map panes
dédiés, DT, plotly ; règles 1/2 (aucune logique métier hors `nemeton` / `foretaccess`).

## Project Scoping & Phased Development

### MVP Strategy & Philosophy

**Approche : problem-solving MVP** — le minimum pour qu'un gestionnaire ONF/coop dise
« c'est utile » : accessibilité terrestre + réseau de desserte proposé, sur **données
publiques**, sans étude externe. **Déjà livré** (v0.109 → v0.112).
**Ressources** : **développeur unique** (repo app), dépendant des releases cœur
`foretaccess` / `nemeton` via `@*release`.

### MVP Feature Set (Phase 1) — livré

**Parcours couverts** : J1 (nominal), J2 (cas limite), J3 partiel (export → QField),
J4 (lecture seule via verrou).
**Capacités must-have** : 3 moteurs terrestres (skidder / porteur / DFCI) + classes
de débardage + **desserte glouton** + worker async / cache / badges + export
GeoPackage + i18n FR/EN.

### Post-MVP Features

**Phase 2 (Growth)** : câble-mât, Steiner + optimiseurs, tracé manuel (waypoints),
typage du réseau, surcoût eau. **Bloqué sur la perf cœur.**
**Phase 3 (Expansion)** : vitesse **interactive** (< 5 min y compris optimiseurs),
**validation terrain** QField / QGIS, **multi-pays** (ETRS89 / EPSG:3035, INSPIRE).

### Risk Mitigation Strategy

- **Technique** : perf des moteurs lourds (glouton 11,5 min, câble > 1 h) +
  `connexe = FALSE` → opt-in « calcul long » + cache **maintenant** ; **briefs cœur**
  (perf, connexité, places de dépôt câble) pour lever le blocage.
- **Marché** : confiance vs bureau d'études → **sorties alignées sur la référence
  Sylvaccess** + export SIG vérifiable.
- **Ressources** : dev solo + cadence des releases cœur → `@*release` (propagation
  auto, pas de bump `Remotes:`) + briefs transmis à une session cœur dédiée.

## Functional Requirements

### Accessibilité d'exploitation

- **FR1** : Le gestionnaire peut cartographier l'accessibilité d'exploitation
  **terrestre** de ses parcelles (débusqueur, porteur, camion DFCI) à partir de
  données publiques.
- **FR2** : Le gestionnaire peut **sélectionner** le sous-ensemble de moteurs à calculer.
- **FR3** : Le gestionnaire peut obtenir les **classes de distance de débardage**.
- **FR4** : Le gestionnaire peut définir une **zone tampon** autour des parcelles pour
  l'emprise d'analyse.

### Création de desserte

- **FR5** : Le gestionnaire peut **générer un réseau de desserte** reliant ses parcelles
  au réseau existant, au moindre coût de construction.
- **FR6** : Le gestionnaire peut consulter le **bilan** du réseau créé (parcelles
  desservies, coût, connexité).

### Cartographie & visualisation

- **FR7** : Le gestionnaire peut visualiser les couches calculées sur une **carte
  interactive** (fonds OSM / satellite).
- **FR8** : Le gestionnaire peut choisir la **couche affichée** et régler son **opacité**.
- **FR9** : Le gestionnaire peut **afficher / masquer** parcelles, desserte et couches
  résultat.
- **FR10** : Le gestionnaire peut lire une **légende traduite** des classes.

### Exécution & cache

- **FR11** : Le gestionnaire peut lancer un calcul long **sans bloquer l'interface**
  (retour immédiat + progression).
- **FR12** : Le gestionnaire peut **retrouver un résultat déjà calculé** sans relancer
  (cache par projet).
- **FR13** : Le système **avertit** qu'un moteur est un « calcul long » avant lancement.
- **FR14** : Le système **empêche** deux calculs simultanés du même type.

### Interopérabilité & export

- **FR15** : Le gestionnaire peut **exporter** les résultats en **GeoPackage**
  réutilisable en SIG.
- **FR16** : L'ETF peut consulter les résultats exportés **sur le terrain**
  (QField / QGIS).

### Qualité, provenance & garde-fous

- **FR17** : Le système indique la **provenance** des sources DFCI (OSM vs heuristique).
- **FR18** : Le système **signale la connexité** du réseau créé.
- **FR19** : Le système restitue des sorties **cohérentes avec la référence**
  Sylvaccess / ForêtAccess.

### Accès & collaboration

- **FR20** : Le co-gestionnaire peut consulter en **lecture seule** un projet verrouillé.
- **FR21** : Le système **bloque les runs** en mode lecture seule (verrou d'édition).

### Capacités d'évolution (Phase 2 — Growth)

- **FR22** *(Growth)* : Le gestionnaire peut calculer le **potentiel d'accès au
  câble-mât**.
- **FR23** *(Growth)* : Le gestionnaire peut générer une desserte **optimisée**
  (Steiner / optimiseurs).
- **FR24** *(Growth)* : Le gestionnaire peut **tracer manuellement** une route via des
  points de passage.
- **FR25** *(Growth)* : Le gestionnaire peut **typer le réseau** (primaire / secondaire
  / tertiaire / temporaire).
- **FR26** *(Growth)* : Le système intègre les **surcoûts de franchissement** (pont /
  buse) au coût de construction.

## Non-Functional Requirements

### Performance

- **NFR-P1** : Un run d'accessibilité terrestre se termine en **< 5 min** sur une AOI
  de gestion type.
- **NFR-P2** : L'UI reste **réactive pendant tout calcul** (retour < 1 s, progression
  rafraîchie chaque seconde).
- **NFR-P3** : Un résultat déjà calculé se **recharge depuis le cache en < 5 s**
  (aucun recalcul).
- **NFR-P4** *(écart connu)* : Le glouton desserte **dépasse** la cible (~11,5 min /
  30 parcelles) → mitigé par opt-in + cache, perf à traiter côté cœur.

### Security

- **NFR-S1** : Authentification **OAuth2/OIDC** (Keycloak / AgentConnect) ; session
  anonyme en profil citoyen jusqu'au login.
- **NFR-S2** : **Aucun secret en dur** — OAuth / LLM / DB en variables d'environnement.
- **NFR-S3** : Écriture **bloquée en lecture seule** (verrou projet) pour l'intégrité
  multi-utilisateurs.

### Scalability

- **NFR-SC1** : Cible **≤ 50 utilisateurs simultanés** sur l'archi golem / Shiny
  (ADR-001) ; au-delà, migration prévue (Plumber + Vue.js).
- **NFR-SC2** : Rasters / LiDAR **jamais persistés en DB** (ADR-002) — cache disque par
  projet, seuls les agrégats en base.

### Accessibility

- **NFR-A1** : Interface **bilingue FR/EN** intégrale (aucun littéral non traduit).
- **NFR-A2** : Couleurs de boutons **sémantiques** ; conformité **WCAG non garantie**
  (backlog).

### Integration

- **NFR-I1** : Consommation de `foretaccess (>= 1.5.0)` via `@*release` ; compatibilité
  maintenue avec la dernière release cœur.
- **NFR-I2** : Acquisition via **IGN (WMS) et OSM** ; **dégradation gracieuse** (repli,
  erreurs structurées) si un service est indisponible.
- **NFR-I3** : Exports **GeoPackage OGC** ouvrables sans perte dans QGIS / QField.

### Reliability

- **NFR-R1** : Tout échec de calcul renvoie une **erreur structurée** (message i18n),
  jamais un crash de session.
- **NFR-R2** : Les workers `future` **libèrent la mémoire** en fin de run ; le cache
  **survit** à un redémarrage de session.

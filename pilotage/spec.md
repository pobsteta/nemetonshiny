# nemetonshiny — spec « pilotage par IA » (proposée)

> Rédigée par Claude (orchestrateur) le 2026-10-24 à partir de `pilotage/brief.md`.
> **Statut : validée par Pascal le 2026-11-09 (porte n° 1).** Les points marqués
> **[question]** ont été tranchés le 2026-11-09 (voir la réponse sous chacun).

## 1. Parcours utilisateur

Les gestes sont faits par un agent : AIGORA, VICTOR, ou plus tard claude.ai par le
connecteur distant. Ils passent tous par `nemetonmcp`, qui appelle nemetonshiny. Le
critère commun est celui de Pascal : **l'utilisateur ne se répète pas**.

| # | L'utilisateur dit (une fois) | Ce que nemetonshiny doit rendre possible |
|---|---|---|
| P1 | « Où en est Dabo ? » | Le projet est trouvé même si le nom est approximatif (« Couchet » donne Couchey) ou ambigu ; dans ce cas, une seule question qui liste les candidats. Le résumé (score global, 12 familles, NDP, UGF, date de calcul) est complet en un appel, avec les heures en ISO 8601 et leur fuseau. |
| P2 | « Lance le calcul de Couchey », puis « où en est-il ? » | Le calcul démarre en moins de 10 s. L'état rendu (avancement, durée, fin estimée, échec avec fin du log) suffit sans relance. Les refus (calcul déjà lancé, verrou tenu par X) disent quoi faire. |
| P3 | « Fais-moi le rapport » / « exporte le GeoPackage » | Les outils longs ne dépassent jamais 600 s par appel : ils sont détachés comme le calcul s'ils peuvent être longs. Le fichier produit est désigné par un chemin, ou par un lien côté `nemetonmcp`. |
| P4 | « Applique les UGF ONF » / « croise avec l'ONF en purgeant » | **Premier appel : rien n'est écrit.** L'outil rend ce qui serait modifié (n parcelles retirées, n UGF remplacées) et un jeton de confirmation. Second appel avec le jeton : l'écriture a lieu. Sans jeton valide, l'outil refuse. |
| P5 | « Ouvre la synthèse de Dabo » | Un lien profond `?project=…&tab=…` valide (déjà livré). |
| P6 | Une erreur, quelle qu'elle soit | Une erreur MCP marquée comme telle (`isError`), classée (`projet_introuvable`, `projet_verrouille`, `calcul_en_cours`, `confirmation_requise`…) dans la langue demandée, avec l'action suivante proposée. |

## 2. Données manipulées

- Les projets sur disque (`project_dir`) : métadonnées, parcelles, UGF, résultats,
  plan d'actions, état du calcul (`compute_job.json`).
- La base PostgreSQL/PostGIS quand elle est configurée (`NEMETON_DB_URL`…) : projets
  synchronisés, verrou multi-utilisateurs, monitoring.
- Aucune donnée nouvelle : seuls la forme des sorties, les erreurs et les gardes
  changent.

**[question] Q1 — Quel est le périmètre de la confirmation ?**
- Lecture stricte de l'arbitrage 4 : confirmation pour toute écriture PostgreSQL/PostGIS.
- Or les outils MCP écrivent aujourd'hui surtout **sur disque** (UGF, parcelles, plan
  d'actions), et la base est synchronisée ensuite par l'app.
- Proposition : confirmer **toute modification d'un projet existant** (disque ou base),
  et laisser libres la lecture, le lancement d'un calcul et les exports.
- **Réponse de Pascal (2026-11-09)** : la proposition est retenue.

## 3. Le contrat avec `nemetonmcp`

`nemetonmcp` appelle par `getFromNamespace` 31 objets internes de nemetonshiny :

- **plan d'actions** : `load_action_plan`, `save_action_plan`, `add_action_to_plan`,
  `update_action_in_plan`, `delete_action_from_plan`, `get_action_by_id`,
  `find_action_index`, `action_plan_annee_base`, constantes `ACTION_PLAN_*` ;
- **projet** : `load_project`, `load_parcels`, `get_project_path`, `get_projects_root`,
  `ug_build_sf`, `load_ug_comments`, `lock_status` ;
- **communes** : `get_commune_geometry`, `save_commune_geometry`, `get_departments` ;
- **MCP** : `mcp_tools`, `mcp_etat_calcul`, `mcp_exporter_gpkg`, `.mcp_resolve_project` ;
- **divers** : `get_expert_profiles`, `marculus_export_bundle`,
  `MARCULUS_CONTEXT_ACTION_TYPES`, `INDICATOR_FAMILIES`.

La forme du contrat est à trancher par ADR (ADR 0001). Quelle qu'elle soit, **un test
de nemetonshiny vérifie la présence et la signature de chaque objet du contrat.** Un
renommage fait échouer la CI d'ici, pas VICTOR en production.

## 4. Hors périmètre

Voir `pilotage/brief.md`, « Ce qu'il ne fait pas ». Ce dépôt n'écrit ni dans
`nemetonmcp`, ni dans `aigora`, ni dans `victor`, ni dans `nemeton` : il leur émet des
briefs.

## 5. Critères d'acceptation

1. **Noms dictés** : une liste de transcriptions approximatives de projets réels
   (fixture) donne le bon projet, ou une liste de candidats, à 100 %. Aucun
   « introuvable » quand un candidat est à distance ≤ 2.
2. **Résumé en un appel** : `resume_projet` rend tout ce qu'affiche l'onglet Synthèse,
   à écart 0 (critère déjà au brief VICTOR), plus les heures ISO 8601 avec fuseau.
3. **Erreurs** : chaque outil, mis en échec, rend une erreur MCP marquée, classée, FR/EN,
   avec une action proposée. Vérifié par un test par classe d'erreur.
4. **Confirmation** : chaque outil qui écrit (périmètre selon Q1), appelé sans jeton,
   n'écrit rien (vérifié sur disque et en base de test) et rend l'aperçu. Avec un jeton
   valide, il écrit. Un jeton périmé ou réutilisé est refusé.
5. **Annotations** : chaque outil porte `read_only_hint`, `destructive_hint` et
   `idempotent_hint` justes, visibles dans `tools/list` (sous réserve que mcptools les
   transmette, à vérifier).
6. **Durée** : aucun outil synchrone ne dépasse 600 s sur le plus gros projet de test.
   `generer_rapport` et `croiser_onf` sont mesurés ; détachés s'ils dépassent 60 s.
7. **Contrat** : un test échoue si un objet du contrat `nemetonmcp` disparaît ou change
   de signature. `CONTRAT.md` et `inst/mcp/README.md` disent vrai (10 outils, purge de
   `croiser_onf` déclarée).
8. **Non-régression** : `devtools::check()` sans warning ; les tests existants restent
   verts sans retouche ; couverture des fichiers touchés ≥ celle du paquet.
9. **Mesure de Pascal** : sur une liste de dix demandes types (P1 à P6), faite dans
   AIGORA avant et après, le nombre de relances (« je répète », « non, Dabo ») baisse.
   Cette liste est à écrire avec Pascal.

## 6. Besoins en référentiels (pour l'`acquereur`)

- La spécification MCP 2025-11-25 : erreurs (`isError`), annotations, confirmation
  (elicitation), tâches longues (`execution.taskSupport`).
- Le comportement réel de mcptools 1.0.3 : transmet-il les annotations ellmer ?
  transforme-t-il une erreur R en `isError` ? supporte-t-il l'elicitation ?
- Un corpus de transcriptions vocales réelles de noms de forêts et de communes (VICTOR),
  pour la fixture du critère 1.

## 7. Briefs à émettre vers les autres dépôts

- **`nemetonmcp`** : contrat adopté (ADR 0001) ; propagation du jeton de confirmation ;
  dédoublonnage des noms d'outils ; renommage `nemetonclaude` → `nemetonmcp` incomplet.
- **`aigora`** : le lien `aigora-nemeton/.claude/skills/nemeton-vues` pointe vers
  `~/dev/nemetonclaude/`, qui n'existe plus depuis le renommage (vérifié : lien cassé) ;
  idem pour le `CLAUDE.md` de l'espace ; retirer des `permissions.allow` les outils
  soumis à confirmation, ou s'en remettre au jeton.
- **`victor`** : consigne pour relayer une demande de confirmation à la voix.

## 8. Décisions structurantes (ADR proposées dans `pilotage/adr/`)

| ADR | Décision à prendre |
|---|---|
| 0001 | Forme du contrat nemetonshiny → nemetonmcp |
| 0002 | Mécanisme de confirmation des écritures |
| 0003 | Ouverture à tous les utilisateurs par Claude : souveraineté des données |
| 0004 | Outils longs : détacher ou garder synchrone |

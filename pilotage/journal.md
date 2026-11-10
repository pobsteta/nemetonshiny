# Journal de nemetonshiny

> **Règle du sommier : on n'efface pas, on ajoute.**
> Chaque inscription est datée et signée (agent ou personne). Une correction est une
> nouvelle ligne qui renvoie à l'ancienne. Un hook refuse toute modification qui
> supprimerait ou réécrirait une inscription existante.
>
> Format : `AAAA-MM-JJ · auteur · mission · ce qui a été fait / décidé / bloqué`
>
> **Le carnet des écarts** (fiche n° 3), en une ligne marquée `écart` :
> `AAAA-MM-JJ · auteur · mission · écart · attendu : … · vu : … · vérifié : comment (ou « non ») · décision : …`
> La décision est l'une des trois : **garder** (vérifié : on adopte), **creuser** (prochain
> essai, daté), **abandonner** (vrai hasard : on le note).
> Règle : on ne sanctionne pas un écart signalé ; on regarde d'abord, on corrige ensuite,
> s'il le faut.
>
> **Les changements de cap** (fiche n° 13), en une ligne marquée `cap`, signée par qui l'a
> décidé : `AAAA-MM-JJ · auteur · mission · cap · ancien → nouveau · pourquoi : … · fait : …`

2026-10-24 · Claude (orchestrateur) · — · méthode installée (`/imperatrice:demarrer`, plugin imperatrice 0.6.0) : `docs/brief.md`, `pilotage/` (équipe, kanban, journal, missions/) et `docs/adr/` créés ; `CLAUDE.md` existant laissé tel quel. Reste à remplir : `docs/brief.md` (ma réponse d'abord, problème, coût complet, gestes terrain, produits, hors périmètre, contraintes) ; décider où mettre les rubriques orchestrateur absentes du `CLAUDE.md` ; `.claude/test-command` à écrire au `/brief`.
2026-10-24 · Claude (orchestrateur) · — · décision de Pascal : brief et ADR de la méthode placés dans `pilotage/` (`docs/` est la sortie pkgdown ignorée par git) ; section « Pilotage (méthode de l'Impératrice) » ajoutée au `CLAUDE.md` ; brief pré-rempli par Claude à la demande de Pascal, sauf « Ma réponse d'abord ».
2026-10-24 · Pascal (inscrit par Claude, mot pour mot) · — · « Ma réponse d'abord » écrite dans `pilotage/brief.md` avant toute critique : optimiser nemetonshiny pour répondre au MCP victor et aigora ; pourquoi : offrir aux utilisateurs les nouvelles interfaces comme l'IA. Contradicteur lancé sur le brief.
2026-10-24 · contradicteur (inscrit par l'orchestrateur) · — · écart · attendu : brief centré sur le pilotage IA (réponse de Pascal) · vu : corps du brief = app entière ; MCP réel servi par nemetonclaude (22 outils, 41 appels .ns() à des internes de nemetonshiny via getFromNamespace, hors CONTRAT.md) ; lots B et C faits, aucune demande MCP ouverte depuis le 2026-10-12 ; 22 outils autorisés en bypassPermissions dont appliquer_ugf ; README MCP périmé (8 outils contre 10 ; « aucun outil ne supprime » faux pour croiser_onf purger) · vérifié : lecture des dépôts aigora-nemeton, victor, nemetonclaude et briefs/traites/INDEX.md ; getFromNamespace, serveur nemetonclaude et purger recontrôlés par l'orchestrateur · décision : à prendre par Pascal
2026-10-24 · Claude (orchestrateur) · — · arbitrages de Pascal reportés mot pour mot dans `pilotage/brief.md` (porteur MCP : nemetonmcp ; critère : moins se répéter dans aigora ; pour tous les utilisateurs ; confirmation dès qu'on touche à PostgreSQL/PostGIS) ; brief recentré sur le pilotage par IA (version « app entière » remplacée, cf. lignes précédentes) ; `pilotage/spec.md` et ADR 0001-0004 proposées ; `.claude/test-command` = devtools::test() (local, `.claude/` ignoré par git) ; constaté : lien `aigora-nemeton/.claude/skills/nemeton-vues` cassé depuis le renommage nemetonclaude → nemetonmcp. ADR soumises au contradicteur.
2026-11-09 · Pascal (inscrit par Claude) · — · porte n° 1 : spec validée ; Q1 = confirmer toute modification d'un projet existant (disque ou base), lecture/calcul/exports libres ; ADR 0001 = contrat déclaré sans export (CONTRAT.md + test présence/signature) ; ADR 0002 = jeton en deux temps côté serveur ; ADR 0004 = mesurer puis détacher au-delà de 60 s. ADR 0003 (souveraineté, plateforme) laissée proposée : ne bloque pas le code de ce dépôt.
2026-11-09 · Claude (orchestrateur) · — · acquisition faite par l'orchestrateur (lecture du namespace mcptools 1.0.3) : annotations ellmer transmises (`tool_annotations_as_json`) ; `isError` posé seulement si l'outil rend un `ellmer::ContentToolResult` avec `@error` ; une erreur R levée devient une erreur JSON-RPC -32603 ; pas d'elicitation. Aujourd'hui `.mcp_call` rend `ok:false` dans un résultat ordinaire (ni l'un ni l'autre).
2026-11-09 · Claude (orchestrateur) · — · `/imperatrice:mission` : 13 missions proposées (pilotage/missions/001-013), équipe et kanban (bac à sable) mis à jour ; manquent : 012 non staffée (attend la mesure 011), liste des dix demandes types du critère 9 à écrire avec Pascal. En attente du « oui » de Pascal (porte n° 2).
2026-11-10 · hook tests_obligatoires · — · arrêt accepté au second essai sans tests verts (délai de 630 s dépassé, `Rscript -e 'devtools::test()'`) : mission non vérifiée, à reprendre.
2026-11-10 · Pascal (inscrit par Claude, mot pour mot) · — · porte n° 2 : « Oui vas y » ; équipe et 13 missions validées telles que proposées le 2026-11-09.
2026-11-10 · Claude (orchestrateur) · — · écart · attendu : `.claude/test-command` (suite complète) tient dans le délai du hook de fin de tour · vu : délai de 630 s dépassé, tests non vérifiés · vérifié : message du hook imperatrice · décision : garder — commande restreinte au périmètre MCP (`devtools::test(filter = "mcp")`) accordée par Pascal ; la suite complète reste la porte de la CI au moment de la PR.

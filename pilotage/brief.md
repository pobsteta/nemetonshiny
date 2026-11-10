# nemetonshiny — brief

> Rédigé par Claude (orchestrateur) à la demande de Pascal ; critiqué par le
> `contradicteur` le 2026-10-24 ; recentré le même jour sur la réponse de Pascal et ses
> quatre arbitrages (ci-dessous). La version précédente, qui décrivait l'app entière, est
> tracée dans `pilotage/journal.md`.

## Ma réponse d'abord (fiche n° 15)

> il faut optimiser nemetonshiny pour répondre au MCP victor et aigora
>
> Pourquoi : il est nécessaire d'offrir aux utlisateurs les nouvelles interfaces comme l'IA pour ça
>
> — Pascal, le 24 octobre 2026, écrit avant de lire la critique du contradicteur.

## Arbitrages de Pascal (2026-10-24, après la critique)

Mot pour mot :

1. Porteur du MCP : « c'est le projet nemetonmcp son nom ».
2. Critère de réussite : « moins j'aurais à me répéter mieux ce sera dans aigora ».
3. Pour qui : « pour tous les utilisateurs ».
4. Outils qui écrivent : « non il faut confirmer dès qu'on touche à la base de données
   postgres/postgis ».

## Le problème, et pour qui (Abstraction)

- **Problème** : piloter Nemeton par une IA (AIGORA dans un terminal, VICTOR à la voix)
  passe par le serveur MCP de `nemetonmcp`. Ce serveur reprend en bloc les outils MCP
  de nemetonshiny, mais aussi une quarantaine de ses **fonctions internes**, que rien ne
  garantit (`CONTRAT.md` ne couvre que les 9 exports). Les réponses obligent
  l'utilisateur à se répéter :
  - un nom de forêt dicté de travers donne « introuvable » ;
  - les erreurs arrivent comme des résultats ordinaires ;
  - les outils longs dépassent le délai d'une tâche ;
  - le contexte se perd.

  Enfin, les outils qui écrivent s'exécutent sans confirmation, alors que VICTOR tourne
  en `bypassPermissions`.
  Constaté par Pascal, seul utilisateur réel aujourd'hui (poste local, stdio).
- **Bénéficiaires** : d'abord Pascal, dans AIGORA et VICTOR ; ensuite **tous les
  utilisateurs** de Nemeton (gestionnaires, propriétaires, élus…), qui y accéderont par
  le connecteur distant de `nemetonmcp`.
- **Coût complet aujourd'hui** : inconnu (non renseigné par Pascal).

## Ce que fait nemetonshiny dans cette chaîne

- **En une phrase** : nemetonshiny fournit à `nemetonmcp` une API stable, documentée et
  testée, qui répond du premier coup, et qui refuse d'écrire en base PostgreSQL/PostGIS
  sans confirmation.
- **La chaîne** :
  ```
  VICTOR (voix) ─┐
  AIGORA (Claude Code) ─┼─> nemetonmcp (serveur MCP, paquet R nemetonclaude ; local stdio
  claude.ai (distant) ─┘     ou distant HTTP + Keycloak)
                               └─> nemetonshiny : API publique + outils mcp_*   ← ce dépôt
                                     └─> nemeton (cœur, inchangé)
  ```
- **Les gestes visés** (formulés par l'utilisateur, une fois) :
  - « où en est Dabo ? » ;
  - « lance le calcul de Couchey », puis « où en est le calcul ? » ;
  - « fais-moi le rapport » ;
  - « exporte le GeoPackage » ;
  - « applique les UGF ONF », avec confirmation ;
  - « ouvre la synthèse ».
- **Ce qu'il produit** : des réponses structurées (JSON) complètes et sans ambiguïté, des
  erreurs classées qui disent quoi faire, une demande de confirmation explicite avant
  toute écriture en base, et un contrat qui casse bruyamment (CI) plutôt qu'en production.

## Ce qu'il ne fait pas (hors périmètre de ce brief)

- Le serveur MCP, les vues (Atlas, Calcul, Plan), le connecteur distant et son
  authentification : dépôt `nemetonmcp`.
- Les skills, la mémoire de conversation et les permissions : dépôt `aigora`, espace
  `aigora-nemeton`.
- La voix, les consignes et les délais : dépôt `victor`.

  Ces trois dépôts reçoivent des briefs émis d'ici (`briefs/vers-…`), puisqu'on
  n'écrit qu'un dépôt par session.
- L'interface Shiny, le terrain (QField, Marculus, Groma), les perspectives IA de l'app
  (Mistral) : inchangés par ce chantier.
- La vitesse des calculs eux-mêmes (R1 feu, LiDAR) : elle relève du cœur `nemeton`.
- Aucune logique métier nouvelle (règle stricte n° 1).

## Contraintes connues

- **Contrat** : ce que `nemetonmcp` consomme devient un contrat versionné ; une rupture
  va dans « Changements cassants » de NEWS (versionnage `AAAA.M.N`).
- **Confirmation** : toute écriture PostgreSQL/PostGIS demandée par un agent exige une
  confirmation (arbitrage 4).
- **Spécification MCP** (2025-11-25) :
  - un humain dans la boucle pour les opérations sensibles ;
  - annotations `readOnly` et `destructive` ;
  - `isError` pour les erreurs d'exécution ;
  - noms d'outils uniques.
- **Tâches** : une tâche VICTOR est coupée à 600 s ; un calcul complet dure plus d'une
  heure, d'où le détachement.
- **Souveraineté** : ouvrir l'accès à tous les utilisateurs par Claude fait partir les
  données de projet vers Anthropic, alors que l'ADR-004 privilégie Mistral. Ce sacrifice
  demande une ADR.
- **Sécurité** : le local reste sur `127.0.0.1`, le distant passe derrière Keycloak
  (ADR-005), et aucun secret n'est dans le code.
- **Langue** : FR/EN pour tout ce qui est rendu à l'utilisateur.
- Les contraintes générales de l'app (R, golem, CI, licences) sont dans `CLAUDE.md`.

## Référentiels et conventions (en lien, pas en copie)

- Spécification MCP : https://modelcontextprotocol.io/specification/2025-11-25/server/tools
- ellmer, `tool_annotations()` : https://ellmer.tidyverse.org/reference/tool_annotations.html
- Contrat public actuel : `CONTRAT.md` ; outils : `inst/mcp/README.md`, `R/service_mcp.R`,
  `R/api.R`.
- Brief antérieur : `specs/BRIEF-pilotage-victor-aigora.md` (lot A livré).
- Consommateur : `/home/pascal/dev/nemetonmcp/r/R/` (`utils.R:40`, `outils.R`, `plan.R`,
  `projet.R`, `atlas.R`, `distant.R`).
- Convention NMT, i18n, règles strictes, ADR 001–013 : `CLAUDE.md` et `platform_nemeton/docs/`.

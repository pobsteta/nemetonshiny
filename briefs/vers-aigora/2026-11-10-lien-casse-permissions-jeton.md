# Brief `aigora` (espace `aigora-nemeton`) — lien cassé, renommage incomplet, permissions et jeton de confirmation

> **Déposé le 2026-11-10** par la session `nemetonshiny` (mission 013, `pilotage/spec.md` § 7).
> **Décrit ce qui est livré sur `main` de nemetonshiny** (`1af92486`). Rien n'a été écrit dans `aigora`.
> **Portée** : `aigora-nemeton/.claude/skills/nemeton-vues`, `CLAUDE.md`, `.claude/settings.json`,
> `.claude/skills/ksuite-setup/scripts/register_mcp.py`, `.claude/skills/vues-victor/SKILL.md`.
> Constats relevés le 2026-11-10 dans `/home/pascal/dev/aigora/aigora-nemeton` (lecture seule).

## 1. Lien `nemeton-vues` cassé (vérifié)

`.claude/skills/nemeton-vues` est un lien symbolique vers `/home/pascal/dev/nemetonclaude/skills/nemeton-vues`.
`/home/pascal/dev/nemetonclaude` **n'existe plus** depuis le renommage du paquet en `nemetonmcp` (0.8.0).
Le skill est donc introuvable (`grep` dans `.claude/skills/nemeton-vues/` : « No such file or directory »).
Le contenu existe à `/home/pascal/dev/nemetonmcp/clients/claude/skills/nemeton-vues`.

À faire :
- repointer le lien vers `~/dev/nemetonmcp/clients/claude/skills/nemeton-vues` ;
- corriger `CLAUDE.md` (ligne du skill `nemeton-vues`, qui cite `~/dev/nemetonclaude/skills/nemeton-vues` et
  ses chemins `vues/...`) et `.claude/skills/vues-victor/SKILL.md` (lignes 9-10, mêmes chemins). Je n'ai pas
  vérifié que l'arborescence `vues/...` annoncée existe à l'identique sous `clients/claude/` : à contrôler.

## 2. Renommage `nemetonclaude` → `nemetonmcp` incomplet

Le lien n'est pas le seul reste. Le **lancement du serveur** utilise encore l'ancien nom :

- `.claude/skills/ksuite-setup/scripts/register_mcp.py:104` : `Rscript -e "nemetonclaude::serveur_mcp()"` ;
  lignes 64-66 : commentaire et clé `nemetonclaude (R package)` ;
- `CLAUDE.md` (section MCP) : « `nemeton` runs `Rscript -e "nemetonclaude::serveur_mcp()"`
  (nemetonclaude >= 0.5.0) ».

`nemetonmcp` exporte `serveur_mcp()` (`NAMESPACE`) sous son nouveau nom : un serveur enregistré avec
l'ancien paquet échoue ou, s'il reste un `nemetonclaude` installé, tourne sur du code périmé qui ne reçoit
rien de ce brief. À faire : `nemetonmcp::serveur_mcp()`, plancher `nemetonmcp >= 0.8.0`, et réenregistrer le
serveur (`register_mcp.py`). Les variables d'environnement `NEMETON_CLAUDE_*` sont remplacées par
`NEMETON_MCP_*` (l'ancienne est encore lue avec un avertissement : `nemetonmcp/r/R/utils.R`, `.env_mcp`).
Les classes d'erreur `nemetonclaude_*` sont devenues `nemetonmcp_*`. Les briefs traités qui citent
`nemetonclaude` (`brief-dev/SKILL.md`) sont de l'historique : ne pas les réécrire.

Variables à poser au lancement du serveur (décision de Pascal, 2026-11-10) : **`TZ=Europe/Paris`** et une
**locale UTF-8** (`C.UTF-8`, repli `en_US.UTF-8`), comme `inst/mcp/server.R` de nemetonshiny. Le script
`register_mcp.py` ne les pose pas aujourd'hui (non vérifié pour l'environnement transmis par `claude -p`).

## 3. Permissions : `permissions.allow` et le jeton de confirmation

`.claude/settings.json` autorise sans confirmation les 22 outils `mcp__nemeton__*` (lignes 16-37), dont
`appliquer_ugf`, `croiser_onf`, `ajouter_action`, `modifier_action`, `supprimer_action`. Et VICTOR lance les
tâches en `bypassPermissions` (`VICTOR_PERMISSION_MODE`, défaut). Rien n'arrête donc une phrase mal
transcrite ; `CLAUDE.md:79` pose cette autorisation comme décision de Pascal pour les 22 outils.

Ce qui change côté nemetonshiny (mission 010, **CASSANT**, PR #259, commit `bade93a8`, tests
`test-service_mcp_jeton.R` et `test-service_mcp_confirmation.R`) : `appliquer_ugf` et `croiser_onf`
**n'écrivent plus au premier appel**. Protocole exact :

1. 1er appel (sans `jeton`) : contrôles faits, **rien d'écrit**. Réponse en erreur `confirmation_requise`
   (`isError: true`) avec `jeton`, `apercu`, `expire_a`, `projet` (identifiant), `projet_nom` (nom résolu).
2. 2e appel : **mêmes arguments** + `jeton` → écriture.
3. Jeton à usage unique, **10 minutes** (provisoire), gardé en mémoire du serveur. Autres paramètres, autre
   projet, déjà utilisé, périmé : erreur `jeton_invalide`, il faut redemander un aperçu. Si le projet est
   introuvable ou ambigu, le jeton **n'est pas consommé**.
4. Contenu de l'aperçu : `appliquer_ugf` → `n_ugf_remplacees`, `n_ugf_nouvelles` ; `croiser_onf` →
   `ecartees` (IDU qui quittent le projet), `n_ecartees`, `n_retenues`.

Deux options, **à trancher par Pascal** (spec § 7 : « retirer des `permissions.allow` les outils soumis à
confirmation, ou s'en remettre au jeton ») :

- **A — s'en remettre au jeton** (état actuel de l'ADR 0002) : laisser `permissions.allow` tel quel. La
  garde est dans le serveur. C'est une barrière, pas une preuve : un agent qui enchaîne seul les deux appels
  la contourne.
- **B — retirer en plus de `permissions.allow`** `appliquer_ugf` et `croiser_onf` (et, quand `nemetonmcp`
  aura livré, `ajouter_action`, `modifier_action`, `supprimer_action`). **Inopérant en `bypassPermissions`**
  (ADR 0002, option C) : n'apporte rien pour les tâches VICTOR, seulement pour les sessions interactives.

Recommandation de l'émetteur (Pascal tranche) : A pour VICTOR, B en plus pour les sessions interactives ;
ne pas considérer B comme une garantie.

**Consigne à ajouter à `CLAUDE.md` de l'espace**, quoi qu'on choisisse : devant une erreur
`confirmation_requise`, ne **jamais** rappeler l'outil avec le jeton de sa propre initiative. Rendre la
main avec l'aperçu (nom et identifiant du projet, chiffres de l'aperçu) ; ne rappeler avec `jeton` que si le
message de la tâche contient la confirmation explicite de l'utilisateur.

**Point non résolu, à trancher par Pascal** : le jeton vit en mémoire du serveur MCP ; or chaque
`claude -p` lancé par VICTOR relance le serveur (`CLAUDE.md:124-126` : « Each `claude -p` restarts the
server »). Un jeton émis dans une tâche est **inconnu** dans la tâche suivante. Tant que ce point n'est pas
tranché (serveur persistant, ou registre persisté côté nemetonshiny), la confirmation en deux tâches
échouera en `jeton_invalide`. Non essayé de bout en bout.

## 4. Autres écarts constatés

- `CLAUDE.md:88` : « `croiser_onf` takes about 35 s » ; nemetonshiny annonce « ~15 s par commune »
  (description de l'outil) et **aucune durée n'a été mesurée sur un gros projet** (ADR 0004, mesure à
  faire, mission 011). Ne pas figer l'une ou l'autre valeur dans la doc.
- Erreurs : lire `code` et `action` de la réponse JSON (forme
  `{ok, classe, code, langue, erreur, action, candidats}`, `inst/mcp/README.md`), pas seulement le texte.
  `candidats` (format `nom (id)`) n'est rempli que pour `projet_ambigu` : demander à l'utilisateur, ne
  jamais choisir.
- Noms de projets dictés : `.mcp_resolve_project` tolère désormais « Dabot » pour « Forêt de Dabo »
  (mission 007, PR #258). Inutile de dupliquer ce rapprochement dans un skill.

## Ce que cette session n'a pas pu vérifier

- Que l'arborescence `vues/...` existe sous `nemetonmcp/clients/claude/skills/nemeton-vues` à l'identique.
- Le comportement de bout en bout du jeton à travers `claude -p`.
- Si `ksuite-setup` transmet `TZ` et la locale au serveur.

## Retour attendu

Un brief de réponse dans `vers-nemetonshiny/` indiquant le choix A ou B pris par Pascal et la date du
réenregistrement du serveur.

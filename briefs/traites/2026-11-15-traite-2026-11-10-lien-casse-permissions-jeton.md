# Brief `aigora` (espace `aigora-nemeton`) — lien cassé, renommage incomplet, permissions et jeton de confirmation

> **Déposé le 2026-11-10** par la session `nemetonshiny` (mission 013, `pilotage/spec.md` § 7).
> **Décrit ce qui est livré sur `main` de nemetonshiny** (`348ff3df`). Rien n'a été écrit dans `aigora`.
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
  ses chemins `vues/...`) et `.claude/skills/vues-victor/SKILL.md` (lignes 9-10, mêmes chemins). L'arborescence
  `vues/...` existe sous `nemetonmcp/clients/claude/` (vérifié le 2026-11-11 : `vues/atlas`, `calcul`,
  `plan`, `selection`, `moteur`, `exemples`) ; seul le préfixe de chemin change.

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
**locale UTF-8** (`C.UTF-8`, repli `en_US.UTF-8`). Le lanceur de nemetonshiny (`inst/mcp/server.R`) les
force depuis la mission 008 ; mais l'espace lance `nemetonmcp::serveur_mcp()`, qui ne passe pas par ce
fichier (voir le brief `vers-nemetonmcp`). Le script `register_mcp.py` ne les pose pas aujourd'hui : à
ajouter dans l'environnement du serveur enregistré (non vérifié pour l'environnement transmis par
`claude -p`). Sans `TZ`, les dates ISO 8601 de `resume_projet` (mission 008, PR #260, `1ea3289b` : champs
`created_at`, `updated_at`, `calcule_le`, `chm_suspect`, `familles[].ndp`, `familles[].n_indicateurs`,
scores à 2 décimales) seraient lues dans le fuseau du poste.

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
3. Jeton à usage unique, **10 minutes** (provisoire), gardé **sur disque** (voir plus bas). Autres paramètres, autre
   projet, déjà utilisé, périmé : erreur `jeton_invalide`, il faut redemander un aperçu. Si le projet est
   introuvable ou ambigu, le jeton **n'est pas consommé**.
4. Contenu de l'aperçu : `appliquer_ugf` → `n_ugf_remplacees`, `n_ugf_nouvelles` ; `croiser_onf` →
   `ecartees` (IDU qui quittent le projet), `n_ecartees`, `n_retenues`.

**Décision de Pascal : option A** (spec § 7 : « retirer des `permissions.allow` les outils soumis à
confirmation, ou s'en remettre au jeton »). `appliquer_ugf` et `croiser_onf` **restent dans
`permissions.allow`** ; le jeton sert de garde, côté serveur. C'est une barrière, pas une preuve : un agent
qui enchaîne seul les deux appels la contourne, d'où la consigne ci-dessous (jamais renvoyer le jeton
d'initiative). Aucun retrait de `settings.json` n'est demandé, pour ces deux outils comme pour les trois
outils d'écriture du plan d'actions quand `nemetonmcp` les aura soumis au jeton.

**Consigne à ajouter à `CLAUDE.md` de l'espace**, quoi qu'on choisisse : devant une erreur
`confirmation_requise`, ne **jamais** rappeler l'outil avec le jeton de sa propre initiative. Rendre la
main : écrire l'aperçu (nom et identifiant du projet, chiffres de l'aperçu), puis **terminer la sortie par
le bloc balisé ci-dessous**. Ne rappeler avec `jeton` que dans la **2e tâche**, dont le prompt est le gabarit « L'utilisateur a confirmé
oralement. » + bloc recopié tel quel (voir plus bas) : l'espace en déduit l'appel (outil `outil`,
`arguments` + `jeton`, tirés du bloc) et ne fait rien d'autre. Si la sortie contient aussi le bloc
`display_report` (`CLAUDE.md:89-90`), le bloc de confirmation vient **en dernier**.

### Transit du jeton entre la tâche 1 et la tâche 2 (décision de Pascal, 2026-11-11)

Format **identique** dans les briefs `vers-victor` et `vers-aigora`. La tâche 1 (celle qui a reçu
`confirmation_requise`) :

1. écrit d'abord, en texte ordinaire, l'aperçu pour l'utilisateur : nom du projet (`projet_nom`),
   identifiant (`projet`), et ce que l'écriture change (`apercu` : `n_ugf_remplacees` / `n_ugf_nouvelles`,
   ou `n_ecartees` / `n_retenues`) ;
2. **termine sa sortie** par un bloc JSON balisé, seul, sur trois lignes, rien après :

```
<<<NEMETON_CONFIRMATION
{"outil":"appliquer_ugf","projet":"20261104_101500_ab12","projet_nom":"Forêt de Dabo","jeton":"k3j9x0q2m8w7e1r5t6y4u0pz","expire_a":"2026-11-11T09:30:00Z","arguments":{"projet":"20261104_101500_ab12","fichier":"/home/pascal/dev/donnees/tenements_dabo.gpkg","remplacer":true}}
NEMETON_CONFIRMATION>>>
```

- Clés, toutes obligatoires : `outil`, `projet` (identifiant résolu), `projet_nom`, `jeton`, `expire_a`
  (ISO 8601 UTC, tel que rendu par le serveur), `arguments` (objet : **exactement** les arguments du 1er
  appel, **sans** `jeton`, avec `projet` = identifiant résolu et tout chemin de fichier **absolu**).
- Les valeurs de l'exemple sont fictives. Pour `croiser_onf`, `arguments` porte `projet` et `purger` s'il
  avait été passé (omis sinon : le réglage du projet s'applique).
- Un seul bloc par sortie. Si la tâche 1 n'a pas reçu `confirmation_requise`, aucun bloc.

**Ordre dans la sortie de la tâche 1.** Si la sortie porte aussi le bloc `display_report` (skill
`diagnostic`, `aigora-nemeton/CLAUDE.md:89-90`), le bloc de confirmation vient **en dernier**, après lui
(décision de l'orchestrateur) : VICTOR l'extrait par ses balises.

**Gabarit unique de la tâche 2** (identique mot pour mot chez VICTOR et dans l'espace aigora). La tâche 2
ne démarre **qu'après un « oui » explicite** de l'utilisateur. Son prompt est exactement :

```
L'utilisateur a confirmé oralement.
<<<NEMETON_CONFIRMATION
{…le bloc de la tâche 1, recopié tel quel…}
NEMETON_CONFIRMATION>>>
```

Rien d'autre : pas d'aperçu relu, pas de consigne reconstruite. Le bloc n'est ni retapé, ni corrigé, ni
complété. **Comment la session de l'espace en déduit l'appel** : elle lit le JSON entre les balises et
appelle l'outil `outil` avec les `arguments` du bloc **plus** `jeton` (valeur du bloc) ; aucun autre
argument, aucun autre outil. Elle n'agit que si le prompt contient la phrase « L'utilisateur a confirmé
oralement. » **et** un bloc valide ; sinon elle ne fait rien d'écrit. Le jeton n'est **jamais lu à voix
haute**, ni affiché, ni journalisé.


**Le jeton survit au redémarrage du serveur** (mission 016, PR #261, `348ff3df`, test
`test-service_mcp_jeton_disque.R`). Chaque `claude -p` relance bien le serveur MCP (`CLAUDE.md:124-126`),
mais le jeton est désormais **un fichier** dans `NEMETON_MCP_JETONS_DIR` (défaut
`tools::R_user_dir("nemetonshiny", "data")/mcp/jetons`, répertoire `0700`, fichiers `0600`) : l'aperçu
(tâche 1) et la confirmation (tâche 2) passent par deux serveurs sans perdre le jeton, et l'usage unique
tient même entre deux serveurs simultanés (VICTOR et AIGORA). À faire pour l'espace :
- **même utilisateur système et même répertoire** pour tous les serveurs qui doivent se passer un jeton
  (ne pas poser un `NEMETON_MCP_JETONS_DIR` différent selon le lanceur) ;
- **chemins absolus** pour `appliquer_ugf` : le chemin du fichier fait partie de l'empreinte du jeton ; un
  chemin relatif la rend dépendante du répertoire de travail, et la 2e tâche, si elle part d'un autre
  répertoire, reçoit `jeton_invalide`. Le dire dans les consignes de l'espace et des skills qui appellent
  `appliquer_ugf`.

## 4. Autres écarts constatés

- `CLAUDE.md:88` : « `croiser_onf` takes about 35 s » ; nemetonshiny annonce « ~15 s par commune »
  (description de l'outil) et **aucune durée n'a été mesurée sur un gros projet** (ADR 0004, mesure à
  faire, mission 011). Ne pas figer l'une ou l'autre valeur dans la doc.
- Erreurs : **décider sur `code`** ; `action` est une phrase traduite (`R/service_mcp.R:85`), à lire
  seulement, pas une clé (forme `{ok, classe, code, langue, erreur, action, candidats}`,
  `inst/mcp/README.md`).
  `candidats` (format `nom (id)`) n'est rempli que pour `projet_ambigu` : demander à l'utilisateur, ne
  jamais choisir.
- Noms de projets dictés : `.mcp_resolve_project` tolère désormais « Dabot » pour « Forêt de Dabo »
  (mission 007, PR #258). Inutile de dupliquer ce rapprochement dans un skill.

## Ce que cette session n'a pas pu vérifier

- Le jeton entre deux `claude -p` réels de VICTOR (la mission 016 le teste en R seulement).
- Si `ksuite-setup` transmet `TZ` et la locale au serveur.

## Retour attendu

Un brief de réponse dans `vers-nemetonshiny/` indiquant la date du réenregistrement du serveur
(`nemetonmcp::serveur_mcp()`, `TZ`, répertoire des jetons).

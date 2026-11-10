# Brief `victor` — relayer à la voix la confirmation d'une écriture (jeton)

> **Déposé le 2026-11-10** par la session `nemetonshiny` (mission 013, `pilotage/spec.md` § 7).
> **Décrit ce qui est livré sur `main` de nemetonshiny** (`1af92486`, PR #259, **changement cassant**).
> Rien n'a été écrit dans `victor`. Constats relevés le 2026-11-10 dans `victor/victor/server.py`
> (lecture seule).

## Le problème

Arbitrage 4 de Pascal : « il faut confirmer dès qu'on touche à la base de données postgres/postgis ».
Aujourd'hui VICTOR lance ses tâches `claude -p` en `bypassPermissions` (`VICTOR_PERMISSION_MODE`,
`server.py:100,534`) et ses instructions disent : « s'il y a un mot bizarre, devine le sens le plus
probable plutôt que de faire répéter » (`server.py:159-160`). Pour une écriture sur un projet, c'est
exactement le mauvais défaut : une phrase mal transcrite peut modifier le mauvais projet.

## Ce que fait désormais le serveur (ADR 0002)

`appliquer_ugf` et `croiser_onf` (et plus tard les outils d'écriture du plan d'actions de `nemetonmcp`) :

1. **1er appel** (sans `jeton`) : rien n'est écrit. Réponse en erreur `confirmation_requise`
   (`isError: true`), JSON `{ok: false, classe, code: "confirmation_requise", action: "confirmer",
   erreur, projet, projet_nom, jeton, apercu, expire_a}`.
   - `projet` : identifiant du projet ; `projet_nom` : son **nom résolu** (c'est celui que le serveur a
     retenu, pas celui que l'utilisateur a dicté) ;
   - `apercu` — `appliquer_ugf` : `n_ugf_remplacees`, `n_ugf_nouvelles` ; `croiser_onf` : `ecartees`
     (IDU qui quittent le projet), `n_ecartees`, `n_retenues` ;
   - `expire_a` : ISO 8601 UTC.
2. **2e appel** : les **mêmes arguments** + `jeton` → écriture.
3. Jeton **à usage unique, 10 minutes** (provisoire), en mémoire du serveur. Refus : `jeton_invalide`
   (inconnu, déjà utilisé, périmé, autres paramètres) : redemander un aperçu. **Si le projet est introuvable
   ou ambigu au 2e appel, le jeton n'est pas consommé.**

Sources : `R/service_mcp.R` (`.mcp_jeton_emettre`, `.mcp_jeton_consommer`, `mcp_appliquer_ugf`,
`mcp_croiser_onf`), `inst/mcp/README.md` (« Confirmation des écritures »), commits `bade93a8`, `20c38ae2`,
tests `test-service_mcp_jeton.R`, `test-service_mcp_confirmation.R`, `test-service_mcp_projet_nomme.R`.

## Consigne à donner à VICTOR (à mettre dans ses instructions, `server.py` ~ligne 210-275, bloc Néméton)

Quand une tâche déléguée revient avec `confirmation_requise` (ou quand tu demandes à l'espace `nemeton`
une écriture qui peut y mener) :

1. **Lis l'aperçu à voix haute, en une ou deux phrases**, **en nommant le projet par son nom ET son
   identifiant** (`projet_nom` et `projet`), puis ce que l'écriture change. Exemple : « Je m'apprête à
   remplacer les unités de gestion du projet Forêt de Dabo, identifiant vingt-six zéro … : douze unités
   actuelles, quatorze dans le fichier. Je confirme ? » Pour `croiser_onf` : dis combien de parcelles
   **quittent** le projet (`n_ecartees`).
2. Si le nom entendu par l'utilisateur diffère du `projet_nom` rendu (ex. il a dit « Dabot », le serveur a
   retenu « Forêt de Dabo »), **dis-le explicitement** avant de demander la confirmation.
3. **N'envoie le jeton qu'après une confirmation orale explicite** (« oui », « confirme », « vas-y »). Un
   silence, un bruit, une hésitation, un changement de sujet ou une phrase ambiguë **ne valent pas
   confirmation** : ne rappelle pas, abandonne le jeton.
4. Ne fabrique, ne devine ni ne réutilise **jamais** un jeton. N'enchaîne **pas** les deux appels sans
   l'attente de la réponse de l'utilisateur : c'est ce qui contourne la garde (ADR 0002 : « barrière, pas
   preuve »).
5. Au-delà de `expire_a` (10 min), ou sur `jeton_invalide`, dis-le et redemande un aperçu : ne retente pas
   avec le même jeton.
6. Lis `code` et `action` de la réponse (forme `{ok, classe, code, langue, erreur, action, candidats}`),
   pas seulement le texte. `projet_ambigu` : lis les `candidats` (« nom (id) ») et demande lequel ; ne
   choisis jamais.

La phrase d'attente actuelle (« Je m'en occupe, {TITLE}. », `server.py:166-167`) ne s'applique pas à
l'aperçu : l'aperçu doit être lu en entier avant toute action.

## Point non résolu, à trancher par Pascal : le jeton ne survit pas à `claude -p`

VICTOR n'appelle pas lui-même les outils MCP d'écriture : il délègue (`delegate_to_claude` → `_run_task`,
`server.py:526-545`) à un `claude -p` **neuf** (pas de `--resume`, `stdin` fermé), et chaque `claude -p`
relance le serveur MCP (`aigora-nemeton/CLAUDE.md`, section MCP). Le jeton est **en mémoire du processus
serveur**. Chemin prévu : tâche 1 → `confirmation_requise` + jeton → VICTOR lit l'aperçu → l'utilisateur
dit oui → tâche 2 avec le jeton → **nouveau serveur, registre vide → `jeton_invalide`**. Tant que ce
point n'est pas tranché, la consigne ci-dessus ne peut pas aboutir en stdio local. Pistes (aucune décidée,
rien n'a été essayé) : serveur MCP **persistant** (`nemetonmcp::serveur_mcp_distant()`, service systemd)
auquel les tâches se connectent ; ou registre persisté côté nemetonshiny (hors du dossier du projet) ; ou
faire parler VICTOR au serveur lui-même plutôt que par `claude -p`. À arbitrer avant l'essai VICTOR de la
durée de 10 minutes (valeur provisoire, `.MCP_JETON_DUREE`).

## Autre écart : la résolution de projet de VICTOR est une copie non tolérante

`resoudre_projet` (`server.py:996-1020`) réimplémente la résolution d'un nom (« comme `.mcp_resolve_project`
de nemetonshiny », `_norm`, ligne 989) : égalité, puis sous-chaîne. Depuis la mission 007 (PR #258, tests
`test-service_mcp_noms_dictes*.R`), nemetonshiny rapproche aussi les noms **mal dictés** (« Dabot » →
« Forêt de Dabo », mot-clé de la commune, distance d'édition ≤ 2 provisoire, planchers de 4 lettres) et
rend `projet_ambigu` quand deux projets sont proches. VICTOR (`nemeton_fiche`, `nemeton_etat`, qui lisent
`~/.local/share/nemeton/projects`) ne le fait pas : les deux voies donnent des réponses différentes pour la
même phrase. C'est une **copie** : soit la supprimer et passer par l'outil, soit la réaligner à la main
(deux vérités). Non tranché ; à décider avec Pascal. Une **écriture** ne doit jamais s'appuyer sur la
résolution de VICTOR : l'aperçu (nom + identifiant du serveur) fait foi.

## Lancement

Le serveur MCP doit être lancé avec `TZ=Europe/Paris` et une locale UTF-8 (décision de Pascal, 2026-11-10,
comme `inst/mcp/server.R`). Les dates ISO 8601 Europe/Paris côté `resume_projet` (mission 008) ne sont **pas
encore livrées**.

## Ce que cette session n'a pas pu vérifier

- Aucun essai vocal : la formulation de l'aperçu à voix haute n'a pas été éprouvée avec la synthèse vocale.
- Le jeton de bout en bout à travers `claude -p` (voir ci-dessus).
- Que `.MCP_JETON_DUREE` = 10 min convient à une conversation vocale (valeur provisoire).

## Retour attendu

Un brief de réponse dans `vers-nemetonshiny/` après l'essai vocal, avec le texte lu à voix haute et le
résultat de la confirmation.

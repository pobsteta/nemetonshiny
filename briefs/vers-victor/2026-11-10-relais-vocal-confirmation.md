# Brief `victor` — relayer à la voix la confirmation d'une écriture (jeton)

> **Déposé le 2026-11-10** par la session `nemetonshiny` (mission 013, `pilotage/spec.md` § 7).
> **Décrit ce qui est livré sur `main` de nemetonshiny** (`348ff3df` ; PR #259, **changement cassant**, et #261, jeton sur disque).
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
3. Jeton **à usage unique, 10 minutes** (provisoire), **sur disque** (mission 016, ci-dessous). Refus : `jeton_invalide`
   (inconnu, déjà utilisé, périmé, autres paramètres) : redemander un aperçu. **Si le projet est introuvable
   ou ambigu au 2e appel, le jeton n'est pas consommé.**

Sources : `R/service_mcp.R` (`.mcp_jeton_emettre`, `.mcp_jeton_consommer`, `mcp_appliquer_ugf`,
`mcp_croiser_onf`), `inst/mcp/README.md` (« Confirmation des écritures »), commits `bade93a8`, `20c38ae2`,
tests `test-service_mcp_jeton.R`, `test-service_mcp_confirmation.R`, `test-service_mcp_projet_nomme.R` ; jeton sur disque : PR #261 (`348ff3df`), `test-service_mcp_jeton_disque.R`.

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

## Le jeton survit à `claude -p` (mission 016, PR #261, `348ff3df`)

VICTOR n'appelle pas lui-même les outils MCP d'écriture : il délègue (`delegate_to_claude` → `_run_task`,
`server.py:526-545`) à un `claude -p` neuf (pas de `--resume`, `stdin` fermé), qui relance le serveur MCP.
Chemin : tâche 1 → `confirmation_requise` + jeton → VICTOR lit l'aperçu → l'utilisateur dit oui → tâche 2
avec le jeton. Ce chemin **tient** : le jeton n'est plus en mémoire du serveur mais dans un fichier de
`NEMETON_MCP_JETONS_DIR` (défaut `tools::R_user_dir("nemetonshiny", "data")/mcp/jetons`, répertoire
`0700`, fichiers `0600`), à usage unique même entre serveurs simultanés. Deux conditions côté VICTOR :

- **même utilisateur système, même répertoire** que les autres clients (AIGORA) : si VICTOR pose un
  `NEMETON_MCP_JETONS_DIR` différent de celui de l'espace `aigora-nemeton`, ou lance le serveur sous un
  autre utilisateur, la tâche 2 ne verra pas le jeton (`jeton_invalide`) ;
- **chemins de fichier absolus** dans le prompt délégué (`appliquer_ugf` : le chemin fait partie de
  l'empreinte du jeton). Un chemin relatif la rend dépendante du répertoire de travail ; si la tâche 2 part
  d'ailleurs, `jeton_invalide`. Le prompt de la tâche 2 doit reprendre **exactement** les arguments de la
  tâche 1, avec un chemin absolu dans les deux.

La durée de 10 minutes reste provisoire (`.MCP_JETON_DUREE`) : à éprouver à la voix.

## Supprimer la copie `resoudre_projet` (décision de Pascal)

`resoudre_projet` (`server.py:996-1020`) et son `_norm` (ligne 989) réimplémentent la résolution d'un nom
(« comme `.mcp_resolve_project` de nemetonshiny ») : égalité, puis sous-chaîne. Depuis la mission 007 (PR
#258, tests `test-service_mcp_noms_dictes*.R`), nemetonshiny rapproche aussi les noms **mal dictés**
(« Dabot » → « Forêt de Dabo » : mot-clé de la commune, distance d'édition ≤ 2 provisoire, planchers de 4
lettres) et rend `projet_ambigu` quand deux projets sont proches. Les deux voies donnent des réponses
différentes pour la même phrase : c'est une copie, donc une fausse vérité en puissance.

**Décision de Pascal : supprimer la copie.** À faire : le **nom part tel quel** (tel que transcrit ou tel
que le modèle l'a compris) au serveur MCP, qui résout selon les règles de la mission 007 et rend
`projet_ambigu` avec `candidats` (format `nom (id)`) ou `projet_introuvable`. VICTOR lit alors `code` et
`candidats` et demande lequel, sans choisir. Concerne `nemeton_fiche` et `nemeton_etat` (qui lisent
aujourd'hui `~/.local/share/nemeton/projects` directement) : ils doivent passer par le serveur MCP
(`resume_projet`, `etat_calcul`, ou délégation) plutôt que par `resoudre_projet`. À vérifier côté VICTOR :
la « voie rapide » (lecture directe, sans Claude Code, accusé vocal) ne doit pas disparaître avec la copie ;
si elle exige de lire les fichiers sans serveur, il faut une autre solution (à voir avec Pascal), mais **pas
une seconde résolution de noms**. Une **écriture** ne s'appuie jamais sur une résolution de VICTOR :
l'aperçu (nom et identifiant du serveur) fait foi.

Depuis la mission 008 (PR #260, `1ea3289b`), `resume_projet` rend l'écran Synthèse complet : `description`,
`created_at`, `updated_at`, `calcule_le` (ISO 8601, fuseau Europe/Paris), `chm_suspect`,
`chm_suspect_max`, `familles[].ndp`, `familles[].n_indicateurs`, scores de famille à 2 décimales
(`updated_at` change de format : heure naïve → ISO 8601 avec fuseau). Ces champs peuvent remplacer ce que
VICTOR relit dans `metadata.json` et `indicators.parquet`.

## Lancement et à venir

Le serveur MCP doit être lancé avec `TZ=Europe/Paris` et une locale UTF-8 (décision de Pascal,
2026-11-10). `inst/mcp/server.R` de nemetonshiny les force ; l'espace lance `nemetonmcp::serveur_mcp()`
(voir les briefs `vers-nemetonmcp` et `vers-aigora`). Les dates ISO 8601 Europe/Paris de `resume_projet`
sont **livrées** (mission 008, ci-dessus).

**Annonce, non livré** : la mission 012 (ADR 0004) détachera `croiser_onf` (2e appel avec jeton → `job_id`
rendu aussitôt) et `generer_rapport`, suivis par `etat_calcul(projet)` ; un seul job par projet (un second
lancement rend `calcul_en_cours`). VICTOR devra alors annoncer « c'est lancé » et suivre l'état, au lieu
d'attendre le résultat. Ne rien coder dessus avant le brief de livraison.

## Ce que cette session n'a pas pu vérifier

- Aucun essai vocal : la formulation de l'aperçu à voix haute n'a pas été éprouvée avec la synthèse vocale.
- Le jeton entre deux `claude -p` réels de VICTOR (la mission 016 le teste en R seulement).
- Que `.MCP_JETON_DUREE` = 10 min convient à une conversation vocale (valeur provisoire).

## Retour attendu

Un brief de réponse dans `vers-nemetonshiny/` après l'essai vocal, avec le texte lu à voix haute et le
résultat de la confirmation.

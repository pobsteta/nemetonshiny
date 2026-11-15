# Mission 028 — Tarifs proposés à Marculus : niveau forêt seul, millésime sans « IFN », numéros ≤ 0 écartés

> Créée par l'orchestrateur le 2026-11-15 sur ordre de Pascal (« traite-les tous […] ne demande
> rien »). Solde le brief `~/dev/briefs/vers-nemetonshiny/2026-10-23-marculus-tarifs-proposes-niveau-foret.md`
> (urgence faible) et la partie app du brief
> `~/dev/briefs/vers-nemetonshiny/2026-11-10-marculus-tarifs-ifn-numeros-negatifs.md` (questions ;
> l'origine des numéros ≤ 0 relève du cœur, l'orchestrateur y répond par brief). **Lire les deux
> briefs en entier.**

## Mission

**En une phrase :** le `.marsync` n'émet plus dans `tarifsProposes` que les propositions de niveau
`foret` à numéro strictement positif (clé omise s'il n'y en a aucune), émet `millesimeTarifs` sous
la forme `2011-2025`, et le message de fin d'export le dit honnêtement.

## Constat (orchestrateur, lu sur `main` le 2026-11-15)

- `R/service_marculus_tarifs.R:140-148` `.marculus_tarifs_millesime()` préfixe « IFN » : le brief
  demande la forme nue `2011-2025` (colonne `millesime` des tables, que Marculus écrit aussi).
- `:156-180` `.marculus_tarifs_essence()` garde tous les niveaux (`foret`, `ser`, `greco`,
  `national`) et tout numéro non `NA`, y compris ≤ 0.
- `R/service_marculus.R:347-356` `marculus_context_from_action()` émet `ser`, `millesimeTarifs`,
  `tarifsProposes` (objet nommé, même vide).
- `:206-214` `.marculus_tarifs_comptes()` et l'i18n `marculus_export_tarifs_fmt`
  (`R/utils_i18n.R:640`) : « proposés pour %d essences sur %d (niveau forêt : %d, SER : %d,
  repli : %d) ».
- Les caches déjà écrits (`<projet>/cache/marculus_tarifs.json`) portent « IFN 2011-2025 » et tous
  les niveaux : le filtrage se fait **à l'émission**, pour qu'un cache ancien donne le bon lot sans
  recalcul. Le cache, lui, garde tout (l'app peut afficher les autres niveaux plus tard).

## Travail attendu

1. À l'émission (`marculus_context_from_action()` ou un helper dédié) : par essence, ne garder que
   les types dont `niveau == "foret"` et `numero > 0` ; `conseille` seulement s'il désigne un type
   gardé ; une essence sans type gardé disparaît ; **omettre la clé `tarifsProposes`** si plus
   aucune essence (et non plus `{}`).
2. `millesimeTarifs` émis sans préfixe « IFN » (retirer le préfixe à l'émission, cache ancien
   compris) ; corriger `.marculus_tarifs_millesime()` pour les caches à venir.
3. `numero ≤ 0` : aussi écarté de tout affichage de l'app qui proposerait un numéro (chercher les
   lecteurs du cache).
4. Message de fin d'export : « numéros proposés au niveau forêt pour X essences sur Y ; les autres
   niveaux sont calculés sur le téléphone » (fr/en, nouvelle clé NMT ou clé existante réécrite) ;
   garder les messages d'échec / absence.
5. **Question du brief, § 4** : combien d'essences atteignent le niveau `foret` sur Couchey ?
   Projet `~/.local/share/nemeton/projects/20261008_212542_uywc` (pas de cache tarifs). Tenter UNE
   fois `marculus_tarifs_preparer()` sur ce projet, en tâche de fond, `timeout 900`, en écrivant le
   cache dans le scratchpad (copie de `travail$chemin`, **jamais dans le dossier du projet**).
   Rapporter le décompte par niveau, ou l'échec (réseau IFN) tel quel.

## Hors périmètre

Les clés lues depuis le terrain (mission 027). La table IFN elle-même (cœur).

## Versionnage

Cycle dev suivant la dernière version sur `main` au moment du rebase (l'orchestrateur la donne).

## Tests

`.marculus_tarifs_essence` / émission : cache mêlant `foret`, `ser`, `national`, un numéro −0,3 et 0
→ seuls les `foret` > 0 ; clé absente si aucun ; millésime « IFN 2011-2025 » → « 2011-2025 » ;
message. Mutation rouge (ex. garder `ser`). Filtre `marculus`, `timeout 540`.

## Lecteurs et appelants

`tarifsProposes` est lu par Marculus seul (brief § 2) ; le cache par `marculus_tarifs_pour_export()`,
`.marculus_tarifs_comptes()` et peut-être un affichage du Plan d'actions (à chercher). nemetonmcp :
vérifier en lecture (`grep -rn tarifs /home/pascal/dev/nemetonmcp`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/028-tarifs-niveau-foret`.
- Relecteur sur la PR. Temps de la main : *appuyer*.

## Statut

Terminée le 2026-11-15 : PR #288 fusionnée (`c90f8d12`), `.9006`. Couchey : 0 essence au niveau forêt, 9/10 au niveau SER (Frêne non résolu). Message dédié au cas 0 essence après relecture.

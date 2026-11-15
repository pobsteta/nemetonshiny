# Brief `nemetonmcp` (complément) — vous recevez déjà les changements cassants ; outils longs livrés

> **Déposé le 2026-11-13** par la session `nemetonshiny` (bilan du 2026-11-13, écart consigné au
> journal `pilotage/journal.md`). **Complète** `2026-11-10-contrat-jeton-erreurs-noms.md` : il en
> **remplace** le § 4 point 6 et le § 8 ; le reste de ce brief reste valable et reste à faire.
> **Décrit ce qui est livré sur `main` de nemetonshiny** (`5bbfb093`, version `2026.10.18.9009`).
> Rien n'a été écrit dans `nemetonmcp` par l'émetteur (lecture seule de votre `main` à `34caed1`).
> Vérifié : les 2 objets internes de nemetonshiny que votre `main` appelle par `.ns()` en plus des outils (`INDICATOR_FAMILIES`, `ACTION_PLAN_FAMILY_CODES`) existent dans `v2026.10.18` : le gel du § 0 ne casse rien.

## 0. Urgent — votre `Remotes:` tire `main` de nemetonshiny

`r/DESCRIPTION` déclare `Remotes: pobsteta/nemetonshiny` **sans ref**, donc la branche `main`, pas
la dernière release. Toute installation de `nemetonmcp` reçoit déjà les **5 changements cassants**
de `2026.10.18.9009`, que nemetonshiny n'a pourtant **pas publiés** : la release `v2026.11.1`
attend justement votre adaptation et l'essai VICTOR sur Couchey.

Constaté dans votre code (`main` à `34caed1`) : **aucun point du brief du 2026-11-10 n'est
appliqué**. Rien ne casse franchement à l'installation : les outils de nemetonshiny que vous
reprenez via `mcp_tools()` portent eux-mêmes le nouveau comportement. Ce sont vos enveloppes qui
le décrivent ou le traitent à l'ancienne (§ 3).

**À faire tout de suite** : figer la ligne sur la dernière release, contre laquelle votre code a
été écrit :

```
Remotes: pobsteta/nemetonshiny@v2026.10.18, pobsteta/nemeton@*release
```

**Ensuite**, adaptez le code sur une branche qui tire `main` de nemetonshiny (en local :
`pak::local_install("/home/pascal/dev/nemetonshiny")`). Fusionnez cette branche **après** la
sortie de `v2026.11.1`, en passant alors à `pobsteta/nemetonshiny@*release` et
`Imports: nemetonshiny (>= 2026.11.1)`. Voir l'ordre complet au § 4.

## 1. Outils longs en tâche de fond — LIVRÉ (mission 012, #264, CASSANT) — remplace le § 8

| Sujet | PR | Commits clés | Tests |
|---|---|---|---|
| `generer_rapport` et 2e appel de `croiser_onf` détachés | #264 | `60ea2629`, `bfb185f2`, `e4513baf` (merge `5bbfb093`) | `test-service_mcp_detache.R`, `test-service_mcp_detache_dev.R`, `test-service_mcp.R` |

Décision : `pilotage/adr/0004` (aucun appel au-delà de 600 s ; détacher au-delà de 60 s mesurés).
Référence des réponses : `inst/mcp/README.md` ; contrat : `CONTRAT.md` § 6 bis ; détail :
`NEWS.md` § `2026.10.18.9009`.

1. **`generer_rapport`** ne rend plus le fichier. Il lance un job et rend aussitôt `ok`, `projet`,
   `job_id`, `type = "rapport"`. Le chemin (`fichier`) se lit dans `etat_calcul` quand
   `statut = "termine"`. Durée mesurée : 49 s à chaud, 220 s sur un process neuf.
2. **`croiser_onf`**, 2e appel (avec `jeton`) : même modèle, `type = "onf"`. Le résultat
   (`n_ugf`, `n_tenements`, `n_parcelles`, `n_cad`, `ecartees`, `ecart_median_m`,
   `indicateurs_perimes`) arrive par `etat_calcul`. Le **1er appel (aperçu) reste synchrone** :
   38 à 70 s mesurés sur Couchey et Marsannay, jusqu'à ~160 s à froid.
3. **`etat_calcul`** :
   - nouveau champ `type` (`calcul`, `rapport`, `onf`) ;
   - `statut` vaut `en_cours` dès le lancement, **`lancement` n'existe plus** ;
   - un job en **échec** rend `ok = false` avec les champs d'erreur classée (`classe`, `code`,
     `langue`, `erreur`, `action`) au premier niveau, à côté de `statut`, **sans `isError`**
     (l'outil a réussi, c'est le job qui a échoué) ;
   - `erreur` est le texte classé, plus le message brut.
4. **Un seul job par projet** : un second lancement (calcul, rapport ou ONF) rend `calcul_en_cours`.
   `generer_rapport` est refusé aussi pendant un calcul lancé depuis l'application
   (`calcul_en_cours`) ou sous verrou d'édition (`projet_verrouille`).
5. **`annuler_calcul`** pendant un rapport ou un croisement ONF : `annule = false` (non annulable).

## 2. Ordre du jeton — remplace le § 4 point 6

La garde passe désormais **avant** la consommation. `.mcp_jeton_consommer()` rejoue d'abord
`.mcp_garde_ecriture` (calcul en cours, verrou, projet ancien) :

- Un refus de garde au 2e appel (`calcul_en_cours`, `projet_verrouille`), pour `appliquer_ugf`
  comme pour `croiser_onf`, **laisse le jeton valable** : le client peut réessayer avec le même
  jeton une fois la cause levée.
- Un jeton inconnu ou déjà utilisé reste `jeton_invalide`, sans passer par la garde.

Vos écritures de plan, une fois passées au jeton (§ 4 du brief du 2026-11-10), en profitent sans
rien changer.

## 3. Ce qui touche votre code (relevé à `34caed1`)

| Où | Aujourd'hui | À faire |
|---|---|---|
| `r/DESCRIPTION` | `Remotes: pobsteta/nemetonshiny` (= `main`) | § 0 |
| `outils.R`, description de `etat_calcul` | cite `lancement` ; ignore `type`, rapport et ONF | décrire les 3 types, retirer `lancement`, dire que le résultat de `generer_rapport` / `croiser_onf` arrive ici |
| `outils.R`, `etat_calcul_vue` | `if (!isTRUE(x$ok)) return(texte)` : un job en échec (`ok = false`) perd `ecoule_s` et sort sans `.mcp_vers_outil` | distinguer **échec de job** (`statut = "echec"`, garder les durées, pas d'`isError`) et **erreur d'outil** (`.ns(".mcp_vers_outil")`, brief du 2026-11-10 § 2.1) |
| `outils.R`, `exporter_gpkg_vue` | texte brut en erreur | `.mcp_vers_outil` (brief du 2026-11-10 § 2.1) |
| `utils.R`, `.mcp_call` | rend `ok`, `erreur`, `classe`, `candidats` | aligner sur `code`, `langue`, `action` (brief du 2026-11-10 § 2.3) |
| `plan.R` | `.refuser_si_verrouille` ; ajouter / modifier / supprimer une action sans jeton | `.mcp_garde_ecriture()` et jeton (brief du 2026-11-10 § 4) |
| `distant.R`, `.ajouter_urls` | le lien signé du PDF s'ajoutait à la réponse de `generer_rapport` | vérifier qu'il s'ajoute bien à la réponse d'`etat_calcul` (le chemin y arrive désormais) ; `etat_calcul` est en `.OUTILS_LECTURE` : un rôle lecture verra le lien d'un rapport lancé par un rôle écriture, à décider |
| `clients/claude/skills/nemeton-vues/SKILL.md` § « UGF depuis le parcellaire ONF » | « `croiser_onf` prend environ 35 s », résumé lu dans le retour | aperçu synchrone (38-160 s), puis 2e appel avec jeton → `job_id` → suivre `etat_calcul` ; résumé lu à `termine` |
| idem, ligne `generer_rapport` | rend le PDF | rend un `job_id` ; PDF et lien dans `etat_calcul` |
| idem, table des erreurs | — | ajouter `calcul_en_cours` sur rapport/ONF, `annule = false` |

## 4. Ordre de livraison proposé

1. `nemetonmcp` : figer `@v2026.10.18` (§ 0) et publier ce correctif seul, tout de suite.
2. `nemetonmcp` : adapter sur une branche (§ 1 à 3 et brief du 2026-11-10), testée contre `main` de
   nemetonshiny installé en local.
3. Pascal : essai VICTOR sur Couchey avec cette branche.
4. `nemetonshiny` : release `v2026.11.1`.
5. `nemetonmcp` : `@*release` + `Imports: nemetonshiny (>= 2026.11.1)`, fusion de la branche, release.

## Ce que cette session n'a pas pu vérifier

- Le comportement de `.ajouter_urls` sur la réponse d'`etat_calcul` (lu, pas exécuté).
- Que le travail en cours non commité de votre arbre local (lot 4 rasters) n'entre pas en conflit
  avec ces changements.
- Les vues HTML (`clients/claude/vues/`) qui suivraient `etat_calcul` : seule la vue Plan a été
  parcourue.

## Retour attendu

Un brief de réponse dans `vers-nemetonshiny/` :
- après l'étape 1 (SHA du gel de `Remotes:`) ;
- puis à la fin de l'étape 2 (SHA de la branche), qui déclenche l'essai Couchey.

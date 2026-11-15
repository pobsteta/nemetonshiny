# Mission 029 — T4 vitalité dynamique câblé dans l'app (cœur nemeton 2026.11.28)

> Créée par l'orchestrateur le 2026-11-15 sur ordre de Pascal (« traite-les tous […] ne demande
> rien »). Solde le brief `~/dev/briefs/vers-nemetonshiny/2026-11-15-t4-vitalite-dynamique.md`, dont
> la copie de référence est `/home/pascal/dev/nemeton/specs/068-trajectoires-finn/brief-nemetonshiny.md`
> (section « T4, vitalité dynamique », lignes 32-74) — **la lire en entier** (lecture seule).
> Cœur publié : tag `v2026.11.28` (`indicateur_t4_vitalite()`, `R/indicateur_t4_vitalite.R` du tag).

## Mission

**En une phrase :** T4 est calculé par l'app avec l'essence dominante en code IFN `ESPAR` et la SER
des UGF, ses statuts `skipped_*` sont traduits, et la vue famille T montre à côté du score
l'accroissement observé et attendu, avec la mention fixe de la référence.

## Constat (orchestrateur, lu sur `main` le 2026-11-15)

- `R/service_compute.R:5231-5236` passe `forms_t` à toute fonction qui a ce formel : T4 reçoit déjà
  la série FORMS-T de T2 (`:997-1006`, `build_forms_t_series()`).
- `:5257-5261` : en mode IFN, P2 reçoit `ser_field = "ser"` — la colonne `ser` existe donc déjà dans
  ce mode (chercher où elle est posée, `.units_for_indicator()` ou `localiser_ser`).
- `:5277-5284` : la colonne `species` des unités (libellés BD Forêt, `enrich_parcels_bdforet()`) ;
  la conversion en `ESPAR` passe par `nemeton::resoudre_espar()` (déjà utilisé par
  `R/service_marculus_tarifs.R`). Le TFV « Sapin, épicéa » est ambigu (brief, point 1).
- Statuts traduits par `indicator_na_banner()` (`R/mod_family.R:842-900`) : clé
  `<code>_<statut>` (ex. `t3_skipped_no_sufosat`, `R/utils_i18n.R:1677`) ; colonne `.t4_status`.
- Le cœur 2026.11.28 a aussi changé la description de la famille T ; c'est déjà aligné
  (`famille_temporel_desc`, PR #283, `.9001`).

## Travail attendu

1. **Arguments** : passer `essence` (vecteur ou colonne `essence` en `ESPAR`, depuis `species`
   via `resoudre_espar()` ; sans correspondance → `NA`, T4 rendra `skipped_no_essence`) et `ser`
   (colonne `ser`, calculée par le même chemin que P2 IFN, **aussi hors mode IFN** si T4 est
   demandé ; sans elle les analogues sont pris partout). Coupes : si T3 est calculé avant T4 sa
   colonne suffit ; sinon ne rien passer (vérifier l'ordre de calcul).
2. **Indicateur conditionnel** : T4 doit apparaître là où apparaissent les autres conditionnels
   (T3, A5…) — sélection, famille T, radar, export — en suivant le motif existant ; il ne doit pas
   peser sur un indice tant qu'il est `NA`.
3. **Statuts** : clés `t4_skipped_no_forms_t`, `t4_skipped_no_coverage`, `t4_skipped_cut`,
   `t4_skipped_drop` (chute de plus de 3 m), `t4_skipped_no_essence`, `t4_skipped_no_analogues`
   (surtout au-delà de 33 m) — fr/en, `\uXXXX`.
4. **Affichage** (vue famille T, popover ou tableau par UGF, au choix du motif existant) :
   `t4_accroissement_obs` / `t4_accroissement_att` (m / 5 ans), `t4_h_dom`, `t4_n_analogues`, et la
   mention fixe « Référence construite en Bourgogne-Franche-Comté, climat 2019-2023 ». Ces colonnes
   doivent survivre à la persistance des résultats (motif `.a5_status` : colonnes préfixées d'un
   point, `R/mod_family.R:1090`).
5. NDP 3 (`accroissement_hdom_finn()`) : **hors périmètre**.
6. Plancher `Imports: nemeton (>= 2026.11.28)` si l'app appelle une API nouvelle (`chore(deps)`
   dans le même PR).

## Tests

Mock de `indicateur_t4_vitalite` (`local_mocked_bindings(.package = "nemeton")`) ou appel réel si
le nemeton installé est ≥ 2026.11.28 (sinon `skip_if`) : arguments reçus (`essence` en ESPAR, `ser`) ;
traduction de chaque statut ; colonnes annexes conservées ; affichage obs / attendu. Mutation rouge.
Filtre `service_compute|mod_family|i18n`, `timeout 540` par filtre (service_compute est long :
cibler avec `test_file` + `filter` sur le nom des tests si besoin).

## Lecteurs et appelants

Rapport Quarto (`inst/quarto/`), export, MCP (`resume_projet`, table des familles unique `.9008`),
`nemetonmcp` (lecture seule : `grep -rn "T3\|conditionnel" /home/pascal/dev/nemetonmcp`). Un nouvel
indicateur ne doit casser aucun test de contrat (`test-contrat_nemetonmcp.R`).

## Versionnage

Version donnée par l'orchestrateur au lancement.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/029-t4-vitalite`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : Dabo (T4 se centre vers 57).

## Statut

Terminée le 2026-11-15 : PR #286 fusionnée (`03125fe5`), `.9004`, plancher nemeton 2026.11.28. Convention de l'orchestrateur : « Sapin, épicéa » → ESPAR 61. CI rouge une fois (compteur 35 → 36 oublié, corrigé `29298c86`). Reste : Dabo (T4 vers 57).

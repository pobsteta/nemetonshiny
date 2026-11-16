# Mission 036 — T4 : essence des libellés BD Forêt de groupe par `resoudre_espar_bdforet()` (cœur nemeton 2026.11.35)

> Créée par l'orchestrateur le 2026-11-16 (« tous les briefs sont codés ? »). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-16-t4-libelles-bdforet-reponse.md` (voie b, choisie par
> Pascal le 2026-11-16 : la convention vit dans le cœur) — **le lire**. Cœur au tag :
> `gh api "repos/pobsteta/nemeton/git/trees/v2026.11.35?recursive=1"` pour trouver le fichier de
> `resoudre_espar_bdforet()`, puis `gh api "repos/pobsteta/nemeton/contents/<fichier>?ref=v2026.11.35" --jq .content | base64 -d`.

## Mission

**En une phrase :** T4 reçoit l'essence ESPAR que le cœur déduit du libellé BD Forêt **et de la SER
de chaque UGF**, et l'app ne tient plus de convention d'essence à elle.

## Constat (orchestrateur, lu sur `main` le 2026-11-16)

- `R/service_t4.R:40-70` `.t4_ajouter_essence(parcels)` : `nemeton::resoudre_espar(sp)` puis
  `.t4_essence_melange()` (« Sapin, épicéa » → `.T4_ESPAR_SAPIN_EPICEA` = 61).
- Appelée par `R/service_production_ifn.R:191` (`.units_for_indicator`) ; la colonne `ser` est déjà
  posée sur les unités quand T4 est demandé (`R/service_compute.R:1096-1102`, `ensure_ugf_ser`).
- Couchey (SER C20) : 34 UGF `skipped_no_essence` (« Chênes décidus », « Pin laricio, pin noir »,
  plus Feuillus / Conifères / Mixte qui doivent rester NA).

## Travail attendu

1. `.t4_ajouter_essence()` : `nemeton::resoudre_espar_bdforet(sp, ser = parcels$ser)` (SER par UGF,
   `NULL` si la colonne manque) à la place de `resoudre_espar()`. Vérifier au tag que `ser` est
   vectorisé par libellé ; sinon appeler par groupe de SER.
2. Retirer `.T4_ESPAR_SAPIN_EPICEA` et `.t4_essence_melange()` (et leurs tests), et la mention de
   la convention dans la roxygen et NEWS (entrée nouvelle qui dit qu'elle est retirée).
3. Une `essence` déjà fournie sur les unités reste prioritaire (comportement actuel).
4. Plancher `Imports: nemeton (>= 2026.11.35)`. Pas d'invalidation automatique des projets (même
   décision que la mission 035) : NEWS invite à relancer le calcul.

## Tests

Mock de `resoudre_espar_bdforet` (`local_mocked_bindings(.package = "nemeton")`) : appelée avec la
SER des UGF ; `essence` existante gardée. Appel réel (le nemeton installé doit être ≥ 2026.11.35 :
l'installer, cf. Moyens) : « Chênes décidus » en SER C20 → "03", « Pin laricio, pin noir » en C20 →
"54", « Feuillus » → NA, « Hêtre » → "09". Mutation rouge (oublier la SER → « Pin laricio, pin noir »
rend 53CO). Filtre `t4|production_ifn`, `timeout 540`.

## Versionnage

`2026.11.2.9004`, entrée NEWS (`feat`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/036-t4-essence-bdforet`.
- Installer le cœur : `Rscript -e 'pak::pkg_install("pobsteta/nemeton@v2026.11.35", ask = FALSE, upgrade = FALSE)'` (depuis /tmp, ~6 min).
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain : recalcul de Couchey → les UGF « Chênes décidus » et « Pin laricio, pin noir » ont un T4 (ou `skipped_no_analogues`), seules Feuillus / Conifères / Mixte restent `skipped_no_essence`.

## Statut

Terminée le 2026-11-16 : PR #300 fusionnée (`253fc266`), `.9004`, plancher nemeton 2026.11.35. Relecture : fusionnable, mutations rouges. Reste : recalcul de Couchey (seules Feuillus, Conifères, Mixte en `skipped_no_essence`).

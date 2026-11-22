# Mission 040 — Typage de la desserte : « rien à typer » avant le volume, vrai motif dans le rapport

> Créée par l'orchestrateur le 2026-11-21 (« lis et code le brief »). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-21-desserte-typage-ordre-et-motif.md` — **le lire** (constat
> rejoué sur « Parc de la colombière », `20261121_083914_gwsa`, run `run_20261121185631_oekh`).

## Mission

**En une phrase :** sur un réseau sans route à créer, le typage finit « rien à typer » (`skipped`)
même sans volume, et quand il échoue le rapport du pipeline dit pourquoi (motif précis).

## Constat (brief, à vérifier sur `main`)

- `R/service_desserte.R` ~1429, `run_desserte_typage()` : le bloc volume (préparation IFN,
  comblement → `desserte_typage_no_volume`) passe **avant** la lecture du réseau et le test
  `nrow(reseau$lignes) == 0L` → `desserte_typage_rien_a_typer` (`status = "empty"`).
- `R/mod_desserte.R` ~1519 (étape enchaînée `desserte_typage`, mission 037) : le message du
  pipeline est toujours `i18n$t("desserte_typage_failed")` + `detail` ; le `reason` de
  `rv_typage()` (`no_volume`, `no_species`, `no_reseau`, `no_parcelles`, `bad_params`,
  `volume_failed`…) est ignoré, alors que la notification de l'onglet l'utilise.

## Travail attendu

1. `run_desserte_typage()` : lire le réseau et rendre `empty` / `desserte_typage_rien_a_typer` si
   `lignes` est vide **avant** tout le bloc volume (préparation IFN comprise). Les contrôles moins
   coûteux qui précèdent (paquets, fichier réseau, parcelles, paramètres) : garder un ordre sensé,
   le dire dans la PR.
2. `mod_desserte.R`, étape `desserte_typage` : message pipeline =
   `pipeline_error_message(i18n$t(rv_typage()$reason %||% "desserte_typage_failed"), detail)`.
   Un seul calcul, partagé avec la notification si possible.
3. Balayer les autres étapes enchaînées du module (`desserte_integrite`, …) et des autres modules
   qui rendent un libellé générique alors qu'un `reason` précis existe ; les aligner. Vérifier que
   chaque `reason` utilisé a bien sa clé i18n FR/EN.

## Tests

Les trois du brief : `lignes` vide + sans P1 → `empty` / `rien_a_typer` (et la préparation IFN
n'est pas appelée — mock) ; `lignes` non vide + sans P1 → `no_volume` inchangé ; rapport du
pipeline avec `reason = "desserte_typage_no_volume"` sans `detail` → libellé `no_volume`. Mutations
rouges (remettre l'ordre ; remettre le libellé générique). Filtre
`desserte|pipeline`, `timeout 540`, fichier par fichier.

## Versionnage

`2026.11.2.9010` (la PR #310 en `.9009` est en cours de merge), entrée NEWS (`fix`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/main`, branche `claude/040-typage-ordre-motif`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : relancer le pipeline sur `20261121_083914_gwsa` → `desserte` `ok`,
  `desserte_typage` « ignorée — rien à typer ».

## Statut

Terminée le 2026-11-22 : PR #311 fusionnée (`fadd0d6f`, `.9010`). Relecture : fusionnable ; défaut intégrité/correction renvoyé à la mission 041.

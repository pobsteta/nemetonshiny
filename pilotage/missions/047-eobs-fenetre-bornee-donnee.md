# Mission 047 — E-OBS : borner la fenêtre d'années sur la donnée

> Créée par l'orchestrateur le 2026-11-23 (Pascal : « oui »). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-23-eobs-derniere-annee-complete.md` — **le lire en entier**.

## Mission

**En une phrase :** la fenêtre d'années E-OBS se termine à
`min(annee_courante - 1, nemeton::eobs_last_complete_year(version, months))`, et reste à
`annee_courante - 1` quand le cœur répond `NA` ; ainsi, au 1er janvier 2027, l'app ne demandera
pas 2026 à une version 34.0e qui s'arrête au 2026-06-30.

## État du cœur

nemeton **v2026.11.41 taguée** : `eobs_last_complete_year(version = "34.0e", months = 6:8)`,
attributs `end_date`, `source` (`"cache"` / `"table"`), `NA` + avertissement si version inconnue
sans cache. Installer depuis GitHub : `pak::pkg_install("pobsteta/nemeton@v2026.11.41")` ;
vérifier la signature réelle (`args()`) avant de coder. Ne jamais toucher `/home/pascal/dev/nemeton`
(lecture seule permise).

## Travail attendu

1. Trouver où l'app calcule la fin de la fenêtre E-OBS (mission 044 : `annee_courante - 1`, dans
   `R/service_regeneration.R` et/ou `R/mod_regeneration.R`, y compris la climatologie
   ombrothermique sur dix ans `.regen_eobs_monthly_clim`). Remplacer par un helper pur
   (ex. `.regen_eobs_fin_fenetre(annee_courante, version, months)`) qui applique le `min` et le
   repli `NA`, l'avertissement du cœur étant capté sans casser la session.
2. Passer les mêmes `months` que la réduction estivale (les retrouver dans le code, ne pas
   les supposer).
3. `DESCRIPTION` : `nemeton (>= 2026.11.41)`.

## Tests

Helper : cœur mocké rendant 2025 avec année courante 2027 → 2025 ; rendant 2026 avec année courante
2026 → 2025 ; rendant `NA` (avec avertissement) → `annee_courante - 1`, sans avertissement qui
remonte ; `months` transmis (contrôle des arguments du mock). Chaque appelant passe par le helper.
Mutations rouges (retirer le `min` ; retirer le repli `NA`). Fichiers ciblés `timeout 540`, un par
un. `grep -nP "[^\x00-\x7F]" R/*.R` sans ajout.

## Versionnage

`2026.11.5.9001`, entrée NEWS (`fix`), section « Dépendances ».

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/claude/cycle-2026.11y` (contient la release
  2026.11.5, PR #322), branche `claude/047-eobs-fenetre-bornee-donnee`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain : rien de visible avant le 2027-01-01 ; vérifier que la fenêtre reste 2016-2025 sur la
  Colombière.

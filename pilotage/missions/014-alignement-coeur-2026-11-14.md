# Mission 014 — Aligner l'app sur nemeton 2026.11.14 (CI rouge)

> Gabarit (fiches n° 9 et 15 de la méthode). Rempli par l'orchestrateur le 2026-11-10 sur décision de Pascal (cap du 2026-11-10). Une mission = une phrase = une branche = une PR.

## Mission

**En une phrase (quoi, et pourquoi) :** Rendre les 5 tests qui figent l'ancien état du cœur (41 indicateurs, description statique de `famille_biodiversite`) cohérents avec `nemeton` 2026.11.14 (N5 et B5 ajoutés, 43 indicateurs, « stratification verticale » dans la famille B), pour que `R-CMD-check` redevienne vert et que les PR du cycle MCP puissent arriver sur `main`.

**Redite par l'agent :** _à compléter par l'agent avant d'agir — s'il n'y arrive pas, il s'arrête._

## Les trois questions (règle de staffing)

| Question | Réponse | Conséquence |
|---|---|---|
| Peut-il redire la mission en une phrase, avec son but ? | oui | |
| Sait-il comment faire ? | oui (échecs exacts ci-dessous) | |
| A-t-il les moyens ? | oui (nemeton 2026.11.14 installable via `pak::pkg_install("pobsteta/nemeton@v2026.11.14")` dans une bibliothèque temporaire, ou lecture de `/home/pascal/dev/nemeton` en lecture seule) | |

**Temps de la main :** appuyer.

## L'Homme

- **Agent :** `dev` · **Modèle :** sonnet
- **Ce qui ne lui est pas demandé :** intégrer N5/B5 dans l'UI (statuts traduits, couches carte, profil vertical), ni les briefs cœur 064/067/068 — hors cycle, décision de Pascal.

## Harnais

- **Peut :** modifier `tests/testthat/test-app_config.R`, `test-app_infrastructure.R`, `test-utils_i18n_families.R`, la clé `famille_biodiversite` (et toute entrée statique `famille_*` désalignée) dans `R/utils_i18n.R` (`\uXXXX`), `NEWS.md`, `DESCRIPTION` (cycle dev `2026.10.18.9001`).
- **Ne doit pas :** écrire dans `nemeton` ; toucher à `main` ; ajouter de la logique métier ; changer `Remotes:`.
- **Sources :** log CI du run 42768323074 (job 129669967689), `nemeton::indicator_families()` / `indicator_labels()` de 2026.11.14, `CLAUDE.md` (i18n, règles strictes), briefs `/home/pascal/dev/nemeton/specs/064-alphaearth-n5/brief-nemetonshiny.md` et `067-forpt-lidar-latent/brief-nemetonshiny.md` (lecture seule, pour comprendre N5/B5).

## Échecs à lever (CI du 2026-11-10, nemeton 2026.11.14)

- `test-app_config.R:330` : `get_all_indicator_codes` longueur attendue 41, obtenue 43.
- `test-app_config.R:431` : `get_column_family_map` longueur attendue 82, obtenue 86.
- `test-app_infrastructure.R:137` : `get_all_indicator_codes` longueur 41 / 43.
- `test-utils_i18n_families.R:144-145` : description FR/EN statique de `famille_biodiversite` ≠ cœur (« … et stratification verticale » / « … and vertical stratification »).

Préférer, quand c'est possible, des tests qui **dérivent** le nombre attendu du cœur (`indicator_labels()`) plutôt qu'un nouveau chiffre en dur qui recasserait à la prochaine release ; le dire dans le rendu.

## Dépend de / rouge attendu jusqu'à

- aucune ; **à fusionner avant #253 et #254** (elle lève le rouge de toutes les PR).

## Tests

- Les 5 tests ci-dessus verts contre nemeton 2026.11.14 ; suite complète `devtools::test()` sans nouvel échec (le test-command filtré `mcp` ne suffit pas ici, la mission touche hors MCP) ; `grep -nP "[^\x00-\x7F]" R/*.R` vide.

## Terrain

- **Échantillon que Pascal vérifiera :** l'infobulle de la famille Biodiversité affiche « stratification verticale ».

## Qui veille, qui tranche

- **Relecteur :** `relecteur`, puis Pascal. Critère d'arrêt : 3 allers-retours. Qui tranche : Pascal.

## Comment saura-t-on que ça marche ?

`R-CMD-check` vert sur la PR 014, puis sur #253 et #254 après rebase.

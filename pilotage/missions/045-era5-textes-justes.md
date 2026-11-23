# Mission 045 — ERA5 : nommer la bonne source, annoncer la vraie durée

> Créée par l'orchestrateur le 2026-11-23 (Pascal : « oui », points 1 et 2 seulement). Solde
> en partie `~/dev/briefs/vers-nemetonshiny/2026-11-23-era5-textes-et-attente-cds.md` — **le lire
> en entier** (constat Colombière : ~4 h de file CDS par mois, ~48 h pour 24 mois).
> Le point 3 (afficher l'attente en file, événement `era5_attente`) attend nemeton 2026.11.40 :
> **hors périmètre**.

## Mission

**En une phrase :** les textes de l'app disent « ERA5 » (0,25°, `reanalysis-era5-single-levels`)
et non « ERA5-Land », et annoncent pour le premier calcul plusieurs heures à deux jours d'attente
dans la file du CDS au lieu de « minutes à heures ».

## Travail attendu (lieux du brief, relevés à `77da50b8` : les retrouver sur `main`)

1. **Source.** Remplacer « ERA5-Land » par « ERA5 » (FR et EN) dans les clés i18n
   `regen_forcing_era5`, `regen_engine_tip`, le texte d'aide de `R/utils_i18n.R` ~6280-6287,
   la description BILJOU ~6656-6657, et les commentaires / roxygen de
   `R/service_regeneration.R` ~502 et ~603. Chercher toute autre occurrence (`grep -rn "ERA5-Land" R/ inst/`)
   et signaler celles qui ne parlent pas du forçage microclimf / BILJOU au lieu de les changer
   à l'aveugle.
2. **Durée.** `regen_engine_tip` : « plusieurs heures à deux jours au premier calcul (attente
   dans la file du CDS, ~12 requêtes par année), quelques minutes ensuite (cache partagé) » —
   FR et EN. `regen_engine_era5_interrupted` : retirer « throttle » (inapplicable à une attente
   en file), le message reste pour les vraies coupures ; le motif `grepl` (~906) ne change pas.
3. Encodage `\uXXXX`, clés existantes conservées (pas de nouvelle clé). Si le roxygen change,
   `devtools::document()`.

## Tests

- Plus aucune occurrence de « ERA5-Land » dans `TRANSLATIONS` pour le forçage microclimf / BILJOU
  (test dans `test-utils_i18n.R` ou un fichier ciblé) ; `regen_engine_tip` mentionne la file CDS
  dans les deux langues. Mutation rouge (remettre « ERA5-Land »).
- Lancer les fichiers touchés et `test-utils_i18n.R`, `test-mod_regeneration*.R`,
  `test-service_regeneration*.R`, fichier par fichier, `timeout 540`.
- `grep -nP "[^\x00-\x7F]" R/*.R` sans ajout.

## Versionnage

`2026.11.4.9001`, entrée NEWS (`fix`, textes). Pas de plancher.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/main`, branche `claude/045-era5-textes-justes`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : infobulle du moteur ERA5 dans reGénération, FR et EN.

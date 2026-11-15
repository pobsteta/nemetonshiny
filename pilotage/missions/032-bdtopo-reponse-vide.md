# Mission 032 — Une réponse BD TOPO vide est une mesure (W1 = 0, pas NA)

> Créée par l'orchestrateur le 2026-11-15 sur ordre de Pascal (« traite-les tous […] continue jusqu'au
> bout »). Solde le brief `~/dev/briefs/vers-nemetonshiny/2026-11-15-w1-reponse-wfs-vide.md` (session
> nemeton, `fix:`, origine : essai VICTOR sur Couchey, W1 = NA sur les 63 UGF) — **le lire en entier**.

## Mission

**En une phrase :** `download_ign_bdtopo()` écrit et rend une couche vide quand le WFS répond sans
erreur avec 0 objet (W1 = 0 au lieu de NA), garde `NULL` en cas d'échec, et télécharge le réseau
hydrographique sur une emprise élargie d'au moins 500 m.

## Constat (orchestrateur, lu sur `main` le 2026-11-15)

- `R/service_compute.R:3245-3305` `download_ign_bdtopo(layer_name, bbox, cache_file)` :
  `:3285-3287` rend `NULL` sur `nrow(result) == 0` comme sur un échec ; `:3297` écrit le cache ;
  `:3303` échec réseau → `NULL`.
- Appelée depuis `:2928` (`"ign_bd_topo" = download_ign_bdtopo(source_name, bbox, cache_file)`) :
  chercher où la `bbox` est calculée (parcelles + marge) et comment le cache (`.bbox.json`) est relu.
- Couches concernées : `water_network`, `water_surfaces`, `roads`, `buildings`.

## Travail attendu

1. Réponse vide réussie → `sf` à 0 ligne en Lambert 93 (géométrie typée, ex.
   `sf::st_sf(geometry = sf::st_sfc(crs = 2154))`), écrite dans `cache_file` avec son `.bbox.json`
   (même motif qu'un cache plein), et rendue. Vérifier que l'écriture GPKG d'un `sf` vide fonctionne
   et que sa relecture rend bien un `sf` à 0 ligne (pas `NULL`) partout où le cache est relu.
2. Échec (`error`, `NULL` de happign) → `NULL` comme aujourd'hui.
3. `water_network` : emprise élargie d'au moins 500 m (`proximity_m` du W1) ; la clé du cache doit
   tenir compte de cette emprise pour qu'un ancien cache plus étroit ne soit pas resservi.
4. Vérifier qu'aucun autre consommateur de ces couches (carte, S1/S2 routes et bâti, R1, desserte…)
   ne casse sur un `sf` vide (chercher les lecteurs ; un `nrow == 0` doit donner un résultat neutre,
   pas une erreur).

## Tests

WFS mocké (`local_mocked_bindings` sur `happign::get_wfs` ou l'indirection existante) : 0 objet →
fichier écrit vide, relu vide, et `indicateur_w1_reseau()` = 0 (appel réel si le nemeton installé le
permet, sinon skip) ; erreur → pas de fichier, `NULL` ; bbox `water_network` élargie de ≥ 500 m.
Mutation rouge (remettre `return(NULL)`). Fichier de test dédié conseillé (`test-service_compute.R`
est très long). `timeout 540`.

## Versionnage

`2026.11.1.9007`, entrée NEWS (`fix`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/032-bdtopo-vide`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain : relancer le calcul de Couchey, W1 doit valoir 0 (ou un score) et non NA.

## Statut

Terminée le 2026-11-15 : PR #290 fusionnée (`4d1ccf55`), `.9007`. Relecture : fusionnable, 57 tests, mutation rouge. Réserve transmise au cœur : S1/S2 restent NA sur couche vide (`vers-nemeton/2026-11-15-s1-s2-couche-vide.md`). Reste : relancer Couchey (W1 attendu 0).

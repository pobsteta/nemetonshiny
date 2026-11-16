# Mission 035 — W1 lit les sources : couche BD TOPO `water_points` (cœur nemeton 2026.11.34)

> Créée par l'orchestrateur le 2026-11-16 (« tous les briefs sont codés ? »). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-16-w1-couche-water-points.md` (cœur, PR nemeton#661) et sa
> copie amont `2026-11-16-w1-sources-couche-detail-hydrographique.md` (nemetonmcp) — **les lire**.
> Cœur au tag : `gh api "repos/pobsteta/nemeton/contents/R/indicators-families.R?ref=v2026.11.34" --jq .content | base64 -d`
> et `inst/datasources/FR.json` au même tag (lecture seule).

## Mission

**En une phrase :** le calcul télécharge la couche BD TOPO `detail_hydrographique` entière, sur
l'emprise élargie de W1, et la passe au cœur sous le nom `water_points`, pour que les sources
comptent dans le bonus de proximité de W1.

## Constat (orchestrateur, lu sur `main` le 2026-11-16)

- `R/service_compute.R:160` déclaration `water_network` dans `DATA_SOURCES` ; `:1667` libellé de
  source ; `:2921` élargissement d'emprise (`.bbox_elargie(bbox, .W1_PROXIMITE_M)`, mission 032) ;
  `:3313` typename BD TOPO. Suivre le même motif à chaque endroit.
- Mission 032 : une réponse WFS vide est une couche vide (mesure), un échec est `NULL`.

## Travail attendu

1. `water_points` dans `DATA_SOURCES` (`source = "ign_bd_topo"`, typename
   `BDTOPO_V3:detail_hydrographique`, `required_for = "indicateur_w1_reseau"`), libellé de source
   i18n si les autres couches en ont un, et partout où `water_network` est énuméré (config FR,
   cache, carte des sources…) si c'est pertinent.
2. Même emprise élargie que `water_network` ; **aucun filtre** côté app (le cœur filtre natures et
   état).
3. Couche absente (échec WFS) : W1 **ne doit pas** devenir NA — ne pas passer `water_points` au
   cœur, il se replie sur le réseau de surface. Couche vide : la passer (mesure). Vérifier que
   l'échec de cette couche ne bloque ni ne marque en échec le calcul de W1 ou de la famille W.
4. Recalcul : **pas** d'invalidation automatique des projets existants (on ne touche pas aux
   `indicators.parquet` des utilisateurs) ; l'entrée NEWS dit qu'il faut relancer le calcul pour
   que W1 tienne compte des sources. Si l'app sait déjà signaler qu'un calcul a été fait avec un cœur
   plus ancien, le dire au rapport ; sinon ne rien créer.
5. Plancher `Imports: nemeton (>= 2026.11.34)`.

## Tests

WFS mocké (motif `tests/testthat/test-bdtopo_reponse_vide.R`) : `water_points` téléchargé sur
l'emprise élargie, passé au cœur dans `layers` ; échec → absent de `layers`, W1 calculé ; vide →
passé. Si le nemeton installé est ≥ 2026.11.34 (il l'est), un appel réel de
`indicateur_w1_reseau()` avec une UGF sans cours d'eau et une source `Source`/« En service » à
100 m : W1 > 0 ; avec une `Fontaine` seule : 0. Mutation rouge. Filtre
`bdtopo|service_compute` ciblé (service_compute est long : `test_file` + `filter`), `timeout 540`.

## Versionnage

`2026.11.2.9003` (la mission 034, en PR #297, prend `.9002` : si elle est fusionnée avant toi,
rebase sur `origin/main` et garde `.9003` au-dessus de son entrée NEWS).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/035-w1-sources`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain : recalcul de Couchey → 6 UGF avec un bonus W1 de 3,5 à 40,6 (Trou de Fiat), 57 à 0.

## Statut

En cours (2026-11-16).

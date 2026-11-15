# Mission 026 — Durée de chaque fichier de test, commande ciblée sous 300 s

> Créée par l'orchestrateur le 2026-11-15 sur demande de Pascal (« fais-le »). Issue de la cause
> n° 2 du Pareto du bilan du 2026-11-15 (tests dépendant de l'extérieur, 9 %) et de l'écart 025 au
> journal : `test-service_mcp_detache.R` ne finit pas sous 400 s en local, vert en CI.

## Mission

**En une phrase :** mesurer la durée de chacun des fichiers `tests/testthat/test-*.R` (182 au
2026-11-15), marquer ceux qui dépassent 60 s avec leur cause, et proposer une commande de tests
ciblée qui tient sous 300 s pour le hook et le relecteur.

## Travail attendu

1. Mesurer, fichier par fichier, avec un plafond `timeout 400` par fichier (un fichier qui
   dépasse est noté « > 400 s », pas relancé) :
   `Rscript -e 'pkgload::load_all(".", quiet=TRUE); testthat::test_file("tests/testthat/<f>", reporter="silent")'`.
   Noter : durée murale, statut (vert / échec / dépassement), nombre de `skip`.
2. Pour chaque fichier > 60 s : la cause probable, lue dans le fichier (réseau, `future` /
   processus détaché, Quarto, raster lourd, `Sys.sleep`, AppDriver…), avec la ligne.
3. Classer en Pareto : quelques fichiers font-ils l'essentiel du temps ?
4. Proposer :
   - la commande ciblée pour `.claude/test-command` (aujourd'hui
     `devtools::test(filter = "mcp")`) qui tient sous 300 s ;
   - pour chaque fichier lent, une piste (garde `skip_on_cran()` / variable d'environnement,
     mock, réduction de fixture), **sans l'appliquer**.

## Ce que la mission ne fait pas

Aucune modification de code ni de test. Les pistes deviennent des missions `dev` si Pascal les
retient.

## Livrable

Un rapport rendu à l'orchestrateur (tableau trié par durée, Pareto, commande proposée), versé par
l'orchestrateur au journal et dans la section Statut ci-dessous.

## Moyens

`analyste` (sonnet), lecture seule, sur l'arbre `main` à jour. Temps de la main : *lâcher*
(mesure sans décision).

## Statut

Confiée le 2026-11-15.

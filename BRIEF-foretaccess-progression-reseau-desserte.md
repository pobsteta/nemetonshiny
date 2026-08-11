# BRIEF cœur foretaccess — reseau_desserte() : aucune progression pendant un calcul de plusieurs dizaines de minutes

> Hand-off depuis la session de dev `nemetonshiny`. **À traiter dans une
> session de dev dédiée sur `/home/pascal/dev/foretaccess`** (un repo =
> une session). Repo concerné : `pobsteta/foretaccess`. Touche le
> **noyau Rust**, pas seulement la couche R. Versions observées :
> `foretaccess 2.0.1`, `nemetonshiny 0.121.9`. Confort d’usage, pas
> correction de bug : rien n’est cassé, mais l’attente est aveugle.

## 1. Symptôme côté app

Sur le projet **Dabo** (4 UGF, 775 ha, tampon 1 km), « Générer la
desserte » a donné à l’utilisateur l’impression de ne rien faire. Le
calcul partait bien — bouton grisé, tâche lancée, chronomètre qui tourne
— mais a dépassé **22 minutes sans rendre la main**, sans aucun signe
d’avancement.

Nous avons corrigé ce que nous pouvions côté app (`nemetonshiny`
v0.121.9) :

- l’avertissement de durée indexait la durée sur le **nombre de
  parcelles**, ce qui est faux — le glouton travaille sur une grille à 5
  m, donc le coût dépend de la **surface** de l’emprise. Corrigé ;
- `run_desserte()` publie désormais sa phase sur un canal disque,
  affiché dans la notification : « Moteur glouton (étape la plus longue)
  (6/6) ».

**Ce que l’app ne peut pas faire** : descendre sous la granularité de
l’étape. Or c’est précisément l’étape « moteur » qui porte l’essentiel
du temps.

## 2. Où le temps passe

Mesures dans `nemetonshiny/R/service_desserte.R` (en-tête, AOI
Chastel-Nouvel, 30 parcelles / 31 ha, ~302 k cellules) :

| étape | durée |
|----|----|
| `preprocess()` + `surface_cout_construction()` | **\< 2 s** |
| acquisitions (MNT, desserte, forêt) | quelques secondes, mises en cache |
| **`reseau_desserte(mode = "glouton")`** | **~11,5 min** |

Sur Dabo (grille ~1,35 M cellules, soit 4,5× Chastel-Nouvel), la même
étape dépasse 22 min. Les cinq autres phases sont donc du bruit : **une
barre de progression n’a de sens que dans le moteur**.

## 3. Constat technique — le point d’accroche est dans Rust, pas dans R

`reseau_desserte()` n’expose aucun paramètre de progression :

    formals(reseau_desserte) :
      pre, cout, parcelles, desserte_existante, heuristique, mode,
      skidding_m, volume_champ, pondere_cout, config, graine

Aucun `progress`, `callback`, `verbose` ou `quiet`. Et la couche R ne
boucle pas : elle prépare puis délègue **en un seul appel**.

``` r

.reseau_glouton <- function(ctx, parcelles, heuristique, skidding_m, volume_champ, graine) {
  parc_ids <- .desserte_cellules_parcelles(parcelles, ctx$grille, volume_champ)
  ordre    <- .desserte_ordre(parc_ids, ctx$road_r, heuristique, graine)
  sources0 <- as.integer(parc_ids$cells[ordre] - 1L)
  res <- do.call(desserte_reseau, c(list(..., sources = sources0, ...)))   # <- UN appel
  list(paths = res$paths, costs = res$costs)
}
```

``` r

desserte_reseau <- function(...) .Call(wrap__desserte_reseau, ...)   # extendr
```

**La boucle par parcelle vit donc dans le noyau Rust.** Toutes les
sources sont passées d’un coup et tous les chemins reviennent d’un coup.

## 4. Deux fausses bonnes idées, à écarter d’emblée

**a) Ajouter un argument `progress = function(i, n)` en R et le rappeler
depuis Rust.** R est mono-thread et son API n’est pas *thread-safe* :
rappeler une closure R depuis un thread Rust est une source de plantages
difficiles à diagnostiquer. À n’envisager que si le solveur est garanti
mono-thread et que l’appel se fait sur le thread R.

**b) Découper l’appel côté R, une parcelle à la fois.** Cela
**changerait les résultats**. Le glouton est séquentiel par construction
: chaque parcelle se raccorde au réseau *tel qu’augmenté par les
parcelles déjà tracées*. Reproduire cela en R supposerait de repasser
`network0` enrichi à chaque itération, c’est-à-dire réimplémenter en R
la logique d’accumulation du noyau — avec un risque de divergence
silencieuse. À écarter.

## 5. Correctif demandé

**Un canal de progression écrit par Rust, poll-é par R.** C’est le seul
mécanisme qui traverse la frontière de processus sans toucher à la
sûreté de R.

1.  `desserte_reseau()` gagne un argument optionnel — p. ex.
    `progress_path = NULL`. Quand il est fourni, le noyau écrit après
    **chaque parcelle traitée** un petit JSON :

    ``` json
    {"i": 17, "n": 42, "ts": 1786000000}
    ```

    Écriture **atomique** (fichier temporaire + `rename`) pour qu’un
    lecteur ne voie jamais un JSON tronqué. Jamais fatale : un échec
    d’écriture ne doit pas interrompre le calcul.

2.  `reseau_desserte()` relaie l’argument, sous un nom stable côté R.

3.  Documenter que le fichier est **supprimé par l’appelant**, pas par
    le moteur — sinon un fichier résiduel ferait afficher une
    progression périmée au lancement suivant.

Ce contrat est exactement celui que `nemetonshiny` consomme déjà pour
reGénération (`engine_status.json`, poll `invalidateLater(1000)`,
péremption à 120 s). Le branchement côté app serait immédiat :
`run_desserte()` passerait `progress_path` et notre lecteur de phase
afficherait « Moteur glouton (17/42) ».

### Variante plus légère, si l’écriture fichier depuis Rust gêne

Un **compteur atomique en mémoire partagée** exposé par une seconde
fonction (`desserte_reseau_progress()`) que R interrogerait. Moins bien
: le calcul tournant dans un worker `future`, la session principale ne
partage pas la mémoire du worker. **Le fichier est le seul canal qui
traverse la frontière de processus** — et il survit à la mort du worker,
ce qui en fait aussi un post-mortem en cas d’OOM.

## 6. Tests attendus

- Sans `progress_path` : comportement et résultats **strictement
  inchangés** (non-régression).
- Avec `progress_path` : le fichier existe pendant le calcul, `i` croît
  de façon monotone, et `n` est constant et égal au nombre de sources.
- `i == n` à la fin, avant le retour de la fonction.
- Un `progress_path` pointant vers un répertoire non inscriptible **ne
  fait pas échouer** le calcul.
- Invariance : sur une AOI de référence, `paths` et `costs` sont
  identiques avec et sans `progress_path`. C’est le test qui compte — il
  garantit que l’instrumentation n’a pas altéré le solveur.

## 7. Priorité

**Basse.** Rien n’est cassé et l’app affiche déjà l’étape. C’est du
confort sur une attente longue, à traiter quand le noyau sera ouvert
pour autre chose.

Si un travail de **performance** du glouton est envisagé un jour, il
aurait plus de valeur que la progression — et les deux se font au même
endroit. À noter que `nemetonshiny/R/service_desserte.R` documente déjà
que le mode `steiner` (N² tracés, estimé \> 5 h à 30 parcelles) et les
optimiseurs (`optimiser_reseau`) restent **non exposés dans l’app** pour
cette raison.

## 8. Références

- Point d’entrée Rust : `desserte_reseau()` →
  `.Call(wrap__desserte_reseau, …)`
- Couche R : `reseau_desserte()` → `.reseau_glouton()` →
  `desserte_reseau()`
- Côté app : `nemetonshiny/R/service_desserte.R` (`DESSERTE_PHASES`,
  `.dess_write_phase`) et `R/mod_desserte.R` (`.dess_read_phase`)
- Projet de repro :
  `~/.local/share/nemeton/projects/20260801_130303_xpdk` (Dabo), tampon
  1 km

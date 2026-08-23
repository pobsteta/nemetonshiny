# BRIEF cœur — rattrapage groupé du 2026-08-23

> **Statut** : ouvert, 2026-08-23.
> **Remplace et regroupe cinq briefs** émis séparément par la session app :
> le rattrapage `PLAN.md` (0.125→0.132), sa suite (0.132.1→0.134.1), les
> houppiers MNH, le diagnostic OOM/SIGTERM (soldé), et le correctif
> `pct_veg` d'`opencanopynemeton`.
> **Deux dépôts concernés**, et c'est pourquoi les chantiers sont numérotés :
> `nemeton` pour §1, §2 et §4 ; `opencanopynemeton` pour §3.
> **Contexte de lecture** : `nemetonshiny@main`, releases v0.132.1 → v0.134.1
> publiées, v0.135.0 en préparation.

---

## Pourquoi un seul document

Cinq briefs ouverts en trois jours, dont deux déjà partiellement traités, font
perdre plus de temps qu'ils n'en font gagner : il faut d'abord établir lequel
est encore vrai. Celui-ci dit, pour chacun, **ce qui reste à faire** — le reste
est archivé dans les fichiers d'origine, qui restent en place pour la trace.

| § | Chantier | Dépôt | État |
|---|---|---|---|
| 1 | Journal `PLAN.md` — 4 releases app | `nemeton` | à faire |
| 2 | Houppiers MNH → couche `houppier` | `nemeton` | à faire, **bloque** l'app |
| 3 | `pct_veg` charge deux fois le raster | `opencanopynemeton` | à faire, **bloque** un calcul réel |
| 4 | Diagnostic OOM/SIGTERM | `nemeton` | **fait**, reste à publier |

Priorité si une seule chose doit être faite : **§3**. C'est la seule qui empêche
aujourd'hui un calcul d'aboutir.

---

## §1 — Journal `PLAN.md` : quatre releases à consigner

Le journal s'arrête au 2026-08-22 (v0.132.0). Depuis, quatre releases, toutes
taguées et publiées :

| Release | `nemetonshiny@SHA` | Date | Apport |
|---|---|---|---|
| v0.132.1 | `4c882d83` | 2026-08-22 | Décroisement de F suivi (spec 049) |
| v0.133.0 | `aa12f180` | 2026-08-22 | L'import CSV remplace le projet courant |
| v0.134.0 | `053d6082` | 2026-08-23 | Le plafond mémoire appartient au cœur |
| v0.134.1 | `2b9a9f5d` | 2026-08-23 | La prudence ne recouvre plus une certitude |

**v0.132.1 — spec 049.** Les quatre contrôles du brief passent sans toucher au
code de production : la table venant du cœur ligne par ligne depuis le dé-fork
de v0.127.0, la correction traverse l'app seule. Un test figeait en revanche le
croisement en **fixture** et tombait à la publication — d'où l'amendement
« Portée réelle » que vous avez ajouté à la spec. Effet de bord à consigner :
plus aucune famille n'étant croisée, ces tests ne peuvent plus *distinguer* une
lecture par colonne d'une lecture par slug ; ils ne gardent plus que la
concordance.

**v0.133.0 — import CSV destructif.** Un import supprime et remplace désormais
le projet courant, toutes composantes comprises. L'ordre protège : nouveau
projet créé, chargé et croisé, *puis* destruction de l'ancien ; tous les chemins
d'échec repartent avant ce point. Corrigé au passage : `app_state$project_id` ne
suivait pas `current_project`, ce qui envoyait les commentaires du nouveau
projet dans le répertoire du précédent et laissait l'ancien verrouillé.
**Décision de Pascal, à acter** : la suppression reste **disque seul**, aucune
ligne PostGIS n'est effacée — ni ici, ni par le bouton Supprimer.

**v0.134.0 — plafond mémoire.** Implémente le brief `2026-08-22-plafond-memoire`
(cœur v0.183.0). L'app supprime `.compute_memory_max()`,
`.total_memory_bytes()` et `.capped_memory_max()` ; plus aucun site d'appel ne
passe `memory_max`. Un test interdit toute fraction de RAM côté app, pour que
les trois plafonds concurrents ne repoussent pas.

**v0.134.1 — cf. §4.**

---

## §2 — Houppiers MNH : la couche que l'app ne peut pas produire

**Demandeur** : export Marculus (`nemetonshiny` v0.135.0). L'app écrit déjà les
couches `parcelle` et `desserte` du GeoPackage que lit l'application de
martelage ; il manque `houppier`, qui pré-remplit la **hauteur** d'une tige par
un point-dans-polygone sur la position GNSS.

Le détail complet — signature proposée, contraintes venues de l'aval, chiffres
mémoire — est dans **`specs/BRIEF-nemeton-houppiers-mnh.md`**, qui reste la
référence. L'essentiel :

- fonction exportée du type `segment_houppiers(chm, aoi, ws, hmin, algorithme)`,
  rendant un `sf` POLYGON avec **`h_max`** en mètres ;
- hauteurs hors **1–70 m** à ne pas produire (le téléphone les rejette) ;
- recouvrements permis : l'aval retient le houppier **le plus haut** ;
- **ré-échantillonner le MNH à 0,5–1 m avant de segmenter** — un houppier fait
  3 à 10 m de diamètre, et le MNH de Couchey fait 418 M cellules à 0,20 m ;
- `h_max` par zonale en streaming, jamais par `values()` global — cf. §3, c'est
  exactement l'erreur qui a coûté une soirée ;
- `lasR` (déjà dépendance) expose `chm`, `local_maximum`, `region_growing`,
  `hulls`.

C'est de la **logique métier** : règle 1 du `CLAUDE.md` de l'app, l'app appelle
et écrit, elle ne calcule pas.

---

## §3 — `opencanopynemeton` : `pct_veg` charge deux fois le raster entier

**La seule urgence de ce document.** Détail complet dans
**`specs/BRIEF-opencanopy-pct-veg-values.md`**.

`R/pipeline_aoi_to_chm.R`, lignes 1827-1828 :

```r
pct_veg <- sum(values(clean_mask, na.rm = TRUE)) /
           sum(!is.na(values(clean_mask))) * 100
```

418 528 295 cellules. `values()` rapatrie la couche entière, **deux fois**, plus
la copie du `!is.na()` : ≈ 4,7 Go de vecteurs R pour un pourcentage qui ne sert
qu'à un `message()`. Le `mask()` de la ligne précédente streame correctement et
a écrit son fichier ; c'est la ligne d'après qui emporte le processus.

Remède : `pct_veg <- as.numeric(global(clean_mask, "mean", na.rm = TRUE)) * 100`.
Sur un masque logique, la moyenne des `TRUE` en ignorant les `NA` **est** la
proportion cherchée.

À vérifier en corrigeant : `grep -n "values(" R/` — ce qui a tué ce run à 0,20 m
tuera le suivant.

**Coût observé de ce défaut** : 3 h 20 de CPU, 11 Go de cache, **zéro
indicateur** — le pipeline meurt à sa dernière étape, après avoir écrit tous ses
livrables.

---

## §4 — Diagnostic OOM/SIGTERM : fait, mais pas publié

Le travail est **complet et accepté** : `nemeton 0.183.1` nomme le scope
transitoire et lit son `Result` auprès de systemd au lieu de l'inférer d'un code
de sortie. L'app a suivi en v0.134.1 (elle n'atténue plus ce que le cœur
affirme, et réserve la formulation prudente au mode dégradé).

**Ce qui reste** : la release. Au 2026-08-23, `v0.183.1` vit sur la branche
`fix/oom-diagnostic-scope-result`, **ni commitée, ni mergée, ni taguée** —
`gh release list --repo pobsteta/nemeton` s'arrête à `v0.183.0`. Or l'app tire
`@*release`, c'est-à-dire les **tags** : tant que celui-ci n'existe pas,
personne ne bénéficie du correctif, et les deux premières lignes du tableau de
la v0.134.1 restent théoriques.

Deux corrections que vous avez apportées à mon brief initial, et que je reprends
à mon compte : mon explication du `-15` (« processx observe le client
systemd-run ») n'était pas le mécanisme réel, et le code de sortie n'est probant
dans **aucun** sens — d'où l'option « constater » plutôt que « inférer », qui
était le bon choix pour une raison plus forte que celle que j'avançais.

**Reliquat non traité, signalé par vous** : `.reconfort_run_py()` ne rend qu'un
code de sortie et reste aveugle au même défaut. L'outillage l'attend (`unit =`,
`.capped_scope_result()`). Hors périmètre app : ce chemin ne passe pas par
`.compute_error_message()`.

---

## Récapitulatif des fichiers de référence

Les cinq briefs d'origine restent en place ; ce document dit lequel est encore
actif, eux disent le détail.

| Fichier | Sort |
|---|---|
| `specs/BRIEF-nemeton-plan-md-0.125-0.132.md` | **appliqué** ; sa §3 est périmée, remplacée par §1 ci-dessus |
| `specs/BRIEF-nemeton-houppiers-mnh.md` | **actif** — référence du §2 |
| `specs/BRIEF-opencanopy-pct-veg-values.md` | **actif** — référence du §3 |
| `specs/BRIEF-nemeton-oom-sigterm-scope.md` | **soldé** — reste la release (§4) |

# BRIEF cœur `foretaccess` — ce qu'il nous manque pour exposer les fonctions desserte encore absentes de l'app

> # 🔀 REMPLACÉ (2026-08-11)
> Ce brief est repris dans **`BRIEF-foretaccess-desserte-consolide.md`**, qui
> regroupe les trois chantiers `foretaccess` encore ouverts. Ne pas le traiter
> séparément — conservé pour la trace du raisonnement.


> Hand-off depuis la session de dev `nemetonshiny`.
> **À traiter dans une session de dev dédiée sur `/home/pascal/dev/foretaccess`** (un repo = une session).
> Repo concerné : `pobsteta/foretaccess`. Versions : `foretaccess 2.0.1`, `nemetonshiny 0.121.11`.

## 0. Rectification de cadrage, avant tout

La demande initiale était « exposer les dernières fonctions non encore exposées ». **Elle n'a pas lieu d'être** : toutes sont déjà **exportées** par `foretaccess`. Ce qui manque est du travail d'intégration **côté app**.

Et surtout : en préparant ce brief, nous avons découvert que **notre propre justification d'exclusion est périmée**. L'en-tête de `nemetonshiny/R/service_desserte.R` affirme que les optimiseurs et le mode Steiner ne sont pas exposés « tant qu'un travail perf n'a pas eu lieu côté `foretaccess` ». Ce travail **a eu lieu**, et votre documentation le dit :

> `reseau_desserte()` `@section Performance` — « A\* **bounded to the corridor** […] a per-parcel trace drops from **minutes to milliseconds**, and with a realistic `skidding_m` the whole greedy runs in **seconds to tens of seconds** rather than minutes. »
>
> `optimiser_reseau()` `@section Performance` — « the optimisers are now **tractable at interactive scale**: on a departmental run, `n_start = 16` costs about the same wall-clock as one greedy build. Reasonable exposable defaults: `n_start` 8-32, `n_iter` 100-300 — **no hard cap is needed** below those. »

Vous nous donnez donc déjà les valeurs à exposer. Il n'y a rien à demander sur ce lot : **c'est à nous de le faire**, et nous mettrons à jour notre en-tête. Ce brief ne porte que sur ce qui reste réellement flou.

## 1. La seule demande bloquante : déclarer `dessertR`

`dessertR` n'apparaît **nulle part** dans le DESCRIPTION de `foretaccess` — ni `Imports`, ni `Suggests`, ni `Remotes`. Il est résolu à l'appel :

```r
.dsr <- function(nom) getExportedValue(.PKG_DESSERTR, nom)   # .PKG_DESSERTR = "dessertR"
```

Quinze fonctions `dsr_*` sont consommées ainsi (`dsr_detecter`, `dsr_measure`, `dsr_repositionner`, `dsr_reseau`, `dsr_trafficability`, `dsr_seuils_grumier`, `dsr_conductivite`, …).

**Conséquences pour nous :**

- notre `Imports: foretaccess (>= 2.0.1)` **n'installe pas** `dessertR`. Sur un poste neuf, deux fonctionnalités déjà livrées cassent : la correction LiDAR de la desserte et le contrôle d'intégrité du réseau ;
- l'échec n'est pas actionnable. Pour `qualifier_desserte()`, c'est l'erreur brute `there is no package called 'dessertR'` ;
- **pire, pour `verifier_integrite_desserte()`, il n'y a pas d'échec du tout.** `.integrite_calculer()` enveloppe `.dsr("dsr_reseau")` dans un `tryCatch(error = NULL)`, et la fonction retombe sur `.integrite_vide()`, dont `n_infractions` vaut `NA`. Affiché tel quel, ce résultat se lit comme **« aucune infraction »** — une fausse réassurance sur exactement la question posée. Nous avons dû poser un `requireNamespace("dessertR")` de notre côté pour ne pas afficher un bilan vide (nemetonshiny v0.121.11).

**Demandes** : déclarer `dessertR` (au minimum en `Suggests` + `Remotes`), et remplacer la dégradation silencieuse de `.integrite_calculer()` par une erreur explicite — ou, à défaut, exposer un prédicat public du type `dessertR_disponible()` pour que les appelants gardent leurs actions proprement.

Fonctions concernées : `detecter_desserte`, `acquire_desserte_lidar`, `qualifier_desserte`, `verifier_integrite_desserte`. Les autres (`acquire_desserte_osm`, `comparer_desserte_osm`, `tracer_desserte`, `optimiser_reseau`, `detecter_desserte_balayage`) n'y touchent pas.

## 2. Enveloppes de coût manquantes

Nous avons quatre fois cette session pris une décision d'intégration sur une hypothèse de coût fausse — MNT à 0,5 m, CVAT, contrôle d'intégrité, `skidding_m`. Nous ne recommencerons pas à l'aveugle.

`reseau_desserte()` et `optimiser_reseau()` ont une `@section Performance` exemplaire. **Les cinq suivantes n'ont aucune indication de coût** :

| fonction | ce que nous ignorons |
|---|---|
| `acquire_desserte_osm()` | coût du téléchargement Overpass et son échelonnement avec l'emprise ; y a-t-il un pavage comme pour le WFS ? |
| `comparer_desserte_osm()` | coût du recoupement, et sens attendu de `corridor_m` |
| `detecter_desserte()` | c'est le plus incertain : détection LiDAR sur MNT + nuage. Ordre de grandeur sur une emprise de ~1 000 ha ? Pic mémoire ? |
| `detecter_desserte_balayage()` | idem, avec en plus le facteur multiplicatif du balayage de `seuils` |
| `tracer_desserte()` | tracé unique entre waypoints : nous supposons « quasi instantané », à confirmer |

**Une phrase par fonction suffit** — même approximative, du type « quelques secondes », « proportionnel à l'emprise », « lire tout le nuage : compter N Go ». Ce qui nous coûte cher, ce n'est pas la lenteur, c'est de la découvrir après l'avoir mise dans un bouton.

Contexte de dimensionnement chez nous : AOI typique 700 à 3 000 ha, grille à 5 m (0,6 à 2,4 M cellules), MNT LiDAR HD à 0,5 m, poste de 31 Go partagé avec RStudio — `systemd-oomd` y tue la session au-delà de ~50 % de pression.

## 3. Intention d'usage : lesquelles sont destinées à l'utilisateur final ?

Certaines de ces fonctions ressemblent à des **primitives de composition** plutôt qu'à des actions d'interface. Nous préférons demander que supposer.

- `desserte_reseau_multistart()` / `_recuit()` / `_riprute()` prennent des vecteurs bruts (`alt`, `obs`, `nr`, `nc`, `sources`…) : ce sont manifestement les entrées Rust de bas niveau, et `optimiser_reseau()` en est la façade. **Confirmez-vous que seul `optimiser_reseau()` doit être exposé ?**
- `specs_desserte_calibrees()` ne prend aucun argument : constantes de calibrage à afficher, ou détail d'implémentation ?
- `detecter_desserte()` face à `qualifier_desserte()` : la première détecte des routes **absentes** de la BD TOPO, la seconde requalifie l'**existant**. Notre lecture est-elle juste, et les deux sont-elles complémentaires dans un même flux, ou exclusives ?
- `desserte_dist_to_end()` : primitive interne, ou utile en propre ?

## 4. Ce que nous comptons faire, sans rien vous demander

Pour que vous sachiez où nous allons, et nous arrêtiez si c'est à côté :

1. **Optimiseurs** — exposer `optimiser_reseau()` avec vos défauts recommandés (`n_start` 8-32, `n_iter` 100-300), en action séparée du calcul de création, comme le contrôle d'intégrité. Corriger notre en-tête périmé.
2. **Mode Steiner** — l'exposer en second choix de moteur. Votre doc le décrit comme « a quality alternative at the cost of N² traces » ; avec le corridor et un `skidding_m` réaliste, notre estimation de « > 5 h » n'a plus de fondement. Nous mesurerons avant de livrer.
3. **Complément OSM** — `acquire_desserte_osm()` + `comparer_desserte_osm()`, sous réserve du §2. Votre spec 028 annonce 92,9 % de pistes validées sur 24 tronçons : le gisement paraît réel.
4. **Détection** — `detecter_desserte()` en dernier, car c'est la plus incertaine en coût et la seule à dépendre de `dessertR` parmi les nouveautés.

## 5. Références

- App : `nemetonshiny/R/service_desserte.R` (en-tête à corriger, appel `reseau_desserte`), `R/mod_desserte.R` (panneaux typage + intégrité, motif d'action séparée)
- Cœur : `reseau_desserte()` et `optimiser_reseau()` `@section Performance` ; `.dsr()` / `.PKG_DESSERTR` ; `.integrite_calculer()` (dégradation silencieuse)
- Mesures app de cette session : `skidding_m = 0` → jamais fini en 22 min contre 39,7 s à 300 m ; `verifier_integrite_desserte()` 376,8 s sur 3 122 tronçons

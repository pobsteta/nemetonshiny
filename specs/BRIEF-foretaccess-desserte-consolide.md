# BRIEF cœur `foretaccess` — desserte : les trois chantiers ouverts, consolidés

> Hand-off depuis la session de dev `nemetonshiny`.
> **Un seul repo à ouvrir : `pobsteta/foretaccess`.** Les trois sujets touchent le même périmètre desserte/détection et peuvent se traiter d'une traite.
> Versions : `foretaccess 2.0.1`, `dessertR 1.3.0`, `nemetonshiny 0.121.15`.
> **Remplace** `BRIEF-foretaccess-exposition-fonctions-desserte.md` et `BRIEF-foretaccess-specs-detection-incompatibles.md`, et reprend le §5 de `BRIEF-dessertR-classement-osm-et-cout-terrassement.md`.

---

## Vue d'ensemble

| # | Sujet | Nature | Priorité |
|---|---|---|---|
| **A** | `dessertR` non déclaré, et dégradation silencieuse de l'intégrité | **Bug** | **Haute** |
| **B** | `detecter_desserte(specs =)` n'accepte pas la calibration `dessertR` | Impasse d'API | Moyenne |
| **C** | Enveloppes de coût et intention d'usage des fonctions non documentées | Information | Basse |
| **D** | Coût de terrassement : branche non poussée | Arbitrage | Basse |

Seul **A** casse quelque chose aujourd'hui. **B** ferme une voie que `dessertR` recommande lui-même. **C** nous fait décider à l'aveugle. **D** attend votre décision.

---

## A. `dessertR` n'est pas déclaré — et son absence ne se voit pas

### A.1 La dépendance est invisible

`dessertR` n'apparaît **nulle part** dans le DESCRIPTION de `foretaccess` — ni `Imports`, ni `Suggests`, ni `Remotes`. Il est résolu à l'appel :

```r
.dsr <- function(nom) getExportedValue(.PKG_DESSERTR, nom)   # .PKG_DESSERTR = "dessertR"
```

Quinze fonctions `dsr_*` sont consommées ainsi : `dsr_detecter`, `dsr_measure`, `dsr_repositionner`, `dsr_reseau`, `dsr_trafficability`, `dsr_seuils_grumier`, `dsr_conductivite`, `dsr_catalog`, `dsr_etat`, `dsr_grille_reference`, `dsr_indice_detection`, `dsr_layers_dtm`, `dsr_layers_pc`, `dsr_sigma_surf`, `dsr_canaux_dalles`.

**Conséquence** : installer `foretaccess` n'installe pas `dessertR`. Sur un poste neuf, quatre fonctions cassent — `qualifier_desserte`, `verifier_integrite_desserte`, `detecter_desserte`, `acquire_desserte_lidar` — dont deux sont déjà livrées dans notre app. Notre `Imports: foretaccess (>= 2.0.1)` ne peut pas y remédier.

### A.2 Le mode d'échec est pire que l'absence

Pour `qualifier_desserte()`, l'erreur est brute mais visible : `there is no package called 'dessertR'`.

**Pour `verifier_integrite_desserte()`, il n'y a pas d'échec du tout.** `.integrite_calculer()` enveloppe `.dsr("dsr_reseau")` dans un `tryCatch(error = NULL)` et la fonction retombe sur `.integrite_vide()`, dont `n_infractions` vaut `NA`. Rendu tel quel dans une interface, ce bilan vide **se lit comme « aucune infraction »** — une fausse réassurance sur exactement la question posée.

Nous avons dû poser un `requireNamespace("dessertR")` de notre côté (`nemetonshiny 0.121.11`) pour afficher « non contrôlée » plutôt qu'un verdict vide.

### A.3 Demandes

1. **Déclarer `dessertR`** — au minimum `Suggests` + `Remotes`.
2. **Remplacer la dégradation silencieuse** de `.integrite_calculer()` par une erreur explicite. À défaut, exposer un prédicat public (`dessertR_disponible()`) pour que les appelants gardent leurs actions proprement.

---

## B. La calibration locale ne peut pas atteindre la détection

### B.1 Le symptôme

`detecter_desserte()` rend **0 tronçon**, et `dessertR` dit lui-même pourquoi :

```
! Bornes mal adaptees a la donnee : "rugosite (86 %)" sature a une extremite.
i Des bornes calibrees sur un AUTRE massif ne se transportent pas.
i Recalibrer avec `dsr_calibrer_specs()` sur ces donnees, ou `bornes = FALSE`.
```

Nous avons suivi la consigne. **Elle ne peut pas être appliquée.**

### B.2 Ce que nous avons d'abord corrigé chez nous

Avant de conclure à un problème cœur : `run_desserte_detection()` passait le MNT **RGE ALTI 5 m** des autres étapes, alors que `detecter_desserte()` cherche une signature de **micro-relief** et défaute à `dtm_res = 1`.

Chiffré avec `dsr_calibrer_specs()`, qui donne l'AUC de chaque canal :

| MNT fourni | canaux retenus | meilleure AUC |
|---|---:|---:|
| RGE ALTI 5 m | **0 / 7** | 0,56 |
| **LiDAR HD 0,5 m** | **5 / 7** | **0,77** (rugosité) |

À 5 m aucun canal ne valait mieux que le hasard. Corrigé en `nemetonshiny 0.121.15`. **Le signal est donc bien là — et la détection rend toujours 0.**

### B.3 L'écart

```r
dsr_calibrer_specs(couches, reference = desserte)$specs
#> List of 5 : rugosite, openness_pos, vesselness, pente, slrm     (liste PLATE)

foretaccess::specs_desserte_calibrees()
#> List of 3 : geomorpho, surface, c_vessel                         (3 groupes IMBRIQUÉS)
```

Chaque documentation est cohérente avec elle-même — `$specs` est « directement utilisable » par `dsr_conductivite()`, et `detecter_desserte(specs =)` renvoie à `specs_desserte_calibrees()`. Ce sont **deux contrats pour un même mot**.

### B.4 Pourquoi `dessertR` n'est pas en cause

Son conseil est **exact dans son cadre** : un appelant direct de `dsr_detecter()` recalibre et passe le résultat à `dsr_conductivite()`. `dsr_calibrer_specs()` fonctionne d'ailleurs bien — 66 s, 5 canaux retenus.

La voie n'est fermée que pour qui arrive **par `foretaccess`**, qui enveloppe `dessertR` sans exposer le chemin que le code enveloppé recommande.

### B.5 Demandes — trois pistes, par ordre de préférence

1. **`detecter_desserte()` calibre lui-même** sur `specs = "auto"` : il a déjà le MNT et la référence, c'est-à-dire tout ce dont `dsr_calibrer_specs()` a besoin. L'appelant n'a aucune raison de connaître deux vocabulaires de specs.
2. **Accepter la forme `dsr_calibrer_specs()$specs`** en plus de la sienne — détection de forme, ou argument distinct.
3. **Exposer un convertisseur** (`specs_depuis_calibration()`). Charge à vous de dire ce qui se perd : la calibration ne produit pas de canal `surface`, qui vient du nuage.

### B.6 Question ouverte que vous pouvez trancher en une minute

`specs = NULL` restaure des bornes dérivées par quantiles de l'emprise. Pour une exploration mono-projet le compromis nous conviendrait, **mais nous n'avons pas pu le vérifier** : chaque essai coûte 12 minutes sur cette AOI.

Si `specs = NULL` rend un résultat non vide là où le défaut rend 0, dites-le — c'est un contournement immédiat, et **B** redevient du confort plutôt qu'un blocage.

### B.7 Ce que nous n'affirmons pas

Nous ne savons pas s'il y a **quoi que ce soit à détecter** sur ces 31 ha de forêt privée. C'est plausible qu'il n'y ait rien. Notre constat se limite à : la voie recommandée est fermée, donc nous ne pouvons pas trancher entre « bornes inadaptées » et « rien à trouver ».

---

## C. Enveloppes de coût et intention d'usage

### C.1 Ce qui manque

`reseau_desserte()` et `optimiser_reseau()` ont une `@section Performance` exemplaire, et elle nous a servi. **Cinq fonctions n'ont aucune indication de coût** :

| fonction | ce que nous ignorons | ce que nous avons mesuré nous-mêmes |
|---|---|---|
| `acquire_desserte_osm()` | échelonnement, pavage ? | 5,9 s à froid — mais **> 10 min** quand Overpass limite le débit (`Waiting 60s for retry backoff`) |
| `comparer_desserte_osm()` | coût du recoupement | **104 s** sur 3 122 × 544 tronçons |
| `detecter_desserte()` | ordre de grandeur, pic mémoire | **729 s**, pic **> 8 Go** sur 1 855 ha |
| `detecter_desserte_balayage()` | facteur du balayage de `seuils` | non mesuré |
| `tracer_desserte()` | tracé unique entre waypoints | non mesuré |

Nous avons dû mesurer nous-mêmes pour ne pas mettre dans un bouton un traitement de 12 minutes à 8 Go. **Une phrase par fonction suffirait** — « quelques secondes », « proportionnel à l'emprise », « lit tout le nuage : compter N Go ».

Contexte de dimensionnement : AOI de 30 à 3 000 ha, grille 5 m, MNT LiDAR 0,5 m, poste de 31 Go partagé avec RStudio, où `systemd-oomd` tue au-delà de ~50 % de pression.

### C.2 Intention d'usage

- `desserte_reseau_multistart/recuit/riprute` prennent des vecteurs bruts (`alt`, `obs`, `nr`, `nc`…) : entrées Rust de bas niveau dont `optimiser_reseau()` est la façade. **Confirmez-vous que seul `optimiser_reseau()` doit être exposé ?**
- `specs_desserte_calibrees()` ne prend aucun argument : constantes à afficher, ou détail d'implémentation ?
- `detecter_desserte()` face à `qualifier_desserte()` : la première détecte l'**absent**, la seconde requalifie l'**existant**. Complémentaires dans un même flux, ou exclusives ?
- `desserte_dist_to_end()` : primitive interne, ou utile en propre ?

### C.3 Ce que nous avons déjà fait sans rien demander

Pour que vous sachiez où nous allons : `optimiser_reseau()`, le mode Steiner, le complément OSM, le contrôle d'intégrité et la détection sont **tous exposés** dans l'app depuis `0.121.11` à `0.121.15`, chacun en action séparée avec sa mesure. Notre ancienne exclusion des optimiseurs (« tant qu'un travail perf n'a pas eu lieu ») était **périmée** — votre `@section Performance` le disait, nous ne l'avions pas relue.

Mesures utiles en retour, sur ForetAccess (30 parcelles / 31 ha, `skidding_m = 100`, `pondere_cout = TRUE`) :

| moteur | durée | routes | coût |
|---|---:|---:|---:|
| glouton | 6,1 s | 4 | 65 983 |
| `optimiser_reseau` multistart, `n_start = 8` | 9,0 s | 3 | **34 312** (−48 %) |
| Steiner | 694,6 s | 5 | **10 420** (−84 %) |

Steiner divise le coût par 6,3 pour 114× le temps. Sur Dabo (4 parcelles de 110–420 ha déjà desservies) il rend 0, ce qui est correct et non un défaut.

---

## D. Coût de terrassement — arbitrage

Branche `feat/cout-terrassement` (**non poussée**) :

```r
surface_cout_construction(pre, cfg, methode_pente = "terrassement", largeur_m = 4)
```

Votre propre brief pose les conditions : le défaut reste le barème, un banc comparatif sur massif réel est requis avant exposition, et les prix au m³ par défaut n'ont aucune valeur défendable.

**Nous ne demandons rien** : c'est votre décision de pousser ou non. Si vous exposez, nous suivrons la consigne — `radioButtons` à côté du choix de moteur, et `largeur_m` pris de la largeur de plateforme visée, pas d'une constante.

Un point qui joue en faveur du terrassement, mesuré chez nous : depuis que nous passons `pondere_cout = TRUE` (votre brief §1), **le coût est devenu la grandeur que le solveur minimise**. Un barème qui saute de 65 €/m entre 34,9 % et 35,1 % de pente pilote donc directement le tracé, ce qui n'était pas le cas quand la surface de coût n'était utilisée que par son masque.

---

## Références

- App : `nemetonshiny/R/service_desserte.R` (création, optimisation, intégrité, détection, OSM), `R/mod_desserte.R` (panneaux)
- Cœur : `detecter_desserte()`, `specs_desserte_calibrees()`, `.dsr()` / `.PKG_DESSERTR`, `.integrite_calculer()`, `optimiser_reseau()` et `reseau_desserte()` `@section Performance`
- dessertR : `dsr_calibrer_specs()`, `dsr_layers_dtm()`, `dsr_conductivite()`, `.dsr_alerter_bornes`
- Données : `~/.local/share/nemeton/projects/20260717_101641_wsfi` (ForetAccess, 30 parcelles / 31 ha) et `20260801_130303_xpdk` (Dabo, 4 parcelles / 774 ha)

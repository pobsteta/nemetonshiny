# BRIEF dessertR → nemetonshiny — classement des linéaires détectés, balisage OSM, coût de terrassement

> **Statut** : ouvert, 2026-08-11. **Amont** : `dessertR 1.3.0` (tag
> `v1.3.0`, publiée) et `foretaccess` branche `feat/cout-terrassement`
> (2 commits, **non poussée**). **Aval** : `nemetonshiny`, module
> `mod_desserte` / `service_desserte`. **Une action est déjà dans votre
> arbre**, non commitée : voir §1.

------------------------------------------------------------------------

## 0. Pourquoi ce brief

`nemetonshiny 0.121.x` a livré la détection de routes non cartographiées
via `dessertR::dsr_detecter()`. La question qui suit immédiatement est :
**qu’est-ce qui a été détecté ?** En forêt gérée, ce que la détection
remonte hors référence n’est pas majoritairement de la desserte — ce
sont des cloisonnements d’exploitation et des layons. `dessertR 1.3.0`
répond à cette question et propose le balisage OpenStreetMap
correspondant.

Le second sujet est indépendant : le tracé produit par « Générer la
desserte » minimisait des mètres alors qu’une surface de coût en €/m
était calculée puis ignorée.

------------------------------------------------------------------------

## 1. Action déjà appliquée dans votre arbre de travail — à commiter

`R/service_desserte.R`, appel à
[`foretaccess::reseau_desserte()`](https://pobsteta.github.io/foretaccess/reference/reseau_desserte.html)
:

``` r

foretaccess::reseau_desserte(pre, cout, parcelles = parcelles,
                             desserte_existante = desserte, mode = engine,
                             skidding_m = skidding_m, pondere_cout = TRUE)
```

**Ce que ça répare.** `pondere_cout` vaut `FALSE` par défaut dans le
cœur (« comportement SylvaRoad »). Sans lui, `.desserte_grille_cout()`
rend une grille neutre à 1,0 : le solveur produisait un tracé **purement
géométrique**, et la surface de coût du Lot 14 — calculée juste
au-dessus, phase « cout » comprise dans la barre de progression — ne
servait que par son masque `franchissable`. On payait le calcul du coût
sans jamais s’en servir.

**Attendez-vous à des tracés différents**, et pas marginalement : le
solveur contourne désormais les fortes pentes et les franchissements au
lieu de couper au plus court. C’est le comportement voulu, mais il
invalide les tracés déjà en cache — prévoir une invalidation ou un
avertissement.

Le diff est resté non commité parce que votre arbre portait déjà une
modification non commitée dans ce même fichier (commentaire d’en-tête
sur l’exposition de Steiner). À vous de les séparer.

------------------------------------------------------------------------

## 2. `dsr_classer()` — qualifier ce que la détection remonte

### L’appel minimal

``` r

classe <- dessertR::dsr_classer(aretes, reference = desserte_existante)
```

`aretes` : la sortie `aretes` de `dsr_reseau()`, ou tout `sf` de
`LINESTRING`. Rien d’autre n’est obligatoire — les critères non
renseignés sont **déclarés inconnus**, pas supposés.

### Ce que ça rend

Quatre colonnes ajoutées :

| Colonne | Contenu |
|----|----|
| `CLASSE` | `route_forestiere`, `piste_forestiere`, `desserte`, `cloisonnement_exploitation`, `layon_parcellaire`, `pare_feu`, `indetermine` |
| `CLASSE_CONF` | part des critères effectivement renseignés (0 à 1) |
| `CLASSE_MOTIF` | les critères qui ont voté, en clair : `reference+!peigne+minerale?+!fosses+connecte+parcelle?` |
| `OSM_TAGS` | proposition de balisage, `NA` si aucune |

`CLASSE_MOTIF` est la colonne à afficher dans l’app, pas seulement
`CLASSE` : `?` signale un critère inconnu, `!` un critère établi comme
faux. Une classe à `CLASSE_CONF` faible est une classe posée sur peu de
choses, et l’utilisateur doit pouvoir le voir.

### Les entrées qui rendent le classement discriminant

Sans elles, la plupart des linéaires sortent en `indetermine` — ce qui
est honnête, pas utile.

| Argument | Source côté app | Ce qu’il débloque |
|----|----|----|
| `stations` | `dsr_measure()` par tronçon, avec une colonne `troncon` | critère fossés |
| `ndvi` | `dsr_ndvi()` sur une ortho IRC (`dsr_ortho_ign()`) | sépare route / piste, et **conditionne le pare-feu** |
| `tpi` | `dsr_slrm(mnt, fenetres_m = 50)` | crête → `pare_feu`, uniquement avec `ndvi` |
| `parcellaire` | contours d’UGF (voir §3) | `layon_parcellaire` |
| `reference` | la desserte existante déjà chargée | ce qu’elle porte est une desserte |

Le critère du **peigne** (faisceaux de parallèles régulièrement espacés
= cloisonnement) ne demande aucune entrée supplémentaire : il est
calculé sur la géométrie seule.

### Ce que l’app ne doit pas faire

- **Ne pas téléverser `OSM_TAGS` automatiquement.** C’est une
  proposition à valider ; un import dans OSM relève des règles de la
  communauté.
- **Ne pas fabriquer de tag d’accès.** `dsr_classer()` n’émet `access=*`
  que si on lui passe `panneaux`, un relevé attestant la restriction —
  un panneau ne se lit pas dans un MNT. L’argument existe déjà pour
  recevoir, plus tard, les photos géolocalisées du jumeau numérique ; il
  émet alors `source:access` avec la provenance.

------------------------------------------------------------------------

## 3. Le parcellaire : ce sont les contours d’UGF

**Décision actée** : on passe les contours d’unités de gestion, pas des
limites cadastrales reconstruites. Chaque UGF porte sa référence
cadastrale, mais c’est bien l’UGF qui fait référence.

Conséquences pratiques :

- ce sont des limites de **gestion**, donc
  `sous_type_parcelle = "section"` — le défaut — convient, et non
  `"border"` qui vaudrait pour du cadastre brut ;
- **passez-le explicitement** si le message vous gêne : fournir
  `parcellaire` sans déclarer `sous_type_parcelle` déclenche une notice
  (même règle que le `regime` de `dsr_cubature()`, une valeur qui ne se
  lit pas dans la géométrie ne se suppose pas en silence) ;
- conséquence assumée : une UGF taillée dans une *portion* de parcelle a
  des côtés de découpe interne que rien ne matérialise au sol. Un
  linéaire qui les suit sortira en `layon_parcellaire` sans en être un.
  `CLASSE_MOTIF` porte alors `parcelle` — c’est là qu’on les retrouve.

------------------------------------------------------------------------

## 4. Le reste de `dessertR 1.3.0` qui vous concerne

- **`dsr_ecart_norme(stations, certu)` + `dsr_rapport(norme = …)`** —
  l’écart entre largeur mesurée et largeur normative Certu, avec
  `BORDS_RESOLUS` qui dit quand l’écart compare une *plateforme* à une
  largeur de *chaussée* et se lit comme un majorant.
- **`dsr_emprise_certu()`** — détecte enfin
  `cpx_classement_administratif` (BD TOPO v3 récente), et `champs`
  **complète** la détection au lieu de la remplacer. Si vous forciez des
  noms de colonnes, vous pouvez ne forcer que celui qui manque.
- **`dsr_cubature(trace, mnt, largeur, regime)`** — si vous l’appelez un
  jour : `regime` est **sans défaut**. Omis, `"elargissement"` est
  supposé *et annoncé*. En `"construction"`, la fonction vérifie que le
  MNT ne porte pas déjà la route et le signale.

------------------------------------------------------------------------

## 5. Coût de terrassement (foretaccess) — disponible, non activé

`foretaccess`, branche `feat/cout-terrassement` (**non poussée**, à
arbitrer) :

``` r

surface_cout_construction(pre, cfg, methode_pente = "terrassement", largeur_m = 4)
```

Le terme de pente devient un coût de **déblai / remblai** chiffré au m³
: continu, là où le barème actuel saute de 65 €/m entre 34,9 % et 35,1
%, et **sensible à la largeur de plateforme** — le volume croît comme
son carré, quand le barème rend la même valeur pour une piste de 3 m et
une route de 6 m.

**Le défaut reste le barème**, délibérément. Avant d’exposer un choix
dans l’app, il faut le banc comparatif sur un massif réel : changer ce
terme change tous les tracés produits. Et les prix au m³ par défaut
n’ont aucune valeur défendable — ce sont des ordres de grandeur pour que
la fonction tourne, à remplacer par un barème du gestionnaire.

Si vous décidez de l’exposer : un `radioButtons` à côté du choix de
moteur, et `largeur_m` pris de la largeur de plateforme visée par le
gestionnaire, pas d’une constante.

------------------------------------------------------------------------

## 6. Ce qui n’est pas fait, et ne doit pas être présenté comme fait

- **Aucun banc de validation n’a tourné** sur le classement. Le critère
  du peigne est vérifié sur géométries de synthèse ; les seuils du
  pare-feu (TPI médian 0,5 m, fenêtre 50 m) sur relief de synthèse.
  Aucun cloisonnement ni pare-feu réel n’a été mesuré.
- `dsr_classer()` n’a tourné que sur l’extrait de 200 m livré avec
  `dessertR`, où elle n’a quasiment rien tranché faute de NDVI et de
  parcellaire — et l’a dit (`CLASSE_CONF` à 0,5).
- **La place de dépôt n’est pas balisée** : `dsr_places()` les détecte,
  mais aucun tag OSM ne fait consensus. Recommandation : les transporter
  au GeoPackage, ne rien proposer à OSM.
- L’optimisation des remblais/déblais **dans la conception de tracé**
  n’existe pas. Le coût de terrassement du §5 est un coût *par cellule*,
  pas une optimisation du profil en long.

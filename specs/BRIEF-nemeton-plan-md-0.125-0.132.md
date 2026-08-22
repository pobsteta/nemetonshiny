# BRIEF `nemeton` — mise à jour du `PLAN.md` partagé (app 0.125.0 → 0.132.0)

> **Statut** : ouvert, 2026-08-22.
> **Dépôt concerné** : `nemeton` uniquement — le `PLAN.md` à sa racine est la
> source unique de vérité partagée avec `nemetonshiny` (ADR-009).
> **Nature** : documentation seule. Aucun code cœur, aucune release cœur.
> **Contexte de lecture** : `nemetonshiny@main` à `5a1afd7c`, cycle dev
> `0.132.0.9000`.
> **Suite de** : `BRIEF-nemeton-plan-md-0.122-0.124.md` (2026-08-15), dont le
> journal s'arrête à v0.124.0.

---

## 0. Pourquoi ce brief, et ce qu'il ne peut pas faire

La règle 12 du `CLAUDE.md` de `nemetonshiny` interdit à la session app **d'écrire**
dans `/home/pascal/dev/nemeton`. La lecture, elle, est autorisée depuis la
révision de cette règle — j'ai donc pu vérifier l'état réel de `PLAN.md` : son
journal se termine sur l'entrée **2026-08-14** couvrant v0.122.14 → v0.124.0.

**Vingt-cinq releases app se sont accumulées depuis** (v0.125.0 → v0.132.0,
119 commits sur `main`), toutes **taguées et publiées**. La règle « ne jamais
clore un chantier sans release poussée » est donc satisfaite pour l'intégralité
de ce lot.

Ce brief livre le **texte à coller** et la **description du chantier** de chaque
groupe, à faire correspondre aux cases réelles par la session cœur. Si un
chantier décrit ci-dessous n'existe pas dans `PLAN.md`, c'est une information en
soi : il faut le créer, pas forcer la correspondance.

## 1. Le lot, vu de haut

Vingt-cinq releases qui se rangent en **cinq chantiers**, dont deux touchent au
cœur de façon structurante (§2.1 et §2.3) et méritent chacun leur entrée.

| Release | Commit `main` | Date | Cycle dev repris | Chantier |
|---|---|---|---|---|
| v0.125.0 | `b1374606` | 2026-08-16 | `0.125.0.9000` | E (UX) |
| v0.125.1 | `3c4c1898` | 2026-08-16 | `0.125.1.9000` | E (UX) |
| v0.126.0 | `c5887a5e` | 2026-08-16 | `0.126.0.9000` | D (justesse) |
| v0.126.1 | `793fd04a` | 2026-08-17 | `0.126.1.9000` | D (justesse) |
| v0.126.2 | `71f39ad1` | 2026-08-17 | `0.126.2.9000` | E (UX) |
| v0.127.0 | `65a806ac` | 2026-08-18 | `0.127.0.9000` | **A (dé-fork)** |
| v0.127.1 | `fb9d72a1` | 2026-08-18 | `0.127.1.9000` | **A (dé-fork)** |
| v0.127.2 | `b6864c19` | 2026-08-18 | `0.127.2.9000` | **A (dé-fork)** |
| v0.128.0 | `c7a745d8` | 2026-08-18 | `0.128.0.9000` | E (UX) |
| v0.128.1 | `2840c5f7` | 2026-08-19 | `0.128.1.9000` | D (justesse) |
| v0.129.0 | `01873e72` | 2026-08-19 | `0.129.0.9000` | **B (ONF)** |
| v0.130.0 | `e3de17a5` | 2026-08-19 | `0.130.0.9000` | **B (ONF)** |
| v0.130.1 → v0.130.8 | `cb27c6bf` … `29c831ad` | 2026-08-19/20 | `0.130.N.9000` | **B (ONF)** |
| v0.130.5 | `6171c4f4` | 2026-08-20 | `0.130.5.9000` | D (justesse, T3) |
| v0.130.9 | `e686816b` | 2026-08-20 | `0.130.9.9000` | E (accent IA) |
| v0.130.10 | `3a8c1025` | 2026-08-20 | `0.130.10.9000` | E (accent IA) |
| v0.131.0 | `50e60ab3` | 2026-08-21 | `0.131.0.9000` | **C (sens)** |
| v0.131.1 | `7173b00f` | 2026-08-21 | `0.131.1.9000` | **C (sens)** |
| v0.132.0 | `ca363953` | 2026-08-21 | `0.132.0.9000` | **B (ONF)** |

## 2. Les entrées de journal à coller

### 2.1 — Chantier A : `INDICATOR_FAMILIES` cesse d'être forké (v0.127.0 → v0.127.2)

**Chantier à chercher** : celui qui porte l'export `indicator_families()` /
`indicator_labels()` (cœur **v0.175.0**, brief
`specs/BRIEF-indicator-families-export.md`) et la **spec 045** (renommage des
deux colonnes L, cœur **v0.176.0**). C'est la contrepartie applicative des deux.

> **2026-08-18 — Le fork de la table des familles est résorbé (app).**
> `app_config.R` portait sa propre copie des 12 familles — 368 lignes — et cette
> copie avait dérivé de deux façons qui atteignaient l'écran : **A5 manquait de
> la famille A** (l'indicateur était calculé par `service_compute.R` puis filtré
> à l'affichage, donc tout ce qui avait été livré pour le rafraîchissement urbain
> restait invisible), et **l'appariement code ↔ colonne, qui est positionnel,
> était croisé pour F et L** — une copie qui compensait dans ses
> `indicator_labels` mais pas dans les clés i18n `indicator_<code>` produisait un
> libellé dépendant de la copie que le lecteur atteignait ; la carte d'érosion
> sortait « F1 — Fertilité des sols ». La table est désormais **lue du cœur**
> (`.build_indicator_families()` sur `nemeton::indicator_families()` +
> `indicator_labels()`, qui apparient code, colonne et libellé ligne par ligne),
> ce qui retire la **classe** de bug et non une instance. Deux tables locales de
> libellés indexées par nom de colonne suivaient encore le slug : elles sont
> remplacées par `indicator_label_by_column()` (v0.127.1). Le renommage des deux
> colonnes L de la spec 045 est suivi en v0.127.2, avec migration à la lecture
> pour qu'un projet calculé avant le renommage reste lisible. Planchers relevés à
> `nemeton (>= 0.175.0)` et `foretaccess (>= 2.4.0)`. Livré
> `nemetonshiny@65a806ac` (v0.127.0), `fb9d72a1` (v0.127.1), `b6864c19`
> (v0.127.2) ; cycles dev `0.127.0.9000` → `0.127.2.9000`.

**À noter pour le cœur** : ce dé-fork est ce qui a rendu **gratuites** les deux
corrections de sens de §2.3. C'est l'argument à consigner si le chantier
`indicator_families()` doit être clos — il a payé deux fois en trois jours.

### 2.2 — Chantier B : parcellaire forestier ONF (v0.129.0 → v0.130.8, v0.132.0)

**Chantier à chercher** : la **spec 046** (`specs/046-parcellaire-onf/`), cœur
**v0.179.0** puis **v0.180.0**.

> **2026-08-19/21 — Les UGF se créent depuis le parcellaire forestier ONF (app).**
> En forêt publique, la parcelle **cadastrale** n'est pas l'unité de gestion : la
> parcelle **forestière** l'est. L'onglet Carte UGF offre deux actions dont la
> distinction est tout le sujet — **croiser** (garde les parcelles cadastrales du
> projet et dit quelles parcelles forestières les recouvrent) et **créer depuis
> le parcellaire** (remplace les UGF par les parcelles forestières). Implémente
> `specs/046-parcellaire-onf/brief-nemetonshiny.md`, plancher relevé à
> `nemeton (>= 0.179.0)`.
>
> La **recette sur le vrai service WFS ONF** (forêt domaniale de Chaux) a révélé
> un défaut que les tests ne pouvaient pas voir : `onf_projet_from_parcelles()`
> plantait dès que le parcellaire n'avait pas exactement le même nombre de lignes
> que les parcelles du projet (idiome `modifyList()` sur un data.frame). Corrigé
> en v0.130.0 ; suivi de huit correctifs de recette (v0.130.1 → v0.130.8) :
> retrait du bouton d'import devenu redondant, calage systématique sur les
> limites cadastrales, bouton ONF qui ne réclame plus de sélection préalable,
> **remontée du tri des parcelles dans le cœur** (`nemeton 0.180.0` écarte
> lui-même les parcelles qu'aucune parcelle forestière ne rencontre et expose
> `parcelles_concernees` ; l'app cesse de le recalculer — plancher relevé à
> `>= 0.180.0`), coche optionnelle « supprimer les parcelles hors forêt publique
> (< 10 %) », rafraîchissement de l'onglet Sélection qui montrait des parcelles
> supprimées, et couche orange fantôme sur la Carte UGF.
>
> **v0.132.0** ferme le chantier par l'autre bout : créer un projet **entier**
> depuis un fichier `commune-code_insee.csv` listant les références cadastrales
> (`A1;A2;…;AO220`), croisé avec le parcellaire ONF. La commune est lue dans le
> **nom** du fichier et un nom hors convention est **refusé, jamais deviné** —
> `A1` existe dans presque toutes les communes de France, un INSEE erroné
> apparierait par coïncidence. Vérifié sur `couchey-21200.csv` : 23 références,
> 23 parcelles, 535,6 ha. Livré `nemetonshiny@01873e72` (v0.129.0) →
> `29c831ad` (v0.130.8), puis `ca363953` (v0.132.0) ; cycles dev
> `0.129.0.9000` → `0.132.0.9000`.

### 2.3 — Chantier C : le sens des indicateurs (v0.131.0, v0.131.1)

**Chantier à chercher** : la **spec 048** (sens du radar, cœur **v0.181.0**).
Voir aussi §3, qui concerne la **spec 049** et n'est pas encore consignable.

> **2026-08-21 — La famille R disait l'inverse de la vérité (app).**
> `nemeton 0.181.0` (spec 048) inverse R1 (feu), R2 (tempête), R3 (sécheresse) et
> R4 (abroutissement) à la normalisation, comme R5 l'était déjà : un score haut
> signifie désormais une **bonne résilience**, non un risque élevé. Côté app,
> trois conséquences. (1) La palette de `famille_risque` n'est plus la palette de
> risque — elle est peinte comme les onze autres familles. (2) Un projet calculé
> **avant** l'inversion porte des valeurs de l'ancien sens : une migration
> (`R/migrate.R`, marqueur de sens versionné) l'invalide **une seule fois** et
> force le recalcul, plutôt que d'afficher des scores muets qui disent le
> contraire. (3) L'app **n'inverse rien elle-même** — un test le verrouille, car
> une inversion applicative doublerait celle du cœur.
>
> L'inversion a rendu visible un manque plus ancien, corrigé en v0.131.1 :
> **aucune colonne `Score` ne disait sa direction**. Une note sous le tableau de
> synthèse le dit maintenant, R comprise. Deux options ont été écartées et
> méritent d'être consignées : renommer l'axe du radar (il n'y a pas d'axe à
> renommer — le radar ne porte que des **lettres**), et renommer la famille en
> « Résilience » (son nom vient du cœur et reste **juste** dans l'onglet Famille,
> où l'on voit les grandeurs brutes : `R1 = 100` y signifie bien un fort risque
> incendie ; le renommer aurait rendu cet onglet faux à son tour). Plancher relevé
> à `nemeton (>= 0.181.0)`. Livré `nemetonshiny@50e60ab3` (v0.131.0) et
> `7173b00f` (v0.131.1) ; cycles dev `0.131.0.9000`, `0.131.1.9000`.

### 2.4 — Chantier D : corrections de justesse de calcul (v0.126.0, v0.126.1, v0.128.1, v0.130.5)

**Chantier à chercher** : à répartir — spec 032 (A5 / LST Thermocity), C2 NDVI,
spec 027/035 (reGénération), spec 030 (T3 coupes rases SUFOSAT).

> **2026-08-16/20 — Quatre réglages qui ne parvenaient pas au cœur (app).**
> Un même motif, trouvé quatre fois : l'app affichait un choix que le cœur ne
> recevait pas.
>
> * **C2 était calculé sur une image d'affichage** (v0.126.1). `indicateur_c2_ndvi`
>   sortait **négatif sur les 30 UGF** de Fordead — physiquement impossible sous
>   couvert. Ni le calcul ni l'ordre des bandes n'étaient en cause : le NDVI était
>   dérivé de l'**orthophoto IRC du WMS IGN**, une image 8 bits étirée pour
>   l'affichage (valeurs 9–247, compression JPEG), et non de la réflectance. Il
>   se calcule désormais sur les scènes **Sentinel-2** que chaque projet suivi
>   cache déjà. Plancher `nemeton (>= 0.174.0)`.
> * **La résolution microclimat était décorative** (v0.128.1). Le radio 2 m / 5 m
>   de reGénération n'entrait dans **aucune** `cfg` : `regen_sensibilite()`
>   recevait toujours son défaut `res = 2`. Avec la coercition qui manquait — le
>   radio porte une chaîne, le cœur attend un numérique.
> * **« Les 5 dernières années » ne voulait pas dire les 5 dernières années**
>   (v0.130.5). T3 recevait `window_years` et `min_proba` mais **jamais**
>   `reference_year` : laissé à `NULL`, le cœur ancrait la fenêtre sur la coupe la
>   plus récente **trouvée dans les UGF analysées** — un massif sans coupe récente
>   voyait donc sa fenêtre glisser en arrière. Implémente
>   `briefs/vers-nemetonshiny/2026-08-20-t3-reference-year.md` §8 ; aucun plancher
>   à bouger, `reference_year` existe dans le cœur depuis l'origine.
> * **A5 disait enfin *pourquoi* il est vide** (v0.126.0). « Pourquoi
>   `cache/layers/lst/` est-il vide ? » avait une réponse — *hors couverture
>   Thermocity* — introuvable depuis l'application : axe vide, carte grise, « NA »
>   dans le détail de famille, et un panneau des sources affirmant « Rafraîchissement
>   urbain : activé ». Quatre situations sont désormais distinguées. Implémente
>   `specs/032-.../brief-nemetonshiny-a5-diagnostic.md`.
>
> Livré `nemetonshiny@c5887a5e`, `793fd04a`, `2840c5f7`, `6171c4f4` ; cycles dev
> `0.126.0.9000`, `0.126.1.9000`, `0.128.1.9000`, `0.130.5.9000`.

### 2.5 — Chantier E : UX — regroupement des réglages et accent IA (v0.125.x, v0.126.2, v0.128.0, v0.130.9, v0.130.10)

**Aucun impact cœur.** À consigner pour mémoire, dans la section app du PLAN si
elle existe, sinon en une entrée courte.

> **2026-08-16/20 — Les calibrages quittent les sidebars ; l'accent IA devient une
> règle (app).** Les réglages qu'on ne touche qu'une fois par massif (seuils FAST
> NDVI/NBR/NDMI et fenêtre roulante, puis les calibrages de quatre autres onglets)
> quittent les sidebars pour **Paramètres › Sources & paramètres**, où ils sont
> **persistés par projet** ; chaque sidebar garde un rappel des valeurs en vigueur.
> La période d'observation, elle, **reste** dans le sidebar : ce n'est pas un
> calibrage, elle change à chaque essai. Les actions de vue passent toutes sous un
> en-tête unique (`action_table_card()`), et la copie morte de
> `normalize_indicator()` est retirée. Enfin, les quatre surfaces qui produisent du
> contenu **généré** — Synthèse, Plan d'actions, reGénération, Famille — partagent
> un accent **ambre `#E8A33D`** et l'icône trois étoiles, et **une ligne « Ambre »
> entre dans le tableau des couleurs de bouton du `CLAUDE.md` app** : cette couleur
> ne dit pas un *niveau d'action* comme les cinq autres, elle dit une
> **provenance**, ce qui l'autorise à échapper à l'échelle plutôt qu'à la rompre.
> Contraste vérifié : texte sombre sur ambre à 5,09:1 (blanc échouerait à 2,16:1),
> verrouillé par un test. Livré `nemetonshiny@b1374606`, `3c4c1898`, `71f39ad1`,
> `c7a745d8`, `e686816b`, `3a8c1025`.

## 3. Ce qui n'est PAS encore consignable — spec 049

L'app a traité le brief `specs/049-famille-f-decroisee/brief-nemetonshiny.md`
(décroisement de F, cœur **v0.182.0**) dans une **v0.132.1 non encore mergée**.
Ne rien cocher tant que le tag n'existe pas. Un brief de suite le consignera, ou
la ligne suivante pourra être ajoutée une fois `v0.132.1` publiée :

> **2026-08-22 — Décroisement de F : l'app n'avait rien à corriger (app).**
> Le dé-fork de v0.127.0 a payé : la table venant du cœur ligne par ligne, la
> correction de la spec 049 traverse l'app sans une ligne de code. Les quatre
> contrôles du brief sont vérifiés (F1 « Fertilité des sols » sur
> `indicateur_f1_fertilite`, F2 « Risque d'érosion » sur `indicateur_f2_erosion`,
> infobulle F2 topographique, `famille_fertilite` inchangée). Seuls des
> **commentaires** restaient à réécrire : quatre blocs de code et deux fichiers de
> test présentaient le croisement de F comme un fait présent. Un test le figeait
> même comme **fixture** (`test-renommage-famille-L.R` asserte `F1 →
> indicateur_f2_erosion`) et **tombait** depuis la publication du cœur — c'est le
> seul effet réel du décroisement côté app, et le brief 049 ne l'annonçait pas.
> Plancher relevé à `nemeton (>= 0.182.0)`. Livré `nemetonshiny@<SHA>` (v0.132.1),
> cycle dev `0.132.1.9000`.

## 4. À vérifier côté cœur (non vérifié depuis l'app)

1. **Le brief 049 sous-estimait sa portée.** Il annonce « une vérification, pas
   forcément du code ». Il y avait du code : un test app figeait le croisement.
   Si d'autres consommateurs du cœur ont fait de même, ils sont rouges depuis
   v0.182.0. Vaut peut-être un mot dans la spec.
2. **`reference_year` de T3** (§2.4) : l'ancrage par défaut sur « la coupe la plus
   récente trouvée dans les UGF analysées » est un comportement cœur qui surprend.
   L'app le contourne en passant toujours la valeur ; à arbitrer côté cœur si le
   défaut doit changer.
3. **Le compteur `parcelles_concernees`** (`nemeton 0.180.0`) est désormais *lu*
   par l'app et non recalculé — son contrat est devenu public de fait.
4. Les entrées ci-dessus citent des versions cœur (0.174.0, 0.175.0, 0.176.0,
   0.179.0, 0.180.0, 0.181.0, 0.182.0) **lues dans le NEWS de l'app**. Les
   recouper avec les tags cœur avant de coller.

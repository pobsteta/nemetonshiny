> ## ✅ CLOS le 2026-08-18 — livré en **v0.127.0.9001**
>
> Les deux tables du §2 sont supprimées, pas corrigées :
> `indicator_label_by_column()` lit `nemeton::indicator_labels()` via
> `INDICATOR_FAMILIES`. Le §3 (alias DB) est documenté sur place et
> volontairement laissé tel quel — l'aller-retour est sans perte.
> `test-libelles-famille-L.R` : 32 assertions.

# BRIEF `nemetonshiny` — les trois tables L croisées côté app

**Dépôt cible** : `/home/pascal/dev/nemetonshiny`. Session dédiée requise.
**Portée** : trois tables de correspondance. Aucun calcul, aucun affichage de
famille en cause.

**Origine** : réponse au brief `BRIEF-nemeton-libelles-famille-L.md` (émis par
la session app le 2026-08-18). **Sa demande n'a pas été appliquée** : les
libellés du cœur sont justes, et les échanger aurait retitré les cartes à faux.
Le détail est dans `PLAN.md` (entrée 2026-08-18). Ce qui reste à corriger est
côté app, et n'a pas d'effet sur l'onglet Paysage.

---

## 1. Le fait établi

Une colonne porte le nom de la **fonction qui la remplit** : `compute_indicator()`
résout la fonction par le nom de l'indicateur (`R/indicators-core.R:199`). Or
pour la famille L, le nom de la fonction contredit ce qu'elle calcule :

| Fonction | Ce qu'elle calcule | Donc la colonne du même nom contient |
|---|---|---|
| `indicateur_l2_fragmentation()` | indice de forme + contraste de matrice + exposition → **sylvosphère / effet lisière** | des valeurs de sylvosphère |
| `indicateur_l1_sylvosphere()` | landscapemetrics COHESION + AI → **fragmentation paysagère** | des valeurs de fragmentation |

Quatre sources concordent dans le cœur : le corps des fonctions, leurs titres
roxygen (« Sylvosphere - Edge Effect (L1) » sur `l2_fragmentation`), l'en-tête de
`tests/testthat/test-indicators-landscape.R`, et `R/i18n.R`.

L'appariement `L1 -> indicateur_l2_fragmentation` du cœur est donc **correct**,
et le libellé « Sylvosphère (effet lisière) » décrit bien les valeurs affichées.
**L'onglet Paysage n'a rien à corriger** : ce que le lecteur prend pour « la
carte de fragmentation » est identifié par le *slug* de la colonne, qui ment.

## 2. Ce qui reste faux côté app

Trois tables sont indexées **par nom de colonne** et suivent le slug, donc
s'inversent :

| Fichier | Ligne | Écrit | Devrait dire |
|---|---|---|---|
| `R/utils_i18n.R` | 2284 | `indicateur_l1_sylvosphere = "Sylvosphère (effet lisière)"` | Fragmentation paysagère / Landscape Fragmentation |
| `R/utils_i18n.R` | 2285 | `indicateur_l2_fragmentation = "Fragmentation paysagère"` | Sylvosphère (effet lisière) / Sylvosphere (Edge Effect) |
| `R/mod_progress.R` | 316 | `indicateur_l2_fragmentation = "Paysage - Fragmentation"` | Paysage - Sylvosphère |
| `R/mod_progress.R` | 317 | `indicateur_l1_sylvosphere = "Paysage - Ratio bordure"` | Paysage - Fragmentation |

Effet visible : pendant le calcul, la barre de progression annonce
« Paysage - Fragmentation » alors qu'elle calcule la sylvosphère, et
réciproquement. Sans conséquence sur les résultats.

**Le mieux** est de ne pas maintenir une deuxième table du tout : les deux
libellés sont déjà dans le cœur, indexables par colonne en une ligne —

```r
lbl <- stats::setNames(
  nemeton::indicator_labels(lang = lang)$label,
  nemeton::indicator_labels()$column_name
)
```

C'est le même mouvement que le dé-fork d'`INDICATOR_FAMILIES` : une source, pas
trois.

## 3. Point mineur, sans urgence

`R/service_db.R:458-459` aliase les colonnes DB à l'envers
(`landscape_edge_ratio <- indicateur_l1_sylvosphere`, qui porte la
fragmentation). L'aller-retour reste **sans perte** — la même table sert à lire
et à écrire — donc aucune donnée n'est fausse ; seul quelqu'un qui interroge la
base par ses propres noms est induit en erreur. À traiter si les noms DB sont
un jour exposés, pas avant : les renommer impose une migration.

## 4. Côté `nemeton`

**Livré** (branche `fix/libelles-famille-L`, aucune release nécessaire) :

- `R/nemeton-package.R` — quatre descriptions d'indicateurs corrigées (les deux
  L annonçaient la mauvaise grandeur, les deux F le mauvais code court).
- Section *Column pairing* d'`indicator_families()` — dit désormais **d'où vient**
  le croisement (le nom des fonctions) et pourquoi échanger les libellés
  retitrerait les cartes à faux.
- `tests/testthat/test-indicator-labels-pairing.R` — 125 assertions : les quatre
  lignes croisées sont exactement `F1 F2 L1 L2` sur 41 (balayage structurel), le
  libellé et l'infobulle décrivent la grandeur portée par la colonne, et
  `indicator_families()` ne contredit pas `indicator_labels()` dans les deux
  langues.

Aucun changement de valeur, aucun changement d'API.

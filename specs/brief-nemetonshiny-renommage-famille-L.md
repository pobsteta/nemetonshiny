> ## ✅ CLOS le 2026-08-18 — livré en **v0.127.1.9001**
>
> §2.a plancher relevé, §2.b slugs renommés dans `service_compute.R` (la liste
> reste **énumérée** : c'est un sous-ensemble curé, la remplacer par
> `indicator_labels()$column_name` ajouterait W4, A3-A5, T3, R5-R7 au calcul —
> changement de comportement hors périmètre). §2.c `migrer_colonnes_l()` posé
> dans les deux chemins de lecture. §2.d : les deux premières tables étaient
> **déjà supprimées** en v0.127.1 ; la troisième (`service_db.R`) est un
> mapping d'anciens noms anglais, pas un alias DB — le commentaire qui le
> décrivait mal a été corrigé.
>
> Le schéma PostGIS garde les anciens noms ; traduction à la frontière.
> `test-renommage-famille-L.R` : 26 assertions.

> **Émis par la session `nemeton` le 2026-08-18**, après la release **v0.176.0**.
> Copie fidèle de `nemeton/specs/brief-nemetonshiny-renommage-famille-L.md`
> (même nom des deux côtés).
>
> **Successeur de** [`brief-nemetonshiny-libelles-famille-L.md`](brief-nemetonshiny-libelles-famille-L.md),
> clos en v0.127.1 : ce qui y était livré (les tables locales de libellés
> supprimées au profit de `indicator_labels()`) reste juste et acquis. Ce
> brief-ci porte la suite, décidée après : le cœur a **renommé les deux
> fonctions L**, donc les noms de colonnes changent.

# BRIEF `nemetonshiny` — famille L : les deux colonnes sont renommées

**Dépôt cible** : `/home/pascal/dev/nemetonshiny`. Session dédiée requise.
**Amont** : `nemeton` **v0.176.0** (spec 045). Plancher à monter.

**Ce brief remplace** sa version du 2026-08-18, émise sous le nom
`brief-nemetonshiny-libelles-famille-L.md` et livrée côté app en **v0.127.1**
(elle concluait « rien à faire côté app, seulement trois tables de libellés à
redresser » — ce qui reste acquis). La décision a
changé : plutôt que documenter un croisement, le cœur a **renommé les deux
fonctions**, ce qui change les noms de colonnes.

---

## 1. Ce qui a bougé, et pourquoi

Une colonne porte le nom de la fonction qui la remplit. Les deux fonctions de
paysage portaient chacune le nom de la métrique de l'**autre** :

| Avant | Après | Ce que ça calcule |
|---|---|---|
| `indicateur_l2_fragmentation` | **`indicateur_l1_effet_lisiere`** | sylvosphère / effet lisière |
| `indicateur_l1_sylvosphere` | **`indicateur_l2_morcellement`** | fragmentation paysagère |

**Aucune valeur ne change.** Les libellés non plus : ils décrivaient déjà les
valeurs. Ce sont les slugs qui cessent de mentir — et avec eux le croisement
`code ↔ colonne` de la famille L, qui disparaît de `indicator_families()`.

Les deux anciens noms de **fonctions** restent appelables (avertissement de
dépréciation, mêmes valeurs). Les deux anciens **slugs** ne sont jamais
recyclés : c'est ce qui garantit qu'une donnée écrite avant la migration ne
peut pas être relue à l'envers.

## 2. À faire côté app

### 2.a — Plancher

`DESCRIPTION` : `nemeton (>= 0.176.0)`.

### 2.b — Listes de colonnes

`R/service_compute.R:308` énumère les indicateurs à calculer. Remplacer :

```r
    # Landscape (L)
    "indicateur_l1_effet_lisiere", "indicateur_l2_morcellement",
    "indicateur_l3_het_spectrale",
```

Idem `R/service_db.R:514`. Le mieux reste de ne plus énumérer du tout :
`nemeton::indicator_labels()$column_name` donne la liste, à jour par
construction.

### 2.c — Relecture des projets existants — **le point qui compte**

Tout projet calculé avant 0.176.0 porte les anciens noms. À la lecture
(parquet, PostGIS, GeoPackage en cache), passer le jeu par :

```r
data <- nemeton::migrer_colonnes_l(data)
```

Renommage sans perte, variantes `_norm` comprises ; un jeu déjà migré ou
étranger revient inchangé, donc l'appel se laisse poser une fois pour toutes
dans le chemin de lecture. Sans lui, les colonnes L d'un ancien projet
n'apparaîtront simplement plus dans l'onglet Paysage.

### 2.d — Les trois tables indexées par colonne

Elles suivaient le slug, donc s'inversaient. Elles deviennent **justes** si on
les réécrit avec les nouveaux noms — mais l'occasion est bonne de les
supprimer :

| Fichier | Ligne | Aujourd'hui |
|---|---|---|
| `R/utils_i18n.R` | 2284-2285 | libellés indexés par colonne, inversés |
| `R/mod_progress.R` | 316-317 | « Paysage - Fragmentation » affiché pendant le calcul de la sylvosphère |
| `R/service_db.R` | 458-459 | alias DB inversés (aller-retour sans perte, donc sans urgence) |

Pour les deux premières, une ligne suffit :

```r
lbl <- stats::setNames(
  nemeton::indicator_labels(lang = lang)$label,
  nemeton::indicator_labels()$column_name
)
```

Pour `service_db.R`, renommer les colonnes DB imposerait une migration : à ne
faire que si ces noms sont un jour exposés. Ils sont cohérents en interne.

## 3. Recette

1. Un projet **neuf** produit `indicateur_l1_effet_lisiere` et
   `indicateur_l2_morcellement` ; l'onglet Paysage affiche « Sylvosphère (effet
   lisière) » et « Fragmentation paysagère » sur les mêmes cartes qu'avant, aux
   mêmes valeurs.
2. Un projet **ancien** rouvert affiche toujours ses deux cartes L — c'est le
   test de `migrer_colonnes_l()`.
3. La barre de progression annonce la bonne métrique.

## 4. Côté `nemeton` — livré

`indicateur_l1_effet_lisiere()`, `indicateur_l2_morcellement()`,
`migrer_colonnes_l()` exportées ; anciens noms dépréciés ; `INDICATOR_FAMILIES`,
`list_indicators()`, la normalisation et l'i18n alignés ; spec 045 ;
`test-renommage-famille-l.R` (27 assertions) et `test-indicator-labels-pairing.R`
(126) verrouillent l'ensemble.

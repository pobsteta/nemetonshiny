> ## ❌ CLOS le 2026-08-18 — **ce brief a été refusé, et à raison**
>
> `nemeton` PR #396 n'a **pas** appliqué l'échange demandé : il aurait retitré
> les cartes à faux. La prémisse du §1 est fausse.
>
> Une colonne porte le nom de la **fonction qui la remplit**, et pour la famille
> L ce nom contredit ce que la fonction calcule : `indicateur_l2_fragmentation()`
> calcule l'effet lisière, `indicateur_l1_sylvosphere()` calcule la
> fragmentation. La colonne au slug « fragmentation » **contient donc de la
> sylvosphère**, et le libellé du cœur décrivait correctement les valeurs
> affichées. L'onglet Paysage n'avait rien de faux.
>
> **L'erreur de méthode** : j'ai déduit le croisement de la table des libellés
> sans ouvrir le corps des fonctions. Quatre sources concordantes disaient le
> contraire (corps des fonctions, titres roxygen, en-tête de
> `test-indicators-landscape.R`, `R/i18n.R`).
>
> Le cœur a livré à la place : descriptions corrigées dans
> `R/nemeton-package.R`, section *Column pairing* qui dit d'où vient le
> croisement, et `test-indicator-labels-pairing.R` (125 assertions) qui
> **interdit** l'échange réclamé ici.
>
> Suite côté app : [`brief-nemetonshiny-libelles-famille-L.md`](brief-nemetonshiny-libelles-famille-L.md),
> livré en v0.127.0.9001.

# BRIEF `nemeton` — les libellés de la famille L sont croisés

**Dépôt cible** : `/home/pascal/dev/nemeton` (cœur). Session dédiée requise.
**Portée** : deux lignes de données. Aucun calcul en cause.

**Origine** : dé-fork d'`INDICATOR_FAMILIES` côté app
(`nemetonshiny` v0.126.2.9001, brief `brief-nemetonshiny-trois-derniers-points.md`
point 3). En remplaçant la copie locale par une lecture de
`nemeton::indicator_labels()`, l'app a hérité d'une incohérence qui existait déjà
des deux côtés.

---

## 1. Le constat

`nemeton::indicator_labels()` en v0.175.0, familles F et L :

| family | code | `column_name` | `label_fr` | |
|---|---|---|---|---|
| F | F1 | `indicateur_f2_erosion` | Risque d'érosion | ✅ cohérent |
| F | F2 | `indicateur_f1_fertilite` | Fertilité des sols | ✅ cohérent |
| L | L1 | `indicateur_l2_fragmentation` | **Sylvosphère (effet lisière)** | ❌ croisé |
| L | L2 | `indicateur_l1_sylvosphere` | **Fragmentation paysagère** | ❌ croisé |
| L | L3 | `indicateur_l3_het_spectrale` | Hétérogénéité spectrale | ✅ |

L'appariement code ↔ colonne est **volontairement croisé** pour ces deux
familles : `F1` désigne l'érosion et `L1` la fragmentation, quoi qu'en disent les
noms de colonnes `f2_` / `l1_`. C'est surprenant mais assumé, et la table le
documente ligne par ligne.

Pour **F**, le libellé suit la colonne : `indicateur_f2_erosion` est libellé
« Risque d'érosion ». Correct.

Pour **L**, le libellé suit le **code** au lieu de la colonne :
`indicateur_l2_fragmentation` est libellé « Sylvosphère (effet lisière) ». Les
deux libellés sont donc à échanger.

## 2. Ce que l'utilisateur voit

Dans l'onglet Paysage, la carte de **fragmentation** porte le titre
« L1 - Sylvosphère (effet lisière) » et la carte de **sylvosphère** porte
« L2 - Fragmentation paysagère ». Les deux cartes sont justes, leurs titres sont
échangés.

Le même défaut existait dans la copie locale de l'app, à l'identique — il n'est
donc pas introduit par le dé-fork. Mais il n'a désormais **qu'un seul endroit où
être corrigé**, ce qui est précisément l'intérêt de l'opération.

## 3. Correction demandée

Échanger `label_fr` / `label_en` (et les `tooltip_*` correspondants) entre `L1`
et `L2` dans la table des libellés, de sorte que :

```
L1  indicateur_l2_fragmentation  ->  Fragmentation paysagère / Landscape Fragmentation
L2  indicateur_l1_sylvosphere    ->  Sylvosphère (effet lisière) / Sylvosphere (Edge Effect)
```

Vérifier au passage les `tooltip_fr` / `tooltip_en` : celui de `L1` décrit
aujourd'hui l'influence des lisières, celui de `L2` la fragmentation du paysage
— ils suivent le même croisement.

## 4. Critères d'acceptation

- **CA-1** — pour chaque ligne de `indicator_labels()`, le libellé décrit la
  grandeur de `column_name`. Un test le vérifie au moins sur les cas croisés
  (F1/F2, L1/L2), par exemple en cherchant « rosion » dans le libellé de la
  colonne d'érosion et « ragmentation » dans celui de la colonne de
  fragmentation.
- **CA-2** — `indicator_families()$labels` reste cohérent avec
  `indicator_labels()` : les deux accesseurs ne doivent pas se contredire.
- **CA-3** — aucun autre couple code ↔ colonne n'est croisé sans que son libellé
  suive la colonne. Un balayage des 41 lignes suffit.

## 5. Pourquoi ce brief vaut plus qu'un échange de deux chaînes

L'appariement positionnel est un piège structurel : il a produit trois copies
divergentes côté app et une incohérence côté cœur, pour la même raison — un
libellé écrit d'après le *code* alors qu'il décrit une *colonne*. Un test qui
relie le libellé à la grandeur mesurée (CA-1) empêche la classe entière de
revenir, là où l'échange seul corrige une instance.

## 6. Côté `nemetonshiny`

**Rien à faire.** L'app lit `indicator_labels()` depuis v0.126.2.9001 ; le
correctif se propagera à la prochaine release cœur sans intervention. Le test
d'identité `test-indicator-families-defork.R` vérifie déjà que l'app ne
réintroduit pas de copie locale — il ne juge pas le contenu des libellés du
cœur, qui est de la responsabilité du cœur.

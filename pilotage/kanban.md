# Kanban de nemetonshiny

> Cinq colonnes (fiche n° 8). Chaque carte porte l'Homme (qui), la Mission (quoi, quand,
> combien) et les Moyens (comment, où, avec qui), et renvoie à sa fiche dans
> `pilotage/missions/`. Elle note aussi depuis quel cycle de `/boucle` elle est dans sa
> colonne ; on change ce numéro chaque fois qu'elle change de colonne.
> **Une carte qui ne bouge pas est souvent la mauvaise nouvelle à dire tout de suite.**

**Dernier cycle :** 0 _(augmenté de 1 à la fin de chaque `/boucle`, même quand elle n'a
rien lancé. Une carte est immobile quand dernier cycle − N ≥ 2.)_

_Modèle de carte :_

```
- [NNN-titre](missions/NNN-titre.md) · depuis le cycle N
  - Homme : qui
  - Mission : quoi, quand, combien
  - Moyens : comment, où, avec qui
```

## Bac à sable

_En attente du « oui » de Pascal (porte n° 2)._

- [001-contrat-nemetonmcp](missions/001-contrat-nemetonmcp.md) · depuis le cycle 0
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 1
  - Moyens : oui/oui/oui
- [002-tests-erreurs-annotations](missions/002-tests-erreurs-annotations.md) · depuis le cycle 0
  - Homme : `testeur` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 1
  - Moyens : oui/oui/oui
- [003-erreurs-classees](missions/003-erreurs-classees.md) · depuis le cycle 0
  - Homme : `dev` (sonnet, main : tenir)
  - Mission : voir la fiche · cycle visé 1
  - Moyens : oui/oui/oui
- [004-annotations-outils](missions/004-annotations-outils.md) · depuis le cycle 0
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 2
  - Moyens : oui/oui/oui
- [005-fixture-noms-dictes](missions/005-fixture-noms-dictes.md) · depuis le cycle 0
  - Homme : `acquereur` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 1
  - Moyens : oui/oui/partiel (corpus réel incertain)
- [006-tests-noms-resume](missions/006-tests-noms-resume.md) · depuis le cycle 0
  - Homme : `testeur` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 2
  - Moyens : oui/oui/oui après 005
- [007-noms-dictes](missions/007-noms-dictes.md) · depuis le cycle 0
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 3
  - Moyens : oui/oui/oui après 006
- [008-resume-complet](missions/008-resume-complet.md) · depuis le cycle 0
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 3
  - Moyens : oui/oui/oui après 006
- [009-tests-confirmation](missions/009-tests-confirmation.md) · depuis le cycle 0
  - Homme : `testeur` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 2
  - Moyens : oui/oui/oui
- [010-confirmation-jeton](missions/010-confirmation-jeton.md) · depuis le cycle 0
  - Homme : `dev` (opus, main : tenir)
  - Mission : voir la fiche · cycle visé 3
  - Moyens : oui/oui/oui après 009
- [011-mesure-outils-longs](missions/011-mesure-outils-longs.md) · depuis le cycle 0
  - Homme : `analyste` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 1
  - Moyens : oui/oui/oui
- [012-detacher-outils-longs](missions/012-detacher-outils-longs.md) · depuis le cycle 0
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 4 (conditionnelle)
  - Moyens : oui/oui/**non** (attend 011)
- [013-briefs-sortants](missions/013-briefs-sortants.md) · depuis le cycle 0
  - Homme : `archiviste` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 4
  - Moyens : oui/oui/oui après 010

## Validé

## Planifié

## Terminé

## Abandonné

_Avec la raison, en une ligne._

## Livraisons

_Une ligne par cycle, ajoutée sous la précédente à la fin de chaque `/boucle`, et une
par `/bilan` : la livraison que Pascal a retenue, pour qui, et ce qu'elle apporte de plus
que la précédente du même rythme. Un cycle sans livraison neuve livre sa mauvaise
nouvelle, car
« une mauvaise nouvelle est une livraison sur laquelle on apprend » (Pascal) : on
l'inscrit avec ce qu'elle nous apprend._

```
- cycle N · livraison · pour qui · ce qu'elle apporte de plus que la précédente
- cycle N · mauvaise nouvelle : … · pour qui · ce qu'elle nous apprend
- bilan du AAAA-MM-JJ · livraison · pour qui · ce qu'elle apporte de plus que la précédente
- bilan du AAAA-MM-JJ · mauvaise nouvelle : … · pour qui · ce qu'elle nous apprend
```

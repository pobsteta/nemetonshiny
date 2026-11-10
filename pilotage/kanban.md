# Kanban de nemetonshiny

> Cinq colonnes (fiche n° 8). Chaque carte porte l'Homme (qui), la Mission (quoi, quand,
> combien) et les Moyens (comment, où, avec qui), et renvoie à sa fiche dans
> `pilotage/missions/`. Elle note aussi depuis quel cycle de `/boucle` elle est dans sa
> colonne ; on change ce numéro chaque fois qu'elle change de colonne.
> **Une carte qui ne bouge pas est souvent la mauvaise nouvelle à dire tout de suite.**

**Dernier cycle :** 1 _(augmenté de 1 à la fin de chaque `/boucle`, même quand elle n'a
rien lancé. Une carte est immobile quand dernier cycle − N ≥ 2.)_

_Modèle de carte :_

```
- [NNN-titre](missions/NNN-titre.md) · depuis le cycle N
  - Homme : qui
  - Mission : quoi, quand, combien
  - Moyens : comment, où, avec qui
```

## Bac à sable

_Vide : toutes les missions validées le 2026-11-10 (porte n° 2)._

## Validé

- [004-annotations-outils](missions/004-annotations-outils.md) · depuis le cycle 1
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 2
  - Moyens : oui/oui/oui (tests : fichier annotations de 37cbbf92)
- [006-tests-noms-resume](missions/006-tests-noms-resume.md) · depuis le cycle 1
  - Homme : `testeur` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 2
  - Moyens : oui/oui/oui (fixture #254)
- [007-noms-dictes](missions/007-noms-dictes.md) · depuis le cycle 1
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 3
  - Moyens : oui/oui/oui après 006
- [008-resume-complet](missions/008-resume-complet.md) · depuis le cycle 1
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 3
  - Moyens : oui/oui/oui après 006
- [009-tests-confirmation](missions/009-tests-confirmation.md) · depuis le cycle 1
  - Homme : `testeur` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 2
  - Moyens : oui/oui/oui
- [010-confirmation-jeton](missions/010-confirmation-jeton.md) · depuis le cycle 1
  - Homme : `dev` (opus, main : tenir)
  - Mission : voir la fiche · cycle visé 3
  - Moyens : oui/oui/oui après 009
- [012-detacher-outils-longs](missions/012-detacher-outils-longs.md) · depuis le cycle 1
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 4
  - Moyens : oui/oui/**oui** (mesure 011 : croiser_onf + generer_rapport)
- [013-briefs-sortants](missions/013-briefs-sortants.md) · depuis le cycle 1
  - Homme : `archiviste` (sonnet, main : appuyer)
  - Mission : voir la fiche · cycle visé 4
  - Moyens : oui/oui/oui après 010 (+ brief victor)

## Planifié

- [014-alignement-coeur-2026-11-14](missions/014-alignement-coeur-2026-11-14.md) · depuis le cycle 1
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : PR #255 prête, attend Pascal · **à fusionner en premier**
  - Moyens : oui/oui/oui
- [001-contrat-nemetonmcp](missions/001-contrat-nemetonmcp.md) · depuis le cycle 1
  - Homme : `dev` (sonnet, main : appuyer)
  - Mission : PR #253 prête, attend Pascal (CI rouge levée par #255)
  - Moyens : oui/oui/oui
- [005-fixture-noms-dictes](missions/005-fixture-noms-dictes.md) · depuis le cycle 1
  - Homme : `acquereur` (sonnet, main : appuyer)
  - Mission : PR #254 prête après 1 aller-retour, attend Pascal (même rouge)
  - Moyens : oui/oui/oui
- [003-erreurs-classees](missions/003-erreurs-classees.md) · depuis le cycle 1
  - Homme : `dev` (sonnet, main : tenir)
  - Mission : plan accepté ; exécution dès qu'une PR libère le plafond
  - Moyens : oui/oui/oui (tests : fichier erreurs de 37cbbf92)

## Terminé

- [002-tests-erreurs-annotations](missions/002-tests-erreurs-annotations.md) · depuis le cycle 1
  - Homme : `testeur` (sonnet)
  - Mission : prêt après 2 allers-retours ; fichiers intégrés en tête de 003 et 004
  - Moyens : —
- [011-mesure-outils-longs](missions/011-mesure-outils-longs.md) · depuis le cycle 1
  - Homme : `analyste` (sonnet)
  - Mission : prêt ; tranche 012 (croiser_onf + generer_rapport à détacher)
  - Moyens : —

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

- cycle 1 · contrat nemetonshiny → nemetonmcp (CONTRAT.md § 6 bis + test de présence/signature, README MCP à 10 outils, PR #253) · pour le mainteneur de nemetonmcp et les utilisateurs de VICTOR/AIGORA · première livraison : un renommage casse la CI de l'app au lieu de VICTOR en production ; mauvaise nouvelle associée : rien encore de sensible dans AIGORA (une erreur y passe toujours pour un succès, cycle 2) et CI rouge par dérive du cœur (levée par #255)

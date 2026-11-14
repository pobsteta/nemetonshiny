# Kanban de nemetonshiny

> Cinq colonnes (fiche n° 8). Chaque carte porte l'Homme (qui), la Mission (quoi, quand,
> combien) et les Moyens (comment, où, avec qui), et renvoie à sa fiche dans
> `pilotage/missions/`. Elle note aussi depuis quel cycle de `/boucle` elle est dans sa
> colonne ; on change ce numéro chaque fois qu'elle change de colonne.
> **Une carte qui ne bouge pas est souvent la mauvaise nouvelle à dire tout de suite.**

**Dernier cycle :** 2 _(augmenté de 1 à la fin de chaque `/boucle`, même quand elle n'a
rien lancé. Une carte est immobile quand dernier cycle − N ≥ 2.)_

_Modèle de carte :_

```
- [NNN-titre](missions/NNN-titre.md) · depuis le cycle N
  - Homme : qui
  - Mission : quoi, quand, combien
  - Moyens : comment, où, avec qui
```

## Bac à sable

_(vide)_

## Validé

_(vide)_

## Planifié

- [bilan-chaine-nemetonmcp] Brief nemetonmcp appliqué (puis aigora, victor) · depuis le cycle 2
  - Homme : Pascal (sessions nemetonmcp, aigora, victor)
  - Mission : ouvrir `briefs/vers-nemetonmcp/2026-11-10-contrat-jeton-erreurs-noms.md` d'ici le 2026-11-17 ; relance si aucun commit nemetonmcp sous 7 jours
  - Moyens : briefs de la mission 013 (`briefs/vers-*/2026-11-10-*.md`)
- [bilan-essai-couchey] Essai VICTOR sur Couchey · depuis le cycle 2
  - Homme : Pascal, à la voix
  - Mission : « où en est Couchey », « applique les UGF ONF » + « oui », « fais le rapport » ; au plus tard le 2026-11-24, après nemetonmcp
  - Moyens : VICTOR + nemetonmcp à jour ; vérifie aussi le bloc NEMETON_CONFIRMATION en vrai `claude -p` et la durée de l'aperçu croiser_onf (écart l. 45)
- [bilan-adr-0003] ADR 0003 (souveraineté) tranchée · depuis le cycle 2
  - Homme : Pascal
  - Mission : accepter ou refuser `pilotage/adr/0003-acces-tous-souverainete.md` avant la release v2026.11.1, puis report dans platform_nemeton
  - Moyens : l'ADR telle que proposée le 2026-10-24
- [bilan-release-2026.11.1] Release v2026.11.1 · depuis le cycle 2
  - Homme : orchestrateur, sur feu vert de Pascal
  - Mission : publier les changements cassants de 2026.10.18.9009 après l'essai Couchey
  - Moyens : `release.yml` (PR vers main)

## Terminé

- [023-surcouche-classe-saine](missions/023-surcouche-classe-saine.md) · depuis le cycle 2
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #275 fusionnée, cycle dev 2026.10.18.9016, non publié
  - Moyens : —
- [022-aucune-alerte-strate](missions/022-aucune-alerte-strate.md) · depuis le cycle 2
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR (une reprise)
  - Mission : #274 fusionnée (7d11cdc4), cycle dev 2026.10.18.9015, non publié
  - Moyens : —
- [021-plan-validation-reconfort-strate](missions/021-plan-validation-reconfort-strate.md) · depuis le cycle 2
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #273 fusionnée (176c4837), cycle dev 2026.10.18.9014, non publié
  - Moyens : —
- [020-retirer-code-fast-orphelin](missions/020-retirer-code-fast-orphelin.md) · depuis le cycle 2
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #271 fusionnée (6a9326d4), cycle dev 2026.10.18.9013, non publié
  - Moyens : —
- [019-plan-validation-fordead-strate](missions/019-plan-validation-fordead-strate.md) · depuis le cycle 2
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #270 fusionnée (fc36d0bd), cycle dev 2026.10.18.9012, non publié
  - Moyens : —
- [018-retirer-apercu-fast](missions/018-retirer-apercu-fast.md) · depuis le cycle 2
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #268 fusionnée (99959307), cycle dev 2026.10.18.9011, non publié
  - Moyens : —
- [017-fast-sur-tot](missions/017-fast-sur-tot.md) · depuis le cycle 2
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #266 fusionnée (61a682c4), cycle dev 2026.10.18.9010, non publié
  - Moyens : —
- [012-detacher-outils-longs](missions/012-detacher-outils-longs.md) · depuis le cycle 2
  - Homme : `testeur` puis `dev` (sonnet)
  - Mission : #264 fusionnée (5bbfb093), cassante
  - Moyens : —
- [001-contrat-nemetonmcp](missions/001-contrat-nemetonmcp.md) · depuis le cycle 2
  - Homme : `dev`
  - Mission : #253 fusionnée (46eee94b)
  - Moyens : —
- [002-tests-erreurs-annotations](missions/002-tests-erreurs-annotations.md) · depuis le cycle 1
  - Homme : `testeur`
  - Mission : tests intégrés à 003/004
  - Moyens : —
- [003-erreurs-classees](missions/003-erreurs-classees.md) · depuis le cycle 2
  - Homme : `dev`
  - Mission : #256 fusionnée (86138435)
  - Moyens : —
- [004-annotations-outils](missions/004-annotations-outils.md) · depuis le cycle 2
  - Homme : `dev`
  - Mission : #257 fusionnée (9cdd1f3c)
  - Moyens : —
- [005-fixture-noms-dictes](missions/005-fixture-noms-dictes.md) · depuis le cycle 2
  - Homme : `acquereur`
  - Mission : #254 fusionnée (54d65ec4)
  - Moyens : —
- [006-tests-noms-resume](missions/006-tests-noms-resume.md) · depuis le cycle 2
  - Homme : `testeur`
  - Mission : tests 35cbf2be intégrés à 007/008
  - Moyens : —
- [007-noms-dictes](missions/007-noms-dictes.md) · depuis le cycle 2
  - Homme : `dev`
  - Mission : #258 fusionnée (63cda006)
  - Moyens : —
- [008-resume-complet](missions/008-resume-complet.md) · depuis le cycle 2
  - Homme : `dev`
  - Mission : #260 fusionnée (1ea3289b)
  - Moyens : —
- [009-tests-confirmation](missions/009-tests-confirmation.md) · depuis le cycle 2
  - Homme : `testeur`
  - Mission : tests 93291ec8 intégrés à 010
  - Moyens : —
- [010-confirmation-jeton](missions/010-confirmation-jeton.md) · depuis le cycle 2
  - Homme : `dev` (opus)
  - Mission : #259 fusionnée (1af92486), cassante
  - Moyens : —
- [011-mesure-outils-longs](missions/011-mesure-outils-longs.md) · depuis le cycle 1
  - Homme : `analyste`
  - Mission : mesure faite, a tranché 012
  - Moyens : —
- [013-briefs-sortants](missions/013-briefs-sortants.md) · depuis le cycle 2
  - Homme : `archiviste`
  - Mission : #262 fusionnée (8de5e07c) après 3 allers-retours + décision de Pascal
  - Moyens : —
- [014-alignement-coeur-2026-11-14](missions/014-alignement-coeur-2026-11-14.md) · depuis le cycle 2
  - Homme : `dev`
  - Mission : #255 fusionnée (95d36dcf)
  - Moyens : —
- [015-table-familles-partagee](missions/015-table-familles-partagee.md) · depuis le cycle 2
  - Homme : `dev`
  - Mission : #263 fusionnée (2e25e84c)
  - Moyens : —
- [016-jeton-persiste](missions/016-jeton-persiste.md) · depuis le cycle 2
  - Homme : `dev` (opus)
  - Mission : #261 fusionnée (348ff3df)
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
- cycle 2 · pilotage vocal sûr : noms dictés de travers retrouvés, aucune écriture sans « oui » (jeton qui survit entre deux tâches VICTOR), resume_projet = écran Synthèse, outils longs en tâche de fond, 4 briefs vers nemetonmcp/aigora/victor (PR #256-#264) · pour Pascal et les utilisateurs de VICTOR/AIGORA · le cycle 1 livrait un contrat ; celui-ci livre des comportements essayables à la voix (essai terrain encore à faire)
- bilan du 2026-11-13 · mauvaise nouvelle : 16 missions et 12 PR fusionnées, rien de publié ni consommé par nemetonmcp, le critère « moins se répéter dans AIGORA » n'a pas bougé · pour Pascal et les utilisateurs de VICTOR/AIGORA · ce qu'elle nous apprend : un chantier qui traverse plusieurs dépôts ne livre qu'au bout de la chaîne ; le prochain cycle se mesure à l'essai de Couchey, pas au nombre de PR

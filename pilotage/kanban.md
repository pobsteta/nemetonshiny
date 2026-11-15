# Kanban de nemetonshiny

> Cinq colonnes (fiche n° 8). Chaque carte porte l'Homme (qui), la Mission (quoi, quand,
> combien) et les Moyens (comment, où, avec qui), et renvoie à sa fiche dans
> `pilotage/missions/`. Elle note aussi depuis quel cycle de `/boucle` elle est dans sa
> colonne ; on change ce numéro chaque fois qu'elle change de colonne.
> **Une carte qui ne bouge pas est souvent la mauvaise nouvelle à dire tout de suite.**

**Dernier cycle :** 3 _(augmenté de 1 à la fin de chaque `/boucle`, même quand elle n'a
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

- [essais-terrain-cycle-3] Essais de Pascal : export « Affouage » réel format 8 (027), T4 sur Dabo vers 57 (029), placette Groma 1.6 avec et sans vidéo, deux clics « Récupérer » → une seule archive (030, 033), fiche Couchey à 63 UGF après réinstallation de nemetonmcp (031), W1 non NA sur Couchey (032) · depuis le cycle 3
  - Homme : Pascal
  - Mission : retours au journal en lignes « écart »
  - Moyens : app v2026.11.2
- [suite-duree-tests] Pistes de la mission 026 (un seul processus, garde `NEMETON_TESTS_LENTS`) · depuis le cycle 3
  - Homme : à décider par Pascal
  - Mission : rapport 026 au journal
  - Moyens : —

- [suite-plateforme-coeur] Reporter l'ADR 0003 dans platform_nemeton, ligne PLAN.md v2026.11.1 dans nemeton · depuis le cycle 3 · échéance 2026-11-20
  - Homme : Pascal (sessions platform_nemeton et nemeton)
  - Mission : `briefs/vers-platform_nemeton/2026-11-15-adr-0003-acces-tous.md` ; `~/dev/briefs/vers-nemeton/2026-11-15-plan-nemetonshiny-v2026.11.1.md` (hors dépôt)
  - Moyens : briefs déposés le 2026-11-15
- [essai-terrain-couchey] Essai VICTOR de bout en bout, fiches PDF par UGF, cartes santé par strate · depuis le cycle 3 · échéance 2026-11-24
  - Homme : Pascal, sur Couchey
  - Mission : VICTOR à la voix (essai par texte fait le 2026-11-15 ; reste le micro et la transcription Gradium) ; liste de contrôle de la fiche 024 (Statut) ; série santé 017-023
  - Moyens : app 2026.11.1 ; retours au journal en lignes « écart »
## Terminé

- [033-groma-incremental-dbh-ruban](missions/033-groma-incremental-dbh-ruban.md) · cycle 3 · PR #292 (`0d9eb553`), `.9008`
- [032-bdtopo-reponse-vide](missions/032-bdtopo-reponse-vide.md) · cycle 3 · PR #290 (`4d1ccf55`), `.9007`

- [026-duree-tests](missions/026-duree-tests.md) · cycle 3 · rapport de l'analyste ; commande ciblée adoptée, à remesurer machine calme
- [027-marculus-csv-formats-6-8](missions/027-marculus-csv-formats-6-8.md) · cycle 3 · PR #284 (`ca837bf8`), `.9002`
- [031-synthese-n-ugf-indicateurs-perimes](missions/031-synthese-n-ugf-indicateurs-perimes.md) · cycle 3 · PR #285 (`93299204`), `.9003`
- [029-t4-vitalite-dynamique](missions/029-t4-vitalite-dynamique.md) · cycle 3 · PR #286 (`03125fe5`), `.9004`
- [030-groma-schema-1-6](missions/030-groma-schema-1-6.md) · cycle 3 · PR #287 (`953729be`), `.9005`
- [028-tarifs-proposes-niveau-foret](missions/028-tarifs-proposes-niveau-foret.md) · cycle 3 · PR #288 (`c90f8d12`), `.9006`

- [brief-c1-infradensite] Brief nemeton du 2026-11-09 : l'app n'atteint jamais le mode CHM du C1 (aucune colonne `dbh`), rien ne change ; réponse déposée dans `vers-nemeton` le 2026-11-15 · depuis le cycle 3
- [brief-tarifs-ifn-negatifs] Brief Marculus du 2026-11-10 : questions d'origine transmises au cœur (`vers-nemeton/2026-11-15-tarifs-ifn-numeros-negatifs.md`) ; filtrage côté app dans 028 · depuis le cycle 3
- [main-rouge-famille-t] Description de la famille T alignée sur nemeton 2026.11.28 (`.9001`, PR #283) · depuis le cycle 3

- [suite-victor-aigora] Briefs victor (relais vocal de la confirmation, journal des dictées) et aigora appliqués le 2026-11-15 (victor `main` 9dd43dd ; aigora-nemeton 9ac3c34, branche locale `confirmation-nemetonmcp` non poussée) ; essai par texte de bout en bout réussi sur Couchey (croiser_onf, jeton entre deux `claude -p` réels) · depuis le cycle 3
- [bilan-chaine-nemetonmcp] Chaîne nemetonmcp close le 2026-11-15 : nemetonmcp#27 fusionnée, release nemetonmcp v2026.11.2 (journal du 2026-11-15, « clôture chaîne nemetonmcp »)
- [025-release-2026-11-1](missions/025-release-2026-11-1.md) · depuis le cycle 3 · Release **v2026.11.1** publiée le 2026-11-15 (PR #280, `eaa47428`, tag posé par release.yml) ; contrat nemetonmcp à 33 objets
- [bilan-essai-couchey] Essai Couchey côté nemetonmcp validé par Pascal le 2026-11-14 (source de seconde main : brief nemetonmcp du 2026-11-15, journal « 025 · accord ») ; l'essai VICTOR de bout en bout reste à faire (carte essai-terrain-couchey)
- [bilan-adr-0003](adr/0003-acces-tous-souverainete.md) ADR 0003 tranchée par Pascal le 2026-11-15 : option A (tout client MCP, avertissement) · report dans platform_nemeton : carte suite-plateforme-coeur
- [024-fiche-pdf-par-ugf](missions/024-fiche-pdf-par-ugf.md) · depuis le cycle 3
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR ; PR #277 (`ca89adf9`), 2026.10.18.9017, issue #20 fermée ; reste l'essai terrain de Pascal (Quarto en tâche de fond)
- [023-surcouche-classe-saine](missions/023-surcouche-classe-saine.md) · depuis le cycle 3
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #275 fusionnée, cycle dev 2026.10.18.9016, non publié
  - Moyens : —
- [022-aucune-alerte-strate](missions/022-aucune-alerte-strate.md) · depuis le cycle 3
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR (une reprise)
  - Mission : #274 fusionnée (7d11cdc4), cycle dev 2026.10.18.9015, non publié
  - Moyens : —
- [021-plan-validation-reconfort-strate](missions/021-plan-validation-reconfort-strate.md) · depuis le cycle 3
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #273 fusionnée (176c4837), cycle dev 2026.10.18.9014, non publié
  - Moyens : —
- [020-retirer-code-fast-orphelin](missions/020-retirer-code-fast-orphelin.md) · depuis le cycle 3
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #271 fusionnée (6a9326d4), cycle dev 2026.10.18.9013, non publié
  - Moyens : —
- [019-plan-validation-fordead-strate](missions/019-plan-validation-fordead-strate.md) · depuis le cycle 3
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #270 fusionnée (fc36d0bd), cycle dev 2026.10.18.9012, non publié
  - Moyens : —
- [018-retirer-apercu-fast](missions/018-retirer-apercu-fast.md) · depuis le cycle 3
  - Homme : `dev` (sonnet), relecteur sur la fiche puis la PR
  - Mission : #268 fusionnée (99959307), cycle dev 2026.10.18.9011, non publié
  - Moyens : —
- [017-fast-sur-tot](missions/017-fast-sur-tot.md) · depuis le cycle 3
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
- bilan du 2026-11-15 · release v2026.11.1 consommée par nemetonmcp v2026.11.2 (erreurs reconnues comme erreurs, noms dictés retrouvés, confirmation avant écriture, outils longs en tâche de fond, contrat à 33 objets) · pour Pascal et les utilisateurs d'AIGORA · ce qu'elle apporte de plus que le bilan du 2026-11-13 : la chaîne livre pour la première fois jusqu'au bout (publié ET consommé) ; VICTOR pas encore (relais du jeton à faire)

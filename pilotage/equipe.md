# Équipe de nemetonshiny

> Produit par l'orchestrateur avec `/imperatrice:mission`, **validé par Pascal** avant
> tout démarrage. Règle de staffing : un agent par mission qui passe les trois
> questions, jamais un agent « par principe ».

- **Statut :** validée le 2026-11-10 (porte n° 2)
- **Validée par :** Pascal (« Oui vas y », 2026-11-10)

## Plafond humain

- **PR ouvertes en même temps, au plus :** 3
- **Rythme de relecture de Pascal :** _…_

## Le vivier (catalogue, pas une équipe)

| Agent | A / plan | Modèle | Écrit du code ? |
|---|---|---|---|
| `contradicteur` | Abstraction · Polos | opus | non |
| `acquereur` | Acquisition · Ponos | sonnet | non (référentiels et données) |
| `archiviste` | Archivage · Pontos | sonnet | non (docs) |
| `analyste` | Analyse · Ponos | sonnet | non |
| `afficheur` | Affichage · Ponos | sonnet | oui (interface) |
| `dev` | réalisation · Ponos | sonnet | oui |
| `testeur` | réalisation · Ponos | sonnet | oui (tests) |
| `relecteur` | contrôle · C3 | opus | non (lecture seule) |

## Missions

| Mission | Agent | Modèle | Temps de la main | Les 3 questions | Cycle |
|---|---|---|---|---|---|
| [001-contrat-nemetonmcp](missions/001-contrat-nemetonmcp.md) | `dev` | sonnet | appuyer | oui/oui/oui | 1 |
| [002-tests-erreurs-annotations](missions/002-tests-erreurs-annotations.md) | `testeur` | sonnet | appuyer | oui/oui/oui | 1 |
| [003-erreurs-classees](missions/003-erreurs-classees.md) | `dev` | sonnet | tenir | oui/oui/oui | 1 |
| [004-annotations-outils](missions/004-annotations-outils.md) | `dev` | sonnet | appuyer | oui/oui/oui | 2 |
| [005-fixture-noms-dictes](missions/005-fixture-noms-dictes.md) | `acquereur` | sonnet | appuyer | oui/oui/partiel (corpus réel incertain) | 1 |
| [006-tests-noms-resume](missions/006-tests-noms-resume.md) | `testeur` | sonnet | appuyer | oui/oui/oui après 005 | 2 |
| [007-noms-dictes](missions/007-noms-dictes.md) | `dev` | sonnet | appuyer | oui/oui/oui après 006 | 3 |
| [008-resume-complet](missions/008-resume-complet.md) | `dev` | sonnet | appuyer | oui/oui/oui après 006 | 3 |
| [009-tests-confirmation](missions/009-tests-confirmation.md) | `testeur` | sonnet | appuyer | oui/oui/oui | 2 |
| [010-confirmation-jeton](missions/010-confirmation-jeton.md) | `dev` | opus | tenir | oui/oui/oui après 009 | 3 |
| [011-mesure-outils-longs](missions/011-mesure-outils-longs.md) | `analyste` | sonnet | appuyer | oui/oui/oui | 1 |
| [012-detacher-outils-longs](missions/012-detacher-outils-longs.md) | `dev` | sonnet | appuyer | oui/oui/**non** (attend 011) | 4 (conditionnelle) |
| [013-briefs-sortants](missions/013-briefs-sortants.md) | `archiviste` | sonnet | appuyer | oui/oui/oui après 010 | 4 |
| [014-alignement-coeur-2026-11-14](missions/014-alignement-coeur-2026-11-14.md) | `dev` | sonnet | appuyer | oui/oui/oui | 1 |
| [015-table-familles-partagee](missions/015-table-familles-partagee.md) | `dev` | sonnet | appuyer | oui/oui/oui (staffing confirmé par Pascal le 2026-11-11) | 2 |
| [016-jeton-persiste](missions/016-jeton-persiste.md) | `dev` | opus | appuyer | oui/oui/oui (décidée par Pascal le 2026-11-10) | 2 |

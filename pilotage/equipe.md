# Équipe de nemetonshiny

> Produit par l'orchestrateur avec `/imperatrice:mission`, **validé par Pascal** avant
> tout démarrage. Règle de staffing : un agent par mission qui passe les trois
> questions, jamais un agent « par principe ».

- **Statut :** validée le 2026-11-10 (porte n° 2)
- **Validée par :** Pascal (« Oui vas y », 2026-11-10)

## Plafond humain

- **PR ouvertes en même temps, au plus :** 3
- **Rythme de relecture de Pascal :** 5 PR par jour ouvré (Pascal, bilan du 2026-11-13)

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
| [017-fast-sur-tot](missions/017-fast-sur-tot.md) | `dev` | sonnet | appuyer | oui/oui/oui (cap de Pascal, journal 2026-11-13) | 3 |
| [018-retirer-apercu-fast](missions/018-retirer-apercu-fast.md) | `dev` | sonnet | appuyer | oui/oui/oui (« Retire le », 2026-11-14) | 3 |
| [019-plan-validation-fordead-strate](missions/019-plan-validation-fordead-strate.md) | `dev` | sonnet | appuyer | oui/oui/oui (« Oui fais le », 2026-11-14) | 3 |
| [020-retirer-code-fast-orphelin](missions/020-retirer-code-fast-orphelin.md) | `dev` | sonnet | appuyer | oui/oui/oui (« oui fais le », 2026-11-14) | 3 |
| [021-plan-validation-reconfort-strate](missions/021-plan-validation-reconfort-strate.md) | `dev` | sonnet | appuyer | oui/oui/oui (2026-11-14) | 3 |
| [022-aucune-alerte-strate](missions/022-aucune-alerte-strate.md) | `dev` | sonnet | appuyer | oui/oui/oui (2026-11-14) | 3 |
| [023-surcouche-classe-saine](missions/023-surcouche-classe-saine.md) | `dev` | sonnet | appuyer | oui/oui/oui (2026-11-14) | 3 |
| [024-fiche-pdf-par-ugf](missions/024-fiche-pdf-par-ugf.md) | `dev` | sonnet | appuyer | oui/oui/oui (« fais la fiche PDF par UGF », 2026-11-14) | 3 |
| [025-release-2026-11-1](missions/025-release-2026-11-1.md) | orchestrateur | — | appuyer | oui/oui/oui (« oui », 2026-11-15) | 3 |

## Ajustements du bilan du 2026-11-13 (validés par Pascal)

- `testeur` : reste ; chaque fiche de test porte la liste de contrôle « test discriminant » (code et classe d'erreur exacts, une mutation qui fait échouer le test, pas de test vacant). Cause n° 1 du Pareto.
- `relecteur` : relit aussi la fiche de mission **avant** le dev (rubrique « décisions de Pascal applicables »), en plus de la PR. Cause n° 2.
- `dev` sur opus : réservé au jeton et à la sécurité (comme 010, 016) ; le reste sur sonnet.
- Temps de la main : 003 et 010 passent de « tenir » à « appuyer » (contrat d'erreurs et de jeton stable).
- `afficheur` : non utilisé, sans objet tant que le chantier reste MCP ; reste au vivier.
- Pas de cycle de code dans ce dépôt tant que la chaîne nemetonmcp → essai Couchey n'est pas bouclée (cartes « Planifié » du kanban).
- Listes de contrôle (test discriminant ; fiche prête pour le dev) : [listes-de-controle.md](listes-de-controle.md).

## Ajustements du bilan du 2026-11-15 (validés par Pascal, « Oui vas-y »)

- Missions 017-025 inscrites ci-dessus (cycle 3), validées une à une par Pascal au journal ; la porte n° 2 n'avait pas été repassée pour elles (constat de l'archiviste).
- `analyste` : mission « durée des tests » avant le prochain cycle de code (carte `duree-tests`). Cause n° 2 du Pareto (tests trop longs pour la machine, 23 %).
- Numérotation : seul l'orchestrateur numérote `DESCRIPTION` / `NEWS` (cycle `.900x`), à la fusion ; les `dev` écrivent leur entrée NEWS sans numéro. Cause n° 4 (missions parallèles, 14 %).
- Fiche-type : rubrique « lecteurs et appelants recensés par grep, repli, périmètre limite » remplie avant relecture. Cause n° 1 (fiches à reprendre, 36 %) ; effet mesuré sur les 3 prochaines missions.
- Pas de nouvelle mission de code dans ce dépôt avant les deux briefs victor (carte `suite-victor`).
- `afficheur` : reste au vivier.
- Listes de contrôle mises à jour : [listes-de-controle.md](listes-de-controle.md).

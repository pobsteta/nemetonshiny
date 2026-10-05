# Serveur MCP nemetonshiny

Pilote Nemeton depuis un assistant (Claude Code, un espace AIGORA, VICTOR)
sans ouvrir l'application. Les outils sont de fines enveloppes de l'API hors
interface (`?api_hors_interface`) : **aucune lecture ne modifie un projet**.

## Enregistrement dans Claude Code

```bash
claude mcp add nemeton -- Rscript -e 'source(system.file("mcp/server.R", package = "nemetonshiny"))'
```

Prérequis : `install.packages("mcptools")`. Le dossier des projets se règle
par `NEMETON_PROJECT_DIR` (sinon le dossier par défaut de l'application).

## Outils

| Outil | Rôle | Écrit |
|-------|------|-------|
| `lister_projets` | projets (id, nom, statut, indicateurs, à jour) | non |
| `resume_projet` | score global, NDP, confiance, 12 familles | non |
| `lancer_calcul` | calcul complet en tâche de fond ; rend la main aussitôt | oui |
| `etat_calcul` | statut, progression, erreur et fin du journal en cas d'échec | non |
| `annuler_calcul` | demande l'arrêt du calcul en cours | signal d'arrêt |
| `generer_rapport` | PDF dans `exports/` du projet | le fichier |
| `exporter_gpkg` | GeoPackage des résultats dans `exports/` | le fichier |
| `url_app` | URL `http://127.0.0.1:<port>/?project=…&tab=…` | non |

Un projet se désigne par son id **ou** par son nom (casse et accents
ignorés). Plusieurs correspondances : l'outil renvoie la liste des candidats
au lieu de choisir.

Chaque réponse est un objet JSON : `{"ok": true, ...}` ou
`{"ok": false, "erreur": ..., "classe": ...}`.

## Calcul en tâche de fond

`lancer_calcul` démarre un `Rscript` détaché (`setsid`) qui survit à la fin
de la tâche de l'assistant, avec le même plafond mémoire et le même journal
que l'application. Il écrit `data/compute_job.json` (statut, pid, erreur) et
`data/compute_mcp.log`. Refusé si :

- un calcul tourne déjà (lancé par l'assistant ou par l'application) ;
- le projet est en cours d'édition dans l'application (verrou) ;
- une migration est nécessaire (indicateurs calculés sous un ancien sens).

`etat_calcul` signale `echec` quand le processus a disparu sans terminer.

## Ouvrir l'application

`url_app` construit le lien ; l'application doit tourner sur ce port
(`NEMETON_APP_PORT`, défaut 3838) :

```bash
Rscript -e 'nemetonshiny::run_app(tour = FALSE, options = list(port = 3838, launch.browser = FALSE))'
```

## Sécurité

- Tout reste local (`127.0.0.1`) ; le serveur parle en stdio.
- Aucun outil ne supprime, ne crée de projet ni n'exécute de code arbitraire.
- Un assistant lancé sans demande d'autorisation (VICTOR,
  `bypassPermissions`) doit tourner dans un espace dédié, jamais dans un
  dépôt de développement.

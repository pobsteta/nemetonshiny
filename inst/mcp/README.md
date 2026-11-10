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
| `appliquer_ugf` | remplace les UGF par celles d'un fichier de tènements (GeoPackage/GeoJSON) ; les indicateurs calculés deviennent périmés | oui, **modifie le projet** |
| `croiser_onf` | construit les UGF depuis les parcelles forestières ONF recalées sur le cadastre ; `purger` : `TRUE` retire les parcelles hors régime forestier, `FALSE` les garde toutes, défaut = réglage du projet ; les indicateurs calculés deviennent périmés | oui, **modifie le projet** |
| `url_app` | URL `http://127.0.0.1:<port>/?project=…&tab=…` | non |

Un projet se désigne par son id **ou** par son nom (casse et accents
ignorés). Plusieurs correspondances : l'outil renvoie la liste des candidats
au lieu de choisir.

Chaque réponse est un objet JSON : `{"ok": true, ...}` ou, en cas d'erreur,
`{"ok": false, "classe", "code", "langue", "erreur", "action", "candidats"}`.
Une erreur est aussi **marquée** : l'outil rend un résultat en erreur
(`isError: true` côté MCP), qu'un agent ne peut pas lire comme un succès.

- `classe` : nom R complet de la classe d'erreur (`nemetonshiny_projet_introuvable`) ;
  `code` : son nom court (`projet_introuvable`).
- `erreur` : le message, en français par défaut. `langue` (`fr` ou `en`) n'est
  acceptée que par `resume_projet` et `generer_rapport`.
- `action` : la suite à proposer (introuvable : `lister_projets` ;
  calcul en cours : `etat_calcul` ; projet verrouillé : attendre ou prévenir
  le détenteur nommé).
- `candidats` : rempli **seulement** pour `projet_ambigu`, au format
  « nom (id) » ; absent pour toute autre erreur.
- Une erreur non répertoriée a pour `classe` `nemetonshiny_erreur_inconnue`
  (condition sans classe `nemetonshiny_*`) ou `nemetonshiny_erreur`
  (classe `nemetonshiny_*` hors liste) ; `action` : le signaler.
- `projet`, `projet_nom`, `jeton`, `apercu`, `expire_a` : remplis **seulement** pour
  `confirmation_requise` (voir ci-dessous).
- Limite actuelle : servis par `nemetonmcp`, `etat_calcul` et
  `exporter_gpkg` sont remplacés par ses propres outils et ne sont **pas
  encore marqués** `isError` (brief en cours) ; les autres outils le sont.

## Confirmation des écritures (ADR 0002)

`appliquer_ugf` et `croiser_onf` (quelle que soit la valeur de `purger`)
écrivent en **deux temps** :

1. Appelés **sans** `jeton`, ils font tous les contrôles (gardes, fichier,
   croisement ONF) et **n'écrivent rien**. Ils rendent l'erreur
   `confirmation_requise` avec `projet` (identifiant) et `projet_nom` (nom
   résolu, cité aussi dans le message), un `apercu`, un `jeton` et `expire_a`
   (ISO 8601, UTC) :
   - `appliquer_ugf` : `n_ugf_remplacees` (UGF actuelles touchées),
     `n_ugf_nouvelles` (UGF du fichier) ;
   - `croiser_onf` : `ecartees` (IDU qui quittent le projet), `n_ecartees`,
     `n_retenues` (rien d'écarté si `purger` résolu vaut `FALSE`).
2. L'agent lit l'aperçu à l'utilisateur. **Seulement s'il confirme**, il
   rappelle le même outil avec les mêmes paramètres et `jeton`.

Le jeton est à usage unique, valable 10 minutes (valeur provisoire), gardé en
mémoire du serveur. Il est lié à l'outil, au projet, aux parcelles du projet
et aux paramètres : fichier et son md5, `remplacer` ; tous les réglages ONF
résolus du projet (dont `purger`). Tout jeton connu présenté est consommé,
même refusé, **sauf** si le projet est introuvable ou ambigu, ou si les
métadonnées ONF du projet sont illisibles : ces erreurs sont levées avant
que le jeton soit lu, il reste donc valable. Refus : `jeton_invalide` (inconnu, périmé, déjà utilisé, autres
paramètres) ; il faut redemander un aperçu. À la confirmation, les gardes
(calcul en cours, verrou) sont rejouées avant le jeton, puis le fichier est
relu ou le croisement recalculé.

C'est une barrière, pas une preuve : un agent qui enchaîne seul les deux
appels contourne la garde.

## Calcul en tâche de fond

`lancer_calcul` démarre un `Rscript` détaché (`setsid`) qui survit à la fin
de la tâche de l'assistant, avec le même plafond mémoire et le même journal
que l'application. Il écrit `data/compute_job.json` (statut, pid, erreur) et
`data/compute_mcp.log`. Refusé si :

- un calcul tourne déjà (lancé par l'assistant ou par l'application) ;
- le projet est en cours d'édition dans l'application (verrou) ;
- le projet a été créé avant la version 1.0.0 (il n'est pas repris : le recréer).

`etat_calcul` signale `echec` quand le processus a disparu sans terminer.

## Ouvrir l'application

`url_app` construit le lien ; l'application doit tourner sur ce port
(`NEMETON_APP_PORT`, défaut 3838) :

```bash
Rscript -e 'nemetonshiny::run_app(tour = FALSE, options = list(port = 3838, launch.browser = FALSE))'
```

## Sécurité

- Tout reste local (`127.0.0.1`) ; le serveur parle en stdio.
- Le serveur force sa locale en UTF-8 au lancement (`C.UTF-8`, repli `en_US.UTF-8` ; un message sur stderr si aucune n'existe), même lancé sous `LC_ALL=C`.
- Aucun outil ne supprime un projet, n'en crée ni n'exécute de code arbitraire.
  Deux outils modifient pourtant un projet existant : `appliquer_ugf` et
  `croiser_onf`. Avec `purger = TRUE`, ce dernier **retire du projet** les
  parcelles hors régime forestier. Les deux n'écrivent qu'après confirmation
  par jeton (ADR 0002, ci-dessus).
- Un assistant lancé sans demande d'autorisation (VICTOR,
  `bypassPermissions`) doit tourner dans un espace dédié, jamais dans un
  dépôt de développement.

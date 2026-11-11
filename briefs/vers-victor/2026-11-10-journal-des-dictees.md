# Brief `victor` — journaliser le texte dicté à côté des appels d'outils

> **Déposé le 2026-11-10** par la session `nemetonshiny` (mission 013, ajout de Pascal du 2026-11-10).
> Rien n'a été écrit dans `victor`. Constats relevés dans `victor/victor/server.py` (lecture seule).

## Pourquoi

La fixture `tests/testthat/fixtures/noms_dictes.csv` de nemetonshiny (mission 005) verrouille la résolution
des noms de projets dictés de travers (mission 007, PR #258). Elle est aujourd'hui **simulée** : ses 33
lignes sont des erreurs plausibles inventées (colonne `source` = « simule: … »), pas de vraies dictées.
Le seuil de distance (2) est **provisoire** (spec P1, critère 1) et ne peut être réglé qu'avec de vrais
textes transcrits. Il faut donc recueillir ce que le STT livre réellement, à côté de ce que VICTOR en fait.

## Ce qui est demandé

Un journal **local, en ajout seul**, une ligne JSON par événement, avec pour chaque tour utilisateur et
chaque appel d'outil :

| Champ | Contenu | Où le prendre dans `server.py` |
|---|---|---|
| `t` | horodatage ISO 8601, fuseau Europe/Paris | à l'écriture |
| `dit` | **texte transcrit tel que reçu**, avant toute correction | `end_of_turn()` → `user_said(text)` (`join_words(self.words)`) ; c'est l'événement `user_final` |
| `outil` | nom de l'outil appelé | `run_tool(name, args)` |
| `args` | arguments de l'appel (le nom de projet tel que le modèle l'a passé) | `run_tool` |
| `delegation` | pour `delegate_to_claude` : le `prompt` délégué (il contient le nom entendu) | `start_task` |
| `resultat` | succès ou erreur ; pour une erreur le `code` (ex. `projet_introuvable`, `projet_ambigu`) et les `candidats` | sortie de la tâche (`task["output"]`) ou résultat de `nemeton_fiche` / `nemeton_etat` |

Le résultat des tâches déléguées n'est connu qu'à la fin de la tâche (`_run_task`) : relier événements par
`task_id`. Les appels qui n'ont pas d'incidence sur un nom de projet peuvent être omis ; garder au moins
`nemeton_fiche`, `nemeton_etat`, `open_nemeton`, `delegate_to_claude` (espace `nemeton`).

Aujourd'hui rien n'est conservé : `run_tool` n'imprime que pour `open_url` et `open_nemeton`
(`server.py:1927-1928`), les journaux vont à `journalctl --user -u victor` (README), et l'historique est
tronqué (`_trim`, `MAX_HISTORY`).

## Contraintes

- **Données personnelles de voix transcrite** : journal **désactivé par défaut**, activé par une variable
  d'environnement (nom à choisir côté VICTOR, par ex. `VICTOR_JOURNAL_DICTEES=<chemin>`), fichier **local,
  hors dépôt, ignoré par git**, jamais envoyé ailleurs. Rotation ou taille maximale à prévoir. Rien de plus
  que le texte et les appels listés (pas d'audio).
- **Ne rien changer au comportement vocal** : l'écriture du journal ne doit ni bloquer ni faire échouer un
  tour (erreur d'écriture avalée et signalée une fois en stderr).
- Pas de secret dans les arguments journalisés (clés d'API, jetons de confirmation : **ne pas écrire le
  `jeton`** de confirmation d'écriture, voir le brief `2026-11-10-relais-vocal-confirmation.md`).

## Comment la fixture sera alimentée

Pascal (ou une session `nemetonshiny`) relit le journal, repère les tours où un nom de projet a été
dicté, et ajoute à `noms_dictes.csv` une ligne `dit` = texte transcrit, `attendu` = nom du projet voulu,
`source` = « victor: AAAA-MM-JJ » (à la place de « simule: … »). Le champ `attendu` est un jugement humain
: le journal ne le fournit pas. Les cas `projet_ambigu` et `projet_introuvable` sont aussi précieux que les
réussites : ils fixent le seuil.

## Ce que cette session n'a pas pu vérifier

- Qu'`end_of_turn()` voit bien le texte brut du STT (Gradium) avant toute normalisation ; `join_words`
  (assemblage des mots) peut déjà en modifier la forme.
- Que les tâches déléguées donnent leur résultat de façon exploitable dans `task["output"]` (texte libre
  de `claude -p`, pas le JSON de l'outil MCP).
- La durée de rétention acceptable : à fixer avec Pascal.

## Retour attendu

Un brief de réponse dans `vers-nemetonshiny/` donnant le nom de la variable, le chemin et un échantillon de
lignes (sans donnée sensible).

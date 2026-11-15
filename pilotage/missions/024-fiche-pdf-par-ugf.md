# Mission 024 — Fiche PDF par UGF du Plan d'actions (une UGF ou ZIP de toutes) — issue #20

> Créée par l'orchestrateur le 2026-11-14 sur demande de Pascal (« fais la fiche PDF par UGF »).
> Solde la dernière issue ouverte de l'épopée Plan d'actions (#7, fermée) : #20.

## Mission

**En une phrase :** Depuis l'onglet Plan d'actions, exporter une **fiche PDF par UGF** : la fiche
de l'UGF sélectionnée, ou, si plusieurs sont sélectionnées, une **archive ZIP** d'une fiche par
UGF sélectionnée (aucune sélection : toutes les UGF ayant des actions visibles), au style du PDF
du plan existant.

## Constat (orchestrateur, 2026-11-14, lu sur `main` 7a6b6ab8)

- PDF du plan **par projet** déjà livré : bouton `download_pdf` (`R/mod_action_plan.R:184-195`,
  handler `:2447-2512`), `generate_action_plan_pdf(project, plan, ug_sf, output_file, language,
  filter_ug_ids, filter_action_ids)` (`R/service_export.R:2478`), gabarit
  `inst/quarto/action_plan_template.qmd` (239 lignes) : page de titre + totaux globaux, puis une
  page par UGF (carte satellite `.render_ug_satellite_png`, tableau des actions, coût / revenu /
  bilan, commentaires). Archive d'une copie dans `exports/` (`.archive_action_plan_pdf`,
  `R/mod_action_plan.R:494`), avec le nom **en dur** `<slug>_action_plan.pdf` (`:501`).
- `generate_action_plan_pdf` accepte déjà `filter_ug_ids` : une fiche d'UGF = un appel filtré, et
  les **totaux, `n_actions`, `n_ugfs` sont déjà calculés sur le sous-ensemble filtré**
  (`R/service_export.R` env. 2570-2582). Le mode fiche ne sert qu'à la page de titre (nom,
  surface de l'UGF) et au titre « Synthèse globale » (gabarit `:103`), faux pour une fiche.
- La liste `data` passée au gabarit (`readRDS(params$data_file)`, seul paramètre `language`) est
  construite **dans** `generate_action_plan_pdf`, après `ensure_quarto_installed()` qui fait un
  `stop` : aujourd'hui intestable hors Quarto. Les actions enrichies (env. 2526-2540) ne portent
  pas `source_origine` / `source_extrait`. Les actions sont dans l'ordre du plan (pas de tri par
  année). L'en-tête YAML du gabarit est figé en français (`:2-4`, `:31`).
- La CI n'installe pas Quarto : tout test de rendu y est sauté.
- Sélection d'UGF dans le module : `selected_ug_rv()` (`:651`, synchronisée carte ⇄ tableau) ;
  actions visibles : `actions_df()`.
- Zip : `utils::zip` déjà utilisé (`R/utils_io.R:172-176`, `R/service_marculus.R:1269`).
- Demandé par l'issue #20 et absent du gabarit : la **liste des sources** de chaque action
  (`source$origine`, `source$extrait_texte`, `R/service_action_plan.R:24`, `:745-746`).

## Travail attendu

0. **Constructeur pur** : extraire `.build_action_plan_pdf_data(project, plan, ug_sf,
   filter_ug_ids, filter_action_ids, mode = c("plan", "fiche"), maps)` (modèle
   `.build_regeneration_pdf_data`, `R/service_export.R` env. 2654), carte injectable ; il porte
   `source_origine` / `source_extrait`, trie les actions de chaque UGF par année (chronologique,
   demandé par #20), et en mode fiche le nom et la surface de l'UGF. `generate_action_plan_pdf`
   l'appelle (PDF projet inchangé hormis le tri chronologique et la section Sources).
1. **Service** : `generate_action_plan_ug_sheets(project, plan, ug_sf, output_file, language,
   ug_ids, filter_action_ids)` dans `R/service_export.R` :
   - Quarto vérifié **une fois avant la boucle** (échec net, pas N erreurs) ;
   - une UGF est en échec si le rendu **renvoie NULL ou lève une erreur** (les deux testés) ;
   - zéro UGF à exporter, ou toutes en échec : erreur explicite (toast + marqueur texte existant),
     jamais un ZIP réduit à `erreurs.txt` ;
   - noms : `.project_export_slug` aplatit les accents (« Épicéa » = « Èpicéa ») → suffixe
     `ug_id` ou `make.unique`, repli sur `ug_id` si libellé vide ; entrées du ZIP à plat (`-j`,
     comme `service_marculus.R:1269`) ;
   - **plafond** de 30 UGF (refus au-delà, message i18n) ; **mesurer** la durée d'un rendu d'une
     UGF et la reporter dans la PR ; notification de progression « i / N » (vérifier qu'elle passe
     pendant un `content` synchrone ; sinon le dire). Si la mesure donne plus de ~2 min au
     plafond, proposer à l'orchestrateur le modèle « préparation en tâche de fond + lien masqué »
     (Marculus, `R/mod_action_plan.R:~2440`), sans le prendre d'office ;
   - 1 UGF → un PDF (`output_file` en `.pdf`) ;
   - plusieurs → un PDF par UGF (nom `<slug>_fiche_<label UGF assaini>.pdf`, sans collision) dans
     un dossier temporaire, puis un ZIP (`output_file`) ; une UGF qui échoue est **ignorée et
     listée** (le ZIP contient les autres + un `erreurs.txt`), jamais un ZIP vide silencieux ;
   - réutilise `generate_action_plan_pdf(filter_ug_ids = uid)` ; pas de duplication du rendu.
2. **Gabarit** : `params: mode: "plan"` dans l'en-tête YAML + `execute_params` ; en mode fiche,
   page de titre = nom + surface de l'UGF et titre de synthèse adapté (les totaux sont déjà ceux
   du sous-ensemble). Section « Sources » par action (dans les deux modes) **seulement si
   `extrait_texte` non vide** ; `origine` est un code (`synthesis`, `family_C`, cf.
   `R/llm_prompts.R:372`) : le traduire via les libellés existants (`nemeton::indicator_families()`
   / i18n), sinon afficher le code brut, sans libellé inventé. En-tête YAML (`title`, `subtitle`,
   `fancyhead`, `lang`) traduit selon la langue.
3. **UI** : un bouton « Fiches PDF par UGF » (`downloadButton`, `btn-outline-success` comme le
   PDF existant, même toast `nemetonShowDownloadToast` / `nemetonHideDownloadToast`), qui exporte
   l'UGF sélectionnée si une seule l'est, les UGF sélectionnées si plusieurs le sont, sinon
   toutes les UGF ayant des actions visibles (voir la décision ci-dessous). Nom de
   fichier `.pdf` ou `.zip` selon le cas (le `filename` du `downloadHandler` doit le savoir).
   Copie archivée dans `exports/` sous `<slug>_fiche_<ugf>.pdf` / `<slug>_fiches_ugf.zip` :
   ajouter un paramètre de nom à `.archive_action_plan_pdf` pour **ne jamais écraser**
   `<slug>_action_plan.pdf` (test de non-écrasement ajouté à `test-mod_action_plan.R:43-107`).
   Cible commune : helper pur `.action_plan_sheet_targets(selected, visible_df)` (UGF + mode
   pdf/zip), appelé **à la fois** par `filename` et `content`, testé directement.
   **Contenu du ZIP (décision de Pascal, 2026-11-14)** : plusieurs UGF sélectionnées → le ZIP
   ne contient **que les UGF sélectionnées** ; aucune UGF sélectionnée → toutes les UGF ayant
   des actions visibles ; une seule sélectionnée → un PDF. Une UGF sélectionnée sans action
   visible est ignorée (et si toutes le sont, erreur explicite, pas de ZIP vide).
4. **i18n** : nouvelles clés FR/EN (NMT, ≤ 30 caractères, `\uXXXX`) pour le bouton, le toast et
   les erreurs ; textes du gabarit via son mécanisme `tr()` existant.
5. **Tests** (hors Quarto quand c'est possible) : choix PDF / ZIP selon le nombre d'UGF ; noms de
   fichiers uniques et assainis ; une UGF en échec n'empêche pas les autres et figure dans
   `erreurs.txt` ; les données passées au gabarit en mode fiche portent les totaux de l'UGF ;
   section Sources présente quand une action a une source. Mocker `generate_action_plan_pdf`
   pour les tests du ZIP. Un test de rendu réel est permis s'il saute proprement sans Quarto
   (`skip_if_not(quarto dispo)`). Au moins 3 mutations rouges.
6. Rappel du piège connu : le contenu d'un `downloadHandler` n'est pas invocable en
   `testServer` → extraire la logique dans le service et la tester en direct.
7. `grep -nP "[^\x00-\x7F]"` vide sur chaque fichier R touché.

## Hors périmètre

- Mise en page fine (maquette v1 par défaut, ajustable après retour de Pascal).
- « Totaux quantité » de #20 (volume, surface) : seuls coût / revenu / bilan existent ; à
  reprendre dans une issue dédiée si Pascal le veut (le dire en fermant #20).
- Export DB / persistance du plan.

## Versionnage

Cycle dev `2026.10.18.9016` → `2026.10.18.9017`, entrée NEWS « Non publié », section nouveautés.
Pas de release. Fermer l'issue #20 à la fusion (`Closes #20` dans la PR).

## Tests

Machine : premier plan, `timeout 540`, filtres ciblés (`action_plan|service_export`) ; rendre
pass / fail / skip.

## Dépendances et terrain

- Aucun rouge attendu. Les tests de rendu sautent en CI (pas de Quarto) : seuls le constructeur
  et le helper de cible protègent en CI.
- Terrain (Pascal) : un projet réel avec plan d'actions — 1 UGF → `.pdf` ; 3 UGF → `.zip` ;
  un libellé accentué ; `exports/<slug>_action_plan.pdf` non écrasé.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`.
- Relecteur sur la fiche avant dev, puis sur la PR.
- Temps de la main : *appuyer*.

## Redite par l'agent : _à compléter_

## Décision de Pascal sur le rendu (2026-11-14)

Durée mesurée par le dev : 39 s pour 1 UGF, 321 s pour 3 UGF (~107 s/UGF), soit 20 à 50 min au
plafond de 30. Pascal retient **A — préparation en tâche de fond + lien masqué** (modèle Marculus,
`R/mod_action_plan.R` env. 2440 ; lien `d-none` cliqué par le serveur avec
`suspendWhenHidden = FALSE`) : l'appli reste utilisable, la progression « i / N » est réelle,
le téléchargement part à la fin. Plafond de 30 UGF conservé. Complément sur la même branche.

## Statut

Terminée le 2026-11-14 : PR #277 fusionnée sur `main` (`ca89adf9`, commits `2696c9ac`, `f7b7a257`, `6a1a02f1`, `820d5d9b`), cycle dev `2026.10.18.9017`, non publié. Issue #20 fermée (totaux quantité signalés hors périmètre). Relecture : prêt. Reste : essai terrain de Pascal (Quarto dans un worker `future`).

Publié dans v2026.11.1 (2026-11-15).

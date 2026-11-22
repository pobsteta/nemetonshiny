# Mission 043 — reGénération : un seul moteur vivant par projet

> Créée par l'orchestrateur le 2026-11-22 (Pascal : « code le brief »). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-22-regen-moteurs-orphelins.md` — **le lire en entier**
> (constat Colombière : 4 moteurs sur le même projet, jobs CDS orphelins).

## Mission

**En une phrase :** un nouveau lancement, l'arrêt du pipeline ou la fin de session arrêtent le
moteur reGénération déjà actif sur le projet, grâce au marqueur de run du cœur.

## État du cœur (2026-11-22)

- nemeton PR #674 fusionnée (`run_memory_capped(marker_path=)`, `capped_run_alive()`,
  `stop_memory_capped()`, `supervise = TRUE`, purge ERA5) mais **pas encore taguée** : dernier tag
  `v2026.11.37`. Donc **garde de capacité partout** (pas de plancher `2026.11.38`, point 6 du brief) :
  `"marker_path" %in% names(formals(rmc))` et
  `exists("capped_run_alive", envir = asNamespace("nemeton"))` (idem `stop_memory_capped`). Sans
  ces fonctions : comportement actuel, sans erreur (point 5).
- Les tests **mockent** ces fonctions (`local_mocked_bindings(..., .package = "nemeton")` n'est pas
  possible pour une fonction absente : passer par un petit wrapper app, ex.
  `.regen_marker_alive(marker)` / `.regen_marker_stop(marker)` qui résolvent la fonction cœur via
  `getExportedValue` si elle existe, et mocker ces wrappers). Ne **pas** installer nemeton depuis
  `main`. Interdit : écrire dans `/home/pascal/dev/nemeton`.

## Travail attendu

Les points 1 à 5 du brief « Côté app », avec les lieux qu'il cite (à vérifier sur `main`,
`R/mod_regeneration.R` ~181, ~1249, ~1804 ; `R/service_pipeline.R:241` ; `R/mod_pipeline.R:232` ;
`R/app_server.R:518`) :

1. `marker_path = <projet>/cache/regeneration/engine_run.json` passé à `run_memory_capped()` sous
   garde, comme `log_path`.
2. Avant tout lancement (bouton et étape pipeline), **avant** l'`unlink()` d'`engine_status.json` :
   moteur vivant → arrêt + notification `regen_moteur_precedent_arrete` (FR/EN, `\uXXXX`,
   ≤ 30 car. pour la clé) ; puis lancement.
3. Arrêt du pipeline (`input$cancel`, projet changé) pendant l'étape `regeneration` → arrêt du
   moteur ; l'échec de tâche qui suit ne produit **pas** de toast d'échec (arrêt voulu : drapeau
   explicite, testable).
4. `session$onSessionEnded` : arrêt du moteur lancé par **cette** session (marqueur mémorisé au
   lancement, effacé en fin de tâche), best-effort, hors contexte réactif (pas de lecture de
   `reactiveValues` sans `isolate`).
5. Logique de décision dans des helpers purs testables (règle : pas de logique dans les modules,
   `ExtendedTask` ne tourne pas sous `testServer`).

Hors périmètre : FORDEAD et `.compute_run_capped` (optionnel du brief), plancher `2026.11.38`.

## Tests

Ceux du brief (marqueur vivant → arrêt avant `unlink` + notification + lancement ; absent/périmé →
rien ; cancel pendant `regeneration` → arrêt, pas de toast ; `onSessionEnded` avec/sans marqueur ;
cœur sans `marker_path` → argument non passé). Mutations rouges (retirer l'arrêt ; inverser l'ordre
arrêt/`unlink`). Fichiers `test-mod_regeneration*.R`, `test-service_pipeline.R`,
`test-mod_pipeline*.R`, `test-utils_i18n.R`, `timeout 540`, fichier par fichier.
`grep -nP "[^\x00-\x7F]" R/*.R` sans ajout.

## Versionnage

`2026.11.3.9001`, entrée NEWS (`fix`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/main`, branche `claude/043-regen-moteur-unique`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal), avec nemeton 2026.11.38 installé : la vérification réelle du brief (un seul
  scope `nemeton-run_regeneration_engine-*` après rechargement + relance ; aucun après redémarrage
  du service).

## Statut

Terminée le 2026-11-22 : PR #316 fusionnée (`19465988`, `.9001`). Relecture : un défaut (arrêt voulu armé sans arrêt réel) corrigé en `c3e1732b`. Reste l essai terrain avec nemeton 2026.11.38.

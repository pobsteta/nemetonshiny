# Listes de contrôle

> Issues des bilans du 2026-11-13 (journal, décision 4) et du 2026-11-15. À recopier dans les fiches concernées.

## Test discriminant (pour le testeur, à recopier dans chaque fiche de test)

- [ ] Code et classe d'erreur exacts, par JSON (pas de sous-chaîne).
- [ ] Chaque test est prouvé par une mutation qui le fait échouer ; la mutation est citée dans le rendu.
- [ ] Aucun test vacant : l'expectation est réellement exécutée (pas avalée par un `tryCatch`), l'`observeEvent` est déclenché.
- [ ] Décompte pass/fail/skip dans le rendu.

## Fiche prête pour le dev (pour le relecteur, avant le dev)

- [ ] Rubrique « Décisions de Pascal applicables » remplie, avec renvoi aux lignes du journal.
- [ ] Aucune contradiction avec ces décisions.
- [ ] Numéros de ligne et chemins cités vérifiés.
- [ ] Critère d'arrêt écrit.
- [ ] PR ouverte seulement après le « prêt » du relecteur.
- [ ] Worktree contenant `pilotage/`.
- [ ] Rubrique « Lecteurs et appelants » remplie : lecteurs et appelants recensés par `grep` (fichier:ligne), repli prévu, limite du périmètre (bilan du 2026-11-15).

## Numérotation de version (bilan du 2026-11-15)

- [ ] Le `dev` écrit son entrée NEWS **sans numéro** de cycle et ne touche pas `Version:` de `DESCRIPTION`.
- [ ] L'orchestrateur numérote (`.900x` suivant) au moment de la fusion, après avoir rebasé la branche sur `main`.

## Écarts au journal

- [ ] Tout écart va en ligne typée `écart` (attendu / vu / vérifié / décision), jamais en prose dans une ligne « fait » ou « fusion ».

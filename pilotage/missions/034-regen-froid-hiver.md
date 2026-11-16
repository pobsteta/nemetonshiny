# Mission 034 — reGénération : le froid d'hiver écarte les essences gélives (cœur nemeton 2026.11.32)

> Créée par l'orchestrateur le 2026-11-16 sur ordre de Pascal (« tous les briefs ont été codés ? »).
> Solde la partie app du brief `~/dev/briefs/traites/2026-11-16-traite-2026-11-16-s1-s2-couche-vide-livre.md`
> (section « Autre nouveauté de 2026.11.32 ») et le constat de
> `~/dev/briefs/traites/2026-11-16-traite-2026-11-15-reponse-nemetonmcp-regen-essences.md` — **les lire**.
> Doc du cœur : `gh api "repos/pobsteta/nemeton/contents/R/regen_rank_species.R?ref=v2026.11.33" --jq .content | base64 -d`
> et le fichier qui définit `regen_tmin_decennale()` (le trouver avec
> `gh api "repos/pobsteta/nemeton/git/trees/v2026.11.33?recursive=1"`), lecture seule.

## Mission

**En une phrase :** l'analyse reGénération calcule une fois, en tâche de fond et avec cache, le
minimum décennal `tmin_decennale` des UGF par `nemeton::regen_tmin_decennale()`, le pose sur les
unités avant `regen_rank_species()`, et l'écran dit quelles essences sont écartées pour le froid.

## Constat (orchestrateur, lu sur `main` le 2026-11-16)

- `R/service_regeneration.R:476-497` `regeneration_species_ranking(units, top_n, region, lai_max)` :
  appelle `regen_rank_species()` sans `tmin_decennale` → à Couchey, Pin d'Alep, Pin brutia, Pin des
  Canaries en tête.
- `R/mod_regeneration.R:~2325` `species_ranking` reactive : calcul synchrone à partir de `rv$result`.
  `regen_tmin_decennale()` coûte **~15 min au premier passage** (fichier E-OBS TN du KNMI, mis en
  cache machine par le cœur) : **jamais dans un reactive ni un observeEvent synchrone** (mémoire du
  projet : travail lourd → tâche de fond, motif `ExtendedTask` / processus enfant déjà utilisé par
  l'analyse reGénération, ex. R7 gel `R/service_regeneration.R:1154-1228`).
- Cœur : une UGF où tout est exclu rend une ligne `rank = NA`, `limiting_factor = "froid_hiver"` ;
  paires exclues dans `attr(, "exclus_froid")`. Sans `tmin_decennale`, rien ne change.

## Travail attendu

1. Calcul de `tmin_decennale` (°C, une valeur par UGF) dans la chaîne de l'analyse reGénération
   (là où tournent déjà les autres étapes longues, en tâche de fond), mis en cache dans
   `cache/regeneration/` avec une clé qui dépend des UGF (motif des caches existants). Échec ou
   réseau absent → colonne absente, avertissement `cli`, le classement reste celui d'aujourd'hui.
   La colonne doit survivre au résultat persisté (`rv$result`) pour que `species_ranking` la lise.
2. `regeneration_species_ranking()` : rien à passer de plus si la colonne est sur `units` (le cœur
   la lit) ; vérifier. Gérer la ligne `rank = NA` / `limiting_factor = "froid_hiver"` partout où le
   classement est affiché ou exporté (`R/mod_regeneration.R:374-411`, `R/service_export.R:3011-3091`,
   `R/llm_prompts.R:263-295`) : pas d'erreur, libellé traduit via `lf_label()`.
3. Afficher dans la fiche parcelle le froid de la station (« hiver décennal : −13,2 °C ») et le
   nombre d'essences écartées pour le froid (depuis `attr(, "exclus_froid")`), clés i18n NMT
   (`\uXXXX`, ≤ 30 caractères).
4. Plancher `Imports: nemeton (>= 2026.11.32)` (`chore(deps)`).

## Tests

Mocks `local_mocked_bindings(.package = "nemeton")` : `regen_tmin_decennale` appelé une fois puis
lu en cache ; échec → classement inchangé, pas d'erreur ; `froid_hiver` traduit fr/en ; ligne
`rank = NA` sans erreur à l'affichage et à l'export ; `exclus_froid` compté. Si le nemeton installé
est ≥ 2026.11.32 (il l'est : 2026.11.31 ou plus, vérifier `packageVersion`), un test réel de
`regen_rank_species()` avec `tmin_decennale = -13.2` qui écarte le Pin d'Alep (sinon `skip_if`).
Mutation rouge. Filtre `regeneration`, `timeout 540`.

## Versionnage

`2026.11.2.9002`, entrée NEWS (`feat`).

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/034-regen-froid-hiver`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : relancer l'analyse reGénération de Couchey (aussi demandé par le cœur
  2026.11.33, microclimf) → plus de pins méditerranéens en tête, hiver décennal ≈ −13,2 °C.

## Statut

Terminée le 2026-11-16 : PR #297 fusionnée (`3bf6ae58`), `.9002`, plancher nemeton 2026.11.32. Reprise après relecture : le calcul du froid ne grise plus les gestes légers, cache par UGF avec clé version du cœur et paramètres, garde de changement de projet testée. Reste : relancer reGénération sur Couchey (hiver décennal ≈ −13,2 °C, plus de pins méditerranéens en tête).

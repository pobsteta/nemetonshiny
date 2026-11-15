# Mission 027 — Import Marculus : CSV `FormatCsv;6` à `;8` et clés `.marsync` du terrain

> Créée par l'orchestrateur le 2026-11-15 sur ordre de Pascal (« traite-les tous en fonction de
> l'urgence […] ne demande rien »). Solde deux briefs Marculus :
> `~/dev/briefs/vers-nemetonshiny/2026-11-10-marculus-csv-format-8-lot-par-essence.md` (urgence à
> fixer, risque de nombres lus avec l'ancien sens) et
> `~/dev/briefs/vers-nemetonshiny/2026-10-24-marculus-csv-formats-6-7.md` (urgence faible, réponses
> de Pascal du 2026-10-24 en fin de brief). **Lire les deux briefs en entier avant de commencer.**

## Mission

**En une phrase :** l'import Marculus lit les formats 6, 7 et 8 (CSV et `.marsync`), range les
nouveaux réglages du terrain dans `reglages_terrain` sans jamais les réémettre, et le résumé de
martelage dit que le volume au lot compte les **parties cochées** quand le fichier est au format 8.

## Constat (orchestrateur, lu sur `main` le 2026-11-15)

- `R/service_marculus_import.R:324-325` : seuls les formats < 2 sont refusés ; les clés inconnues
  de l'en-tête sont ignorées. Les formats 6-8 sont donc déjà importés, mais leurs clés perdues.
- `:352` `partHouppier` lu au format ≥ 5 ; `:632-653` construit `reglages_terrain` (affouage,
  volume max, tarif, numéro, coefficient de forme, part de houppier), affichés par
  `.marculus_lots_bloc()` (`R/mod_action_plan.R:3550` env.).
- `.marsync` : `.marculus_ctx_completer()` (`:89`) et l'encodage de `partHouppier` (`:108-222`).
- `R/utils_i18n.R:764-765` : texte d'aide « FormatCsv;2 à 5 » (fr, en).
- `AffouageLotM3` (tige, `:384`) et `AffouageLotTotalM3` (contexte, `:620-624` →
  `volume_lots_affouage_m3`) : leur **sens change** au format 8 (brief format 8, § 2).

## Travail attendu

1. **Lecture des clés** (CSV et `.marsync`), en suivant exactement les briefs :
   - format 6 : `TarifsEssence` (`essence=TARIF:numéro|…`, champ éventuellement entre guillemets
     CSV ; numéro ignoré pour `AUCUN` et `EMERGE`) et `SteresParM3` ;
   - format 7 : `CommuneInsee`, `Ser`, `MillesimeTarifs` (toujours présentes, vides = absentes) ;
   - format 8 : `PartsLot` (`essence=tige:0|1,houppier:0|1|…`) ; `.marsync` : `partsLot` ne porte
     que les essences réglées, **une essence absente compte tige et houppier** ;
   - valeurs de `Tarif` nouvelles : `SCHAEFFER_INTERMEDIAIRE`, `SCHAEFFER_TRES_LENT`.
   Les ranger dans `reglages_terrain` (noms snake_case français : `tarifs_essence`,
   `steres_par_m3`, `commune_insee`, `ser`, `millesime_tarifs`, `parts_lot`), plus le numéro de
   format lu (`format_csv`) pour savoir quel sens portent les volumes au lot.
2. **Jamais réémis** : `communeInsee`, `tarifsEssence`, `steresParM3`, `partsLot` ne doivent pas
   apparaître dans un lot envoyé au téléphone (`R/service_marculus.R`, `marculus_context_from_action()`).
   Vérifier par un test. `ser` et `millesimeTarifs` continuent d'être émis comme aujourd'hui
   (mission 028 pour leur forme).
3. **Sens du volume au lot** : dans `.marculus_lots_bloc()`, quand `format_csv >= 8` (ou `partsLot`
   présent dans un `.marsync`), afficher sous le titre une ligne « Parties comptées au lot : Chêne
   tige + houppier, Hêtre houppier… » ; avant le format 8, la règle ancienne (houppier seul s'il
   existe, sinon la tige). Ne jamais additionner ni comparer des volumes au lot de formats
   différents sans le dire (si l'app compare quelque part, le signaler dans la PR).
4. **Contrôle croisé SER / millésime** (réponse de Pascal, question 2 du brief 6-7) : si la SER ou le
   millésime relus diffèrent de ce que l'app a émis pour l'action, l'afficher (petite mention dans le
   même bloc), sans rien corriger.
5. Texte d'aide `R/utils_i18n.R:764-765` : « FormatCsv;2 à 8 » (fr et en). Nouvelles clés i18n en
   convention NMT, accents en `\uXXXX`.

## Hors périmètre

- Proposer des cases tige / houppier depuis l'app (non décidé par Pascal).
- La forme du millésime émis et `tarifsProposes` : mission 028.

## Versionnage

Cycle dev `2026.11.1.9000` → `.9001`, entrée NEWS « Non publié ».

## Tests

Fixtures CSV formats 6, 7, 8 (reprendre les extraits des briefs, dont un `TarifsEssence` entre
guillemets avec `;`), un `.marsync` avec `partsLot` partiel ; format 2-5 inchangé (non-régression) ;
test « clés jamais réémises » ; rendu du bloc lots (format 7 vs 8). Une mutation rouge (ex. réémettre
`steresParM3`). Machine : premier plan, `timeout 540`, filtre `marculus`.

## Lecteurs et appelants

`reglages_terrain` est lu par `R/mod_action_plan.R` (bloc lots), par l'export du plan (chercher
`reglages_terrain` dans `R/service_export.R`, `inst/quarto/`) et peut-être par nemetonmcp
(`/home/pascal/dev/nemetonmcp`, lecture seule) : ajouter des clés ne doit casser aucun lecteur.

## Moyens

- Homme : `dev` (sonnet), worktree depuis `main`, branche `claude/027-marculus-csv-6-8`.
- Relecteur sur la PR. Temps de la main : *appuyer*.

## Statut

Confiée le 2026-11-15.

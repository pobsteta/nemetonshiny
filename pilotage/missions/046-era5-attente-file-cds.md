# Mission 046 — ERA5 : afficher l'attente dans la file du CDS

> Créée par l'orchestrateur le 2026-11-23 (Pascal : « oui », point 3 du brief). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-23-era5-textes-et-attente-cds.md` — **le lire en entier**
> (points 1 et 2 déjà livrés par la mission 045, PR #320).

## Mission

**En une phrase :** pendant l'attente ERA5 au CDS, la progression de reGénération affiche
« ERA5 mois 3/24 — en file CDS depuis 3 h 40 » (ou « — en cours au CDS ») au lieu d'un
« mois 3/24 » figé pendant des heures.

## État du cœur

- nemeton **v2026.11.40 taguée** (2026-11-23). Installer la release depuis GitHub :
  `Rscript -e 'pak::pkg_install("pobsteta/nemeton@v2026.11.40")'` — **ne jamais toucher
  `/home/pascal/dev/nemeton`** (lecture seule permise). Vérifier dans le code installé que
  l'événement `regen_expo:era5_attente` (et `biljou:era5_attente`) existe bien, avec ses champs
  `mois_i`, `mois_n`, `attente_file_s`, `etat_cds` (`"accepted"` / `"running"`), avant de coder.
  Si l'API diffère du brief, s'arrêter et le dire.

## Travail attendu

1. `R/service_regeneration.R`, callback `on_prog` (~786 au relevé du brief, à retrouver) : traiter
   `regen_expo:era5_attente` et `biljou:era5_attente` → phase `era5_lot` avec `mois_i`, `mois_n`,
   `attente_file_s`, `etat_cds` écrits dans `engine_status.json`. `regen_expo:era5_mois` inchangé.
   **Aucun ntfy** pour cet événement (il revient toutes les 5 min).
2. Libellé de progression : nouvelle(s) clé(s) i18n FR/EN (convention NMT, ≤ 30 caractères,
   `\uXXXX`), durée formatée en « 3 h 40 » / « 3 h 40 min » côté EN à ta convenance cohérente
   avec l'existant, variante `accepted` (en file depuis …) et `running` (en cours au CDS).
   Formatage dans un helper pur testable.
3. `DESCRIPTION` : `nemeton (>= 2026.11.40)` (section « Dépendances » de NEWS).

## Tests

Ceux du brief : un événement `regen_expo:era5_attente` simulé avec `attente_file_s = 13200`,
`etat_cds = "accepted"` écrit les champs dans `engine_status.json` et le libellé rendu contient
« 3 h 40 » ; même test avec `biljou:era5_attente` ; variante `running`. Vérifier qu'aucun ntfy
n'est émis (mock). Mutations rouges (retirer une des deux clés dans `on_prog` ; formater sans
les minutes). Fichiers ciblés `timeout 540`, un par un. `grep -nP "[^\x00-\x7F]" R/*.R` sans ajout.

## Versionnage

`2026.11.4.9002`, entrée NEWS (`feat`, nouveau libellé), section « Dépendances ».

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/claude/cycle-2026.11x`, branche
  `claude/046-era5-attente-file-cds`.
- Relecteur sur la PR. Temps de la main : *appuyer*.
- Terrain (Pascal) : « Tout calculer » sur un projet sans cache ERA5 — le libellé affiche
  l'attente en file et la voit croître, puis passe au mois suivant.

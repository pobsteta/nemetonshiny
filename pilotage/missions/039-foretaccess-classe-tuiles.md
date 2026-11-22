# Mission 039 — foretaccess 2.6.3 : plancher et alerte reconnue par sa classe

> Créée par l'orchestrateur le 2026-11-21 (Pascal : « oui »). Solde
> `~/dev/briefs/vers-nemetonshiny/2026-11-21-reponse-foretaccess-2.6.3-cache-incomplet.md` — **le
> lire**. foretaccess v2.6.3 publiée et installée localement.

## Mission

**En une phrase :** l'app exige foretaccess 2.6.3 et reconnaît l'alerte « tuiles perdues » par sa
classe `foretaccess_tuiles_perdues`, plus par son texte.

## Constat (orchestrateur, `main` au 2026-11-21, `441d03bb`)

- `DESCRIPTION:15` : `foretaccess (>= 2.6.2)`.
- `R/service_desserte.R:123-136` `.capturer_alertes_tuiles()` : handler `warning` qui filtre sur
  le texte (« tuile » + « WFS »). Roxygen au-dessus (l. 108-122) qui décrit ce filtrage et la 2.6.2.
- Tests : `tests/testthat/test-service_tuiles_en_echec.R` et
  `test-service_tuiles_en_echec_services.R` simulent l'avertissement par du texte (`warning(...)` /
  `cli_warn` sans classe).
- foretaccess 2.6.3 : plus de cache pour une couche incomplète (l'alerte revient à chaque run),
  fausse alarme corrigée, classe `foretaccess_tuiles_perdues` en plus de `rlang_warning`/`warning`,
  texte inchangé.

## Travail attendu

1. `DESCRIPTION` : `foretaccess (>= 2.6.3)`.
2. `.capturer_alertes_tuiles()` : handler sur la classe `foretaccess_tuiles_perdues` (forme du
   brief), plus d'appariement sur le texte. Les autres avertissements passent inchangés.
   Roxygen mis à jour (2.6.3, classe, plus de cache incomplet).
3. Tests : simuler l'avertissement avec sa classe (`rlang::warn(..., class =
   "foretaccess_tuiles_perdues")` ou `warningCondition(class = ...)`). Ajouter : un avertissement
   au **même texte mais sans la classe** n'est pas capturé (il passe) ; un avertissement classé avec
   un autre texte est capturé.
4. Mentionner dans NEWS le changement côté foretaccess utile à l'utilisateur : l'alerte revient à
   chaque run tant que l'IGN ne rend pas la couche entière (plus de cache amputé), plus de fausse
   alarme.

## Tests

Fichiers `test-service_tuiles_en_echec*.R` verts, `timeout 540`, fichier par fichier. Mutation
rouge : remettre le filtre sur le texte → le test « classé, autre texte » échoue ; retirer le
`muffle` → échec. `grep -nP "[^\x00-\x7F]" R/*.R` sans ajout.

## Versionnage

`2026.11.2.9009`, entrée NEWS (`fix`), section « Dépendances ».

## Moyens

- Homme : `dev` (sonnet), worktree depuis `origin/main`, branche `claude/039-foretaccess-classe`.
- Relecteur sur la PR. Temps de la main : *appuyer*.

## Statut

Terminée le 2026-11-21 : PR #310 fusionnée (`1e5956fb`, `.9009`, plancher `foretaccess (>= 2.6.3)`).

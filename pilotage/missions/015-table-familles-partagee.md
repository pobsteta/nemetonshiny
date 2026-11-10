# Mission 015 — Table des familles partagée (écran, API, MCP)

> Créée par l'orchestrateur le 2026-11-10 sur décision de Pascal (relecture de 008). **Au bac à sable**, à staffer après 010.

## Mission

**En une phrase :** Extraire dans `R/service_synthesis.R` un helper unique `project_family_table(project, family_sf, language)` qui produit la table des 12 familles (score à 2 décimales, NDP, nombre d'indicateurs présents ou prévus, nom), utilisé par l'onglet Synthèse, par `project_synthesis_summary` (donc `projet_lire()$synthese`) et par `mcp_resume_projet`, pour que les trois disent la même chose.

## Constat (relecture 008, 2026-11-10)

- `project_synthesis_summary` arrondit à 1 décimale (53,3) alors que l'écran et le MCP rendent 53,33 ; `test-service_synthesis.R:122` fige la 1 décimale.
- Le comptage `present_count` existe deux fois (`R/mod_synthesis.R:557`, `R/service_mcp.R:~217`) et se replie différemment quand `indicators_sf` n'est pas un sf.

## L'Homme

- **Agent :** `dev` (sonnet, main : appuyer) — à confirmer par Pascal au staffing.

## Tests

- Écart 0 entre écran, `projet_lire()$synthese` et `resume_projet` sur Dabo, Couchey, Marsannay ; le test qui fige la 1re décimale est corrigé (annoncé) ; aucune logique métier ajoutée.

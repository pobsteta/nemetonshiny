# Action Plan Module for nemetonApp (Lots 1-2 - skeleton + interactive map/table)

Renders the "Plan d'actions" tab. The current implementation covers:

\* S1: persistence + audit + validation (\`R/service_action_plan.R\`) \*
S2: LLM extraction + parser (\`R/llm_prompts.R\`) \* S3: navbar entry +
module skeleton + i18n \* S4: Leaflet map of UGFs colored by year / type
/ priority + popups \* S5: DT table with multi-select + inline edition
for the editable columns (annee, type, intensite, priorite, statut,
commentaire, quantites) \* S6: bidirectional synchronisation: clicking a
UGF on the map filters the table to that UGF, selecting rows in the
table highlights / zooms the corresponding UGFs on the map. Filters
apply to both.

Subsequent stories (S7 IA, S8 Kanban view, S10/11 ponts terrain, S12/13
exports, S15 permissions) will land in following lots.

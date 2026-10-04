# IFN production by sylvoecoregion (spec 054) - application services

Wiring of the core's opt-in IFN production modes (\`nemeton \>=
0.204.0\`):

\* \`ensure_ugf_ser()\` - the sylvoecoregion (SER) code of each UGF,
which every IFN mode needs. Without it the core falls back on the
national figure - honestly flagged, but of no use. \*
\`build_production_ifn_summary()\` / \`read_production_ifn_summary()\` -
the production of the whole massif
(\`nemeton::ifn_production_domaines()\`) and the harvest / production
ratio of each SER (\`nemeton::ifn_taux_prelevement_production()\`),
computed in the compute worker and persisted next to the parcels.

No figure is computed here: every number comes from \`nemeton\`.

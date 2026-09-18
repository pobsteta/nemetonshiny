# Optional Theia sources configuration module

The two \*\*opt-in Theia sources\*\* of the application, grouped in
their own tab of the settings (gear) modal:

\* \*\*Coupes rases (SUFOSAT)\*\* - national Sentinel-1 clear-cut
detection feeding the T3 indicator (spec 030): toggle + \`window_years\`
/ \`min_proba\`. \* \*\*Rafraichissement urbain (LST)\*\* -
Theia/Thermocity surface coolness feeding the A5 indicator (spec 032):
toggle + \`buffer_m\`.

Both blocks used to live in the project card (\`mod_project\`), where
they stretched an already long form and were easy to miss. They belong
with the other external-service settings, next to the Theia credentials
they depend on - hence this module, mounted as a tab of
\`mod_theia_config\`'s modal.

The tab then grew four \*\*calibration\*\* blocks that are not sources
at all - Suivi sanitaire (FAST thresholds + FORDEAD anomaly threshold),
Accessibilite (buffer), Desserte (buffer, skidding, max slope, slope
pricing) and reGeneration (phenology, expert overrides, forcing,
resolution). They share one property: they are set once per massif and
then left alone, whereas the sidebars they came from are where one
varies a run. Each persists on the project metadata and each block owns
its save button.

Both sources are \*\*enabled by default\*\* (see
\`project_sufosat_enabled()\` / \`project_lst_enabled()\`): a project
that never visited this tab still gets T3 and A5. The Theia fetch stays
gated on credentials being configured, and a failed / out-of-coverage
fetch degrades to \`NA\` per unit - never an error.

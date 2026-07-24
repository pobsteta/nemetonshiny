# Enrich the radar indicator sf with a live R5 dépérissement column

Best-effort: returns \`base_sf\` unchanged when there is no bound
monitoring zone, no reachable DB, or no dieback alert. The added column
is named \`indicateur_r5_deperissement\` (raw, "high = dieback") so
\`nemeton::create_family_index()\` detects it via the core
\`INDICATOR_FAMILIES\$R\` config and inverts it for the radar.

## Usage

``` r
add_r5_to_indicators(base_sf, project)
```

## Arguments

- base_sf:

  The per-UGF indicator \`sf\` (\`project\$indicators_sf\`).

- project:

  The current project (reads \`metadata\$monitoring_zone_id\`).

## Value

\`base_sf\`, possibly with an added \`indicateur_r5_deperissement\`
column (NA on UGFs with no intersecting alert / no routed method).

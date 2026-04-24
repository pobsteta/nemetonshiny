# Field Ingest Module for nemetonApp

QField return path (E5.b). Takes a GeoPackage produced by a QField field
session, validates it against the placette / arbre schemas, aggregates
per-plot dendrometric metrics and attaches them to the project's forest
units. Persisting the field data bumps the NDP level (0 -\> 2 with
placettes only, 0 -\> 3 once trees are captured at a sufficient density)
via
[`nemeton::detect_ndp()`](https://pobsteta.github.io/nemeton/reference/detect_ndp.html).

This MVP persists the validated field data and the new NDP level on the
project, but does *not* rerun `compute_all_indicators`. The user
triggers a recompute from the Home tab afterwards; the indicators that
consume field aggregates (P1, P2, B2, C1, R2) will then pick them up
through the project layers.

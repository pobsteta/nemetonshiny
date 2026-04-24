# nemetonshiny: Shiny front-end for the Nemeton systemic forest analysis platform

The nemetonshiny package provides the golem-based Shiny application that
consumes the business logic of the nemeton core package. It covers
project management, indicator radar visualisation, per-family
dashboards, QField field workflow (export + ingest), OAuth2
authentication and LLM-powered expert profiles.

This package contains **no business logic**: every indicator
calculation, every NDP / CV / sample-size helper lives in nemeton.
Dependency direction is strict: nemetonshiny imports nemeton, never the
other way around (ADR-009).

## Launching the app

    library(nemetonshiny)
    run_app()

## Main tabs

- **Selection** - cadastral search, parcel picking, project creation and
  compute trigger.

- **Synthesis** - global score, radar, augmented NDP badges, AI-driven
  perspectives per actor profile.

- **Terrain** - two sub-tabs:

  - *Export terrain*: design a sampling plan from a target error + CV
    (derived from BD Forêt v2 or manual) and export it as a QField
    `.qgz` project.

  - *Import terrain*: ingest a GeoPackage returned by QField, validate
    it, attach aggregates to the project and bump the NDP.

- **Indicator families** - one panel per family with a table, a map and
  expert commentary.

## Internationalisation

Language is handled via an in-house translator in
[`utils_i18n`](https://pobsteta.github.io/nemetonshiny/reference/utils_i18n.md).
Currently 358+ FR / EN keys.

## Links

- GitHub: <https://github.com/pobsteta/nemetonshiny>

- Core package: <https://github.com/pobsteta/nemeton>

## Author

**Pascal Obstétar** (<pascal.obstetar@gmail.com>)

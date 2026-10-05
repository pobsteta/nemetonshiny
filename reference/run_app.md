# Launch the nemetonApp Shiny Application

Launches the interactive nemetonApp for parcel selection and forest
indicator analysis. The application allows users to:

- Search and select cadastral parcels on an interactive map

- Create projects with metadata

- Calculate the nemeton indicators (12 families) automatically

- Analyze results by indicator family (12 tabs)

- Export PDF reports and GeoPackage data

## Usage

``` r
run_app(
  language = NULL,
  project_dir = NULL,
  max_parcels = 30L,
  tour = TRUE,
  options = list(),
  ...
)
```

## Arguments

- language:

  Character. Interface language: "fr" (French, default) or "en"
  (English). If NULL, the system language is auto-detected.

- project_dir:

  Character. Directory for storing projects. Default: the
  `NEMETON_PROJECT_DIR` environment variable when set, otherwise the
  user data directory (`rappdirs::user_data_dir("nemeton")/projects`,
  i.e. `~/.local/share/nemeton/projects` on Linux), or
  `~/.nemeton/projects` when rappdirs is not installed.

- max_parcels:

  Integer. Maximum number of parcels that can be selected simultaneously
  on the map. Default: `30`. Must be a positive integer.

- tour:

  Logical. Auto-start the guided tour (cicerone) for a visitor who has
  never seen it. Default: `TRUE`. Set to `FALSE` to boot straight into
  the app - useful for demos, screencasts and automated tests, where the
  tour's client-side JS runs 2 s after connection and interferes.
  Disabling only suppresses the AUTO-start: the tour stays available
  from the help menu. Overridable per-session with the `NEMETON_TOUR`
  environment variable (`0`/`false` to disable).

- options:

  List of Shiny app options (`port`, `host`, `launch.browser`...),
  merged over the defaults: the browser opens only in an interactive
  session. Example:
  `run_app(options = list(port = 3838, host = "0.0.0.0"))`.

- ...:

  Additional arguments passed to
  [`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

## Value

A Shiny application object (invisibly).

## Details

The application requires an internet connection for:

- Cadastral parcel data (API Cadastre / happign fallback

- External data layers (INPN, IGN BD Foret, Corine Land Cover)

Projects are saved locally in GeoParquet format for efficient caching.

## Accessibility

The application follows WCAG 2.1 AA guidelines:

- Color contrast ratio \>= 4.5:1

- Keyboard navigation support

- Colorblind-friendly viridis palettes

- Touch targets \>= 44px

## Examples

``` r
if (interactive()) {
  # Launch with default settings (French)
  run_app()

  # Launch in English
  run_app(language = "en")

  # Custom project directory
  run_app(project_dir = "~/my_nemeton_projects")

  # Raise the selectable parcels limit to 50
  run_app(max_parcels = 50)

  # Boot without the guided tour (demo, screencast, automated test)
  run_app(tour = FALSE)

  # Serve on a fixed port, reachable from other machines (server, Docker)
  run_app(options = list(port = 3838, host = "0.0.0.0"))
}
```

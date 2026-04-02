#' Launch the nemetonApp Shiny Application
#'
#' @description
#' Launches the interactive nemetonApp for parcel selection and forest indicator

#' analysis. The application allows users to:
#' \itemize{
#'   \item Search and select cadastral parcels on an interactive map
#'   \item Create projects with metadata
#'   \item Calculate all 29 nemeton indicators automatically
#'   \item Analyze results by indicator family (12 tabs)
#'   \item Export PDF reports and GeoPackage data
#' }
#'
#' @param language Character. Interface language: "fr" (French, default) or "en" (English).
#'   If NULL, the system language is auto-detected.
#' @param project_dir Character. Directory for storing projects.
#'   Default: \code{~/.nemeton/projects}
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'
#' @return A Shiny application object (invisibly).
#'
#' @details
#' The application requires an internet connection for:
#' \itemize{
#'   \item Cadastral parcel data (API Cadastre / happign fallback
#'   \item External data layers (INPN, IGN BD Forêt, Corine Land Cover)
#' }
#'
#' Projects are saved locally in GeoParquet format for efficient caching.
#'
#' @section Accessibility:
#' The application follows WCAG 2.1 AA guidelines:
#' \itemize{
#'   \item Color contrast ratio >= 4.5:1
#'   \item Keyboard navigation support
#'   \item Colorblind-friendly viridis palettes
#'   \item Touch targets >= 44px
#' }
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Launch with default settings (French)
#'   run_app()
#'
#'   # Launch in English
#'   run_app(language = "en")
#'
#'   # Custom project directory
#'   run_app(project_dir = "~/my_nemeton_projects")
#' }
run_app <- function(language = NULL,
                    project_dir = NULL,
                    ...) {
  # Check required packages

  check_app_dependencies()

 # Set options
  app_options <- list(
    language = language %||% detect_system_language(),
    project_dir = project_dir %||% get_default_project_dir()
  )

  # Ensure project directory exists
  if (!dir.exists(app_options$project_dir)) {
    dir.create(app_options$project_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_alert_success("Created project directory: {app_options$project_dir}")
  }

  # Store options + disable Shiny/bslib busy indicators (white screen overlays)
  options(
    nemeton.app_options = app_options,
    shiny.busy_indicators = FALSE
  )

  # Configure async computation (ExtendedTask uses future for separate R process)
  if (requireNamespace("future", quietly = TRUE)) {
    future::plan("multisession")
    cli::cli_alert_info("Async computation enabled (future::multisession)")
  }

  # Launch app
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    options = list(launch.browser = TRUE),
    ...
  )
}


#' Check required dependencies for nemetonApp
#'
#' @return NULL invisibly. Stops with error if dependencies missing.
#' @noRd
check_app_dependencies <- function() {
  required_pkgs <- c("shiny", "bslib", "leaflet", "sf", "jsonlite", "httr2")
  missing <- vapply(required_pkgs, function(pkg) {
    !requireNamespace(pkg, quietly = TRUE)
  }, logical(1))

  if (any(missing)) {
    cli::cli_abort(c(
      "Missing required packages for nemetonApp:",
      "i" = "Install with: install.packages(c({paste0('\"', names(missing)[missing], '\"', collapse = ', ')}))"
    ))
  }

  invisible(NULL)
}


#' Detect system language
#'
#' @return Character: "fr" or "en"
#' @noRd
detect_system_language <- function() {
  lang <- Sys.getenv("LANG", "en_US.UTF-8")
  if (grepl("^fr", lang, ignore.case = TRUE)) {
    return("fr")
  }
  return("en")
}


#' Get default project directory
#'
#' @return Character path to default project directory
#' @noRd
get_default_project_dir <- function() {
  # Use rappdirs if available, otherwise fallback to ~/.nemeton
 if (requireNamespace("rappdirs", quietly = TRUE)) {
    base_dir <- rappdirs::user_data_dir("nemeton", "nemeton")
  } else {
    base_dir <- file.path(Sys.getenv("HOME"), ".nemeton")
  }

  file.path(base_dir, "projects")
}


#' Get current app options
#'
#' @return List of app options
#' @noRd
get_app_options <- function() {
  getOption("nemeton.app_options", list(
    language = "fr",
    project_dir = get_default_project_dir()
  ))
}

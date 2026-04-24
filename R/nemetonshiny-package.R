"_PACKAGE"

#' nemetonshiny: Shiny front-end for the Nemeton systemic forest analysis platform
#'
#' @description
#' The \pkg{nemetonshiny} package provides the golem-based Shiny
#' application that consumes the business logic of the \pkg{nemeton}
#' core package. It covers project management, indicator radar
#' visualisation, per-family dashboards, QField field workflow
#' (export + ingest), OAuth2 authentication and LLM-powered expert
#' profiles.
#'
#' This package contains \strong{no business logic}: every indicator
#' calculation, every NDP / CV / sample-size helper lives in
#' \pkg{nemeton}. Dependency direction is strict: \pkg{nemetonshiny}
#' imports \pkg{nemeton}, never the other way around
#' (ADR-009).
#'
#' @section Launching the app:
#'
#' \preformatted{
#' library(nemetonshiny)
#' run_app()
#' }
#'
#' @section Main tabs:
#'
#' \itemize{
#'   \item \strong{Selection} - cadastral search, parcel picking,
#'     project creation and compute trigger.
#'   \item \strong{Synthesis} - global score, radar, augmented NDP
#'     badges, AI-driven perspectives per actor profile.
#'   \item \strong{Terrain} - two sub-tabs:
#'     \itemize{
#'       \item \emph{Export terrain}: design a sampling plan from a
#'         target error + CV (derived from BD Forêt v2 or manual)
#'         and export it as a QField \code{.qgz} project.
#'       \item \emph{Import terrain}: ingest a GeoPackage returned
#'         by QField, validate it, attach aggregates to the project
#'         and bump the NDP.
#'     }
#'   \item \strong{Indicator families} - one panel per family with a
#'     table, a map and expert commentary.
#' }
#'
#' @section Internationalisation:
#'
#' Language is handled via an in-house translator in
#' \code{\link{utils_i18n}}. Currently 358+ FR / EN keys.
#'
#' @section Links:
#'
#' \itemize{
#'   \item GitHub: \url{https://github.com/pobsteta/nemetonshiny}
#'   \item Core package: \url{https://github.com/pobsteta/nemeton}
#' }
#'
#' @section Author:
#'
#' \strong{Pascal Obstétar} (\email{pascal.obstetar@@gmail.com})
#'
#' @docType _PACKAGE
#' @name nemetonshiny-package
#' @aliases nemetonshiny
NULL

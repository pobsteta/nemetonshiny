#' Authentication Module using shinyOAuth
#'
#' @description
#' OAuth2/OIDC authentication module for nemetonApp.
#' Supports Keycloak, Google, GitHub, Microsoft, and any OIDC provider.
#' Authentication is optional: if no OAuth credentials are configured,
#' the app runs without authentication (mode anonyme).
#'
#' @name mod_auth
#' @keywords internal
NULL


#' Check if OAuth authentication is configured
#'
#' Returns TRUE if the necessary environment variables for OAuth are set.
#'
#' @return Logical. TRUE if OAuth is configured.
#'
#' @noRd
is_oauth_configured <- function() {
  provider <- Sys.getenv("NEMETON_OAUTH_PROVIDER", "")
  client_id <- Sys.getenv("NEMETON_OAUTH_CLIENT_ID", "")
  nchar(provider) > 0 && nchar(client_id) > 0
}


#' Get OAuth client configuration
#'
#' Creates a shinyOAuth client from environment variables.
#'
#' @return An \code{oauth_client} object, or NULL if not configured.
#'
#' @details
#' Environment variables:
#' \itemize{
#'   \item \code{NEMETON_OAUTH_PROVIDER}: Provider type ("keycloak", "google", "github", "microsoft", "oidc")
#'   \item \code{NEMETON_OAUTH_CLIENT_ID}: OAuth client ID
#'   \item \code{NEMETON_OAUTH_CLIENT_SECRET}: OAuth client secret (optional for PKCE)
#'   \item \code{NEMETON_OAUTH_REDIRECT_URI}: Redirect URI (default: "http://127.0.0.1:3838")
#'   \item \code{NEMETON_OAUTH_SCOPES}: Comma-separated scopes (default: "openid,profile,email")
#'   \item \code{NEMETON_KEYCLOAK_URL}: Keycloak realm URL (for Keycloak provider)
#' }
#'
#' @noRd
get_oauth_client <- function() {
  if (!requireNamespace("shinyOAuth", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg shinyOAuth} is not installed. Authentication disabled.")
    return(NULL)
  }

  if (!is_oauth_configured()) {
    return(NULL)
  }

  provider_type <- tolower(Sys.getenv("NEMETON_OAUTH_PROVIDER", ""))
  client_id <- Sys.getenv("NEMETON_OAUTH_CLIENT_ID", "")
  client_secret <- Sys.getenv("NEMETON_OAUTH_CLIENT_SECRET", "")
  redirect_uri <- Sys.getenv("NEMETON_OAUTH_REDIRECT_URI", "http://127.0.0.1:3838")
  scopes <- strsplit(Sys.getenv("NEMETON_OAUTH_SCOPES", "openid,profile,email"), ",")[[1]]
  scopes <- trimws(scopes)

  # Creer le provider selon le type
  provider <- tryCatch({
    switch(provider_type,
      "keycloak" = {
        realm_url <- Sys.getenv("NEMETON_KEYCLOAK_URL", "")
        if (nchar(realm_url) == 0) {
          cli::cli_abort("NEMETON_KEYCLOAK_URL is required for Keycloak provider")
        }
        shinyOAuth::oauth_provider_oidc_discover(issuer = realm_url)
      },
      "google" = shinyOAuth::oauth_provider_google(),
      "github" = shinyOAuth::oauth_provider_github(),
      "microsoft" = shinyOAuth::oauth_provider_microsoft(),
      "oidc" = {
        issuer <- Sys.getenv("NEMETON_OAUTH_ISSUER", "")
        if (nchar(issuer) == 0) {
          cli::cli_abort("NEMETON_OAUTH_ISSUER is required for OIDC provider")
        }
        shinyOAuth::oauth_provider_oidc_discover(issuer = issuer)
      },
      {
        cli::cli_abort("Unknown OAuth provider: {provider_type}. Use: keycloak, google, github, microsoft, oidc")
      }
    )
  }, error = function(e) {
    cli::cli_warn("Failed to create OAuth provider: {e$message}")
    return(NULL)
  })

  if (is.null(provider)) return(NULL)

  # Creer le client
  tryCatch({
    args <- list(
      provider = provider,
      client_id = client_id,
      redirect_uri = redirect_uri,
      scopes = scopes
    )
    # Client secret est optionnel (PKCE peut fonctionner sans)
    if (nchar(client_secret) > 0) {
      args$client_secret <- client_secret
    }
    do.call(shinyOAuth::oauth_client, args)
  }, error = function(e) {
    cli::cli_warn("Failed to create OAuth client: {e$message}")
    NULL
  })
}


#' Authentication module server
#'
#' Manages OAuth authentication state. If OAuth is not configured,
#' returns an always-authenticated state (mode anonyme).
#'
#' @param id Character. Module namespace ID.
#'
#' @return A reactiveValues with:
#'   \describe{
#'     \item{authenticated}{Logical. TRUE if user is authenticated.}
#'     \item{user_name}{Character. Display name of the user, or "Anonyme".}
#'     \item{user_email}{Character. Email of the user, or NULL.}
#'     \item{user_roles}{Character vector. User roles from the token.}
#'   }
#'
#' @noRd
mod_auth_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Etat d'authentification
    auth_state <- shiny::reactiveValues(
      authenticated = FALSE,
      user_name = NULL,
      user_email = NULL,
      user_roles = character(0)
    )

    client <- get_oauth_client()

    if (is.null(client)) {
      # Mode anonyme : pas de configuration OAuth
      auth_state$authenticated <- TRUE
      auth_state$user_name <- "Anonyme"
      cli::cli_alert_info("OAuth not configured. Running in anonymous mode.")
      return(auth_state)
    }

    # Mode OAuth : utiliser shinyOAuth
    auth <- shinyOAuth::oauth_module_server("oauth", client)

    # Observer le changement d'etat d'authentification
    shiny::observe({
      if (isTRUE(auth$authenticated)) {
        userinfo <- tryCatch(auth$token@userinfo, error = function(e) list())
        auth_state$authenticated <- TRUE
        auth_state$user_name <- userinfo$name %||%
                                userinfo$preferred_username %||%
                                userinfo$email %||%
                                "Utilisateur"
        auth_state$user_email <- userinfo$email
        auth_state$user_roles <- userinfo$realm_access$roles %||% character(0)
        cli::cli_alert_success("User authenticated: {auth_state$user_name}")
      } else {
        auth_state$authenticated <- FALSE
        auth_state$user_name <- NULL
        auth_state$user_email <- NULL
        auth_state$user_roles <- character(0)
      }
    })

    auth_state
  })
}


#' User info badge for navbar
#'
#' Displays the authenticated user's name in the navbar.
#'
#' @param auth_state reactiveValues from mod_auth_server.
#' @param lang Character. Language code.
#'
#' @return A Shiny UI element.
#'
#' @noRd
auth_user_badge <- function(auth_state, lang = "fr") {
  if (!isTRUE(auth_state$authenticated)) {
    return(NULL)
  }

  name <- auth_state$user_name %||% "Anonyme"

  htmltools::tags$span(
    class = "navbar-text text-light me-3",
    style = "font-size: 0.85rem;",
    shiny::icon("user", class = "me-1"),
    name
  )
}

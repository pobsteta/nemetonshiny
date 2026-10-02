# tests/testthat/test-mod_auth.R
# Tests pour le module d'authentification OAuth

test_that("is_oauth_configured returns FALSE when no env vars", {
  withr::with_envvar(c(NEMETON_OAUTH_PROVIDER = "", NEMETON_OAUTH_CLIENT_ID = ""), {
    expect_false(is_oauth_configured())
  })
})

test_that("is_oauth_configured returns FALSE when only provider set", {
  withr::with_envvar(c(NEMETON_OAUTH_PROVIDER = "keycloak", NEMETON_OAUTH_CLIENT_ID = ""), {
    expect_false(is_oauth_configured())
  })
})

test_that("is_oauth_configured returns TRUE when both vars set", {
  withr::with_envvar(c(NEMETON_OAUTH_PROVIDER = "keycloak", NEMETON_OAUTH_CLIENT_ID = "my-app"), {
    expect_true(is_oauth_configured())
  })
})

test_that("get_oauth_client returns NULL when not configured", {
  withr::with_envvar(c(NEMETON_OAUTH_PROVIDER = "", NEMETON_OAUTH_CLIENT_ID = ""), {
    expect_null(get_oauth_client())
  })
})

test_that("get_oauth_client returns NULL when shinyOAuth not installed", {
  # Mock requireNamespace to return FALSE
  withr::with_envvar(c(NEMETON_OAUTH_PROVIDER = "github", NEMETON_OAUTH_CLIENT_ID = "test"), {
    local_mocked_bindings(
      requireNamespace = function(pkg, ...) if (pkg == "shinyOAuth") FALSE else TRUE,
      .package = "base"
    )
    expect_null(get_oauth_client())
  })
})

test_that("mod_auth_server returns auth_state in anonymous mode", {
  # Sans configuration OAuth, le module doit retourner un etat authentifie anonyme
  withr::with_envvar(c(NEMETON_OAUTH_PROVIDER = "", NEMETON_OAUTH_CLIENT_ID = ""), {
    testServer(mod_auth_server, {
      expect_true(auth_state$authenticated)
      expect_equal(auth_state$user_name, "Anonyme")
      expect_null(auth_state$user_email)
    })
  })
})

test_that("mod_auth_server echoue ferme quand OAuth est configure mais indisponible", {
  # Keycloak injoignable, shinyOAuth absent, discovery en erreur : le client
  # vaut NULL. La session ne doit PAS retomber en mode anonyme editeur.
  withr::with_envvar(c(NEMETON_OAUTH_PROVIDER = "keycloak",
                       NEMETON_OAUTH_CLIENT_ID = "nemeton-app"), {
    local_mocked_bindings(get_oauth_client = function() NULL)
    suppressWarnings(testServer(mod_auth_server, {
      expect_false(isTRUE(auth_state$authenticated))
      expect_false(isTRUE(auth_state$anonymous))
      expect_false(can_edit_action_plan(auth_state))
      expect_false(can_admin_app(auth_state))
    }))
  })
})

test_that("le mode anonyme reste editeur et administrateur", {
  withr::with_envvar(c(NEMETON_OAUTH_PROVIDER = "", NEMETON_OAUTH_CLIENT_ID = "",
                       NEMETON_AUTH_DEV_ROLES = ""), {
    testServer(mod_auth_server, {
      expect_true(auth_state$anonymous)
      expect_true(can_edit_action_plan(auth_state))
      expect_true(can_admin_app(auth_state))
    })
  })
})

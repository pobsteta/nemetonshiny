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

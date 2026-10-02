# Tests for the Theia configuration module (mod_theia_config.R)
# UI sanity + save-key reactive flow via testServer().

test_that("mod_theia_config_ui returns a navbar item with the gear link", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonshiny:::mod_theia_config_ui("theia_config")
      html <- as.character(ui)
      expect_true(grepl("theia_config-open", html))
    }
  )
})


test_that("mod_theia_config_server saves a valid key", {
  skip_if_not_installed("shiny")

  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home, TLD_ACCESS_KEY = "", TLD_SECRET_KEY = ""))

  # Mode anonyme (pas d'OAuth) : poste mono-utilisateur, administrateur.
  app_state <- shiny::reactiveValues(
    language = "fr",
    auth = list(authenticated = TRUE, anonymous = TRUE, user_roles = character()))

  shiny::testServer(
    nemetonshiny:::mod_theia_config_server,
    args = list(app_state = app_state),
    {
      session$setInputs(access_key = "acc", secret_key = "sec")
      session$setInputs(save_key = 1)
      expect_identical(Sys.getenv("TLD_ACCESS_KEY"), "acc")
      expect_true(nemetonshiny:::theia_api_key_configured())
    }
  )
})


test_that("les cles du serveur sont refusees a un utilisateur OAuth non administrateur", {
  skip_if_not_installed("shiny")

  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home, TLD_ACCESS_KEY = "", TLD_SECRET_KEY = "",
                        MISTRAL_API_KEY = "garde"))

  for (roles in list(character(), "gestionnaire")) {
    app_state <- shiny::reactiveValues(
      language = "fr",
      auth = list(authenticated = TRUE, anonymous = FALSE, user_roles = roles))
    shiny::testServer(
      nemetonshiny:::mod_theia_config_server,
      args = list(app_state = app_state),
      {
        session$setInputs(access_key = "acc", secret_key = "sec")
        session$setInputs(save_key = 1)
        expect_identical(Sys.getenv("TLD_ACCESS_KEY"), "")
        expect_false(nemetonshiny:::theia_api_key_configured())
        session$setInputs(llm_provider = "mistral", llm_delete_key = 1)
        expect_identical(Sys.getenv("MISTRAL_API_KEY"), "garde")
      }
    )
  }
})


test_that("mod_theia_config_server ignores an incomplete key", {
  skip_if_not_installed("shiny")

  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home, TLD_ACCESS_KEY = "", TLD_SECRET_KEY = ""))

  app_state <- shiny::reactiveValues(language = "fr")

  shiny::testServer(
    nemetonshiny:::mod_theia_config_server,
    args = list(app_state = app_state),
    {
      session$setInputs(access_key = "acc", secret_key = "")
      session$setInputs(save_key = 1)
      expect_identical(Sys.getenv("TLD_ACCESS_KEY"), "")
    }
  )
})

# Tests du service Python : resolveur, runner isole, et la REGLE.

test_that("engine_python resout l'interpreteur d'un moteur declare", {
  skip_if_not_installed("reticulate")

  p <- nemetonshiny:::engine_python("opencanopy")
  # Sur un poste sans l'env conda, NA est la bonne reponse : un moteur absent
  # est un mode degrade (l'appelant se replie), pas une erreur.
  if (!is.na(p)) {
    expect_true(file.exists(p))
    expect_match(basename(p), "^python")
  }
})


test_that("engine_python rend NA sur un moteur inconnu, sans lever", {
  expect_true(is.na(nemetonshiny:::engine_python("moteur_inexistant")))
  expect_true(is.na(nemetonshiny:::engine_python("")))
})


test_that("l'echappatoire explicite l'emporte sur l'env conda", {
  # Un poste non standard doit pouvoir se debloquer sans toucher au code.
  withr::with_options(
    list(nemetonshiny.opencanopy_python = "/bin/sh"),
    expect_identical(nemetonshiny:::engine_python("opencanopy"), "/bin/sh")
  )
  # Un chemin qui n'existe pas est ignore, pas rendu tel quel : sinon on
  # epinglerait l'enfant sur un interpreteur fantome.
  withr::with_options(
    list(nemetonshiny.opencanopy_python = "/chemin/qui/n/existe/pas"),
    {
      p <- nemetonshiny:::engine_python("opencanopy")
      expect_false(identical(p, "/chemin/qui/n/existe/pas"))
    }
  )
})


test_that("run_with_python epingle l'interpreteur DANS l'enfant, pas ici", {
  skip_if_not_installed("callr")
  skip_if(is.na(nemetonshiny:::engine_python("opencanopy")),
          "env conda open_canopy absent sur ce poste")

  avant <- Sys.getenv("RETICULATE_PYTHON", unset = NA)

  vu <- nemetonshiny:::run_with_python(
    "opencanopy",
    function() c(py = Sys.getenv("RETICULATE_PYTHON"),
                 renv = Sys.getenv("R_ENVIRON_USER"))
  )

  # L'enfant voit l'interpreteur du moteur...
  expect_identical(unname(vu[["py"]]), nemetonshiny:::engine_python("opencanopy"))
  # ... et R_ENVIRON_USER vide : sans cela, un RETICULATE_PYTHON traine dans
  # le .Renviron de l'utilisateur ecraserait l'epinglage qu'on vient de poser,
  # et l'enfant lierait le mauvais interpreteur en annoncant le bon.
  expect_identical(unname(vu[["renv"]]), "")
  # ... tandis que CETTE session reste intacte.
  expect_identical(Sys.getenv("RETICULATE_PYTHON", unset = NA), avant)
})


test_that("run_with_python releve l'erreur de l'enfant", {
  skip_if_not_installed("callr")
  skip_if(is.na(nemetonshiny:::engine_python("opencanopy")),
          "env conda open_canopy absent sur ce poste")

  # Une erreur dans l'enfant ne doit pas se perdre dans le tuyau.
  expect_error(
    nemetonshiny:::run_with_python("opencanopy", function() stop("boum enfant")),
    "boum enfant"
  )
})


test_that("run_with_python refuse un moteur dont l'env est introuvable", {
  expect_error(
    nemetonshiny:::run_with_python("moteur_inexistant", function() TRUE),
    "not found"
  )
})


# ---- LA REGLE ---------------------------------------------------------
#
# `reticulate` lie un interpreteur UNE FOIS par processus et ne le relie
# jamais. Les exigences des moteurs se contredisent (opencanopy veut
# RETICULATE_PYTHON epinglee, FORDEAD la veut absente), donc aucun reglage
# global ne peut les servir tous : chaque moteur tourne dans son processus.
#
# Ce test gele l'etat connu. Il echoue sur toute NOUVELLE liaison en
# processus - c'est son seul but. Ajouter un fichier a la liste ci-dessous
# demande d'ecrire POURQUOI la liaison y est acceptable.

test_that("aucune nouvelle liaison reticulate en processus", {
  # Fichiers ou une liaison en processus est connue et acceptee :
  #
  #   service_rvt.R - le moteur rvt-py tourne dans un worker `future`
  #     (mod_accessibility.R, `rvt_task`), jamais dans la session Shiny : le
  #     chemin synchrone (`.rvt_is_cheap`) ne lit que le cache disque et
  #     n'importe rien. Aucun autre moteur Python ne tourne dans ce worker,
  #     donc la liaison n'entre en conflit avec personne. A revoir le jour ou
  #     un second moteur y tournerait.
  connus <- c("service_rvt.R")

  lient <- paste0(
    "reticulate::(import|py_run_string|py_run_file|source_python|",
    "py_module_available|py_config|py_discover_config|use_python|",
    "use_condaenv|use_virtualenv|py_eval)\\s*\\("
  )

  fichiers <- list.files(testthat::test_path("..", "..", "R"),
                         pattern = "\\.R$", full.names = TRUE)
  coupables <- character(0)
  for (f in fichiers) {
    src <- readLines(f, warn = FALSE)
    # Les commentaires ne lient rien - ce fichier-ci en cite plusieurs.
    src <- src[!grepl("^\\s*#", src)]
    if (any(grepl(lient, src))) coupables <- c(coupables, basename(f))
  }

  expect_setequal(coupables, connus)
})

# Tests — légende bivariée 2D (squelette, en attente du raster classe bivariée cœur).

test_that("bivariate_palette_default retourne 9 couleurs indexées 1..9", {
  pal <- nemetonshiny:::bivariate_palette_default()
  expect_length(pal, 9)
  expect_setequal(names(pal), as.character(1:9))
  expect_true(all(grepl("^#[0-9a-fA-F]{6}$", pal)))
})

test_that("bivariate_legend_html construit une grille 3×3 (9 cellules)", {
  skip_if_not_installed("htmltools")
  tag <- nemetonshiny:::bivariate_legend_html(axis_x = "Précip.", axis_y = "T°max",
                                              title = "Contexte")
  expect_true(inherits(tag, "shiny.tag"))
  html <- as.character(tag)
  # 9 cases colorées : 9 divs 20x20.
  expect_equal(lengths(regmatches(html, gregexpr("width:20px;height:20px", html)))[1], 9L)
  expect_match(html, "Contexte")
  expect_match(html, "Précip\\.")
})

test_that("bivariate_legend_html accepte une palette du cœur (meta$palette_matrix)", {
  skip_if_not_installed("htmltools")
  core_pal <- setNames(sprintf("#%06x", seq(0, 8) * 0x111111L + 0x100000L), as.character(1:9))
  html <- as.character(nemetonshiny:::bivariate_legend_html(palette = core_pal))
  # La couleur de la classe 7 (coin chaud+sec) doit être celle fournie.
  expect_match(html, core_pal[["7"]], fixed = TRUE)
})

test_that("bivariate_legend_html dessine une grille 5×5 (25 cases) via ncol", {
  skip_if_not_installed("htmltools")
  # Le cœur eobs_downscale_bivariate livre 25 classes + ncol = 5 (comme l'IF IGN).
  pal <- setNames(sprintf("#%06x", seq_len(25) * 0x0A0A0AL), as.character(1:25))
  html <- as.character(nemetonshiny:::bivariate_legend_html(palette = pal, ncol = 5))
  # 25 cases (cellules réduites à 16px au-delà de 3×3).
  expect_equal(lengths(regmatches(html, gregexpr("width:16px;height:16px", html)))[1], 25L)
  # La classe 25 (coin) doit apparaître.
  expect_match(html, pal[["25"]], fixed = TRUE)
})

test_that("bivariate_legend_html déduit N de la longueur de palette sans ncol", {
  skip_if_not_installed("htmltools")
  pal <- setNames(sprintf("#%06x", seq_len(25) * 0x0A0A0AL), as.character(1:25))
  html <- as.character(nemetonshiny:::bivariate_legend_html(palette = pal))  # ncol NULL
  expect_equal(lengths(regmatches(html, gregexpr("width:16px;height:16px", html)))[1], 25L)
})

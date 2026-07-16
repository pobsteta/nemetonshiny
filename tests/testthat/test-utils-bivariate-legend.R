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
  # 25 cases (cellules 22px au-delà de 3×3, lisibles sans ascenseur).
  expect_equal(lengths(regmatches(html, gregexpr("width:22px;height:22px", html)))[1], 25L)
  # La classe 25 (coin) doit apparaître.
  expect_match(html, pal[["25"]], fixed = TRUE)
})

test_that("bivariate_legend_html déduit N de la longueur de palette sans ncol", {
  skip_if_not_installed("htmltools")
  pal <- setNames(sprintf("#%06x", seq_len(25) * 0x0A0A0AL), as.character(1:25))
  html <- as.character(nemetonshiny:::bivariate_legend_html(palette = pal))  # ncol NULL
  expect_equal(lengths(regmatches(html, gregexpr("width:22px;height:22px", html)))[1], 25L)
})

test_that("bivariate_legend_html trace les lignes 0 (pointillé blanc) + min/max des axes", {
  skip_if_not_installed("htmltools")
  pal <- setNames(sprintf("#%06x", seq_len(25) * 0x0A0A0AL), as.character(1:25))
  html <- as.character(nemetonshiny:::bivariate_legend_html(
    palette = pal, ncol = 5,
    zero = list(tmax = 0.2, precip = 0.6),   # fractions du cœur
    x_range = c(-80, -40, 0, 40),            # seuils précip
    y_range = c(0, 0.4, 0.8, 1.2)))          # seuils T°max
  # Deux lignes 0 en pointillé blanc.
  expect_equal(lengths(regmatches(html, gregexpr("dashed #fff", html)))[1], 2L)
  # Grille 5×5 à 22px -> W=H=110px. rr=0 (vertical) à 0.6 × 110 = 66px ;
  # tx=0 (horizontal) à 0.2 × 110 = 22px.
  expect_match(html, "left:66.0px", fixed = TRUE)
  expect_match(html, "bottom:22.0px", fixed = TRUE)
  # Min/max des deux axes présents.
  expect_match(html, ">-80<"); expect_match(html, ">40<")
  expect_match(html, ">0<");   expect_match(html, ">1.2<")
})

test_that("bivariate_legend_html rend un sous-titre sous le titre", {
  skip_if_not_installed("htmltools")
  pal <- setNames(sprintf("#%06x", seq_len(25) * 0x0A0A0AL), as.character(1:25))
  html <- as.character(nemetonshiny:::bivariate_legend_html(
    palette = pal, ncol = 5,
    title = "Tendance bivariée",
    subtitle = "(T°max estivale × précipitations)"))
  expect_match(html, "Tendance bivariée", fixed = TRUE)
  expect_match(html, "(T°max estivale × précipitations)", fixed = TRUE)
  # Le sous-titre est en petite fonte régulière.
  expect_match(html, "font-size:9px;font-weight:400", fixed = TRUE)
})

test_that("bivariate_legend_html reste propre sans zero/ranges (repli)", {
  skip_if_not_installed("htmltools")
  # Sans zero ni ranges : aucune ligne 0, aucune borne — comme avant.
  html <- as.character(nemetonshiny:::bivariate_legend_html(title = "X"))
  expect_false(grepl("dashed #fff", html))
  expect_equal(lengths(regmatches(html, gregexpr("width:20px;height:20px", html)))[1], 9L)
})

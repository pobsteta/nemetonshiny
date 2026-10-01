# Production IFN par sylvoecoregion (spec 054) : parametres projet, cablage du
# calcul (P2 IFN / E1 flux sans CHM, colonnes annexes, reprise), localisation
# SER en cache, resume du massif et affichage.

.ifn_units <- function(ser = c("C51", "C51"), n = length(ser)) {
  pts <- lapply(seq_len(n), function(i) {
    sf::st_buffer(sf::st_point(c(850000 + 1000 * i, 6680000)), 200)
  })
  sf::st_sf(
    ug_id = paste0("ug", seq_len(n)),
    species = rep("FASY", n),
    ser = ser,
    geometry = sf::st_sfc(pts, crs = 2154)
  )
}

# ---------------------------------------------------------------------------
# Parametres projet
# ---------------------------------------------------------------------------

test_that("les modes de production sont opt-in : defaut CHM / stock", {
  p <- nemetonshiny:::project_production_ifn_params(NULL)
  expect_equal(p$p2_source, "chm")
  expect_equal(p$e1_mode, "stock")
  expect_null(p$taux_mobilisation)
})

test_that("E1 flux retombe en stock tant que P2 n'est pas en mode IFN", {
  p <- nemetonshiny:::project_production_ifn_params(
    list(production_ifn = list(p2_source = "chm", e1_mode = "flux")))
  expect_equal(p$e1_mode, "stock")
  expect_null(p$taux_mobilisation)
})

test_that("taux_mobilisation : part choisie bornee, ou ifn_ser", {
  p <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "ifn_fh", e1_mode = "flux", e1_taux_type = "fixe",
    e1_taux = 1.7)))
  expect_equal(p$taux_mobilisation, 1)

  p <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "ifn_fh", e1_mode = "flux", e1_taux_type = "ifn_ser")))
  expect_identical(p$taux_mobilisation, "ifn_ser")

  p <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "nimporte", e1_mode = "flux")))
  expect_equal(p$p2_source, "chm")
})

test_that("set_project_production_ifn ecrit puis relit les memes valeurs", {
  withr::with_tempdir({
    with_mocked_bindings(
      get_app_options = function() list(project_dir = getwd()),
      {
        pid <- nemetonshiny:::create_project(name = "IFN", parcels = NULL)$id
        nemetonshiny:::set_project_production_ifn(
          pid, p2_source = "ifn_fh", e1_mode = "flux",
          e1_taux_type = "fixe", e1_taux = 0.45)
        m <- nemetonshiny:::load_project_metadata(pid)
        p <- nemetonshiny:::project_production_ifn_params(m)
        expect_equal(p$p2_source, "ifn_fh")
        expect_equal(p$e1_mode, "flux")
        expect_equal(p$taux_mobilisation, 0.45)
        # La valeur a transmettre au coeur n'est pas persistee : elle se derive.
        expect_null(m$production_ifn$taux_mobilisation)
      }
    )
  })
})

# ---------------------------------------------------------------------------
# Cablage du calcul
# ---------------------------------------------------------------------------

test_that(".production_ifn_mode leve l'exigence CHM pour P2 IFN et E1 flux", {
  cfg <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "ifn_fh", e1_mode = "flux", e1_taux = 0.6)))
  expect_true(nemetonshiny:::.production_ifn_mode("indicateur_p2_station", cfg))
  expect_true(nemetonshiny:::.production_ifn_mode("indicateur_e1_bois_energie", cfg))
  expect_false(nemetonshiny:::.production_ifn_mode("indicateur_p1_volume", cfg))
  expect_false(nemetonshiny:::.production_ifn_mode("indicateur_p2_station", NULL))
  expect_false(nemetonshiny:::.production_ifn_mode(
    "indicateur_p2_station", nemetonshiny:::project_production_ifn_params(NULL)))
})

test_that("compute_single_indicator : P2 IFN sans CHM ne s'arrete pas et garde ses annexes", {
  cfg <- nemetonshiny:::project_production_ifn_params(
    list(production_ifn = list(p2_source = "ifn_fh")))
  units <- .ifn_units()
  vals <- suppressMessages(nemetonshiny:::compute_single_indicator(
    "indicateur_p2_station", units, list(production_ifn = cfg)))
  expect_length(vals, 2L)
  expect_true(all(vals > 1 & vals < 15))
  expect_equal(vals[1], vals[2])  # meme SER -> meme valeur
  annex <- attr(vals, "nemeton_annex")
  expect_setequal(names(annex),
                  c(".p2_rse", ".p2_provenance", ".p2_nature", ".p2_ser"))
  expect_equal(unique(annex$.p2_provenance), "ifn_prod_ser")
})

test_that("compute_single_indicator : P2 en mode CHM sans CHM reste bloque", {
  units <- .ifn_units()
  expect_error(
    nemetonshiny:::compute_single_indicator(
      "indicateur_p2_station", units,
      list(production_ifn = nemetonshiny:::project_production_ifn_params(NULL))),
    regexp = ".")
})

test_that("compute_all_indicators : E1 flux lit P2 et signale la recolte observee", {
  cfg <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "ifn_fh", e1_mode = "flux", e1_taux_type = "ifn_ser")))
  res <- suppressWarnings(suppressMessages(nemetonshiny:::compute_all_indicators(
    parcels = .ifn_units(),
    layers = list(production_ifn = cfg),
    indicators = c("indicateur_p2_station", "indicateur_e1_bois_energie"))))
  expect_true(all(!is.na(res$indicateur_p2_station)))
  expect_true(all(!is.na(res$indicateur_e1_bois_energie)))
  expect_equal(unique(res$.e1_mode), "recolte_observee")
  expect_equal(unique(res$.e1_taux), "ifn_ser")
  expect_equal(unique(res$.p2_provenance), "ifn_prod_ser")
})

test_that("compute_all_indicators : E1 flux avec une part choisie = ressource_flux", {
  cfg <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "ifn_fh", e1_mode = "flux", e1_taux = 0.6)))
  res <- suppressWarnings(suppressMessages(nemetonshiny:::compute_all_indicators(
    parcels = .ifn_units(),
    layers = list(production_ifn = cfg),
    indicators = c("indicateur_p2_station", "indicateur_e1_bois_energie"))))
  expect_equal(unique(res$.e1_mode), "ressource_flux")
  expect_equal(unique(res$.e1_taux), "0.6")
})

test_that(".write_production_annex retire les annexes d'un mode precedent", {
  res <- data.frame(indicateur_p2_station = 1:2, .p2_rse = c(3, 3),
                    .p2_provenance = "ifn_prod_ser", check.names = FALSE)
  out <- nemetonshiny:::.write_production_annex(
    res, "indicateur_p2_station", c(10, 11), NULL)
  expect_false(any(c(".p2_rse", ".p2_provenance") %in% names(out)))

  res <- data.frame(indicateur_e1_bois_energie = 1:2, .e1_taux = "0.6",
                    check.names = FALSE)
  out <- nemetonshiny:::.write_production_annex(
    res, "indicateur_e1_bois_energie", NULL, NULL)
  expect_false(".e1_taux" %in% names(out))
})

test_that(".units_for_indicator ne touche que E1 en mode flux", {
  units <- .ifn_units()
  results <- data.frame(indicateur_p2_station = c(5, 6),
                        .p2_provenance = "ifn_prod_ser", check.names = FALSE)
  flux <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "ifn_fh", e1_mode = "flux", e1_taux = 0.5)))
  out <- nemetonshiny:::.units_for_indicator(
    "indicateur_e1_bois_energie", units, results, flux)
  expect_equal(out$P2, c(5, 6))
  expect_equal(out$P2_provenance, c("ifn_prod_ser", "ifn_prod_ser"))
  expect_identical(nemetonshiny:::.units_for_indicator(
    "indicateur_p1_volume", units, results, flux), units)
})

test_that(".production_mode_stale detecte un changement de mode a la reprise", {
  ifn <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "ifn_fh", e1_mode = "flux", e1_taux = 0.6)))
  chm <- nemetonshiny:::project_production_ifn_params(NULL)
  both <- c("indicateur_p2_station", "indicateur_e1_bois_energie")

  stored_chm <- data.frame(indicateur_p2_station = 12,
                           indicateur_e1_bois_energie = 0.4)
  expect_setequal(nemetonshiny:::.production_mode_stale(stored_chm, both, ifn),
                  both)
  expect_length(nemetonshiny:::.production_mode_stale(stored_chm, both, chm), 0L)

  stored_ifn <- data.frame(indicateur_p2_station = 5,
                           .p2_provenance = "ifn_prod_ser",
                           indicateur_e1_bois_energie = 0.3, .e1_taux = "0.6",
                           check.names = FALSE)
  expect_length(nemetonshiny:::.production_mode_stale(stored_ifn, both, ifn), 0L)
  expect_setequal(nemetonshiny:::.production_mode_stale(stored_ifn, both, chm),
                  both)

  # Autre part recoltee : seul E1 se recalcule.
  autre <- nemetonshiny:::project_production_ifn_params(list(production_ifn = list(
    p2_source = "ifn_fh", e1_mode = "flux", e1_taux_type = "ifn_ser")))
  expect_equal(nemetonshiny:::.production_mode_stale(stored_ifn, both, autre),
               "indicateur_e1_bois_energie")
})

# ---------------------------------------------------------------------------
# Localisation SER et resume du massif
# ---------------------------------------------------------------------------

test_that("ensure_ugf_ser localise une fois puis relit le cache", {
  withr::with_tempdir({
    n_calls <- 0L
    local_mocked_bindings(
      localiser_ser = function(units, ...) {
        n_calls <<- n_calls + 1L
        units$ser <- rep("C51", nrow(units))
        units
      },
      .package = "nemeton"
    )
    units <- .ifn_units()
    units$ser <- NULL
    out <- nemetonshiny:::ensure_ugf_ser(units, getwd())
    expect_equal(out$ser, c("C51", "C51"))
    expect_true(file.exists(file.path("data", "ugf_ser.rds")))
    out2 <- nemetonshiny:::ensure_ugf_ser(units, getwd())
    expect_equal(out2$ser, c("C51", "C51"))
    expect_equal(n_calls, 1L)

    # Une UGF redessinee est relocalisee.
    moved <- units
    sf::st_geometry(moved)[1] <- sf::st_buffer(
      sf::st_point(c(900000, 6700000)), 100)
    nemetonshiny:::ensure_ugf_ser(moved, getwd())
    expect_equal(n_calls, 2L)
  })
})

test_that("ensure_ugf_ser ne met pas en cache un echec (tout NA)", {
  withr::with_tempdir({
    local_mocked_bindings(
      localiser_ser = function(units, ...) stop("WFS down"),
      .package = "nemeton"
    )
    units <- .ifn_units()
    units$ser <- NULL
    out <- suppressWarnings(nemetonshiny:::ensure_ugf_ser(units, getwd()))
    expect_true(all(is.na(out$ser)))
    expect_false(file.exists(file.path("data", "ugf_ser.rds")))
  })
})

test_that("build_production_ifn_summary persiste massif et ratios", {
  withr::with_tempdir({
    local_mocked_bindings(
      ifn_production_domaines = function(domaines, ...) {
        data.frame(id = "massif", valeur = 5.4, rse = 6, n_placettes = 3,
                   poids_direct = 0.1, part_bordure = 0.7, surface_ha = 800)
      },
      ifn_taux_prelevement_production = function(ser, definition = "ign", ...) {
        data.frame(ser = ser, definition = definition, ratio = 0.9, rse = 11)
      },
      .package = "nemeton"
    )
    nemetonshiny:::build_production_ifn_summary(.ifn_units(), getwd())
    s <- nemetonshiny:::read_production_ifn_summary(getwd())
    expect_equal(s$massif$valeur, 5.4)
    expect_equal(nrow(s$ratios), 2L)  # une SER x deux definitions
    expect_setequal(s$ratios$definition, c("ign", "vidange"))
  })
})

test_that("read_production_ifn_summary renvoie NULL sans fichier", {
  withr::with_tempdir({
    expect_null(nemetonshiny:::read_production_ifn_summary(getwd()))
    expect_null(nemetonshiny:::read_production_ifn_summary(NULL))
  })
})

# ---------------------------------------------------------------------------
# Affichage
# ---------------------------------------------------------------------------

test_that("les colonnes annexes ne sont jamais prises pour des indicateurs", {
  df <- data.frame(ug_id = "a", indicateur_p2_station = 5, .p2_rse = 2.5,
                   check.names = FALSE)
  expect_equal(nemetonshiny:::get_indicator_cols(df), "indicateur_p2_station")
})

test_that("P2 IFN : libelle de sylvoecoregion, RSE, echelon et mise en garde", {
  i18n <- nemetonshiny:::get_i18n("fr")
  df <- data.frame(indicateur_p2_station = c(5.37, 5.37),
                   indicateur_p2_station_norm = c(35.8, 35.8),
                   .p2_rse = 2.56, .p2_provenance = "ifn_prod_ser",
                   .p2_nature = "fay_herriot", .p2_ser = "C51",
                   check.names = FALSE)
  lbl <- nemetonshiny:::indicator_display_label(
    df, "indicateur_p2_station_norm", i18n)
  expect_match(lbl, "^P2 - ")
  expect_match(lbl, i18n$t("p2_ifn_label"), fixed = TRUE)

  html <- as.character(nemetonshiny:::production_ifn_banner(
    df, "indicateur_p2_station_norm", i18n))
  expect_match(html, "C51")
  expect_match(html, "5,37")
  expect_match(html, "2,6 %")
  expect_match(html, i18n$t("p2_echelon_ser"), fixed = TRUE)
  expect_match(html, htmltools::htmlEscape(i18n$t("p2_ifn_not_station")),
               fixed = TRUE)
})

test_that("P2 en mode CHM : ni bandeau ni libelle IFN", {
  i18n <- nemetonshiny:::get_i18n("fr")
  df <- data.frame(indicateur_p2_station = 12)
  expect_null(nemetonshiny:::production_ifn_banner(
    df, "indicateur_p2_station", i18n))
  expect_equal(
    nemetonshiny:::indicator_display_label(df, "indicateur_p2_station", i18n),
    nemetonshiny:::clean_indicator_label("indicateur_p2_station", i18n))
})

test_that("E1 recolte observee : jamais presente comme un potentiel", {
  i18n <- nemetonshiny:::get_i18n("fr")
  df <- data.frame(indicateur_e1_bois_energie = 0.5,
                   .e1_mode = "recolte_observee", .e1_taux = "ifn_ser",
                   check.names = FALSE)
  lbl <- nemetonshiny:::indicator_display_label(
    df, "indicateur_e1_bois_energie", i18n)
  expect_match(lbl, i18n$t("e1_mode_recolte_observee"), fixed = TRUE)
  html <- as.character(nemetonshiny:::production_ifn_banner(
    df, "indicateur_e1_bois_energie", i18n))
  expect_match(html, "text-warning")
  expect_match(html, "pas un potentiel")
})

test_that("E1 flux : la part recoltee est affichee", {
  i18n <- nemetonshiny:::get_i18n("fr")
  df <- data.frame(indicateur_e1_bois_energie = 0.3,
                   .e1_mode = "ressource_flux", .e1_taux = "0.6",
                   check.names = FALSE)
  html <- as.character(nemetonshiny:::production_ifn_banner(
    df, "indicateur_e1_bois_energie", i18n))
  expect_match(html, "0,6")
})

test_that("panneau du massif : poids direct faible, bordure, petite surface", {
  i18n <- nemetonshiny:::get_i18n("fr")
  s <- list(
    massif = data.frame(valeur = 5.4, rse = 6, n_placettes = 3,
                        poids_direct = 0.1, part_bordure = 0.7,
                        surface_ha = 800),
    ratios = data.frame(ser = "C51", definition = c("ign", "vidange"),
                        ratio = c(0.92, 0.85), rse = c(11.7, 12)))
  html <- as.character(nemetonshiny:::production_ifn_panel(s, i18n))
  esc <- function(k) htmltools::htmlEscape(i18n$t(k))
  expect_match(html, esc("prod_massif_poids_faible"), fixed = TRUE)
  expect_match(html, esc("prod_massif_bordure_elevee"), fixed = TRUE)
  expect_match(html, esc("prod_massif_petit"), fixed = TRUE)
  expect_match(html, esc("prod_ratio_avert"), fixed = TRUE)
  expect_match(html, "0,92")

  # Un grand massif bien echantillonne n'appelle aucune reserve.
  s$massif$poids_direct <- 0.6
  s$massif$part_bordure <- 0.1
  s$massif$surface_ha <- 20000
  html <- as.character(nemetonshiny:::production_ifn_panel(s, i18n))
  expect_no_match(html, esc("prod_massif_poids_faible"), fixed = TRUE)
  expect_no_match(html, esc("prod_massif_petit"), fixed = TRUE)

  expect_null(nemetonshiny:::production_ifn_panel(NULL, i18n))
})

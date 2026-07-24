# ===========================================================================
# Service — Validation ACCESSFOR (référence IGN, onglet Accessibilité)
# ===========================================================================
#
# Compare les classes de débardage calculées par l'app (skidder ForêtAccess) à la
# couche nationale officielle **ACCESSFOR** de l'IGN (projet INRAE/IGN, WFS), même
# filiation Sylvaccess. La table de correspondance est figée côté cœur par
# `foretaccess::accessfor_correspondance()` (jointure sur l'ENTIER, jamais le
# libellé) ; l'app récupère la couche ACCESSFOR (WFS `happign`) et calcule un taux
# d'accord par classe. Règles 1/2 : aucune logique métier ici — crosswalk cœur +
# rasterisation/tabulation.

#' WFS layer names of the IGN ACCESSFOR coverage (one per terrestrial engine)
#' @noRd
ACCESSFOR_WFS_LAYERS <- c(
  skidder = "IGNF_ACCESSIBILITE-PHYSIQUE-FORETS-:acces_skidder",
  porteur = "IGNF_ACCESSIBILITE-PHYSIQUE-FORETS-:acces_porteur")

#' Validate an app skidding-class raster against the IGN ACCESSFOR reference
#'
#' Fetches the ACCESSFOR layer (WFS IGN) for the AOI, rasterises its `class`
#' attribute onto the app raster's grid, reclassifies the app `classes_debardage`
#' raster to ACCESSFOR codes via `foretaccess::accessfor_correspondance()`, and
#' computes the agreement (overall + per ACCESSFOR class) on forest cells common
#' to both.
#'
#' Best-effort and structured: returns `list(status = "error", reason = ...)`
#' instead of throwing.
#'
#' @param classes_debardage_path Path to the app `acc_classes_debardage.tif`.
#' @param engine One of `names(ACCESSFOR_WFS_LAYERS)` (only `skidder` shipped by
#'   ACCESSFOR as skidding distance bands; `porteur` available too).
#' @return A named list: `status`, `overall_pct` (% cells in agreement),
#'   `n_cells`, `table` (per-class data.frame), or an error list.
#' @noRd
run_accessfor_validation <- function(classes_debardage_path, engine = "skidder") {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "accessfor_no_foretaccess"))
  }
  if (!requireNamespace("happign", quietly = TRUE)) {
    return(list(status = "error", reason = "accessfor_no_happign"))
  }
  if (length(engine) != 1L || !engine %in% names(ACCESSFOR_WFS_LAYERS)) {
    return(list(status = "error", reason = "accessfor_bad_engine"))
  }
  layer <- ACCESSFOR_WFS_LAYERS[[engine]]
  if (is.null(classes_debardage_path) || !file.exists(classes_debardage_path)) {
    return(list(status = "error", reason = "accessfor_no_layer"))
  }

  rast <- tryCatch(terra::rast(classes_debardage_path), error = function(e) NULL)
  if (is.null(rast)) return(list(status = "error", reason = "accessfor_no_layer"))

  # Emprise de la requête WFS = étendue du raster app (couvre le tampon), pas les
  # seules parcelles : garantit que la couche ACCESSFOR recouvre tout le raster.
  aoi <- tryCatch(
    sf::st_as_sfc(sf::st_bbox(rast)),
    error = function(e) NULL)
  if (is.null(aoi)) return(list(status = "error", reason = "accessfor_no_layer"))
  af <- tryCatch(happign::get_wfs(x = aoi, layer = layer),
                 error = function(e) structure(list(msg = conditionMessage(e)),
                                               class = "acc_err"))
  if (inherits(af, "acc_err")) {
    return(list(status = "error", reason = "accessfor_wfs_failed", detail = af$msg))
  }
  if (!inherits(af, "sf") || nrow(af) == 0L || !("class" %in% names(af))) {
    return(list(status = "error", reason = "accessfor_wfs_empty"))
  }
  af <- tryCatch(sf::st_transform(af, terra::crs(rast)), error = function(e) af)

  # Crosswalk cœur : valeur classes_debardage app -> code ACCESSFOR (sur l'entier).
  corr <- tryCatch(foretaccess::accessfor_correspondance(), error = function(e) NULL)
  if (!is.data.frame(corr) || !all(c("fa_value", "accessfor_class") %in% names(corr))) {
    return(list(status = "error", reason = "accessfor_no_crosswalk"))
  }

  # Notre raster (valeurs 1..9) -> codes ACCESSFOR (via crosswalk). `subst` matche
  # les LIBELLÉS sur un raster catégoriel (piège S4) : on repart d'une grille NUE
  # (`terra::rast(rast)` ne copie pas les niveaux) remplie des valeurs numériques,
  # pour un `subst` fiable sur les entiers.
  ok <- stats::complete.cases(corr[, c("fa_value", "accessfor_class")])
  our_num <- tryCatch({
    rnum <- terra::rast(rast)
    terra::values(rnum) <- as.numeric(terra::values(rast, mat = FALSE))
    terra::subst(rnum, from = corr$fa_value[ok], to = corr$accessfor_class[ok])
  }, error = function(e) NULL)
  if (is.null(our_num)) return(list(status = "error", reason = "accessfor_reclass_failed"))

  # ACCESSFOR rasterisé sur la grille app.
  af_r <- tryCatch(
    terra::rasterize(terra::vect(af), our_num, field = "class"),
    error = function(e) NULL)
  if (is.null(af_r)) return(list(status = "error", reason = "accessfor_rasterize_failed"))

  # Raster ACCESSFOR AFFICHABLE : reclassé des codes ACCESSFOR vers NOS valeurs de
  # bande (fa_value) + mêmes niveaux/coltab que le raster « classes de débardage »
  # → il se colore et se légende à l'identique, directement comparable à l'œil.
  # Écrit à côté des autres couches du run (cache d'accessibilité).
  accessfor_raster_path <- NULL
  af_disp <- tryCatch({
    okv <- stats::complete.cases(corr[, c("accessfor_class", "fa_value")])
    r <- terra::subst(af_r, from = corr$accessfor_class[okv], to = corr$fa_value[okv])
    lv <- terra::levels(rast)[[1]]
    if (is.data.frame(lv) && nrow(lv) > 0L) levels(r) <- lv
    ct <- tryCatch(terra::coltab(rast)[[1]], error = function(e) NULL)
    if (is.data.frame(ct)) terra::coltab(r) <- ct
    r
  }, error = function(e) NULL)
  if (!is.null(af_disp)) {
    p <- file.path(dirname(classes_debardage_path),
                   paste0("accessfor_", engine, ".tif"))
    if (isTRUE(tryCatch({ terra::writeRaster(af_disp, p, overwrite = TRUE); TRUE },
                        error = function(e) FALSE))) {
      accessfor_raster_path <- p
    }
  }

  # Comparaison sur les cellules communes (les deux non NA). `c()` non nommé sur
  # SpatRaster empile les couches ; on renomme les colonnes par position ensuite.
  df <- tryCatch(stats::na.omit(terra::as.data.frame(c(our_num, af_r))),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) < 2L || ncol(df) < 2L) {
    return(list(status = "error", reason = "accessfor_no_overlap"))
  }
  df <- df[, 1:2]
  names(df) <- c("our", "af")
  df$our <- as.integer(df$our); df$af <- as.integer(df$af)

  n <- nrow(df)
  overall <- round(100 * mean(df$our == df$af), 1)

  # Table par classe ACCESSFOR : n cellules ACCESSFOR, dont en accord.
  cats <- corr[!is.na(corr$accessfor_class),
               c("accessfor_class", "accessfor_cat")]
  cats <- cats[!duplicated(cats$accessfor_class), ]
  by_af <- split(df, df$af)
  tab <- do.call(rbind, lapply(names(by_af), function(k) {
    d <- by_af[[k]]; kc <- as.integer(k)
    data.frame(
      accessfor_class = kc,
      libelle = cats$accessfor_cat[match(kc, cats$accessfor_class)] %||% NA_character_,
      cellules = nrow(d),
      accord_pct = round(100 * mean(d$our == d$af), 1),
      stringsAsFactors = FALSE)
  }))
  tab <- tab[order(tab$accessfor_class), ]

  list(
    status = "success",
    engine = engine,
    overall_pct = overall,
    n_cells = n,
    table = tab,
    accessfor_raster_path = accessfor_raster_path)
}

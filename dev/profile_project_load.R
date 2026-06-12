#!/usr/bin/env Rscript
# dev/profile_project_load.R — Harnais de mesure du temps de CHARGEMENT
# d'un projet nemetonshiny (chemin `load_project()`), à lancer EN LOCAL
# sur le PC de dev (Windows / Linux / macOS). Non-invasif : aucune
# modification du package, aucun bump de version. Sert à localiser où
# partent les secondes lors de l'ouverture d'un projet depuis l'Accueil.
#
# ── Usage (depuis la racine du repo nemetonshiny) ────────────────────
#
#   # 1) Projet le plus récent, breakdown + (si dispo) flame graph :
#   Rscript dev/profile_project_load.R
#
#   # 2) Projet précis (id = nom du dossier sous le projects root) :
#   Rscript dev/profile_project_load.R 20260115_103000_ab12cd
#
#   # 3) Sous PowerShell, idem (une ligne) :
#   #   Rscript dev/profile_project_load.R 20260115_103000_ab12cd
#
# Sortie : un tableau trié des durées par étape (cold), le total cold
# vs warm (effet cache OS/lecture parquet), et — si le paquet `profvis`
# est installé — un flame graph HTML `dev/profile_project_load.html` à
# ouvrir dans un navigateur pour voir l'arbre d'appels détaillé.
#
# Aucune donnée n'est envoyée nulle part : tout est local.

# ── 0. Localisation du repo + chargement du package ──────────────────
repo <- tryCatch(normalizePath(getwd(), winslash = "/"),
                 error = function(e) getwd())
if (!file.exists(file.path(repo, "DESCRIPTION"))) {
  stop("Lance ce script depuis la racine du repo nemetonshiny ",
       "(le dossier qui contient DESCRIPTION).")
}

# Petit chrono : exécute `expr`, enregistre (label, secondes), renvoie
# la valeur. `store` est l'environnement d'accumulation.
.store <- new.env(parent = emptyenv())
.store$rows <- list()
timeit <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  val <- force(expr)
  dt <- proc.time()[["elapsed"]] - t0
  .store$rows[[length(.store$rows) + 1L]] <- list(label = label, secs = dt)
  cat(sprintf("  [%7.3f s] %s\n", dt, label))
  invisible(val)
}

cat("\n=== nemetonshiny — profilage chargement projet ===\n\n")
cat("Repo :", repo, "\n")
cat("R    :", R.version.string, "\n\n")

cat("1) Chargement du package (pkgload::load_all)\n")
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Le paquet 'pkgload' est requis : install.packages('pkgload').")
}
timeit("pkgload::load_all('.')",
       suppressMessages(pkgload::load_all(repo, quiet = TRUE)))

# Accès aux fonctions internes (non exportées) du package.
ns  <- asNamespace("nemetonshiny")
get_fn <- function(name) get(name, envir = ns)

# ── 1. Résolution de l'id projet ─────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
pid  <- if (length(args) >= 1L && nzchar(args[[1]])) {
  args[[1]]
} else {
  Sys.getenv("NEMETON_PROFILE_PROJECT", "")
}
if (!nzchar(pid)) {
  recent <- tryCatch(get_fn("list_recent_projects")(limit = 1L),
                     error = function(e) NULL)
  if (is.null(recent) || !nrow(recent)) {
    stop("Aucun projet trouvé sous le projects root. Passe un id en ",
         "argument : Rscript dev/profile_project_load.R <project_id>")
  }
  pid <- recent$id[[1]]
  cat(sprintf("\n(Projet auto-sélectionné = le plus récent : %s)\n", pid))
}
cat(sprintf("\n2) Projet ciblé : %s\n", pid))

ppath <- tryCatch(get_fn("get_project_path")(pid), error = function(e) NULL)
if (is.null(ppath)) stop("Projet introuvable : ", pid)
cat("   Dossier :", ppath, "\n\n")

# ── 2. Chargement COLD complet (1er appel, caches OS froids) ──────────
cat("3) load_project() — total COLD (1er appel)\n")
load_project <- get_fn("load_project")
proj <- timeit("load_project() TOTAL (cold)", load_project(pid))

# ── 3. Breakdown par étape (appels individuels) ──────────────────────
# Reproduit les étapes internes de load_project() pour mesurer le coût
# de chacune. Note : certaines relisent les métadonnées → léger double
# comptage, sans incidence sur le classement relatif des coûts.
cat("\n4) Breakdown par étape\n")
timeit("load_project_metadata()",  get_fn("load_project_metadata")(pid))
timeit("load_parcels()",           get_fn("load_parcels")(pid))
timeit("load_commune_geometry()",  get_fn("load_commune_geometry")(pid))
timeit("load_indicators()",        get_fn("load_indicators")(pid))
timeit("load_comments()",          get_fn("load_comments")(pid))
timeit("ensure_project_migrated()", get_fn("ensure_project_migrated")(pid, proj))
if (isTRUE(tryCatch(get_fn("has_ug_data")(proj), error = function(e) FALSE))) {
  timeit("ug_build_sf()",          get_fn("ug_build_sf")(proj))
} else {
  cat("  [   --   ] ug_build_sf() : pas d'UGF pour ce projet (sauté)\n")
}

# ── 4. Chargement WARM (2e appel, caches chauds) ─────────────────────
cat("\n5) load_project() — total WARM (2e appel, cache chaud)\n")
timeit("load_project() TOTAL (warm)", load_project(pid))

# ── 5. Tableau de synthèse trié ──────────────────────────────────────
rows <- .store$rows
df <- data.frame(
  etape   = vapply(rows, function(r) r$label, character(1)),
  secondes = vapply(rows, function(r) round(r$secs, 3), numeric(1)),
  stringsAsFactors = FALSE
)
# Exclut les 2 totaux du tri des étapes (on les remet en tête).
is_total <- grepl("TOTAL", df$etape)
breakdown <- df[!is_total & !grepl("load_all", df$etape), , drop = FALSE]
breakdown <- breakdown[order(-breakdown$secondes), , drop = FALSE]

cat("\n=== SYNTHÈSE (étapes triées par coût décroissant) ===\n\n")
print(breakdown, row.names = FALSE)
cat("\n--- Totaux ---\n")
print(df[is_total, , drop = FALSE], row.names = FALSE)

cat("\nAstuce : l'écart cold→warm = coût de lecture disque (parquet/gpkg).",
    "\nUne étape chère en cold mais quasi nulle en warm = I/O à optimiser",
    "\n(format, taille, ou mise en cache). Une étape chère même en warm =",
    "\ncalcul CPU (ex. opérations sf sur de gros polygones).\n")

# ── 6. Flame graph profvis (optionnel) ───────────────────────────────
if (requireNamespace("profvis", quietly = TRUE)) {
  cat("\n6) Flame graph profvis (load_project x3)…\n")
  pv <- profvis::profvis({
    for (i in 1:3) get("load_project", envir = ns)(pid)
  })
  out_html <- file.path(repo, "dev", "profile_project_load.html")
  ok <- tryCatch({
    if (requireNamespace("htmlwidgets", quietly = TRUE)) {
      htmlwidgets::saveWidget(pv, out_html, selfcontained = TRUE)
      TRUE
    } else FALSE
  }, error = function(e) { cat("   (saveWidget a échoué :", conditionMessage(e), ")\n"); FALSE })
  if (isTRUE(ok)) {
    cat("   Flame graph écrit :", out_html, "\n")
    cat("   Ouvre-le dans un navigateur pour l'arbre d'appels détaillé.\n")
  }
} else {
  cat("\n6) profvis non installé — flame graph ignoré.\n",
      "   Pour l'activer : install.packages('profvis')\n", sep = "")
}

cat("\n=== Fin du profilage ===\n\n")

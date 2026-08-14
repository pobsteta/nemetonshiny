# test-static_assets.R — un seul fichier par asset, et c'est celui qu'on sert
#
# `app_ui.R` servait `custom.min.css`, un fichier « minifié » qui ne l'était pas
# (une copie manuelle commentée de `custom.css`) et que rien ne régénérait — ni
# build, ni script, ni CI. Il a dérivé de deux commits, et deux règles n'ont
# jamais atteint le navigateur : `.leaflet-control.nmt-bivariate-control`
# (ascenseurs de la légende bivariée E-OBS) et `td.action-comment-trigger`
# (cellule commentaire du plan d'action). Les deux features avaient été livrées
# et testées côté R ; leur CSS dormait dans le fichier source.
#
# Le JS avait le même couple `custom.js` / `custom.min.js` : copie octet pour
# octet, non minifiée, non régénérée. Encore à jour au moment du constat, mais
# le même piège — la prochaine édition n'aurait pas été servie.
#
# Les copies sont supprimées : on sert les sources. Ces tests empêchent la
# classe de bug de revenir — une copie non régénérée, ou un lien qui la vise.

.css_dir <- function() {
  test_path("..", "..", "inst", "app", "www", "css")
}

test_that("il n'y a qu'un seul fichier CSS applicatif", {
  d <- .css_dir()
  skip_if_not(dir.exists(d))
  files <- list.files(d, pattern = "\\.css$")

  expect_identical(files, "custom.css")
  # Une copie « min » n'est acceptable qu'avec un build qui la produise ET une
  # étape CI qui vérifie qu'elle est à jour. Ni l'un ni l'autre n'existe ici.
  expect_false("custom.min.css" %in% files)
})

test_that("aucune copie min non regeneree du JS applicatif", {
  d <- test_path("..", "..", "inst", "app", "www", "js")
  skip_if_not(dir.exists(d))
  files <- list.files(d, pattern = "\\.js$")

  expect_true("custom.js" %in% files)
  expect_false("custom.min.js" %in% files)
  # Les `.min.js` VENDORÉS (Sortable) sont légitimes : ils arrivent minifiés de
  # l'amont, on ne les régénère pas. Seules les copies de NOS sources posent
  # problème — d'où la vérification ciblée sur `custom.*`, pas sur `*.min.js`.
  expect_true(any(grepl("^Sortable-.*\\.min\\.js$", files)))
})

test_that("la regle de lisibilite des infobulles de carte est presente", {
  f <- file.path(.css_dir(), "custom.css")
  skip_if_not(file.exists(f))
  css <- paste(readLines(f, warn = FALSE), collapse = "\n")

  # `.leaflet-tooltip` ne fixe aucune taille et hérite du `font: 12px/1.5` de
  # `.leaflet-container` : la règle doit exister ET viser plus grand que 12px.
  expect_match(css, "\\.leaflet-tooltip\\s*\\{")
  bloc <- regmatches(
    css, regexpr("\\.leaflet-tooltip\\s*\\{[^}]*font-size:\\s*([0-9.]+)px", css))
  expect_length(bloc, 1L)
  px <- as.numeric(sub(".*font-size:\\s*([0-9.]+)px.*", "\\1", bloc))
  expect_gt(px, 12)
})

test_that("les regles perdues par la copie stale sont bien servies", {
  # Régression directe : ces deux règles existaient dans la source et manquaient
  # dans le fichier servi. Elles doivent être dans le fichier qu'`app_ui` cible.
  f <- file.path(.css_dir(), "custom.css")
  skip_if_not(file.exists(f))
  css <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(css, "nmt-bivariate-control", fixed = TRUE)
  expect_match(css, "action-comment-trigger", fixed = TRUE)
})

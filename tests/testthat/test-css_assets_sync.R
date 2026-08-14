# test-css_assets_sync.R — le CSS servi doit être le CSS écrit
#
# `app_ui.R` sert `www/css/custom.min.css`, mais les règles sont écrites dans
# `custom.css`. Rien ne relie les deux : ni build, ni script, ni CI. Résultat
# constaté le 2026-08-14, `custom.min.css` était en retard de deux commits, et
# deux règles n'étaient jamais arrivées au navigateur —
# `.leaflet-control.nmt-bivariate-control` (ascenseurs de la légende bivariée)
# et `td.action-comment-trigger` (affordance de la cellule commentaire du plan
# d'action). Les deux features avaient été livrées et testées côté R.
#
# Ce test échoue dès que les deux fichiers divergent. Il ne dit pas COMMENT les
# resynchroniser (aujourd'hui : une copie — le fichier « min » n'est pas
# minifié), il dit seulement qu'on ne peut plus l'oublier.

test_that("le CSS servi est identique au CSS source", {
  src    <- test_path("..", "..", "inst", "app", "www", "css", "custom.css")
  served <- test_path("..", "..", "inst", "app", "www", "css", "custom.min.css")
  skip_if_not(file.exists(src) && file.exists(served))

  a <- readLines(src, warn = FALSE)
  b <- readLines(served, warn = FALSE)

  # Message utile en cas d'échec : les règles présentes d'un côté seulement.
  seuls_source <- setdiff(trimws(a), trimws(b))
  seuls_servis <- setdiff(trimws(b), trimws(a))
  expect_identical(
    a, b,
    info = paste0(
      "custom.min.css a divergé de custom.css — l'app sert le premier.\n",
      "Uniquement dans la source : ",
      paste(utils::head(seuls_source[nzchar(seuls_source)], 5), collapse = " | "),
      "\nUniquement dans le servi  : ",
      paste(utils::head(seuls_servis[nzchar(seuls_servis)], 5), collapse = " | ")))
})

test_that("la regle de lisibilite des infobulles de carte est servie", {
  served <- test_path("..", "..", "inst", "app", "www", "css", "custom.min.css")
  skip_if_not(file.exists(served))
  css <- paste(readLines(served, warn = FALSE), collapse = "\n")

  # `.leaflet-tooltip` ne fixe aucune taille et hérite du `font: 12px/1.5` de
  # `.leaflet-container` : la règle doit exister ET viser plus grand que 12px.
  expect_match(css, "\\.leaflet-tooltip\\s*\\{")
  taille <- regmatches(
    css, regexpr("\\.leaflet-tooltip\\s*\\{[^}]*font-size:\\s*([0-9.]+)px", css))
  expect_true(length(taille) == 1L)
  px <- as.numeric(sub(".*font-size:\\s*([0-9.]+)px.*", "\\1", taille))
  expect_gt(px, 12)
})

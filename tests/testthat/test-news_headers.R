# test-news_headers.R — chaque version publiée a son titre dans NEWS.md
#
# Trois fois de suite (0.122.6, 0.122.7, puis 0.122.8), la rédaction d'une
# nouvelle entrée a REMPLACÉ le titre de la version précédente au lieu de le
# conserver : les sections de l'ancienne se sont retrouvées sous le titre de la
# nouvelle, y compris dans des tags publiés. Le contrôle CI `version-consistency`
# ne voit rien — il ne compare que la TÊTE du fichier à DESCRIPTION.
#
# Depuis la v0.122.9, `release.yml` extrait le corps de la release GitHub de la
# section correspondante de NEWS.md : un titre manquant fait désormais publier
# DEUX versions de notes dans une seule release. D'où ce test.
#
# CHANGELOG.md sert de référence : il liste une section `## [X.Y.Z]` par version
# publiée, et il n'a jamais souffert du même défaut (ses entrées sont insérées
# après un marqueur `## [Unreleased]` stable, jamais à la place d'un titre).

test_that("chaque version du CHANGELOG a son titre dans NEWS.md", {
  chg  <- test_path("..", "..", "CHANGELOG.md")
  news <- test_path("..", "..", "NEWS.md")
  skip_if_not(file.exists(chg) && file.exists(news))

  versions_chg <- unique(sub("^## \\[([0-9]+\\.[0-9]+\\.[0-9]+)\\].*$", "\\1",
                             grep("^## \\[[0-9]", readLines(chg, warn = FALSE),
                                  value = TRUE)))
  skip_if(length(versions_chg) == 0)

  titres <- readLines(news, warn = FALSE)
  versions_news <- sub("^# nemetonshiny ([0-9.]+).*$", "\\1",
                       grep("^# nemetonshiny [0-9]", titres, value = TRUE))

  # Le CHANGELOG remonte plus loin que NEWS.md sur certains dépôts : on ne
  # vérifie que les versions couvertes par la plage de NEWS.md.
  plancher <- min(package_version(versions_news))
  attendues <- versions_chg[package_version(versions_chg) >= plancher]

  manquantes <- setdiff(attendues, versions_news)
  expect_identical(
    manquantes, character(0),
    info = paste0(
      "titre absent de NEWS.md pour : ", paste(manquantes, collapse = ", "),
      "\nCause habituelle : l'entrée suivante a REMPLACÉ ce titre au lieu de ",
      "s'insérer au-dessus. Les sections de cette version se lisent alors sous ",
      "le titre de la suivante, et `release.yml` publie les deux dans une seule ",
      "release."))
})

test_that("les titres de NEWS.md sont uniques et strictement decroissants", {
  news <- test_path("..", "..", "NEWS.md")
  skip_if_not(file.exists(news))

  v <- sub("^# nemetonshiny ([0-9.]+).*$", "\\1",
           grep("^# nemetonshiny [0-9]", readLines(news, warn = FALSE),
                value = TRUE))
  skip_if(length(v) < 2)

  expect_identical(anyDuplicated(v), 0L,
                   info = paste("titre en double :",
                                paste(v[duplicated(v)], collapse = ", ")))

  pv <- package_version(v)
  decroissant <- all(pv[-length(pv)] > pv[-1])
  expect_true(decroissant,
              info = "les versions de NEWS.md ne sont pas du plus récent au plus ancien")
})

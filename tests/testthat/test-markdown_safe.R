# Rendu Markdown des sources RAG et des reponses IA sans HTML brut
# (brief coeur 0.210.0 : format_citations(format = "markdown") non echappe)

test_that("markdown_safe neutralises raw HTML", {
  h <- as.character(markdown_safe("a <img src=x onerror=alert(1)> b <b>gras</b>"))
  expect_false(grepl("<img", h, fixed = TRUE))
  expect_false(grepl("<b>", h, fixed = TRUE))
  expect_true(grepl("&lt;img", h, fixed = TRUE))
  h2 <- as.character(markdown_safe("<script>alert(1)</script>"))
  expect_false(grepl("<script", h2, fixed = TRUE))
})

test_that("markdown_safe keeps Markdown and the citation autolinks", {
  md <- paste0("## Sources\n\n[^1] Auteur, **Titre**, p. 3. <https://exemple.org/doc?id=1>\n\n",
               "> citation\n\nContact <mailto:a@b.fr>, 3 < 5")
  h <- as.character(markdown_safe(md))
  expect_true(grepl('<a href="https://exemple.org/doc?id=1">', h, fixed = TRUE))
  expect_true(grepl('<a href="mailto:a@b.fr">', h, fixed = TRUE))
  expect_true(grepl("<strong>Titre</strong>", h, fixed = TRUE))
  expect_true(grepl("<blockquote>", h, fixed = TRUE))
  expect_true(grepl("<h2>", h, fixed = TRUE))
  expect_true(grepl("3 &lt; 5", h, fixed = TRUE))
})

test_that("markdown_safe accepts NULL and vectors", {
  expect_identical(as.character(markdown_safe(NULL)), as.character(shiny::markdown("")))
  expect_true(grepl("a\nb|a<br|<p>a", as.character(markdown_safe(c("a", "b")))))
})

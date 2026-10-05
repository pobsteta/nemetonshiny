# AOI stable : la carte n'est plus reconstruite a chaque reassignation (audit 1.0)

test_that(".projet_aoi_stable invalidates only when the parcels change", {
  sq <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100), c(x, 100), c(x, 0))))
  u <- function(n) sf::st_sf(ug_id = paste0("u", seq_len(n)),
                             geometry = sf::st_sfc(lapply(seq_len(n) * 200 + 800000, sq), crs = 2154))
  st <- shiny::reactiveValues(current_project = list(id = "p", indicators_sf = u(2)))
  rendus <- 0L
  serveur <- function(input, output, session) {
    aoi <- .projet_aoi_stable(st)
    shiny::observe({ aoi(); rendus <<- rendus + 1L })
  }
  shiny::testServer(serveur, {
    session$flushReact()
    n0 <- rendus
    # Meme projet, meme geometrie, autre champ (reglages enregistres)
    st$current_project <- list(id = "p", indicators_sf = u(2), metadata = list(x = 1))
    session$flushReact()
    expect_identical(rendus, n0)
    # Une parcelle de plus : la carte doit suivre
    st$current_project <- list(id = "p", indicators_sf = u(3))
    session$flushReact()
    expect_identical(rendus, n0 + 1L)
  })
})

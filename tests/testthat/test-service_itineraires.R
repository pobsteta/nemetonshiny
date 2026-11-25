# Mission 066 : itineraires sylvicoles dans le plan d'actions. Le jugement est
# celui du coeur (nemeton) : ces tests verifient que l'app le montre sans
# l'alterer, traduit chaque code, ne cache pas la donnee manquante, presente un
# rang et non une note, et dit « poids provisoires » / « catalogue en cours de
# sourcage ». Le catalogue de test est FICTIF ; sans reseau.

skip_si_noyau_ancien <- function() {
  testthat::skip_if_not(nemetonshiny:::.itin_noyau_disponible(),
                        "nemeton sans les itineraires (>= 2026.11.4)")
}

.it <- function(code, fam = "futaie_reguliere", domaine = list(),
                effets = list(P = list(sens = "+"), B = list(sens = "-")),
                statut = "source") {
  d <- list(essences_objectif = list(), ser = list(), protections_exclues = list())
  for (n in names(domaine)) d[[n]] <- domaine[[n]]
  list(code = code, famille_itineraire = fam,
       libelle = list(fr = paste("Libelle", gsub("_", " ", code)),
                      en = paste("Label", gsub("_", " ", code))),
       domaine = d, regles = list(rotation_ans = NULL), effets_attendus = effets,
       sources = if (statut == "source") list(list(ref = paste("Test", code), pages = "p. 1")) else list(),
       statut_source = statut)
}

# Chaque itineraire d'exclusion echoue sur un seul critere.
.catalogue_066 <- function() {
  l <- list(
    .it("production", effets = list(P = list(sens = "+"), E = list(sens = "+"),
                                    B = list(sens = "-"), N = list(sens = "-"))),
    .it("conservation", fam = "libre_evolution",
        effets = list(P = list(sens = "-"), E = list(sens = "-"),
                      B = list(sens = "+"), N = list(sens = "+"))),
    .it("x_station", domaine = list(p2_min = 8)),
    .it("x_ser", domaine = list(ser = list("B2"))),
    .it("x_essence", domaine = list(essences_objectif = list("09"))),
    .it("x_pente", domaine = list(pente_max_pct = 30)),
    .it("x_desserte", domaine = list(desserte_requise = TRUE, desserte_max_m = 300)),
    .it("x_source", statut = "a_sourcer")
  )
  names(l) <- vapply(l, `[[`, "", "code")
  l
}

.projet_066 <- function(avec_desserte = FALSE, ugf = TRUE) {
  ind <- sf::st_sf(
    ug_id = c("UGF1", "UGF2"),
    indicateur_p2_station = c(5, 12),
    .p2_ser = c("A1", "B2"),
    essence = c("03", "03"),
    pente_pct = c(50, 10),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0)))),
      sf::st_polygon(list(rbind(c(200, 0), c(300, 0), c(300, 100), c(200, 100), c(200, 0)))),
      crs = 2154))
  if (avec_desserte) ind$desserte_m <- c(100, 100)
  if (!ugf) ind <- ind[0, ]
  list(id = "p66", indicators_sf = ind)
}

.html <- function(res, lang = "fr") {
  as.character(nemetonshiny:::itineraires_rendre(res, nemetonshiny:::get_i18n(lang)))
}

# ---- Cas 1 : admissibles et exclus, chaque code traduit ------------------------

test_that("chaque code d'exclusion est traduit en FR et EN, cle <= 30 caracteres", {
  for (cd in nemetonshiny:::ITIN_RAISONS) {
    cle <- nemetonshiny:::.itin_cle_raison(cd)
    expect_lte(nchar(cle), 30L)
    for (l in c("fr", "en")) {
      i18n <- nemetonshiny:::get_i18n(l)
      expect_true(i18n$has(cle))
      txt <- i18n$t(cle)
      expect_true(nzchar(txt))
      expect_false(grepl(cd, txt, fixed = TRUE))   # jamais le code brut
    }
  }
  expect_equal(nemetonshiny:::.itin_cle_raison("code_futur"), "itin_r_inconnue")
  expect_true(is.na(nemetonshiny:::.itin_cle_raison(NA_character_)))
})

test_that("UGF avec admissibles et exclus : raisons traduites, aucun code brut", {
  skip_si_noyau_ancien()
  res <- nemetonshiny:::itineraires_calculer(.projet_066(), "profil_naturaliste",
                                             catalogue = .catalogue_066())
  expect_equal(res$statut, "ok")
  for (l in c("fr", "en")) {
    html <- .html(res, l)
    # aucun identifiant snake_case (code de raison, statut, cle i18n) a l'ecran
    expect_false(grepl("[a-z]+_[a-z_]+", html), info = l)
    expect_false(grepl("itin_|NA<|>NA", html), info = l)
  }
  tab <- nemetonshiny:::itineraires_table_admissibles(
    res$admissibles, res$catalogue, nemetonshiny:::get_i18n("fr"))
  expect_equal(nrow(tab), 2L * 8L)                       # aucune ligne perdue
  u1 <- tab[tab$ugf == "UGF1", ]
  expect_equal(u1$statut[u1$itineraire == "Libelle production"], "admissible")
  expect_equal(u1$statut[u1$itineraire == "Libelle x station"], "exclu")
  expect_equal(u1$raison[u1$itineraire == "Libelle x station"],
               nemetonshiny:::get_i18n("fr")$t("itin_r_station_insuffisante"))
  expect_equal(u1$raison[u1$itineraire == "Libelle x source"],
               nemetonshiny:::get_i18n("fr")$t("itin_r_source_manquante"))
  expect_equal(u1$raison[u1$itineraire == "Libelle x pente"],
               nemetonshiny:::get_i18n("fr")$t("itin_r_pente"))
  u2 <- tab[tab$ugf == "UGF2", ]
  expect_equal(u2$statut[u2$itineraire == "Libelle x pente"], "admissible")
})

# ---- Cas 2 : donnee manquante jamais silencieuse -------------------------------

test_that("sans desserte, la ligne reste avec « donnee manquante »", {
  skip_si_noyau_ancien()
  res <- nemetonshiny:::itineraires_calculer(.projet_066(), "profil_naturaliste",
                                             catalogue = .catalogue_066())
  expect_false(res$desserte)
  i18n <- nemetonshiny:::get_i18n("fr")
  tab <- nemetonshiny:::itineraires_table_admissibles(res$admissibles, res$catalogue, i18n)
  d <- tab[tab$itineraire == "Libelle x desserte", ]
  expect_equal(nrow(d), 2L)
  expect_true(all(d$statut == "manquant"))
  expect_true(all(d$raison == i18n$t("itin_r_donnee_manquante")))
  expect_true(all(d$etat == i18n$t("itin_etat_manque")))
  html <- .html(res)
  expect_match(html, i18n$t("itin_desserte_absente"), fixed = TRUE)
  # avec la colonne desserte_m, le verdict est rendu et la mention disparait
  res2 <- nemetonshiny:::itineraires_calculer(.projet_066(avec_desserte = TRUE),
                                              "profil_naturaliste",
                                              catalogue = .catalogue_066())
  expect_true(res2$desserte)
  tab2 <- nemetonshiny:::itineraires_table_admissibles(res2$admissibles, res2$catalogue, i18n)
  expect_true(all(tab2$statut[tab2$itineraire == "Libelle x desserte"] == "admissible"))
  expect_false(grepl(i18n$t("itin_desserte_absente"), .html(res2), fixed = TRUE))
})

# ---- Cas 3 : le profil change le classement ; un rang, jamais une note ---------

test_that("changer de profil change le classement, affiche comme un rang", {
  skip_si_noyau_ancien()
  cat <- .catalogue_066()
  i18n <- nemetonshiny:::get_i18n("fr")
  meilleur <- function(profil) {
    r <- nemetonshiny:::itineraires_calculer(.projet_066(), profil, catalogue = cat)
    t <- nemetonshiny:::itineraires_table_classement(r$comparaison, r$catalogue, i18n)
    t$itineraire[t$ugf == "UGF2" & t$rang == 1]
  }
  # un profil « production » et le naturaliste du coeur (poids du coeur, non modifies)
  expect_equal(meilleur("profil_industrie_bois"), "Libelle production")
  expect_equal(meilleur("profil_naturaliste"), "Libelle conservation")
  r <- nemetonshiny:::itineraires_calculer(.projet_066(), "profil_naturaliste", catalogue = cat)
  t <- nemetonshiny:::itineraires_table_classement(r$comparaison, r$catalogue, i18n)
  expect_false("score_global" %in% names(t))
  expect_true(all(c("rang", "evaluees", "gagnantes", "perdantes") %in% names(t)))
  expect_type(t$rang, "integer")
  html <- .html(r)
  expect_false(grepl("/ ?100|score_global|score", html))
  expect_match(html, i18n$t("itin_rang_note"), fixed = TRUE)
})

# ---- Cas 4 : mentions poids provisoires et catalogue en sourcage ---------------

test_that("« poids provisoires » et « catalogue en cours de sourcage » sont dits en FR et EN", {
  skip_si_noyau_ancien()
  res <- nemetonshiny:::itineraires_calculer(.projet_066(), "profil_naturaliste",
                                             catalogue = .catalogue_066())
  fr <- .html(res, "fr"); en <- .html(res, "en")
  expect_match(fr, "Poids provisoires")
  expect_match(en, "Provisional weights")
  expect_match(fr, "Catalogue en cours de sourçage")
  expect_match(en, "Catalogue being sourced")
  expect_match(fr, "1 itinéraire\\(s\\) sur 8")
  # catalogue entierement source : la mention de sourcage disparait, pas celle des poids
  tout_source <- lapply(.catalogue_066(), function(it) { it$statut_source <- "source"; it })
  res2 <- nemetonshiny:::itineraires_calculer(.projet_066(), "profil_naturaliste",
                                              catalogue = tout_source)
  expect_false(grepl("Catalogue en cours de sourçage", .html(res2)))
  expect_match(.html(res2), "Poids provisoires")
})

test_that("le catalogue reel du coeur passe de bout en bout, sans erreur", {
  skip_si_noyau_ancien()
  res <- nemetonshiny:::itineraires_calculer(.projet_066(), "profil_gestionnaire_expert")
  expect_equal(res$statut, "ok")
  expect_gt(nrow(res$admissibles), 0L)
  expect_match(.html(res), "Poids provisoires")
})

test_that("les profils proposes sont les quinze du coeur, libelles lisibles", {
  skip_si_noyau_ancien()
  ch <- nemetonshiny:::.itin_profils_choix("fr")
  expect_setequal(unname(ch), names(nemeton::profils_poids()))
  expect_length(ch, 15L)
  expect_false(any(grepl("^profil_", names(ch))))
  expect_true(all(nzchar(names(ch))))
})

# ---- Cas 5 : sans projet ou sans UGF, un message -------------------------------

test_that("sans projet, sans UGF ou cœur ancien : un message, pas d'erreur", {
  i18n <- nemetonshiny:::get_i18n("fr")
  skip_si_noyau_ancien()
  r0 <- nemetonshiny:::itineraires_calculer(NULL, "profil_naturaliste")
  expect_equal(r0$statut, "sans_projet")
  expect_match(.html(r0), i18n$t("itin_aucun_projet"), fixed = TRUE)
  r1 <- nemetonshiny:::itineraires_calculer(.projet_066(ugf = FALSE), "profil_naturaliste")
  expect_equal(r1$statut, "sans_ugf")
  expect_match(.html(r1), i18n$t("itin_aucune_ugf"), fixed = TRUE)
  r2 <- nemetonshiny:::itineraires_calculer(list(id = "x"), "profil_naturaliste")
  expect_equal(r2$statut, "sans_ugf")
})

test_that("une erreur du coeur devient un message, jamais une exception", {
  skip_si_noyau_ancien()
  res <- nemetonshiny:::itineraires_calculer(.projet_066(), "profil_inconnu",
                                             catalogue = .catalogue_066())
  expect_equal(res$statut, "erreur")
  expect_match(.html(res), nemetonshiny:::get_i18n("fr")$t("itin_erreur"), fixed = TRUE)
})

test_that("le module serveur rend le contenu et reagit au profil", {
  skip_si_noyau_ancien()
  etat <- shiny::reactiveValues(current_project = NULL, language = "fr")
  shiny::testServer(nemetonshiny:::mod_itineraires_server, args = list(app_state = etat), {
    session$setInputs(profil = "profil_naturaliste")
    expect_match(as.character(output$contenu$html), "Ouvrez un projet")
    etat$current_project <- .projet_066()
    session$flushReact()
    expect_match(as.character(output$contenu$html), "Poids provisoires")
    etat$language <- "en"
    session$flushReact()
    expect_match(as.character(output$contenu$html), "Provisional weights")
  })
})

test_that("l'UI du module et l'onglet du plan d'actions se construisent", {
  ui <- nemetonshiny:::mod_itineraires_ui("it")
  expect_match(as.character(htmltools::renderTags(ui)$html), "it-profil")
  ap <- as.character(htmltools::renderTags(nemetonshiny:::mod_action_plan_ui("ap"))$html)
  expect_match(ap, "ap-itineraires-profil")
})

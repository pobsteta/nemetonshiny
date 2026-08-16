# Tests for the shared UI helpers
# R/utils_ui.R - action_table_card()

test_that("action_table_card builds the collapsible header pattern", {
  card <- nemetonshiny:::action_table_card(
    "demo_collapse",
    "Tableau des actions",
    shiny::actionButton("demo_btn", "Faire")
  )

  html <- as.character(card)

  # En-tete vert cliquable, qui pilote le corps par son id.
  expect_true(grepl("card-header bg-success text-white", html, fixed = TRUE))
  expect_true(grepl('data-bs-toggle="collapse"', html, fixed = TRUE))
  expect_true(grepl('data-bs-target="#demo_collapse"', html, fixed = TRUE))
  expect_true(grepl('id="demo_collapse"', html, fixed = TRUE))
  expect_true(grepl("Tableau des actions", html, fixed = TRUE))
  expect_true(grepl("collapse-icon", html, fixed = TRUE))

  # Le contenu passe en ... atterrit dans le corps.
  expect_true(grepl("demo_btn", html, fixed = TRUE))
})


test_that("action_table_card honours the open flag", {
  opened <- as.character(
    nemetonshiny:::action_table_card("c1", "T", open = TRUE, "x")
  )
  closed <- as.character(
    nemetonshiny:::action_table_card("c2", "T", open = FALSE, "x")
  )

  expect_true(grepl('class="collapse show"', opened, fixed = TRUE))
  expect_true(grepl('aria-expanded="true"', opened, fixed = TRUE))

  expect_false(grepl('class="collapse show"', closed, fixed = TRUE))
  expect_true(grepl('class="collapse"', closed, fixed = TRUE))
  expect_true(grepl('aria-expanded="false"', closed, fixed = TRUE))
})


test_that("action_table_card lets the caller pick classes and icon", {
  html <- as.character(
    nemetonshiny:::action_table_card(
      "c3", "T",
      icon = "box-arrow-up",
      card_class = "card mt-3",
      body_class = "card-body p-2",
      "x"
    )
  )

  expect_true(grepl('class="card mt-3"', html, fixed = TRUE))
  expect_true(grepl('class="card-body p-2"', html, fixed = TRUE))
})


# ==============================================================================
# The three views that must share the block
# ==============================================================================

test_that("Desserte groups every action under the action table", {
  html <- as.character(nemetonshiny:::mod_desserte_ui("de"))

  expect_true(grepl("de-dess_actions_collapse", html, fixed = TRUE))

  # Les six actions du panneau droit sont DANS le bloc, pas a cote.
  body <- sub('.*id="de-dess_actions_collapse"', "", html)
  for (btn in c("de-run_typage", "de-run_integrite", "de-run_optim",
                "de-run_osm", "de-run_detect", "de-export_gpkg")) {
    expect_true(grepl(btn, body, fixed = TRUE), info = btn)
  }

  # Le bilan et l'opacite ne sont PAS des actions : ils restent au-dessus.
  head_part <- sub('id="de-dess_actions_collapse".*', "", html)
  expect_true(grepl("de-opacity", head_part, fixed = TRUE))
  expect_true(grepl("de-summary", head_part, fixed = TRUE))
})


test_that("reGeneration groups its exports under the action table", {
  html <- as.character(nemetonshiny:::mod_regeneration_ui("rg"))

  expect_true(grepl("rg-regen_actions_collapse", html, fixed = TRUE))

  body <- sub('.*id="rg-regen_actions_collapse"', "", html)
  for (btn in c("rg-export_terrain", "rg-export_gpkg",
                "rg-export_pdf", "rg-persist_db")) {
    expect_true(grepl(btn, body, fixed = TRUE), info = btn)
  }
})


test_that("the Plan d'actions reference block still carries its actions", {
  html <- as.character(nemetonshiny:::mod_action_plan_ui("ap"))

  expect_true(grepl("ap-actions_collapse", html, fixed = TRUE))

  body <- sub('.*id="ap-actions_collapse"', "", html)
  for (btn in c("ap-show_history", "ap-generate_all", "ap-add_action",
                "ap-export_terrain", "ap-download_gpkg", "ap-download_pdf",
                "ap-save_db")) {
    expect_true(grepl(btn, body, fixed = TRUE), info = btn)
  }
})

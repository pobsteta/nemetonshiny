# Tests for the planificateur expert profile and action-plan prompt helpers.

test_that("planificateur profile is loaded with fr/en label and prompt", {
  profiles <- nemetonshiny:::get_expert_profiles()
  expect_true("planificateur" %in% names(profiles))
  prof <- profiles[["planificateur"]]
  expect_true(all(c("label", "prompt") %in% names(prof)))
  expect_true(all(c("fr", "en") %in% names(prof$label)))
  expect_true(all(c("fr", "en") %in% names(prof$prompt)))
  expect_true(nchar(prof$prompt$fr) > 100)
  expect_true(nchar(prof$prompt$en) > 100)
})

test_that("build_action_plan_prompt embeds horizon, ug_ids and comments", {
  comments <- list(
    synthesis = "Le massif présente un déficit de biodiversité.",
    families = list(
      C = "Stock de carbone correct.",
      B = "Connectivité insuffisante."
    )
  )
  prompt <- nemetonshiny:::build_action_plan_prompt(
    comments,
    ug_ids = c("ug_a", "ug_b"),
    horizon_annees = 15L,
    language = "français",
    scope = "all"
  )
  expect_type(prompt, "character")
  expect_match(prompt, "ug_a", fixed = TRUE)
  expect_match(prompt, "ug_b", fixed = TRUE)
  expect_match(prompt, "15", fixed = TRUE)
  expect_match(prompt, "déficit de biodiversité", fixed = TRUE)
  expect_match(prompt, "family_B", fixed = TRUE)
  expect_match(prompt, "JSON schema", fixed = TRUE)
  # Balance hint must be embedded in the user prompt.
  expect_match(prompt, "bilan cumule", fixed = TRUE)
  # Schema documents both monetary fields.
  expect_match(prompt, "cout_eur", fixed = TRUE)
  expect_match(prompt, "revenu_eur", fixed = TRUE)
})

test_that("build_action_plan_prompt restricts to a single ug_id when scope=by_ug", {
  prompt <- nemetonshiny:::build_action_plan_prompt(
    comments = list(synthesis = "x", families = list()),
    ug_ids = c("ug_a", "ug_b"),
    scope = "by_ug",
    ug_id = "ug_a",
    language = "English"
  )
  expect_match(prompt, "Restrict strictly to UGF 'ug_a'", fixed = TRUE)
})

test_that("parse_action_plan_response keeps only valid actions and forces statut=proposee", {
  raw <- jsonlite::toJSON(list(actions = list(
    list(ug_id = "ug_a", type = "eclaircie", annee_cible = 3L,
         priorite = "moyenne",
         objectifs_lies = list("C"),
         source = list(origine = "synthesis",
                       extrait_texte = "..."),
         statut = "planifiee"),  # should be overwritten
    list(ug_id = "ghost", type = "plantation", annee_cible = 4L,
         priorite = "haute"),    # invalid: unknown ug_id
    list(ug_id = "ug_a", type = "voodoo", annee_cible = 1L,
         priorite = "basse")     # invalid: bad type
  )), auto_unbox = TRUE)
  res <- nemetonshiny:::parse_action_plan_response(
    as.character(raw),
    ug_ids = c("ug_a"),
    horizon_annees = 10L
  )
  expect_length(res$actions, 1L)
  expect_equal(res$actions[[1]]$statut, "proposee")
  expect_length(res$errors, 2L)
})

test_that("parse_action_plan_response strips markdown code fences", {
  payload <- '{"actions":[{"ug_id":"ug_x","type":"observation","annee_cible":1,"priorite":"basse"}]}'
  fenced <- paste0("Sure, here you go:\n```json\n", payload, "\n```")
  res <- nemetonshiny:::parse_action_plan_response(
    fenced, ug_ids = "ug_x", horizon_annees = 5L
  )
  expect_length(res$actions, 1L)
  expect_equal(res$actions[[1]]$ug_id, "ug_x")
})

test_that("build_action_plan_chat_prompt embeds question + context + plan summary", {
  prompt <- nemetonshiny:::build_action_plan_chat_prompt(
    question = "Ajoute une eclaircie sur ug_a en annee 4.",
    ctx = list(
      comments = list(synthesis = "synth", families = list(C = "fam C")),
      ug_ids = c("ug_a", "ug_b"),
      horizon = 12L
    ),
    plan_summary_json = '[{"id":"act_1","ug_id":"ug_a"}]',
    language = "francais"  # falls into the English branch
  )
  expect_type(prompt, "character")
  expect_match(prompt, "ug_a", fixed = TRUE)
  expect_match(prompt, "Ajoute une eclaircie", fixed = TRUE)
  expect_match(prompt, "act_1", fixed = TRUE)
  expect_match(prompt, "12", fixed = TRUE)
})

test_that("build_action_plan_chat_prompt embeds the full JSON schema + econ hint", {
  # Regression: the refine-chat flow used to reference the generation
  # prompt by function name without inlining the schema, so the LLM
  # left surface/volume/cost fields empty when refining a plan.
  prompt <- nemetonshiny:::build_action_plan_chat_prompt(
    question = "Ajoute une eclaircie sur ug_a en annee 4.",
    ctx = list(
      comments = list(synthesis = "synth", families = list(C = "fam C")),
      ug_ids = c("ug_a", "ug_b"),
      horizon = 12L
    ),
    plan_summary_json = '[{"id":"act_1","ug_id":"ug_a"}]',
    language = "français"
  )
  # Full schema must be inlined so the LLM knows about `quantite`.
  expect_match(prompt, "JSON schema", fixed = TRUE)
  expect_match(prompt, "quantite", fixed = TRUE)
  expect_match(prompt, "volume_m3", fixed = TRUE)
  expect_match(prompt, "surface_ha", fixed = TRUE)
  expect_match(prompt, "nb_tiges", fixed = TRUE)
  expect_match(prompt, "cout_eur", fixed = TRUE)
  expect_match(prompt, "revenu_eur", fixed = TRUE)
  # Economic hint must travel with the chat prompt too.
  expect_match(prompt, "bilan cumule", fixed = TRUE)
  # Schema HORIZON placeholder must be substituted with the actual horizon.
  expect_false(grepl("1..HORIZON", prompt, fixed = TRUE))
  expect_match(prompt, "1..12", fixed = TRUE)
})

test_that(".action_plan_json_schema lists every persisted quantite field", {
  schema <- nemetonshiny:::.action_plan_json_schema()
  for (field in c("volume_m3", "surface_ha", "nb_tiges",
                  "rdi", "cout_eur", "revenu_eur")) {
    expect_match(schema, field, fixed = TRUE,
                 info = sprintf("schema must document field '%s'", field))
  }
})

test_that("parse_action_plan_response handles empty/garbage input gracefully", {
  expect_equal(
    nemetonshiny:::parse_action_plan_response("", ug_ids = "ug_a")$actions,
    list()
  )
  expect_equal(
    nemetonshiny:::parse_action_plan_response("not json", ug_ids = "ug_a")$actions,
    list()
  )
})

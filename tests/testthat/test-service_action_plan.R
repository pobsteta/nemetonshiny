# Tests for service_action_plan.R

# ---- Helpers ----------------------------------------------------------

make_action <- function(...) {
  defaults <- list(
    ug_id = "ug_1",
    type = "eclaircie",
    annee_cible = 3L,
    priorite = "moyenne",
    statut = "proposee"
  )
  utils::modifyList(defaults, list(...))
}

# ---- Empty plan -------------------------------------------------------

test_that("init_empty_action_plan returns a well-formed plan", {
  plan <- nemetonshiny:::init_empty_action_plan("proj_x", horizon_annees = 10L)
  expect_equal(plan$project_id, "proj_x")
  expect_equal(plan$horizon_annees, 10L)
  expect_equal(plan$version, nemetonshiny:::ACTION_PLAN_SCHEMA_VERSION)
  expect_equal(plan$actions, list())
  expect_equal(plan$audit, list())
})

# ---- Validation -------------------------------------------------------

test_that("validate_action accepts a minimal valid action", {
  v <- nemetonshiny:::validate_action(make_action(),
                                      ug_ids = c("ug_1", "ug_2"),
                                      horizon_annees = 5L)
  expect_true(v$valid)
  expect_length(v$errors, 0L)
})

test_that("validate_action flags missing required fields", {
  bad <- list(ug_id = "ug_1")
  v <- nemetonshiny:::validate_action(bad, ug_ids = "ug_1", horizon_annees = 5L)
  expect_false(v$valid)
  expect_true(any(grepl("missing field", v$errors)))
})

test_that("validate_action rejects unknown ug_id", {
  v <- nemetonshiny:::validate_action(make_action(ug_id = "ghost"),
                                      ug_ids = c("ug_1"),
                                      horizon_annees = 5L)
  expect_false(v$valid)
  expect_true(any(grepl("unknown ug_id", v$errors)))
})

test_that("validate_action rejects out-of-horizon annee_cible", {
  v <- nemetonshiny:::validate_action(make_action(annee_cible = 99L),
                                      ug_ids = "ug_1",
                                      horizon_annees = 20L)
  expect_false(v$valid)
  expect_true(any(grepl("annee_cible", v$errors)))
})

test_that("validate_action rejects invalid type / statut / priorite", {
  expect_false(nemetonshiny:::validate_action(make_action(type = "voodoo"),
                                              ug_ids = "ug_1")$valid)
  expect_false(nemetonshiny:::validate_action(make_action(statut = "ghost"),
                                              ug_ids = "ug_1")$valid)
  expect_false(nemetonshiny:::validate_action(make_action(priorite = "extreme"),
                                              ug_ids = "ug_1")$valid)
})

test_that("validate_action requires type_libre when type='autre'", {
  v1 <- nemetonshiny:::validate_action(make_action(type = "autre"),
                                       ug_ids = "ug_1")
  expect_false(v1$valid)
  v2 <- nemetonshiny:::validate_action(make_action(type = "autre",
                                                   type_libre = "bouturage"),
                                       ug_ids = "ug_1")
  expect_true(v2$valid)
})

test_that("validate_action checks family codes in objectifs_lies", {
  v <- nemetonshiny:::validate_action(
    make_action(objectifs_lies = c("C", "ZZ")),
    ug_ids = "ug_1"
  )
  expect_false(v$valid)
  expect_true(any(grepl("family code", v$errors)))
})

# ---- Status transitions ----------------------------------------------

test_that("is_valid_status_transition enforces the kanban", {
  expect_true(nemetonshiny:::is_valid_status_transition("proposee", "validee"))
  expect_true(nemetonshiny:::is_valid_status_transition("validee",  "planifiee"))
  expect_true(nemetonshiny:::is_valid_status_transition("planifiee", "realisee"))
  expect_true(nemetonshiny:::is_valid_status_transition("planifiee", "validee"))
  # Realisee is terminal
  expect_false(nemetonshiny:::is_valid_status_transition("realisee", "validee"))
  # Cannot jump from proposee to realisee
  expect_false(nemetonshiny:::is_valid_status_transition("proposee", "realisee"))
  # Identity is always valid
  expect_true(nemetonshiny:::is_valid_status_transition("validee", "validee"))
})

# ---- CRUD -------------------------------------------------------------

test_that("add_action_to_plan appends action and audit entry", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  plan <- nemetonshiny:::add_action_to_plan(plan, make_action(),
                                            ug_ids = "ug_1", user = "alice")
  expect_length(plan$actions, 1L)
  expect_length(plan$audit, 1L)
  expect_equal(plan$audit[[1]]$op, "create")
  expect_equal(plan$audit[[1]]$user, "alice")
  expect_match(plan$actions[[1]]$id, "^act_")
  expect_equal(plan$actions[[1]]$cree_par, "alice")
})

test_that("add_action_to_plan rejects invalid actions", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  expect_error(
    nemetonshiny:::add_action_to_plan(plan,
                                      make_action(type = "voodoo"),
                                      ug_ids = "ug_1"),
    regexp = "invalid"
  )
})

test_that("update_action_in_plan logs field changes and bumps modifie_*", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  plan <- nemetonshiny:::add_action_to_plan(plan, make_action(),
                                            ug_ids = "ug_1", user = "alice")
  aid  <- plan$actions[[1]]$id
  plan <- nemetonshiny:::update_action_in_plan(plan, aid,
                                               list(priorite = "haute"),
                                               ug_ids = "ug_1", user = "bob")
  act <- nemetonshiny:::get_action_by_id(plan, aid)
  expect_equal(act$priorite, "haute")
  expect_equal(act$modifie_par, "bob")
  upd <- Filter(function(e) e$op == "update", plan$audit)
  expect_length(upd, 1L)
  expect_equal(upd[[1]]$champ, "priorite")
  expect_equal(upd[[1]]$ancien, "moyenne")
  expect_equal(upd[[1]]$nouveau, "haute")
})

test_that("update_action_in_plan rejects invalid status transition", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  plan <- nemetonshiny:::add_action_to_plan(plan, make_action(),
                                            ug_ids = "ug_1")
  aid <- plan$actions[[1]]$id
  expect_error(
    nemetonshiny:::update_action_in_plan(plan, aid,
                                         list(statut = "realisee"),
                                         ug_ids = "ug_1"),
    regexp = "transition"
  )
})

test_that("delete_action_from_plan removes and audits", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  plan <- nemetonshiny:::add_action_to_plan(plan, make_action(),
                                            ug_ids = "ug_1")
  aid <- plan$actions[[1]]$id
  plan <- nemetonshiny:::delete_action_from_plan(plan, aid, user = "carol")
  expect_length(plan$actions, 0L)
  del <- Filter(function(e) e$op == "delete", plan$audit)
  expect_length(del, 1L)
  expect_equal(del[[1]]$user, "carol")
})

test_that("bulk_upsert inserts new actions and updates existing", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  plan <- nemetonshiny:::add_action_to_plan(plan,
                                            make_action(id = "act_seed"),
                                            ug_ids = "ug_1")
  plan <- nemetonshiny:::bulk_upsert_actions(plan, list(
    list(id = "act_seed", priorite = "haute",
         ug_id = "ug_1", type = "eclaircie", annee_cible = 3L,
         statut = "proposee"),
    make_action(ug_id = "ug_2", annee_cible = 2L)
  ), ug_ids = c("ug_1", "ug_2"))
  expect_length(plan$actions, 2L)
  expect_equal(nemetonshiny:::get_action_by_id(plan, "act_seed")$priorite,
               "haute")
})

# ---- Filtering --------------------------------------------------------

test_that("filter_actions narrows by ug / annee / type / statut / famille", {
  plan <- nemetonshiny:::init_empty_action_plan("p", horizon_annees = 10L)
  plan <- nemetonshiny:::add_action_to_plan(plan,
            make_action(ug_id = "ug_1", type = "eclaircie",
                        annee_cible = 3L,
                        objectifs_lies = c("C", "B")),
            ug_ids = c("ug_1", "ug_2"))
  plan <- nemetonshiny:::add_action_to_plan(plan,
            make_action(ug_id = "ug_2", type = "plantation",
                        annee_cible = 5L,
                        objectifs_lies = c("P")),
            ug_ids = c("ug_1", "ug_2"))

  expect_length(nemetonshiny:::filter_actions(plan, ug_id = "ug_1"), 1L)
  expect_length(nemetonshiny:::filter_actions(plan, type = "plantation"), 1L)
  expect_length(nemetonshiny:::filter_actions(plan, annee_min = 4L), 1L)
  expect_length(nemetonshiny:::filter_actions(plan, famille = "B"), 1L)
  expect_length(nemetonshiny:::filter_actions(plan, statut = "proposee"), 2L)
})

# ---- Round-trip JSON --------------------------------------------------

test_that("save_action_plan + load_action_plan round-trip preserves the plan", {
  tmp_root <- file.path(tempdir(), "nemeton_action_plan_test")
  unlink(tmp_root, recursive = TRUE)
  dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)

  pid <- "proj_test"
  proj_dir <- file.path(tmp_root, pid)
  dir.create(file.path(proj_dir, "data"), recursive = TRUE,
             showWarnings = FALSE)

  with_mocked_bindings(
    get_project_path = function(project_id) {
      if (project_id == pid) proj_dir else NULL
    },
    {
      plan <- nemetonshiny:::init_empty_action_plan(pid, horizon_annees = 15L)
      plan <- nemetonshiny:::add_action_to_plan(plan,
                make_action(ug_id = "ug_A", annee_cible = 2L,
                            objectifs_lies = c("C")),
                ug_ids = c("ug_A"))
      expect_true(nemetonshiny:::save_action_plan(pid, plan))

      loaded <- nemetonshiny:::load_action_plan(pid)
      expect_equal(loaded$horizon_annees, 15L)
      expect_length(loaded$actions, 1L)
      expect_equal(loaded$actions[[1]]$ug_id, "ug_A")
    }
  )

  unlink(tmp_root, recursive = TRUE)
})

# ---- DataFrame export -------------------------------------------------

test_that("audit_to_dataframe collapses entries to a tidy data.frame", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  plan <- nemetonshiny:::add_action_to_plan(plan, make_action(),
                                            ug_ids = "ug_1", user = "alice")
  aid <- plan$actions[[1]]$id
  plan <- nemetonshiny:::update_action_in_plan(plan, aid,
                                               list(priorite = "haute"),
                                               ug_ids = "ug_1", user = "bob")
  audit <- nemetonshiny:::get_action_audit(plan, aid)
  df <- nemetonshiny:::audit_to_dataframe(audit)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_true(all(c("ts", "user", "op", "champ",
                    "ancien", "nouveau") %in% names(df)))
  upd <- df[df$op == "update", , drop = FALSE]
  expect_equal(upd$champ[1], "priorite")
  expect_equal(upd$ancien[1], "moyenne")
  expect_equal(upd$nouveau[1], "haute")
})

test_that("actions_to_dataframe returns one row per action with stable cols", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  plan <- nemetonshiny:::add_action_to_plan(plan,
            c(make_action(),
              list(quantite = list(volume_m3 = 12.5, surface_ha = 0.5),
                   objectifs_lies = c("C", "B"))),
            ug_ids = "ug_1")
  df <- nemetonshiny:::actions_to_dataframe(plan)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_true(all(c("ug_id", "annee_cible", "volume_m3",
                    "objectifs_lies", "cout_eur",
                    "revenu_eur") %in% names(df)))
  expect_equal(df$volume_m3[1], 12.5)
  expect_equal(df$objectifs_lies[1], "C,B")
})

test_that("actions_to_dataframe reads cout_eur AND revenu_eur from quantite", {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  plan <- nemetonshiny:::add_action_to_plan(
    plan,
    c(make_action(),
      list(quantite = list(cout_eur = 250, revenu_eur = 1800))),
    ug_ids = "ug_1"
  )
  df <- nemetonshiny:::actions_to_dataframe(plan)
  expect_equal(df$cout_eur[1], 250)
  expect_equal(df$revenu_eur[1], 1800)
})

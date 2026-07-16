# test-llm_prompts.R
# Tests for LLM prompt generation functions
# R/llm_prompts.R

library(testthat)

# ==============================================================================
# EXPERT_PROFILES - Structure and Content Tests
# ==============================================================================

test_that("get_expert_profiles() contains all expected expert profiles", {
  profiles <- nemetonshiny:::get_expert_profiles()

  expect_type(profiles, "list")

  # Check all expected profiles exist
  expected_profiles <- c("generalist", "owner", "manager",
                         "producer", "hunter")
  for (profile in expected_profiles) {
    expect_true(profile %in% names(profiles),
                info = sprintf("Missing profile: %s", profile))
  }
})

test_that("get_expert_profiles() has label and prompt with fr/en for all profiles", {
  profiles <- nemetonshiny:::get_expert_profiles()

  for (profile_name in names(profiles)) {
    profile <- profiles[[profile_name]]
    expect_true("label" %in% names(profile),
                info = sprintf("Missing label for profile: %s", profile_name))
    expect_true("prompt" %in% names(profile),
                info = sprintf("Missing prompt for profile: %s", profile_name))
    expect_true("fr" %in% names(profile$prompt),
                info = sprintf("Missing French prompt for profile: %s", profile_name))
    expect_true("en" %in% names(profile$prompt),
                info = sprintf("Missing English prompt for profile: %s", profile_name))
  }
})

test_that("get_expert_profiles() prompts are non-empty strings", {
  profiles <- nemetonshiny:::get_expert_profiles()

  for (profile_name in names(profiles)) {
    profile <- profiles[[profile_name]]

    expect_type(profile$prompt$fr, "character")
    expect_type(profile$prompt$en, "character")
    expect_true(nchar(profile$prompt$fr) > 50,
                info = sprintf("French prompt too short for profile: %s", profile_name))
    expect_true(nchar(profile$prompt$en) > 50,
                info = sprintf("English prompt too short for profile: %s", profile_name))
  }
})

test_that("get_expert_profiles() French prompts contain French-specific content", {
  profiles <- nemetonshiny:::get_expert_profiles()

  # French prompts should contain French words/phrases
  french_indicators <- c("Tu es", "analyse", "Fournis", "orient")

  for (profile_name in names(profiles)) {
    fr_prompt <- profiles[[profile_name]]$prompt$fr
    has_french <- any(vapply(french_indicators, function(x) grepl(x, fr_prompt, ignore.case = TRUE), logical(1)))
    expect_true(has_french,
                info = sprintf("French prompt for %s lacks French content", profile_name))
  }
})

test_that("get_expert_profiles() English prompts contain English-specific content", {
  profiles <- nemetonshiny:::get_expert_profiles()

  # English prompts should contain English words/phrases
  english_indicators <- c("You are", "analyze", "Provide", "focused")

  for (profile_name in names(profiles)) {
    en_prompt <- profiles[[profile_name]]$prompt$en
    has_english <- any(vapply(english_indicators, function(x) grepl(x, en_prompt, ignore.case = TRUE), logical(1)))
    expect_true(has_english,
                info = sprintf("English prompt for %s lacks English content", profile_name))
  }
})

# ==============================================================================
# BUILD_SYSTEM_PROMPT - Function Tests
# ==============================================================================

test_that("build_system_prompt returns French prompt for French language", {
  result <- nemetonshiny:::build_system_prompt("fran\u00e7ais", expert = "generalist")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  # Should end with language instruction

  expect_match(result, "fran\u00e7ais", fixed = TRUE)
})

test_that("build_system_prompt returns English prompt for English language", {
  result <- nemetonshiny:::build_system_prompt("English", expert = "generalist")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_match(result, "English", fixed = TRUE)
})

test_that("build_system_prompt uses different expert profiles", {
  result_generalist <- nemetonshiny:::build_system_prompt("English", expert = "generalist")
  result_owner <- nemetonshiny:::build_system_prompt("English", expert = "owner")
  result_hunter <- nemetonshiny:::build_system_prompt("English", expert = "hunter")

  # All should be different

  expect_false(result_generalist == result_owner)
  expect_false(result_generalist == result_hunter)
  expect_false(result_owner == result_hunter)
})

test_that("build_system_prompt falls back to generalist for invalid expert",
{
  result_invalid <- nemetonshiny:::build_system_prompt("English", expert = "invalid_expert")
  result_generalist <- nemetonshiny:::build_system_prompt("English", expert = "generalist")

  # Should fall back to generalist
  expect_equal(result_invalid, result_generalist)
})

test_that("build_system_prompt handles all expert profiles", {
  experts <- c("generalist", "owner", "manager",
               "producer", "hunter")

  for (expert in experts) {
    result_fr <- nemetonshiny:::build_system_prompt("fran\u00e7ais", expert = expert)
    result_en <- nemetonshiny:::build_system_prompt("English", expert = expert)

    expect_type(result_fr, "character")
    expect_type(result_en, "character")
    expect_true(nchar(result_fr) > 50,
                info = sprintf("French prompt too short for expert: %s", expert))
    expect_true(nchar(result_en) > 50,
                info = sprintf("English prompt too short for expert: %s", expert))
  }
})

test_that("build_system_prompt appends language instruction", {
  result_fr <- nemetonshiny:::build_system_prompt("fran\u00e7ais", expert = "generalist")
  result_en <- nemetonshiny:::build_system_prompt("English", expert = "generalist")

  # Both should contain "Reponds en" followed by language
  expect_match(result_fr, "R\u00e9ponds en fran\u00e7ais", fixed = TRUE)
  expect_match(result_en, "R\u00e9ponds en English", fixed = TRUE)
})

# ==============================================================================
# BUILD_ANALYSIS_PROMPT - Function Tests
# ==============================================================================

test_that("build_analysis_prompt generates prompt from family config and data", {
  # Create test data
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),
    C1 = c(50, 60, 55),
    C2 = c(70, 75, 72)
  )

  # Get Carbon family config
  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)

  # Should mention family name
  expect_match(result, "Carbon", ignore.case = TRUE)
  # Should mention number of parcels
  expect_match(result, "3")
  # Should include statistics
  expect_match(result, "mean")
})

test_that("build_analysis_prompt handles sf objects correctly", {
  # Create sf test data
  units <- create_test_units(n_features = 3)
  units$C1 <- c(50, 60, 55)
  units$C2 <- c(70, 75, 72)

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  # Should not error with sf object
  result <- nemetonshiny:::build_analysis_prompt(family_config, units, "English")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("build_analysis_prompt calculates correct statistics", {
  test_data <- data.frame(
    id = c("P1", "P2", "P3", "P4", "P5"),
    C1 = c(10, 20, 30, 40, 50)  # min=10, max=50, mean=30, sd~15.81
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  # Check statistics are included
  expect_match(result, "min=10")
  expect_match(result, "max=50")
  expect_match(result, "mean=30")
  expect_match(result, "n=5")
})

test_that("build_analysis_prompt handles NA values in statistics", {
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),
    C1 = c(50, NA, 70),
    C2 = c(NA, NA, NA)  # All NA
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  expect_type(result, "character")
  # Should handle all-NA column gracefully
  expect_match(result, "no data|n=0|n=2", ignore.case = TRUE)
})

test_that("build_analysis_prompt works with French language", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    C1 = c(50, 60)
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "fran\u00e7ais")

  expect_type(result, "character")
  # Should use French family name
  expect_match(result, "Carbone|Vitalit", ignore.case = TRUE)
})

test_that("build_analysis_prompt includes family code", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    W1 = c(10, 15)
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$W

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  # Should include family code
  expect_match(result, "code: W|W\\)", ignore.case = TRUE)
})

test_that("build_analysis_prompt handles different families", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    indicator = c(50, 60)
  )

  families_to_test <- c("C", "B", "W", "R", "S")

  for (fam_code in families_to_test) {
    family_config <- nemetonshiny:::INDICATOR_FAMILIES[[fam_code]]

    result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

    expect_type(result, "character")
    expect_true(nchar(result) > 0,
                info = sprintf("Empty prompt for family: %s", fam_code))
  }
})

test_that("build_analysis_prompt calculates CV correctly", {
  # CV = coefficient of variation = (sd/mean) * 100
  test_data <- data.frame(
    id = c("P1", "P2", "P3", "P4"),
    C1 = c(100, 100, 100, 100)  # No variation, CV should be 0
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  # CV should be 0% for constant values
  expect_match(result, "CV=0%|CV=0\\.0%")
})

# ==============================================================================
# BUILD_SYNTHESIS_PROMPT - Function Tests
# ==============================================================================

test_that("build_synthesis_prompt generates prompt from family scores", {
  # Create test data with family scores
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),
    famille_carbone = c(60, 70, 65),
    famille_biodiversite = c(50, 55, 52),
    famille_eau = c(40, 45, 42)
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)

  # Should mention number of parcels
  expect_match(result, "3")
  # Should include global score
  expect_match(result, "Global score|Score global", ignore.case = TRUE)
})

test_that("build_synthesis_prompt handles sf objects correctly", {
  # Create sf test data with family scores
  units <- create_test_units(n_features = 3)
  units$famille_carbone <- c(60, 70, 65)
  units$famille_biodiversite <- c(50, 55, 52)

  # Should not error with sf object
  result <- nemetonshiny:::build_synthesis_prompt(units, "English")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("build_synthesis_prompt calculates global score correctly", {
  # Create test data with known family scores
  test_data <- data.frame(
    id = c("P1", "P2"),
    famille_carbone = c(60, 80),  # mean = 70
    famille_biodiversite = c(40, 60)   # mean = 50
  )
  # Global = mean of family means = (70 + 50) / 2 = 60

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  # Should show global score around 60
  expect_match(result, "60|60\\.0")
})

test_that("build_synthesis_prompt includes family names from config", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    famille_carbone = c(60, 70),
    famille_eau = c(40, 50)
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  # Should include family names
  expect_match(result, "Carbon|Vitality", ignore.case = TRUE)
  expect_match(result, "Water", ignore.case = TRUE)
})

test_that("build_synthesis_prompt works with French language", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    famille_carbone = c(60, 70),
    famille_biodiversite = c(50, 55)
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "fran\u00e7ais")

  expect_type(result, "character")
  # Should use French text
  expect_match(result, "synth\u00e8se|parcelles|Score global", ignore.case = TRUE)
})

test_that("build_synthesis_prompt handles NA values in family scores", {
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),
    famille_carbone = c(60, NA, 70),
    famille_biodiversite = c(NA, NA, NA)  # All NA
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  expect_type(result, "character")
  # Should handle NA gracefully
  expect_match(result, "no data|n=0|n=2", ignore.case = TRUE)
})

test_that("build_synthesis_prompt includes min/max/mean statistics", {
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),
    famille_carbone = c(40, 60, 80)  # min=40, max=80, mean=60
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  # Should include statistics
  expect_match(result, "mean=60|mean=60\\.0")
  expect_match(result, "min=40|min=40\\.0")
  expect_match(result, "max=80|max=80\\.0")
})

test_that("build_synthesis_prompt adds recommendations request in English", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    famille_carbone = c(60, 70)
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  # Should ask for analysis
  expect_match(result, "strengths|weaknesses|recommendations", ignore.case = TRUE)
})

test_that("build_synthesis_prompt adds recommendations request in French", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    famille_carbone = c(60, 70)
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "fran\u00e7ais")

  # Should ask for analysis in French
  expect_match(result, "points forts|faiblesses|recommandations", ignore.case = TRUE)
})

test_that("build_synthesis_prompt handles unknown family codes gracefully", {
  # Create data with a family column that doesn't match config
  test_data <- data.frame(
    id = c("P1", "P2"),
    famille_carbone = c(60, 70),
    family_X = c(30, 40)  # Unknown family
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  expect_type(result, "character")
  # Should mention unknown family
  expect_match(result, "unknown|X", ignore.case = TRUE)
})

test_that("build_synthesis_prompt handles all 12 families", {
  # Create data with all family codes
  test_data <- data.frame(
    id = c("P1", "P2"),
    famille_carbone = c(60, 70),
    famille_biodiversite = c(50, 55),
    famille_eau = c(40, 45),
    famille_air = c(55, 60),
    famille_sol = c(45, 50),
    famille_paysage = c(65, 70),
    famille_temporel = c(35, 40),
    famille_risque = c(70, 75),
    famille_social = c(50, 55),
    famille_production = c(60, 65),
    famille_energie = c(55, 60),
    famille_naturalite = c(45, 50)
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  expect_type(result, "character")
  expect_true(nchar(result) > 200)  # Should be substantial with all families
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("Prompt generation workflow works end-to-end for family analysis", {
  # Create realistic test data
  units <- create_test_units(n_features = 5)
  units$C1 <- c(150, 180, 160, 170, 165)
  units$C2 <- c(0.7, 0.8, 0.75, 0.78, 0.72)

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  # Generate prompts
  system_prompt <- nemetonshiny:::build_system_prompt("English", expert = "generalist")
  analysis_prompt <- nemetonshiny:::build_analysis_prompt(family_config, units, "English")

  expect_type(system_prompt, "character")
  expect_type(analysis_prompt, "character")
  expect_true(nchar(system_prompt) > 50)
  expect_true(nchar(analysis_prompt) > 50)
})

test_that("Prompt generation workflow works end-to-end for synthesis", {
  # Create family scores data
  units <- create_test_units(n_features = 4)
  units$famille_carbone <- c(65, 72, 68, 70)
  units$famille_biodiversite <- c(45, 52, 48, 50)
  units$famille_eau <- c(55, 62, 58, 60)
  units$famille_risque <- c(75, 82, 78, 80)

  # Generate prompts
  system_prompt <- nemetonshiny:::build_system_prompt("English", expert = "owner")
  synthesis_prompt <- nemetonshiny:::build_synthesis_prompt(units, "English")

  expect_type(system_prompt, "character")
  expect_type(synthesis_prompt, "character")
  expect_true(nchar(system_prompt) > 50)
  expect_true(nchar(synthesis_prompt) > 100)
})

test_that("French and English prompts are consistently different", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    C1 = c(50, 60),
    famille_carbone = c(55, 65)
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  # System prompts
  system_fr <- nemetonshiny:::build_system_prompt("fran\u00e7ais", expert = "generalist")
  system_en <- nemetonshiny:::build_system_prompt("English", expert = "generalist")
  expect_false(system_fr == system_en)

  # Analysis prompts
  analysis_fr <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "fran\u00e7ais")
  analysis_en <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")
  expect_false(analysis_fr == analysis_en)

  # Synthesis prompts
  synthesis_fr <- nemetonshiny:::build_synthesis_prompt(test_data, "fran\u00e7ais")
  synthesis_en <- nemetonshiny:::build_synthesis_prompt(test_data, "English")
  expect_false(synthesis_fr == synthesis_en)
})

# ==============================================================================
# Edge Cases and Error Handling
# ==============================================================================

test_that("build_analysis_prompt handles single-row data", {
  test_data <- data.frame(
    id = "P1",
    C1 = 50
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  expect_type(result, "character")
  expect_match(result, "1 parcelle|1 parcel", ignore.case = TRUE)
})

test_that("build_synthesis_prompt handles single-row data", {
  test_data <- data.frame(
    id = "P1",
    famille_carbone = 60
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  expect_type(result, "character")
  expect_match(result, "1 parcelle|1 parcel", ignore.case = TRUE)
})

test_that("build_analysis_prompt handles data with only id column (no indicators)", {
  # Create data with only the standard excluded columns
  # get_indicator_cols excludes: nemeton_id, id, geo_parcelle, geometry, geom,
  # nomcommune, codecommune, area, surface_geo
  # When only these exist, no indicator columns remain
  test_data <- data.frame(
    id = c("P1", "P2"),
    area = c(1000, 2000)
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  # The function should handle this and return a prompt with "no data" for stats
  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("build_synthesis_prompt handles data with no family_* columns", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    C1 = c(50, 60)  # Not famille_carbone
  )

  result <- nemetonshiny:::build_synthesis_prompt(test_data, "English")

  expect_type(result, "character")
  # Should mention that there are no family scores or handle gracefully
})

test_that("build_analysis_prompt handles zero-variance data", {
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),
    C1 = c(50, 50, 50)  # No variance
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  expect_type(result, "character")
  # SD should be 0
  expect_match(result, "sd=0")
})

test_that("build_analysis_prompt handles negative values", {
  test_data <- data.frame(
    id = c("P1", "P2", "P3"),
    C1 = c(-10, 0, 10)
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  expect_type(result, "character")
  expect_match(result, "min=-10")
})

test_that("build_analysis_prompt handles very large values", {
  test_data <- data.frame(
    id = c("P1", "P2"),
    C1 = c(1e6, 2e6)
  )

  family_config <- nemetonshiny:::INDICATOR_FAMILIES$C

  result <- nemetonshiny:::build_analysis_prompt(family_config, test_data, "English")

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# ==============================================================================
# Expert Profile Specific Tests
# ==============================================================================

test_that("owner profile mentions financial/patrimoine concepts", {
  result_fr <- nemetonshiny:::build_system_prompt("fran\u00e7ais", expert = "owner")
  result_en <- nemetonshiny:::build_system_prompt("English", expert = "owner")

  # French should mention patrimoine/financier concepts
  expect_match(result_fr, "patrimoine|rentabilit|financ", ignore.case = TRUE)

  # English should mention financial/estate concepts
  expect_match(result_en, "profitability|asset|financial|estate", ignore.case = TRUE)
})

test_that("manager profile mentions silviculture concepts", {
  result_fr <- nemetonshiny:::build_system_prompt("fran\u00e7ais", expert = "manager")
  result_en <- nemetonshiny:::build_system_prompt("English", expert = "manager")

  # French should mention sylviculture
  expect_match(result_fr, "sylvicult|peuplement|itin\u00e9raire", ignore.case = TRUE)

  # English should mention silviculture
  expect_match(result_en, "silvicultur|stand|management", ignore.case = TRUE)
})

test_that("hunter profile mentions game management concepts", {
  result_fr <- nemetonshiny:::build_system_prompt("fran\u00e7ais", expert = "hunter")
  result_en <- nemetonshiny:::build_system_prompt("English", expert = "hunter")

  # French should mention cynegetic concepts
  expect_match(result_fr, "cyn\u00e9g\u00e9tique|gibier|abroutissement", ignore.case = TRUE)

  # English should mention game/wildlife concepts
  expect_match(result_en, "game|wildlife|browsing", ignore.case = TRUE)
})

test_that("producer profile mentions timber industry concepts", {
  result_fr <- nemetonshiny:::build_system_prompt("fran\u00e7ais", expert = "producer")
  result_en <- nemetonshiny:::build_system_prompt("English", expert = "producer")

  # French should mention filiere bois
  expect_match(result_fr, "fili\u00e8re|bois|march|production", ignore.case = TRUE)

  # English should mention timber industry
  expect_match(result_en, "timber|market|production|return", ignore.case = TRUE)
})

# Reset language to English at end of tests
nemeton::nemeton_set_language("en")

test_that("get_expert_choices hides the internal JSON-only 'planificateur' profile", {
  ch <- nemetonshiny:::get_expert_choices("fr")
  # `planificateur` is a JSON-only profile (action-plan internal) — it must
  # NOT be offered as a perspective profile (would return raw JSON).
  expect_false("planificateur" %in% ch)
  expect_false("planificateur" %in% names(ch))
  # …but it stays available to build_system_prompt() for the action plan.
  expect_true("planificateur" %in% names(nemetonshiny:::get_expert_profiles()))
  expect_true(grepl("JSON",
    nemetonshiny:::build_system_prompt("français", "planificateur")))
  # Legitimate perspective profiles remain offered.
  expect_true("generalist" %in% ch)
  expect_true("naturaliste" %in% ch)
})

test_that("get_expert_choices hides the internal 'regeneration' profile", {
  ch <- nemetonshiny:::get_expert_choices("fr")
  # `regeneration` is the reGénération « Affiner » profile (hardcoded by
  # mod_regeneration) — not a general perspective, must stay out of the dropdown.
  expect_false("regeneration" %in% ch)
  expect_false("regeneration" %in% names(ch))
  # …but it stays loadable for build_system_prompt().
  profiles <- nemetonshiny:::get_expert_profiles()
  expect_true("regeneration" %in% names(profiles))
  expect_equal(profiles[["regeneration"]]$label$fr, "Conseiller en régénération")
  sp <- nemetonshiny:::build_system_prompt("français", "regeneration")
  expect_true(nchar(sp) > 0)
  # Profil narratif (prose), pas JSON comme planificateur.
  expect_false(grepl("exclusivement du JSON", sp))
})

test_that("adaptation_climat profile is loaded with a regeneration-focused prompt (spec 027 L5)", {
  profiles <- nemetonshiny:::get_expert_profiles()
  expect_true("adaptation_climat" %in% names(profiles))

  ap <- profiles[["adaptation_climat"]]
  expect_equal(ap$label$fr, "Adaptation climatique")
  expect_equal(ap$label$en, "Climate adaptation")

  # Le prompt cible les indicateurs reGénération et les familles A/R/W/C.
  fr <- ap$prompt$fr
  expect_match(fr, "njstress")
  expect_match(fr, "régénération", ignore.case = TRUE)
  expect_match(fr, "biodiversité")  # flag des conflits B/N
  en <- ap$prompt$en
  expect_match(en, "njstress")
  expect_match(en, "regeneration", ignore.case = TRUE)

  # Sélectionnable comme n'importe quel profil (system prompt non vide).
  sp <- nemetonshiny:::build_system_prompt("français", "adaptation_climat")
  expect_true(nchar(sp) > 0)
})

test_that("build_regen_advice_prompt résume top-3 + station et cadre le LLM", {
  rk <- data.frame(
    ug_id = c("U1", "U1", "U1", "U2"), rank = c(1, 2, 3, 1),
    label = c("Pin d'Alep", "Chêne vert", "Cèdre", "Sapin"),
    suitability = c(97, 94, 88, 70),
    limiting_factor = c("secheresse", "gel", "chaleur", "ombre"),
    confidence = "eleve", stringsAsFactors = FALSE)
  st <- data.frame(ug_id = c("U1", "U2"), njstress = c(30, 10),
                   rew_min = c(0.1, 0.4), d_tmax = c(3, 2), r7_gel_days = c(6, 1))

  fr <- nemetonshiny:::build_regen_advice_prompt(rk, st, "français")
  expect_match(fr, "UGF U1", fixed = TRUE)
  expect_match(fr, "Chêne vert", fixed = TRUE)          # essence classée
  expect_match(fr, "97/100", fixed = TRUE)                   # score d'adéquation
  expect_match(fr, "jours stress=30", fixed = TRUE)          # condition de station
  # Garde-fou : interdit de recommander hors classement.
  expect_match(fr, "absente des classements", fixed = TRUE)

  en <- nemetonshiny:::build_regen_advice_prompt(rk, st, "English")
  expect_match(en, "management units", fixed = TRUE)
  expect_match(en, "absent from the rankings", fixed = TRUE)

  # Troncature au-delà de 20 UGF (prompt borné).
  big_ids <- paste0("U", seq_len(25))
  rk_big <- data.frame(ug_id = big_ids, rank = 1L, label = "X",
    suitability = 80, limiting_factor = "gel", confidence = "eleve",
    stringsAsFactors = FALSE)
  expect_match(nemetonshiny:::build_regen_advice_prompt(rk_big, data.frame(ug_id = big_ids),
    "français"), "20 premières UGF", fixed = TRUE)

  # Consigne libre de l'utilisateur (panneau « Affiner ») injectée en priorité.
  q_fr <- nemetonshiny:::build_regen_advice_prompt(rk, st, "français",
    question = "Et sur sol calcaire ?")
  expect_match(q_fr, "Consigne de l'utilisateur", fixed = TRUE)
  expect_match(q_fr, "Et sur sol calcaire ?", fixed = TRUE)
  q_en <- nemetonshiny:::build_regen_advice_prompt(rk, st, "English",
    question = "What about limestone soils?")
  expect_match(q_en, "User instruction", fixed = TRUE)
  # Consigne vide / NULL -> pas de section consigne.
  expect_no_match(nemetonshiny:::build_regen_advice_prompt(rk, st, "français"),
    "Consigne de l'utilisateur", fixed = TRUE)
})

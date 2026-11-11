# Mission 016 : le registre des jetons de confirmation MCP est sur disque.
# Isolation de TOUTE la suite : jamais d'ecriture dans le vrai
# tools::R_user_dir("nemetonshiny", "data") pendant les tests. Un repertoire
# temporaire unique, efface en fin de suite (teardown_env). Les tests qui
# veulent un registre vierge posent leur propre NEMETON_MCP_JETONS_DIR.
withr::local_envvar(
  NEMETON_MCP_JETONS_DIR = withr::local_tempdir(
    pattern = "mcp-jetons-", .local_envir = testthat::teardown_env()),
  .local_envir = testthat::teardown_env()
)

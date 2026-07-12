# Neutralise ntfy pour TOUTE la suite de tests.
#
# Le `.Renviron` de dev porte `NEMETON_NTFY_TOPIC` (topic réel). Sans cette
# neutralisation, tout test qui exécute un moteur (reGénération, monitoring…)
# atteint `.ntfy_config()` -> `.ntfy_send()` et envoie un VRAI push sur le
# téléphone — titré « … fileXXXX » (basename du répertoire temporaire, faute de
# metadata.json), au lieu du nom de projet. En retirant la variable ici,
# `.ntfy_config()` renvoie NULL et tous les envois deviennent des no-op.
#
# Les tests qui valident explicitement ntfy (ex. test-service_monitoring.R)
# posent leur propre `withr::with_envvar(NEMETON_NTFY_TOPIC = ...)` dans un bloc
# local, qui prime sur cette neutralisation de session.
withr::local_envvar(
  c(NEMETON_NTFY_TOPIC = NA),
  .local_envir = testthat::teardown_env()
)

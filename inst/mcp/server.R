# Serveur MCP de nemetonshiny (stdio).
#
# Expose a un assistant (Claude Code, AIGORA, VICTOR) les outils de
# `nemetonshiny:::mcp_tools()` : lister / resumer les projets, lancer et suivre
# un calcul, produire le rapport PDF et le GeoPackage, ouvrir l'application sur
# un projet. Voir inst/mcp/README.md.
#
# stdout est le canal du protocole : rien ne doit y etre ecrit ; les messages
# de chargement et de progression partent sur stderr.

if (!requireNamespace("mcptools", quietly = TRUE)) {
  stop("Le paquet 'mcptools' est requis : install.packages(\"mcptools\").",
       call. = FALSE)
}
# Locale UTF-8 forcee : lancee depuis un client sans locale (LC_ALL=C), la
# normalisation des noms et les messages accentues en dependent.
.locale_ok <- FALSE
for (.l in c("C.UTF-8", "en_US.UTF-8")) {
  if (nzchar(Sys.setlocale("LC_CTYPE", .l))) { .locale_ok <- TRUE; break }
}
if (!.locale_ok) {
  cli::cli_alert_warning("Aucune locale UTF-8 disponible (C.UTF-8, en_US.UTF-8) : les accents peuvent etre mal lus.")
}
suppressPackageStartupMessages(loadNamespace("nemetonshiny"))
# `session_tools = FALSE` : sinon mcptools TRANSFERE chaque appel d'outil vers
# une session R interactive ouverte (RStudio rendue visible par
# `btw::btw_mcp_session()`, cas d'AIGORA), ou nemetonshiny n'est pas charge -
# l'appel ne revient jamais. Les outils s'executent ici, dans ce processus.
mcptools::mcp_server(tools = nemetonshiny:::mcp_tools(), session_tools = FALSE)

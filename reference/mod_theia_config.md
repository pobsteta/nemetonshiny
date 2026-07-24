# External API keys configuration module

Navbar entry that opens a tabbed modal to configure the API keys used by
the application :

\* \*\*Theia / DATA TERRA\*\* — access + secret key for the Python
\`teledetection\` SDK, plus the Python prerequisite status and the
provenance / licensing of the Theia data sources. \* \*\*LLM\*\* — API
key for one of Mistral / Anthropic / OpenAI, used by \`ellmer\` to drive
the expert perspectives.

The module is named \`mod_theia_config\` for historical reasons — it was
originally Theia-only. The wiring (\`run_app.R\`) still calls that name
; only the modal layout has been widened to a tabset.

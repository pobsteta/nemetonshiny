# Chained-run orchestration ("Tout calculer")

Application-level state machine that runs every engine of the app one
after another, then the AI generations, and reports what happened.

Deliberately Shiny-free: the whole decision logic (what runs next, what
is skipped, what the final report says) is plain data so it can be
tested without a session. \`mod_pipeline.R\` holds the UI and the
signalling; the engines themselves stay where they are, in their own
modules.

Why signalling rather than calling the engines directly: each engine is
an \`ExtendedTask\` defined inside its module's server, and its
arguments are built from that tab's inputs (selected engines, buffer,
corrected network, S2 period...). An outside orchestrator would have to
duplicate all of it and would drift the day a tab gains an option.
Instead the orchestrator posts a request on \`app_state\` and the owning
module launches its own engine through the very code path its button
uses.

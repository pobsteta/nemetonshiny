# Chained-run module ("Tout calculer")

One button in the Selection tab that runs every engine of the app one
after another, then the AI generations, and reports what happened.

This module owns the \*ordering\* and the \*reporting\*. It never runs
an engine itself: it posts a request on \`app_state\$pipeline_request\`
and the owning module answers on \`app_state\$pipeline_answer\` (see the
protocol section of \`service_pipeline.R\`). That is what lets each
engine keep its own guards, its own tab options and its own progress
feedback.

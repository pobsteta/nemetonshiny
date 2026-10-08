# ONF forest-parcel service (spec 046, spec 058 core)

Application-side wiring for the \*\*ONF forest parcels\*\* (the
"parcellaire forestier"). In public forests the \*cadastral\* parcel is
not the management unit: the \*forest\* parcel is, and it is the one
materialised on the ground. The core owns the whole acquisition
(\`nemeton::load_onf_parcelles_source()\`) and the whole crossing
arithmetic (\`nemeton::construire_ugf_onf()\`, nemeton \>= 1.2.0); this
file only turns their output into a project, so \`mod_ug\` stays free of
business logic (rules \#1 and \#2).

One path: \[onf_projet_croise()\]. The cadastral parcels are never
warped: the ONF layer is rubber-sheeted onto them, then each cadastral
parcel is cut along the forest parcels and the pieces are grouped into
UGF, each carrying its ONF forest parcel in columns (\`UG_ONF_COLS\`).

The former chain - the core's first crossing function, with the "whole
parcel above 90 removed on 2026-10-08 (brief
\`onf-nouveau-chemin-seul\`), with no option to go back to it.

The WFS is reachable over \*\*HTTP only\*\*; every call therefore
happens server-side, never from the browser (mixed content would be
blocked).

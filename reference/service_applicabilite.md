# Applicability verdicts for the source-conditioned indicators (core v0.175.0)

Whether R5 and A5 can say anything about \*this\* forest, asked
\*\*before\*\* the computation rather than discovered after it. The core
owns the judgement (\`nemeton::r5_applicabilite()\`,
\`nemeton::a5_applicabilite()\`) and returns a \*\*stable key\*\*; the
app translates it and decides what to skip.

Two nuances this file exists to protect:

\* \*\*\`eligible_fordead_out_of_calibration\` is not a refusal.\*\*
R5's validation area is the ONF/DSF 2024 one - Vosges, Jura, Ain,
Savoie, Haute-Savoie, 27 565 km2. Outside it a silver fir is still a
silver fir: the computation runs, only its confidence classes are
extrapolated. None of the local projects sits inside that area, and
Fordead and Dabo are nonetheless 100 usable signal, so this verdict is
rendered as information and \*\*never short-circuits\*\*. \*
\*\*\`a5_applicabilite()\` without \`lst\` answers at the scale of the
AOI.\*\* A STAC query knows bounding boxes, not pixels: \`eligible\`
there means "the coverage exists", not "every unit is scoreable".
Passing the cached raster switches to a per-unit verdict, the only one
able to return \`eligible_partial\`.

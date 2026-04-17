# Generate PDF report (main entry point)

Generates a PDF report, using Quarto if available or falling back to
simple PDF generation.

## Usage

``` r
generate_report_pdf(
  project,
  family_scores,
  output_file,
  language = "fr",
  synthesis_comments = NULL,
  family_comments = NULL,
  cover_image = NULL,
  use_quarto = TRUE
)
```

## Arguments

- project:

  List. Project object.

- family_scores:

  sf. Family scores.

- output_file:

  Character. Path to output PDF.

- language:

  Character. Language code.

- synthesis_comments:

  Character. Optional synthesis comments (supports markdown).

- family_comments:

  Named list. Optional comments per family (keyed by family code,
  supports markdown).

- cover_image:

  Character. Optional path to cover image for first page.

- use_quarto:

  Logical. Whether to try Quarto first. Default TRUE.

## Value

Character. Path to generated PDF.

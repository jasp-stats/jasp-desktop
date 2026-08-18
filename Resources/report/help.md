# Report

Reports are standalone documents that present JASP analysis results alongside your own explanatory text.

Unlike regular analyses, reports:
- Do **not** run R code or connect to the statistical engine
- Can pull tables, plots, and collections from **multiple** existing analyses
- Support Markdown formatting for headings, lists, emphasis, and more

## How reports work

1. Analyses are run normally (e.g., a t-test, ANOVA, regression)
2. A report is created referencing those analyses' output elements by name
3. Markdown text blocks can be inserted between the elements for narrative

Reports appear in the results panel just like regular analyses and are saved as part of your JASP file.

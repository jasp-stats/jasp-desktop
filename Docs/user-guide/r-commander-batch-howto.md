# Creating several analyses at once with R in JASP

This guide explains how to use the **R in JASP** window to create many analyses in one go, for
example the same analysis for every variable, or with a range of settings.


## The idea

Every analysis in JASP can be shown as R code. Turn on **Show R syntax** (in the analysis panel, or
under *Preferences → Results*) and you'll see something like:

```r
jaspDescriptives::Descriptives(
    version = "0.96.5",
    formula = ~ contGamma)
```

Pasting this into **R in JASP** and clicking **Add analysis** creates that analysis. To create
*many* analyses, you wrap that same call in `batchAnalysis()` and click **Run code**:

```r
batchAnalysis(
  jaspDescriptives::Descriptives,
  version       = "0.96.5",
  formula       = ~ contGamma,
  quantilesType = as.character(1:7)
)
```

This adds seven Descriptives analyses, one for each value of `quantilesType`. The output window
ends with `Added 7 analyses`.


## How the arguments are combined

After the analysis function you list its arguments, just like in the R syntax. Each argument is
either:

- **a single value**, which is used for every analysis (`formula = ~ contGamma` above), or
- **several values**, which produces one analysis per value (`quantilesType = as.character(1:7)`).

All arguments with several values must have the same length; they are matched up position by
position (the first analysis gets the first value of each, the second gets the second, and so on).
Arguments are *not* crossed with each other. If the lengths don't match, you get an error telling
you which argument is the problem.

### Passing several values as one value

Some options naturally take more than one value, such as a list of percentiles. By default
`batchAnalysis()` would treat that as "one analysis per value". Wrap it in `list()` or `I()` to pass
it as a single value instead:

```r
batchAnalysis(
  jaspDescriptives::Descriptives,
  formula          = ~ contGamma,
  percentiles      = TRUE,
  percentileValues = list(c(25, 50, 75))   # one analysis with three percentiles
)
```

### Varying the variables

A formula such as `~ contGamma` counts as a single value. To run an analysis for several variables,
either pass a list of formulas:

```r
batchAnalysis(
  jaspDescriptives::Descriptives,
  formula = list(~ contNormal, ~ contGamma, ~ contExpon)
)
```

or build the formula yourself in a function, as described next.


## Using your own function

For anything more involved, pass a function instead of the analysis. It is called once per
analysis with the varying arguments, and should return the result of an analysis call:

```r
batchAnalysis(
  function(variable) {
    jaspDescriptives::Descriptives(formula = as.formula(paste("~", variable)))
  },
  variable = c("contNormal", "contGamma", "contExpon")
)
```

Inside the function you can do whatever you like: compute settings, use `if`, or even call
analyses from different modules.


## Titles

Each analysis gets a title so you can tell them apart. By default it is the analysis name followed by
the values that differ, e.g. `Descriptives: quantilesType=1`.

You can choose your own titles with `.override = list(title = ...)`:

```r
# A template: {name} is replaced by the value of that argument
batchAnalysis(jaspDescriptives::Descriptives,
              formula       = ~ contGamma,
              quantilesType = as.character(1:7),
              .override     = list(title = "Quantiles type {quantilesType}"))

# One title per analysis
batchAnalysis(jaspDescriptives::Descriptives,
              formula   = list(~ contNormal, ~ contGamma),
              .override = list(title = c("Normal data", "Gamma data")))

# A function of (number, arguments, analysis)
batchAnalysis(jaspDescriptives::Descriptives,
              formula       = ~ contGamma,
              quantilesType = as.character(1:7),
              .override     = list(title = function(i, args, analysis) paste0("Run ", i)))
```


## Trying it out first

Add `.dryRun = TRUE` to see what would be created without actually adding anything:

```r
batchAnalysis(jaspDescriptives::Descriptives,
              formula       = ~ contGamma,
              quantilesType = as.character(1:7),
              .dryRun       = TRUE)
```

```
Dry run: 7 analyses would be added:
  1. Descriptives: quantilesType=1
  2. Descriptives: quantilesType=2
  ...
```

Remove `.dryRun = TRUE` and run again to create them.


## When something goes wrong

If one of the analyses fails (for instance because of a typo in a setting), the others are still
created. The output window shows what went wrong and a summary:

```
Error in analysis 2: ...
Batch summary: 6 of 7 analyses succeeded, 1 failed.
```

If a setting is accepted by R but doesn't fit the analysis, the analysis is still added and the
problem is shown in that analysis' panel, just as when you use **Add analysis**.


## Things to keep in mind

- **`batchAnalysis()` must be the last thing your code does.** JASP looks at the value your code
  ends with. If you store it first (`x <- batchAnalysis(...)`) nothing is added, unless you end with
  a line containing just `x`.
- **Use Run code, not Add analysis.** The **Add analysis** button only works for a single analysis
  call.
- **`for` loops don't add analyses**, because a loop doesn't return anything. Use `batchAnalysis()`
  or `lapply()` instead.
- **Plain `lapply()` works too.** Code that ends with a list of analysis calls adds all of them:

  ```r
  lapply(c("contNormal", "contGamma"), function(variable) {
    jaspDescriptives::Descriptives(formula = as.formula(paste("~", variable)))
  })
  ```

  You just don't get the automatic titles, error summary or dry run.
- **A single analysis call also works with Run code.** Running `jaspDescriptives::Descriptives(...)`
  with **Run code** adds that analysis, the same as **Add analysis** would.
- Other R code is not affected: printing data, fitting models and so on works as before.

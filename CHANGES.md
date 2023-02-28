JASP Release Notes
==================

Version 0.11
------------

New Features and improvements:

  - Machine Learning module
      - Regression analyses
          - Boosting Regression
          - K-Nearest Neighbors Regression
          - Random Forest Regression
          - Regularized Linear Regression
      - Classification analyses
          - Boosting Classification
          - K-Nearest Neighbors Classification
          - Linear Discriminant Classification
          - Random Forest Classification
      - Clustering analyses
          - Density-Based Clustering
          - Fuzzy C-Means Clustering
          - Hierarchical Clustering
          - K-Means Clustering
          - Random Forest Clustering
  - MAD and MAD robust for Descriptives
  - Additional option to copy analyses
  - Updated the data library
  - Updated support for SPSS files
  - Added support for STATA/SAS files

Bugfixes:

  - Character encoding issues in Mediation module (jasp-issues [#420](https://github.com/jasp-stats/jasp-issues/issues/420), [#430](https://github.com/jasp-stats/jasp-issues/issues/430), [#435](https://github.com/jasp-stats/jasp-issues/issues/435), [#436](https://github.com/jasp-stats/jasp-issues/issues/436), [#465](https://github.com/jasp-stats/jasp-issues/issues/465), [#480](https://github.com/jasp-stats/jasp-issues/issues/480))
  - JASP freezing on launch ([jasp-issue #424](https://github.com/jasp-stats/jasp-issues/issues/424))
  - 2-Way ANOVA erroneously saying there are aliased coefficients in model. Wrong error message for empty cell in the interaction ([jasp-issue #441](https://github.com/jasp-stats/jasp-issues/issues/441))
  - Labelled outliers don’t match case number when using filters ([jasp-issue #443](https://github.com/jasp-stats/jasp-issues/issues/443))
  - Error Message upon opening `*.sav` -> bad allocation ([jasp-issue #445](https://github.com/jasp-stats/jasp-issues/issues/445))
  - Post Hoc test Repeated Measures ANOVA ([jasp-issue #459](https://github.com/jasp-stats/jasp-issues/issues/459))
  - MAD incorrect value? ([jasp-issue #492](https://github.com/jasp-stats/jasp-issues/issues/492))
  - Distribution plot not updating for ‘split’ variables ([jasp-issue #493](https://github.com/jasp-stats/jasp-issues/issues/493))
  - Memory Leak in JASP Engine fixed (Plots and state were kept)

Version 0.10.2
--------------

Bugfixes:

  - Summary Stats and Planning crash in absence of data fixed

Version 0.10.1
-------------

New Features and improvements:

  - AUDIT module
  - Mediation analysis
  - Post-hoc test for between*within interactions in mixed ANOVA
  - Q-Q plot in descriptives
  - Fixes for the Bain module
  - Effect sizes for independent t-test
  - Added documentation to the Bayesian A/B test
  - Order analyses by Drag & Drop
  - Sort option in OSF menu to order files on name and date
  - Alphabetic ordering of variables

Bugfixes:

  - MANOVA problem ([jasp-issue #378](https://github.com/jasp-stats/jasp-issues/issues/378))
  - BAIN model hypotheses errors ([jasp-issue #380](https://github.com/jasp-stats/jasp-issues/issues/380))
  - UI suggestions: a more native feel ([jasp-issue #381](https://github.com/jasp-stats/jasp-issues/issues/381))
  - Incorrect degrees of freedom for Welch’s independent t-test [JASP 0.10] ([jasp-issue #386](https://github.com/jasp-stats/jasp-issues/issues/386))
  - Incomplete labelling of outliers ([jasp-issue #387](https://github.com/jasp-stats/jasp-issues/issues/387))
  - Error in multigroup SEM ([jasp-issue #388](https://github.com/jasp-stats/jasp-issues/issues/388))
  - SEM module doesn’t recognize intercepts ([jasp-issue #394](https://github.com/jasp-stats/jasp-issues/issues/394))
  - Repeated measures factorial ANOVA misreading data ([jasp-issue #400](https://github.com/jasp-stats/jasp-issues/issues/400))

Version 0.10
------------

New Features and improvements:

  - New interface
      - Panel redesign
          - Data panel, analysis input panel and results panel can be manipulated much more intuitively with sliders and show/hide buttons
          - All analyses input forms have been rewritten and made more consistent
          - Changed the analysis input panel to have an overview of all opened analyses and added the possibility to change titles, to show documentation, and remove analyses
      - Improvement of the ribbon bar
      - Enhanced the navigation through the file menu; it is now possible to use keys and hovering
      - Added possibility to scale the entire application with Ctrl +, Ctrl – and Ctrl 0
  - New analyses
      - MANOVA
      - Confirmatory Factor Analysis
      - Bayesian Multinomial Test
      - Bayesian A/B Test (beta)
      - Bain (module; beta)
  - New user settings
      - Ability to let JASP log its workflow and save it to a file (useful for bug reporting)
      - An option to let JASP remember your used modules in subsequent sessions
      - An import threshold that specifies how integer columns should be read (categorical or scale)
      - Early developer mode that allows dynamic modules to be added (beta)
  - Improvements
      - Added/updated help files for most analyses
      - R engine updated from 3.4.4 to 3.5.2
      - Added Šidák correction for post-hoc tests (AN(C)OVA)
      - Added bootstrapping for post-hoc tests (AN(C)OVA)
      - Added bootstrapping for marginal means (AN(C)OVA)
      - Ability to plot a covariate on the horizontal axis (ANCOVA)
      - Added post-hoc tests for interactions (RM ANOVA)
      - Added marginal means (RM ANOVA)
      - Added bootstrapping for marginal means (RM ANOVA)
      - Added confidence intervals for post-hoc tests (RM ANOVA)
      - More complete Sphericity tests (Chi-squared and df; RM ANOVA)
      - Added generalized eta squared effect size (RM ANOVA)
      - Added Cox & Snell R-squared (Logistic regression)
      - Added Wald test for coefficients (Logistic regression)
      - Added casewise diagnostics (Logistic regression)
      - Added bootstrapping for coefficients (Logistic regression)
      - Added R-squared for observed variables (SEM)
      - Added Shapiro-Wilks test (Descriptives)
      - Added median absolute deviation (Descriptives)
      - Added interquartile range (Descriptives)
      - Enhanced stability of Bayesian Mann-Whitney U test (Bayesian t-test)
      - Included table and plot for single model and model averaged posterior distributions (Bayesian ANOVA’s)
      - Added single model and model averaged QQ plots (Bayesian ANOVA’s)

Bugfixes:

  - Network analysis breakdown ([jasp-issue #158](https://github.com/jasp-stats/jasp-issues/issues/158))
  - In network analysis appears error whenever I want to tick something ([jasp-issue #229](https://github.com/jasp-stats/jasp-issues/issues/229))
  - JASP keeps crashing while performing network analysis ([jasp-issue #264](https://github.com/jasp-stats/jasp-issues/issues/264))
  - JASP crashes after Network analysis ([jasp-issue #306](https://github.com/jasp-stats/jasp-issues/issues/306))
  - Show full path in recent folders window ([jasp-issue #294](https://github.com/jasp-stats/jasp-issues/issues/294))
  - Chart legend not shown in repeated measures ANOVA ([jasp-issue #270](https://github.com/jasp-stats/jasp-issues/issues/270))
  - Bug in the repeated measures ANOVA line plots ([jasp-issue #296](https://github.com/jasp-stats/jasp-issues/issues/296))
  - Width of descriptives plot in RM ANOVA ([jasp-issue #305](https://github.com/jasp-stats/jasp-issues/issues/305))
  - No boxplots when filters active ([jasp-issue #281](https://github.com/jasp-stats/jasp-issues/issues/281))
  - Boxplots – analysis terminated unexpectedly ([jasp-issue #309](https://github.com/jasp-stats/jasp-issues/issues/309))
  - BUG report on descriptive statistics splitted by groups ([jasp-issue #239](https://github.com/jasp-stats/jasp-issues/issues/239))
  - Error message while trying to create boxplots ([jasp-issue #304](https://github.com/jasp-stats/jasp-issues/issues/304))
  - Dealing with missing values ([jasp-issue #324](https://github.com/jasp-stats/jasp-issues/issues/324))
  - There is no package called ‘withr’ ([jasp-issue #278](https://github.com/jasp-stats/jasp-issues/issues/278))
  - Logistic regression plot ([jasp-issue #337](https://github.com/jasp-stats/jasp-issues/issues/337))
  - Jitter element causes crash!([jasp-issue #343](https://github.com/jasp-stats/jasp-issues/issues/343))
  - Pool SE across RM factors: toggle is mis-calibrated ([jasp-issue #307](https://github.com/jasp-stats/jasp-issues/issues/307))
  - Figures do not resize ([jasp-issue #323](https://github.com/jasp-stats/jasp-issues/issues/323))
  - RM Bayesian ANOVA display bug ([jasp-issue #276](https://github.com/jasp-stats/jasp-issues/issues/276))
  - Changing variable levels messes up cell order ([jasp-issue #237](https://github.com/jasp-stats/jasp-issues/issues/237))
  - JASP crashes when loading .txt (UTF-8) and .csv ([jasp-issue #291](https://github.com/jasp-stats/jasp-issues/issues/291))
  - Binomial test analysis terminated unexpectedly ([jasp-issue #287](https://github.com/jasp-stats/jasp-issues/issues/287))
  - JASP error running repeated-measures ANOVA. A: Change interpreter to handle number only headers ([jasp-issue #358](https://github.com/jasp-stats/jasp-issues/issues/358))
  - JASP Friedman Conover test doesn’t match R (posthoc.friedman.conover.test) ([jasp-issue #314](https://github.com/jasp-stats/jasp-issues/issues/314))
  - Request: more helpful error message in correlation plot ([jasp-issue #347](https://github.com/jasp-stats/jasp-issues/issues/347))
  - In Bayesian ANOVA, choosing BF10 or BF01 changes the the Model Comparison output, but not the Analysis of Effects ([jasp-issue #238](https://github.com/jasp-stats/jasp-issues/issues/238))
  - Add scroll bar to Compute Column feature ([jasp-issue #316](https://github.com/jasp-stats/jasp-issues/issues/316))

Version 0.9.2
-------------

New Features and improvements:

  - Export to csv (works with computed columns)
  - WIX installer for Windows (enables you to choose a path for the installation of JASP)
  - Added extra transformations to compute column
  - Adapted behaviour of ifElse and replaceNA to work on the labels
  - User interface scaling
  - Default white background for plots, but you can choose transparent as well
  - Modern JASP logo has replaced the old software icon
  - Made replaceNA applicable for all types
  - Increase size of data viewer in options
  - Left columns with variable names abbreviated [compute column and filter]

Internal:

  - Drop levels of factors by default after applying a filter
  - Increased startup-speed for Windows

Bugfixes:

  - fixed plots for descriptives
  - Base64 encoding in meta-analysis ([jasp-issue #248](https://github.com/jasp-stats/jasp-issues/issues/248))
  - Overlap in plot text Bayesian linear regression ([jasp-issue #211](https://github.com/jasp-stats/jasp-issues/issues/211))

Version 0.9.1
-------------

New Features and improvements:

  - [Compute columns using a drag-and-drop interface or R code](https://jasp-stats.org/wp-content/uploads/2018/10/compute_cols_1.gif)
  - Improvements in classical linear regression
      - [Bootstrap confidence intervals of coefficients](https://jasp-stats.org/wp-content/uploads/2018/10/conf_int_bootstrap2.gif)
      - [Cook’s distance (case-wise diagnostics)](https://jasp-stats.org/wp-content/uploads/2018/10/cooks_dist.gif)
      - [Partial plots](https://jasp-stats.org/wp-content/uploads/2018/10/partial_plots.gif)
  - Improvements in ANOVA
      - [Homogeneity corrections: Brown-Forsythe and Welch](https://jasp-stats.org/wp-content/uploads/2018/10/homogeneity_corrections.gif)
      - [Show df for contrast analysis](https://jasp-stats.org/wp-content/uploads/2018/10/df_contrasts.gif)
      - [Option to not assume equal variances in contrast analysis (AN(C)OVA)](https://jasp-stats.org/wp-content/uploads/2018/10/anovacontrast_noequalvar.gif)
      - [Option to add confidence intervals to post-hoc tests and contrasts](https://jasp-stats.org/wp-content/uploads/2018/10/ANOVA_confint_posthoc.gif)
      - [Dunn, Dunnett, Games-Howell post-hoc tests](https://jasp-stats.org/wp-content/uploads/2018/10/ANOVA_posthoc_types.gif)

Bugfixes:

  - Output window automatically scrolls to the top after addition of new elements ([jasp-desktop#2594](https://github.com/jasp-stats/jasp-desktop/issues/2594))
  - Rendering on Windows 7 ([jasp-desktop#2669](https://github.com/jasp-stats/jasp-desktop/issues/2669))
  - Filter fixes
  - ANOVA: Fix state issue for Welch/BrownForsythe ANOVA ([jasp-desktop#2747](https://github.com/jasp-stats/jasp-desktop/pull/2747))
  - Minor fixes ANOVA ([jasp-desktop#2771](https://github.com/jasp-stats/jasp-desktop/pull/2771))
  - Fix multigroup SEM ([jasp-desktop#2750](https://github.com/jasp-stats/jasp-desktop/pull/2750))
  - BF Subscript in table Bayesian Binomial Test disappeared ([jasp-desktop#2760](https://github.com/jasp-stats/jasp-desktop/pull/2760))
  - New descriptive distribution plot ([jasp-desktop#2746](https://github.com/jasp-stats/jasp-desktop/pull/2746))

Version 0.9.0.1
-------------

Bugfixes:

  - Fix issue [jasp-desktop#2641](https://github.com/jasp-stats/jasp-desktop/issues/2641): JASP 0.9 does not recognize special characters on Mac and Windows
  - Request [jasp-desktop#2657](https://github.com/jasp-stats/jasp-desktop/pull/2657): Small fix in LogReg Tjur R²
  - Request [jasp-desktop#2638](https://github.com/jasp-stats/jasp-desktop/pull/2638): LaTeX and Apply Filter

Version 0.9
-------------

New Features and improvements:

  - Filtering: use either R code or a drag-and-drop GUI to select cases of interest (an in-depth explanation on filtering in JASP will be posted soon)
  - The JASP data library: an initial collection of over 50 data sets, including the examples from two popular stats textbooks
  - Exporting tables from JASP in LaTeX format
  - Non-parametric tests:
      - Friedman and Durbin tests / nonparametric (classical RM ANOVA)
      - Kruskal-Wallis / nonparametric (classical ANOVA)
      - Connover’s nonparametric post hoc test (classical RM ANOVA)
      - Dunn’s nonparametric post hoc test (classical ANOVA)
      - Mann-Whitney / Wilcoxon rank sum test (Bayesian independent samples T-Test)
  - Resizing tables and plots with ctrl+/ctrl-/ctrl=
  - R engine updated from 3.3.3 to 3.4.4
  - SEM module upgraded
  - Improved Bayesian linear regression
  - Improved contrasts for RM ANOVA
  - Hand pointer to scroll through the data viewer
  - Open file via welcome screen
  - Improvement of placement tags of outliers
  - Graphs in network analysis displays in colourblind mode by default

Bugfixes:

  - Fix issue [jasp-desktop#2381](https://github.com/jasp-stats/jasp-desktop/issues/2381): One Sample T-Test: Possible error in the estimation of the 95% CI for Mean Difference
  - Fix issue [jasp-desktop#2074](https://github.com/jasp-stats/jasp-desktop/issues/2074): Linear regression bugs
  - Fix issue [jasp-desktop#2438](https://github.com/jasp-stats/jasp-desktop/issues/2438): Bayesian descriptive plot for RM ANOVA does not work
  - Fix issue [jasp-desktop#2346](https://github.com/jasp-stats/jasp-desktop/issues/2346): SumStats module Bayesian Regression does not have addInfo for robustness check
  - Fix issue [jasp-desktop#2400](https://github.com/jasp-stats/jasp-desktop/issues/2400), [jasp-desktop#2425](https://github.com/jasp-stats/jasp-desktop/issues/2425): Bayesian linear regression error when adding interaction
  - Fix issue [jasp-desktop#2467](https://github.com/jasp-stats/jasp-desktop/issues/2467): Add standardized alpha for reliability analysis
  - Fix issue [jasp-desktop#2074](https://github.com/jasp-stats/jasp-desktop/issues/2074): Linear regression bugs
  - Fix issue [jasp-desktop#2497](https://github.com/jasp-stats/jasp-desktop/pull/2597): Fix Meta-analysis
  - Fix issue [jasp-desktop#2541](https://github.com/jasp-stats/jasp-desktop/issues/2541), [jasp-desktop#1929](https://github.com/jasp-stats/jasp-desktop/issues/1929): Bayesian correlation “terminates unexpectedly”

Version 0.8.6
-------------

New features:

  - A revamped Bayesian linear regression thanks to the [BAS package](https://cran.r-project.org/web/packages/BAS/BAS.pdf)
  - Multinomial analysis
  - Install JASP on most Linux distributions more easily due to [Flatpak](https://flatpak.org/) support
  - Plots now update directly when resizing them
  - A tooltip for variables so that long names can be still read
  - _Z_-test
  - Simple main effects for repeated measures ANOVA
  - High DPI support on Windows, improving display resolution

Bugfixes:

  - Unable to plot network graph when non-ascii character in filename ([jasp-desktop#2252](https://github.com/jasp-stats/jasp-desktop/issues/2252))
  - Repeated-measures ANOVA error with high(ish) number of cells (complex numbers or unequal size arrays) ([jasp-desktop#2241](https://github.com/jasp-stats/jasp-desktop/issues/2241))
  - Re-enable SEM on Linux ([jasp-desktop#2267](https://github.com/jasp-stats/jasp-desktop/issues/2267))
  - Beter guess of the delimiter when loading a CSV file ([jasp-desktop#2259](https://github.com/jasp-stats/jasp-desktop/pull/2259))
  - Issue with EFA explanatory parallel oblimin Factoranalysis ([jasp-desktop#2249](https://github.com/jasp-stats/jasp-desktop/issues/2249))
  - Small changes in the UI for Linear Regression and Factor analyses ([jasp-desktop#2291](https://github.com/jasp-stats/jasp-desktop/pull/2291))
  - Microsoft Visual C++ Runtime Library Runtime Error! ([jasp-desktop#2286](https://github.com/jasp-stats/jasp-desktop/issues/2286))
  - If SEM analysis jasp file is opened, the model specified is not shown in the text area ([jasp-desktop#2297](https://github.com/jasp-stats/jasp-desktop/pull/2297))

Version 0.8.5
-------------

Improvements:
 - Network module: new clustering table/plot
 - ANOVA : simple effects
 - ANOVA : fix for repeated contrast
 - Logistic regression: changes to stepwise regression.
 - Logistic regression : AIC fior variable selection & removal p-values

Bugfixes:
 - Independent sample Bayesian t-test ([jasp-desktop#2196](https://github.com/jasp-stats/jasp-desktop/issues/2196))
 - Wrong valid percent in frequency table ([jasp-desktop#2202](https://github.com/jasp-stats/jasp-desktop/issues/2202))
 - Improve ODS Importer ([jasp-desktop#2167](https://github.com/jasp-stats/jasp-desktop/issues/2167))


Version 0.8.4
-------------

New Features:
 - Added meta analysis
 - Added network analysis
 - Improved the module layout
 - Added stepwise methods to logistic regression
 - Added effect size measures to post-hoc analyses in ANOVA / ANCOVA
 - Improved backwards compatibility for Linux users with older R versions

Bugfixes:
 - Fixed situation where JASPEngine process would linger after closing JASP
 - Better handling of utf8 characters ([jasp-desktop#1695](https://github.com/jasp-stats/jasp-desktop/issues/1695), [jasp-desktop#1704](https://github.com/jasp-stats/jasp-desktop/issues/1704) and [jasp-desktop#1719](https://github.com/jasp-stats/jasp-desktop/issues/1719))
 - Fixed confidence intervals cohens d (t-test)
 - Fixed bug where prior posterior plot could not be made
 - Fixed confusion in Subjective priors widget


Version 0.8.3.1
---------------

Hotfix:
 - p-value chi-square Logistic Regression ([jasp-desktop#2054](https://github.com/jasp-stats/jasp-desktop/issues/2054))


Version 0.8.3
-------------

New Features:

 - New Progressbar (implemented for Bayesian ANOVAs and Regression)
 - Preference menu - specify missing values and  number of decimals
 - JASPTools package
 - Added logistic regression
 - Added hierarchical regression
 - Added ability to add/change missing values
 - Added progress bar to Bayesian ANOVA’s and regression
 - Added preference option to change the number of decimals that are displayed
 - Reworked the layout of the Preferences window
 - Added option to toggle frequentist correlation table to pairwise display
 - Added ability to change y-axis in RM ANOVA descriptives plots
 - Added effect sizes to post-hoc tests ANOVA
 - Added confidence intervals to rank correlations
 - Changed display of Bayesian and frequentist correlations to below diagonal
 - Removed autofilling of options when another instance of an analysis is started

Changes:

 - Remove unused R packages
 - Additional Info for T-Test and Correlation Bayesian Pairs Robustness plots (Common module)

Bugfixes:

 - Fixed analysis crash when RM and between factors name match ([jasp-desktop#1906](https://github.com/jasp-stats/jasp-desktop/issues/1906))
 - Fixed bug where JASP crashes if you load data between running analyses
 - Fixed issue where files from old JASP versions cause problems in newer versions
 - Fixed bug where About window would go behind main window
 - Fixed analysis crash when vovk-sellke was selected in correlations ([jasp-desktop#1959](https://github.com/jasp-stats/jasp-desktop/issues/1959))
 - Fixed issue with importing of PSPP files ([jasp-desktop#1966](https://github.com/jasp-stats/jasp-desktop/issues/1966))
 - Fixed issue where RM factor is reset when the analysis options are refreshed ([jasp-desktop#1921](https://github.com/jasp-stats/jasp-desktop/issues/1921))
 - Changed base of logarithm to e in Bayesian correlation matrix ([jasp-desktop#1981](https://github.com/jasp-stats/jasp-desktop/issues/1981))
 - Fixed vanishing of footnotes in Bayesian correlation matrix
 - Fixed plot for logBF robustness in correlation pairs


Version 0.5
-----------

New Features:

 - Classical Linear Regression (basic implementation)
 - Bayesian ANCOVA
 - Bayesian Crosstabs
 - Custom models in Bayesian ANOVA
 - X-Squared in Classical Crosstabs

Changed:

 - Refinement of all the T-Tests (these are *really* polished now)

Bugfixes:

 - Fixes to add Linux support
 - Several bug fixes

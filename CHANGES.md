JASP Release Notes
==================

Version 0.8.5
-------------

Improvements:
 - Network module: new clustering table/plot
 - ANOVA : simple effects
 - ANOVA : fix for repeated contrast
 - Logistic regression: changes to stepwise regression.
 - Logistic regression : AIC fior variable selection & removal p-values

Bugfixes:
 - Independent sample Bayesian t-test #2196
 - Wrong valid percent in frequency table #2202
 - Improve ODS Importer #2167


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
 - Better handling of utf8 characters ((#1695), (#1704) and (#1719)
 - Fixed confidence intervals cohens d (t-test)
 - Fixed bug where prior posterior plot could not be made
 - Fixed confusion in Subjective priors widget


Version 0.8.3.1
---------------

Hotfix:
 - p-value chi-square Logistic Regression (#2054)


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

 - Fixed analysis crash when RM and between factors name match (#1906)
 - Fixed bug where JASP crashes if you load data between running analyses
 - Fixed issue where files from old JASP versions cause problems in newer versions
 - Fixed bug where About window would go behind main window
 - Fixed analysis crash when vovk-sellke was selected in correlations (#1959)
 - Fixed issue with importing of PSPP files (#1966)
 - Fixed issue where RM factor is reset when the analysis options are refreshed (#1921)
 - Changed base of logarithm to e in Bayesian correlation matrix (#1981)
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

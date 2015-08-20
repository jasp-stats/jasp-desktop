Descriptives
============

Descriptives allows you to obtain basic descriptive statistics, histograms and density plots, correlation plots, and frequency tables.

Default Options
-------
###Statistics:
- Central Tendency (only for continuous variables):
  - Mean: Arithmetic mean of the data points
- Dispersion (only for continuous variables):
  - Std. deviation: Standard deviation of the data points
  - Minimum: Minimum value of the data points
  - Maximum: Maximum value of the data points
  
Default Output
-------
### Descriptive Statistics:
- Valid: Number of valid cases
- Missing: Number of missing values
- Mean: Arithmetic mean of the data points 
- Std. Deviation: Standard deviation of the data points 
- Minimum: Minimum value of the data points 
- Maximum: Maximum value of the data points 

Additional Options
------------------
###Display Plots:
- For continuous variables, displays a histogram and the fit of a nonparametric density estimator 
- For nominal and ordinal variables, displays a frequency distribution

### Display Correlation Plot:
- For continuous variables, displays histograms, density plots, and scatterplots

###Display Frequency Tables (nominal and ordinal variables):
- Displays a frequency table for each variable

###Statistics (only for continuous variables):
- Percentile Values:
  - Quartiles: Displays the 25th, 50th, and 75th quartile of the data points
  - Cut points for x equal groups: Displays the cut points that divide the data into x equal groups; default is 4 equal groups
  - Percentiles: Displays the xth percentile; percentile values must be separated by comma
- Central Tendency:
  - Median: Median of the data points
  - Mode: Mode of the data points; if more than one mode exists, only the first is reported
  - Sum: Sum of the data points
- Dispersion:
  - Variance: Variance of the data points
  - Range: Range of the data points; i.e., Maximum-Minimum
  - S.E. Mean: Standard error of the mean
- Distribution:
  -Skewness: Skew of the distribution of the data points
  -Kurtosis: Kurtosis of the distribution of the data points
  
Additional Output
------------------
###Display Plots:
- For continuous variables, displays a histogram and the fit of a nonparametric density estimator 
- For nominal and ordinal variables, displays a frequency distribution

### Display Correlation Plot:
- Displays a matrix of plots between continuous variables, with scatterplots between the variables in the off-diagonal entries, and histograms and density plots in the diagonal entries.
 The line represents the fit of a 1st, 2nd, 3rd, or 4th order polynomial (the selection is based on the Bayesian information criterion; Schwarz, 1978).

###Display Frequency Tables (nominal and ordinal variables):
- Displays a frequency table for each variable
  - Frequency: Frequency of occurrence of each value
  - Percent: Percentage of occurrence of each value
  - Valid Percent: Valid percentage of occurrence of each value
  - Cumulative Percent: Cumulative percentage

###Descriptive Statistics:
  - Std. Error of Mean: Standard error of the mean
  - Median: Median of the data points 
  - Mode: Mode of the data points
  - Variance: Variance of the data points
  - Skewness: Skew of the distribution of the data points
  - Std. Error of Skewness: Standard error of the skewness
  - Kurtosis: Kurtosis of the distribution of the data points
  - Std. Error of Kurtosis: Standard error of kurtosis
  - Range: Range of the data points 
  - Sum: Sum of the data points
  - Quartiles: 25th, 50th, and 75th quartile of the data points
  - Cut points for x equal groups: Cut points that divide the data into x equal groups
  - Percentiles: Displays the xth percentiles

References
-------
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.
- Schwarz, G. (1978). Estimating the dimension of a model. *Annals of Statistics, 6*, 461-464.
- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers.

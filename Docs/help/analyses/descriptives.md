Descriptives
============

Descriptives allows you to obtain basic descriptive statistics, histograms and density plots, correlation plots, and frequency tables.

Options
-------
###Statistics:
- Central Tendency:
  - Mean: Arithmetic mean of the data points (only for numeric variables).
- Dispersion:
  - Std. deviation: Standard deviation of the data points (only for numeric variables).
  - Minimum: Minimum of the data points (only for numeric variables).
  - Maximum: Maximum of the data points (only for numeric variables).
  
Output
-------
### Descriptive Statistics:
- Valid: Number of valid cases.
- Missing: Number of missing cases.
- Mean: Arithmetic mean of the data points. 
- Std. Deviation: Standard deviation of the data points. 
- Minimum: Minimum of the data points. 
- Maximum: Maximum of the data points. 

Example
-------
### Data set: 
- Beer Alcohol Content

### Input: 
- Obtain the basic descriptive statistics of the variables "Percent Alcohol", "Calories", "Carbohydrates", "Brand", and "Brewery".

### Output: 
- The basic descriptive statistics are displayed in the table titled "Descriptive Statistics".

Additional Options
------------------
###Display Plots:
- For numeric variables, displays a histogram and a density plot for each variable.
- For nominal and ordinal variables, displays a frequency distribution for each variable.

### Display Correlation Plot:
- Displays histograms, density plots, and scatterplots between numerical variables.

###Display Frequency Tables (nominal and ordinal variables):
- For nominal and ordinal variables, displays a frequency table for each variable.

###Statistics:
- Percentile Values:
  - Quartiles: Displays the 25th, 50th, and 75th quartile of the data points (only for numeric variables).
  - Cut points for x equal groups: Displays the cut points that devide the data into x equal groups; default is 4 equal groups (only for numeric variables).
  - Percentiles: Displays the xth percentile; percentile values must be separated by comma (only for numeric variables).
- Central Tendency:
  - Median: Median of the data points (only for numeric variables).
  - Mode: Mode of the data points; if more than one mode exists, only the first is reported (only for numeric variables).
  - Sum: Sum of the data points (only for numeric variables).
- Dispersion:
  - Variance: Variance of the data points (only for numeric variables).
  - Range: Range of the data values (i.e., Maximum-Minimum; only for numeric variables).
  - S.E. Mean: Standard error of the mean (only for numeric variables).
- Distribution:
  -Skewness: Skew of the distribution of data points.
  -Kurtosis: Kurtosis of the distribution of data points.
  
Additional Output
------------------
###Display Plots:
- For numeric variables, displays a histogram and a density plot for each variable.
- For nominal and ordinal variables, displays a frequency distribution for each variable.

### Display Correlation Plot:
- Displays a matrix of plots between numerical variables, with scatterplots between the variables in the off-diagonal entries, and histograms and density plots in the diagonal entries.

###Display Frequency Tables (nominal and ordinal variables):
- For nominal and ordinal variables, displays a frequency table for each variable.

###Descriptive Statistics:
  - Std. Error of Mean: Standard error of the mean.
  - Median: Median of the data points. 
  - Mode: Mode of the data points.
  - Variance: Variance of the data points.
  - Skewness: Skew of the distribution of data points.
  - Std. Error of Skewness: Standard error of skew.
  - Kurtosis: Kurtosis of the distribution of data points.
  - Std. Error of Kurtosis: Standard error of kurtosis.
  - Range: Range of the data points. 
  - Sum: Sum of the data points.
  - Quartiles: Displays the 25th, 50th, and 75th quartile of the data points.
  - Cut points for x equal groups: Displays the cut points that divide the data into x equal groups.
  - Percentiles: Displays the xth percentiles.

References
-------
 - Moore, D.S., McCabe, G.P., Craig, B.A. (2012). Introduction to the Practice of Statistics (7th ed.). New York, NY: W.H. Freeman and Company.
 - Whitlock, M.C. & Schluter, D. (2015). The Analysis of Biological Data (2nd ed.). Greenwood Village, Colorado: Roberts and Company Publishers.


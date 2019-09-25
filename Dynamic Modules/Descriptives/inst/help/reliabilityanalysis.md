Reliability analysis
===

Reliability analysis allows the user to test the scale's ability to consistently measure a variable.

### Input
---

#### Assignment Box
- Variables: All variables of interest.

#### Scale statistics
- McDonald's w.
- Cronbach's alpha.
  - Unstandardized.
  - Standardized.
- Gutmann's lambda 6.
- Greatest lower bound.
- Average interitem correlation.
- Mean: Arithmetic mean of the data points.
- Standard deviation: Standard deviation of the data points.

#### Individual item statistics
Statistics for individual items.
- McDonald's w.
- Cronbach's alpha: The most common measure of scale reliability.
- Gutmann's lambda 6.
- Mean: Arithmetic mean of the data points.
- Standard deviation: Standard deviation of the data points.
- Item-rest correlation: The item-rest correlation indicates how the item correlates with. the sum of the rest of the items.

### Reverse-scaled items
- This allows the user to select reverse-scaled items that need to be recoded.

### Advanced Options
- Missing Values:
  - Exclude cases analysis by analysis: In case of multiple correlations tests within a single analysis, each test will be conducted using all cases with valid data for the variables for the particular test.
 Sample sizes may therefore vary across the tests.
  - Exclude cases listwise: In case of multiple correlation tests within a single analysis, each test will be conducted using only cases with valid data for all variables. Sample size is therefore constant across the tests.
- Confidence Interval: 
  - Cronbach's alpha  (analytic)
    - Confidence: default is 95%.

### Output 
--- 
#### Reliability Analysis 
Scale Reliability Statistics: 
- mean: Arithmetic mean of the data points.
- sd: Standard deviation of the data points. 
- McDonald's w. 
- Cronbach's alpha: The most common measure of scale reliability. 
- Gutmann's lambda 6. 
- Average interitem correlation. 
- `...`% Confidence Interval: Confidence Interval for Croanbach's alpha. 
  - Lower: The lower bound of the confidence interval. 
  - Upper: The upper bound of the confidence interval. 

#### Item Statistics 
Item Reliability Statistics: 
- The first column contains all the variables included in the analysis. 
- Mean: Arithmetic mean of the data points.
- Standard deviation: Standard deviation of the data points.
- Item-rest correlation: The item-rest correlation indicates how the item correlates with. the sum of the rest of the items.
- If item dropped: 
  - McDonald's w.
  - Cronbach's alpha. 
  - Gutmann's lambda 6. 

### R Packages
---
- Psych

### Example 
- For an example go to `Open` --> `Data Library` --> `Descriptives` --> `Fear of Statistics`. 

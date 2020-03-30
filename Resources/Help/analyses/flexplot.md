Flexplot 
=== 

Flexplot allows the user to create graphical displays of data, using barcharts and histograms for univariate data, and various different types of scatterplots for bivariate and multivariate data. 

### Input 
--- 
#### Assignment Box 
- Dependent Variable: The variable of interest. This is also called the outcome variable. If only an outcome variable is specified, Flexplot will produce a histogram for numeric data and a barchart for categorical data. If independent variable(s) and/or panelled variable(s) are specified, this variable will be displayed on the Y axis. 
- Independent Variable(s): The variable(s) for which we wish to visually assess relationships with the DV. The first variable chosen shows up on the X axis, either as a scatterplot (for numeric predictors) or as a beeswarm plot (for categorical variables). The second variable chosen will show up as different colors/lines/symbols. If the second varaible chosen is numeric, it will be binned first.
- Panelled Variable(s): Variables specified in these boxes will be binned (if numeric) then displayed as different subplots. The first variable specified as a panelled variable will form the column plots, while the second will form the row plots. 

### Options 
- Point controls: 
    - Point transparency: The degree of transparency of the dots in the graphics
    - Jitter in X: The maximal amount of "jittering" used on the X axis to remove overlap between datapoints. The maximal amount of jittering is proportional to the density of the data. In other words, the maximum jittering will occur at the mode of the dataset, and little to no jittering will occur in locations where there is no overlap. 
    - Jitter in Y: The maximal amount of "jittering" used on the Y axis to remove overlap between datapoints. The maximal amount of jittering is proportional to the density of the data. In other words, the maximum jittering will occur at the mode of the dataset, and little to no jittering will occur in locations where there is no overlap. 
  
- Visual Statistics:
    - Plot confidence bands: Should 95% confidence intervals be displayed?
    - Fitted line (scatterplots): The type of fitted line displayed when the x axis is a numeric variable. These can be one of the following:
      - Loess: A non-parametric loess line
      - Regression: A straight (regression) line
      - Quadratic: A line that includes both a linear effect and a quadratic (squared) term
      - Cubic: A line that includes a linear, squared, and cubed term
    - Intervals (categorical predictors):
      - Quartiles: Horizontal lines are displayed at the 25/75th percentiles, with a dot for the median
      - Standard errors: Horizontal lines are displayed at +1/-1 standard errors from the mean, with a dot for the mean
      - Standard deviations: Horizontal lines are displayed at +1/-1 standard deviations from the mean, with a dot for the mean
- Other plot controls:
    - GGplot theme: the type of GGplot theme to use when displaying the data. Can be one of the following:
      - JASP
      - Black and white
      - Minimal
      - Classic
      - Dark
    - Ghost lines: Ghost lines are a visual aid that can be used when doing panelled plots. Ghost lines simply repeat the fitted line from one panel across the other panels to make it easier to make comparisons across panels. 
      


### References 
--- 
-	Fife, D.A., (in press). The Eight Steps of Data Analysis: A Graphical Framework to Promote Sound Statistical Analysis. *Perspectives on Psychological Science.* doi: 10.31234/osf.io/r8g7c
- Fife, D.A., (2020). Flexplot: graphically-based data analysis. https://doi.org/10.31234/osf.io/kh9c3


### R Packages
---
- ggplot2
- flexplot
- cowplot
- ggplot2 
- MASS 
- tibble
- withr 
- dplyr 
- magrittr
- forcats 
- purrr
- plyr
- R6

### Example 
--- 
- For more details about flexplot in JASP, watch this <a href="https://www.youtube.com/watch?v=N2vM74rw6-Q&list=PL8F480DgtpW8pF6MmNaEUR95n1RmIgasP&feature=youtu.be">video</a>.

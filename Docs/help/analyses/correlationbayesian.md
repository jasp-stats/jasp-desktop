Bayesian Correlation Matrix
==========================

The Bayesian Correlation Matrix allows you to test the null hypothesis that the population Pearson product-moment correlation between pairs of variables equals 0.

Assumptions
-----------
- Numeric variables.
- The data are a random sample from the population for each variable.
- The pairs of variables follow a bivariate normal distribution in the population.

Options
-------
### Hypothesis:
- Correlated: Two-sided alternative hypothesis that the population correlation does not equal 0. 
- Correlated positively: One-sided alternative hypothesis that the population correlation is larger than 0.
- Correlated negatively: One-sided alternative hypothesis that the population correlation is smaller than 0.

### Bayes Factor
- BF10: Bayes factor to quantify evidence for the alternative hypothesis relative to the null hypothesis.
- BF01: Bayes factor to quantify evidence for the null hypothesis relative to the alternative hypothesis.
- Log(BF10): Natural logarithm of BF10.

- Report Bayes factors: Report Bayes factor for each test.
 
Output
-------
### Bayesian Pearson Correlations:
- Pearson's R: Pearson product-moment correlation coefficient.
- BF10 (or BF01): Bayes factor. If one-sided test is requested: 
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is larger than 0.
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population correlation is smaller than 0.
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is larger than 0.
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis relative to the one-sided alternative hypothesis that the population correlation is smaller than 0.

Example
-------

### Data set: 
- NBA Business Revenue and Team Value

### Input: 
- The null hypotheses that the pairwise population correlations between the business revenue ("Revenue"), team value ("Value"), debt ("Debt"), and income ("Income") of teams in the National Basketball Association (NBA) equals 0 are tested
  against the two-sided alternative hypotheses that the pairwise correlations does not equal 0.

### Output: 
- The Pearson correlation between "Value" and "Revenue" equals -0.296; the data are 1.039501 times more likely under the null hypothesis that the population correlation equals 0 
than under the two-sided alternative hypothesis that the population correlation does not equal 0, BF10 = 0.962. 
- The Pearson correlation between "Value" and "Debt" equals 0.986; the data are 3.772e+19 times more likely under the two-sided alternative hypothesis that the population correlation does 
not equal 0 than under null hypothesis that the population correlation equals 0, BF10 = 3.772e+19.
- The Pearson correlation between "Value" and "Income" equals 0.683; the data are 2995.516 times more likely under the two-sided alternative hypothesis that the population correlation does 
not equal 0 than under null hypothesis that the population correlation equals 0, BF10 = 2995.516.
- The Pearson correlation between "Revenue" and "Debt" equals -0.311; the data are 1.126 times more likely under the two-sided alternative hypothesis that the population correlation does 
not equal 0 than under null hypothesis that the population correlation equals 0, BF10 = 1.126.
- The Pearson correlation between "Revenue" and "Income" equals -0.276; the data are 1.267427 times more likely under the null hypothesis that the population correlation equals 0 
than under the two-sided alternative hypothesis that the population correlation does not equal 0, BF10 = 0.789.
- The Pearson correlation between "Debt" and "Income" equals 0.678; the data are 2494.484 times more likely under the two-sided alternative hypothesis that the population correlation does 
not equal 0 than under null hypothesis that the population correlation equals 0, BF10 = 2494.484.

Additional Options
-------
- Flag supported correlations: Mark correlations in the "Bayesian Pearson Correlations" table that are supported by the Bayes factor.

### Plots
  - Correlation Matrix: Displays a (matrix of) scatterplot(s) between the variables.
    - Densities for variables: Add a histogram and the corresponding density plot for each variable to the Correlation Matrix.
    - Posteriors under H1: Adds a density plot of the posterior of the correlation under the alternative hypothesis to the Correlation Matrix.

Additional Output
-------
### Pearson Correlations:
- Flag supported correlations: Correlations that are supported to the Bayes factor are marked with:
  - *Bayes factor > 10 if the data are at least 10 times more likely under the chosen hypothesis.
  - **Bayes factor > 30 if the data are at least 30 times more likely under the chosen hypothesis.
  - ***Bayes factor > 100 if the data are at least 100 times more likely under the chosen hypothesis.

### Plots:
  - Correlation Matrix: Displays a (matrix of) scatterplot(s) between the variables. The scatterplots appear in the upper off-diagonal entries of the
    matrix. The solid black line in the scatterplots represents (TBA).
    - Densities for variables: Displays a histogram and the corresponding density plot for each variable in the diagonal entries of the matrix.
    - Posteriors under H1: Displays density plot(s) of the posterior of the correlation(s) [rho] under the alternative hypothesis in the lower off-diagonal entries of the
    matrix.
      
References
-------
 - TBA
















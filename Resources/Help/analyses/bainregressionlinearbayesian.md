
Bayesian Linear Regression
==========================

Bayesian linear regression allows you to evaluate (informative) hypotheses using the Bayes factor. A simple example would be the Bayesian evaluation of H0: b1 = b2 = b3 versus H1: b1 > b2 > b3 versus Hu: no restrictions on the three regression coefficients.


### Specification of the Bayesian Linear Regression

- Choose the dependent variable from the variable list and move it to the Dependent Variable box.
- Choose the predictors from the variable list and move them to the Covariates box.
- When you execute Bayesian Linear Regression for the first time tick both additional statistics and the plot. When you return to Bayesian Linear Regression you will know what each of these four options renders and you can tick only the options you need.
- If you hypotheses involve the comparison of regression coefficients, for example, b1 = b2 = b3, then the regression coefficients have to be on the same scale. You can achieve this by ticking the Standardize box under additional options, which implies that your hypotheses and the results are in terms of standardized regression coefficients.
- When you tick model constraints a box opens in which you can specify the hypotheses you want to evaluate. You need to adhere to the following specification rules:
(1) Each hypothesis is concluded with a ;
(2) The regression coefficients are referred to using the names of the predictors to which they correspond, for example, age, weight and size, if those are the names of the predictors placed in the Covariates box.
(3) To specify a hypothesis you can use =, <, >, (), +, -, *, &, numbers, and predictor names. A number of example:
a) age = weight = size which is equivalent to age = weight & weight = size
b) age > weight > size
c) age > weight & age > size which is equavalent to age > (weight, size)
d) age + weight < 2*size
e) age > 0 & weight > 0 & size > 0
A complete specification would, for example, look like: age = weight = size; age > weight > size;
For further elaboration you are referred to the document "Easy Hypothesis Specification" that can be found on the Bain website at https://informative-hypotheses.sites.uu.nl/software/bain/

### Results obtained after running Bayesian Linear Regression

- To be able to properly interpret the results of a Bayesian Linear Regression, you are required to read the TUTORIAL by Hoijtink, Mulder, van Lissa, and Gu (2018) that can be retrieved from the Psychological Methods website or from the Bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- If you want to understand the technical background of Bain you should read Gu, Mulder, and Hoijtink (2017) and Hoijtink, Gu, and Mulder (2018) that can be retrieved from the British Journal of Mathematical and Statistical Psychology website or from the Bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- Four pieces of results are obtained after running a Bayesian Linear Regression:
(1) The table in which the Bayes facor of each hypothesis specified versus its complement (that is, not the hypothesis) is presented. This table also contains the posterior model probabilies of each hypothesis. Both for a set excluding and a set including the unconstrained hypothesis.
(2) The Bayes factor matrix in which the mutual Bayes factors of the hypotheses specified in the Model Constraints box are presented.
(3) A desriptives table containing the estimates of the regression coefficients, their standard error (se) and 95% credible interval.
(4) A plot of the pmp's (excluding and including the unconstrained hypothesis) visually highlighting the support in the data for each hypothesis entertained.

### References

- Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology.
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods.
- Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. Britisch Journal of Mathematical and Statistical Psychology. DOI:10.1111/bmsp.12110
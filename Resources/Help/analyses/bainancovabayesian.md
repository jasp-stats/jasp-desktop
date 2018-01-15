
Bayesian ANCOVA
==========================

Bayesian ANCOVA allows you to evaluate (informative) hypotheses using the Bayes factor. A simple example would be the Bayesian evaluation of H0: m1 = m2 = m3 versus H1: m1 > m2 > m3 versus Hu: no restrictions on the three adjusted means.


### Specification of the Bayesian ANCOVA

- Choose the dependent variable from the variable list and move it to the Dependent Variable box.
- Choose the factor from the variable list and move it to the Fixed Factors box.
- Choose the covariate(s) from the variable list and move it/them to the Covariates box.
Note that, all groups have to be collected in ONE factor. If you have, for example, a factor with the levels young-old and a factor with the levels female-male, you have to create ONE new factor with the levels young female, old female, young male, old male.
- When you execute Bayesian ANCOVA for the first time tick both additional statistics and both plots. When you return to Bayesian ANCOVA you will know what each of these four options renders and you can tick only the options you need.
- When you tick model constraints a box opens in which you can specify the hypotheses you want to evaluate. You need to adhere to the following specification rules:
(1) Each hypothesis is concluded with a ;
(2) The levels of the ONE factor are referred to as follows: factor.levelname. If, for example, there is a factor age with levels y, m, o. They are reffered to using age.y, age.m, and age.o, respectively.
(3) To specify a hypothesis you can use =, <, >, (), +, -, *, &, numbers, and factor levels. A number of example:
a) age.y = age.m = age.o which is equivalent to age.y = age.m & age.m=age.o
b) age.y > age.m > age.o
c) age.y > age.m & age.m > age.o which is equavalent to age.y > (age.m,age.o)
d) age.y + age.m < 2*age.o
e) age.y - age.o > .8
A complete specification would, for example, look like: age.y = age.m = age.o; age.y > age.m > age.o;
For further elaboration you are referred to the document "Easy Hypothesis Specification" that can be found on the Bain website at https://informative-hypotheses.sites.uu.nl/software/bain/

### Results obtained after running Bayesian ANCOVA

- To be able to properly interpret the results of a Bayesian ANCOVA, you are required to read the TUTORIAL by Hoijtink, Mulder, van Lissa, and Gu (2018) that can be retrieved from the Psychological Methods website or from the Bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- If you want to understand the technical background of Bain you should read Gu, Mulder, and Hoijtink (2017) and Hoijtink, Gu, and Mulder (2018) that can be retrieved from the British Journal of Mathematical and Statistical Psychology website or from the Bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- Five pieces of results are obtained after running a Bayesian ANCOVA:
(1) The table in which the Bayes facor of each hypothesis specified versus its complement (that is, not the hypothesis) is presented. This table also contains the posterior model probabilies of each hypothesis. Both for a set excluding and a set including the unconstrained hypothesis.
(2) The Bayes factor matrix in which the mutual Bayes factors of the hypotheses specified in the Model Constraints box are presented.
(3) A coefficients table containing for each group in the ANCOVA the sample size, the adjusted mean, standard error (se) and 95% credible interval. Furthermore, this table contains the same information for the covariate(s).
(4) A plot of the pmp's (excluding and including the unconstrained hypothesis) visually highlighting the support in the data for each hypothesis entertained.
(5) A plot of the adjusted means and their credible intervals.

### References

- Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology.
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods.
- Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. Britisch Journal of Mathematical and Statistical Psychology. DOI:10.1111/bmsp.12110
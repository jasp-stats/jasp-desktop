Bain One Sample t-test
==========================

The bain (Bayesian informative hypotheses evaluation) One Sample t-test allows you test the nulthypothesis that the mean of a variable is zero versus one-sided and two-sided alternative hypotheses.

### Specification of the bain One Sample t-test

- Choose variables from the variable list and move them to the box on the right.
- When you execute the bain One Sample t-test for the first time tick the additional statistics and both plots. When you return to the bain One Sample t-test you will know what each of these three options renders and you can tick only the options you need.
- By default 95% credible intervals will be presented in the results. If desired the degree of belief (by default 95%) can be changed.
- You can choose from five testing situations (note that the reference value "ref"" can be entered in the "Test value" box):

1. H0: m = ref versus H1: m (no constraint on the mean)
2. H0: m = ref versus H1: m > ref
3. H0: m = ref versus H1: m < ref
4. H1: m > ref versus H2: m < ref
5. H0: m = ref versus H1: m > ref versus H2: m < ref

- If under the Bayes Factors label you choose BF01, values of the Bayes factor larger than 1 express support in favor of H0. If you choose BF10, values of the Bayes factor larger than 1 express support in favor of H1 (or H2).

### Results obtained after running the bain One Sample t-test

- To be able to properly interpret the results of the bain One Sample t-test, you are required to read the TUTORIAL by Hoijtink, Mulder, van Lissa, and Gu (2018) that can be retrieved from the Psychological Methods website or from the bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- If you want to understand the technical background of bain you should read Gu, Mulder, and Hoijtink (2017) and Hoijtink, Gu, and Mulder (2018) that can be retrieved from the British Journal of Mathematical and Statistical Psychology website or from the bain website at https://informative-hypotheses.sites.uu.nl/software/bain/
- Four pieces of results for each pair of variables are obtained after running a bain One Sample t-test:

1. The table in which the Bayes facor of H0 versus the alternative hypothese is presented. This table also contains the posterior model probability of each hypothesis. 
2. A desriptives table containing for each variable the sample size, sample mean, sample standard deviation (sd), standard error (se) and 95% credible interval. 
3. A plot of the pmp's visually highlighting the support in the data for each hypothesis entertained.
4. A plot of the mean difference between pairs of measurements and their credible interval.

### References

- Gu, X., Mulder, J., and Hoijtink, H. (2018). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110
- Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A tutorial on testing hypotheses using the Bayes factor. Psychological Methods. DOI: 10.1037/met0000201 
- Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology. DOI: 10.1111/bmsp.12145

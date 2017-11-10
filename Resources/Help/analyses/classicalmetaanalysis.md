# Meta-analysis in JASP
The new release of JASP supports an extensive arrange of commonly used techniques for meta-analysis. These include fixed and random effects analysis, fixed and mixed effects meta-regression, forest and funnel plots, tests for funnel plot asymmetry, trim-and-fill and fail-safe N analysis, and more. The engine behind this analysis power is the software developed in the [metafor-project](http://www.metafor-project.org/). Here we’ll give a quick run through of all the functionality currently supported in JASP.

## Example analysis 

In the running example below, we’ll use the BCG vaccine data set that compiles available evidence for the effectiveness of the BCG vaccine in preventing tuberculosis. The data is available here:

| trial | author               | year | tpos | tneg  | cpos  | cneg  | ablat | alloc      | ES      | SE     |
| ----- | -------------------- | ---- | ---- | ----- | ----- | ----- | ----- | ---------- | ------- | ------ |
| 1     | Aronson              | 1948 | 4    | 119   | 11    | 128   | 44    | random     | -0.9387 | 0.5976 |
| 2     | Ferguson & Simes     | 1949 | 6    | 300   | 29    | 274   | 55    | random     | -1.6662 | 0.4562 |
| 3     | Rosenthal et al      | 1960 | 3    | 228   | 11    | 209   | 42    | random     | -1.3863 | 0.6583 |
| 4     | Hart & Sutherland    | 1977 | 62   | 13536 | 248   | 12619 | 52    | random     | -1.4564 | 0.1425 |
| 5     | Frimodt-Moller et al | 1973 | 33   | 5036  | 47    | 5761  | 13    | alternate  | -0.2191 | 0.2279 |
| 6     | Stein & Aronson      | 1953 | 180  | 1361  | 372   | 1079  | 44    | alternate  | -0.9581 | 0.0995 |
| 7     | Vandiviere et al     | 1973 | 8    | 2537  | 10    | 619   | 19    | random     | -1.6338 | 0.4765 |
| 8     | TPT Madras           | 1980 | 505  | 87886 | 499   | 87892 | 13    | random     | 0.0120  | 0.0633 |
| 9     | Coetzee & Berjak     | 1968 | 29   | 7470  | 45    | 7232  | 27    | random     | -0.4717 | 0.2387 |
| 10    | Rosenthal et al      | 1961 | 17   | 1699  | 65    | 1600  | 42    | systematic | -1.4012 | 0.2746 |
| 11    | Comstock et al       | 1974 | 186  | 50448 | 141   | 27197 | 18    | systematic | -0.3408 | 0.1119 |
| 12    | Comstock & Webster   | 1969 | 5    | 2493  | 3tneg | 2338  | 33    | systematic | 0.4466  | 0.7309 |
| 13    | Comstock et al       | 1976 | 27   | 16886 | 29    | 17825 | 33    | systematic | -0.0173 | 0.2676 |

The data can be downloaded from [here](https://docs.google.com/spreadsheets/d/e/2PACX-1vQqe6EXN1l8PLWsKgbwBeCKMzp4ZWMSwGmrywA0GGLw714kp3qI7yJrS8TtzqZO2w0I6riKOt1KoCMZ/pub?gid=535697682&single=true&output=csv).

The columns code meta-information about the publication (`author`, `year`), about the reported statistics extracted from the publications (`tpos`, `tneg`, `cpos`, `cneg`), and study characteristics (`ablat`, `alloc`). The columns `tpos`, `tneg`, `cpos`, `cneg` where the number of tbc tests that came out positive, respectively negative, in the treatment and control condition. The studies varied in patient allocation, which could be either random, alternate, or systematic (`alloc`), and varied in geographical latitude of which the absolute value (`ablat`) was determined to be evaluated as a moderator of the effect size.

The additional columns `ES` and `SE` are, respectively, the estimated effect size (chosen to be the log odds ratio) for the study, and its standard error. They were computed in a spreadsheet program with the formulas


> $$ES = \ln\left(\frac{\text{tpos}}{\text{tneg}}\right) - \ln\left(\frac{\text{cpos}}{\text{cneg}}\right)$$, and $$SE=\sqrt{\frac{1}{\text{tpos}} + \frac{1}{\text{tneg}} + \frac{1}{\text{cpos}} + \frac{1}{\text{cneg}}}$$.

Alternatively, they can be computed with the [effect size tool](https://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-OR1.php) on the Campbell Collaboration website. A next release of JASP will incorporate a similar effect size calculator tool.


## Opening the meta-analysis module
![Accessing the meta-analysis module](https://www.dropbox.com/s/qj971vlb0cgrhmf/Screenshot%202017-11-07%2013.49.32.png?raw=1)


If we open the data in JASP we can choose the meta-analysis module from the ‘+’ popup menu, and clicking the ‘Classical’ button. The interface that is displayed is shown below. The main input fields are “Effect Size” and “Effect Size Standard Error”. These are required fields; without inserting any of the available variables in these fields the output will be blank as shown. Other important fields are


1. “Methods”, in which you specify the type of analysis that you want to run. In particular, whether you want to do a Fixed effects analysis or a Random effects analysis, and what type of estimator for the residual heterogeneity variance τ². The default is a random effects analysis with a restricted maximum likelihood estimator (REML).
2. “Study labels”, which allows you to associate a text label with each of the studies.
3. “Covariates” in which you insert any numerical covariates to be included in a meta-regression.
4. “Factors”, in which you insert any nominal scale covariates to be included in a meta-regression.
![](https://www.dropbox.com/s/nmcgozefqd5guxr/Screenshot%202017-11-07%2016.20.36.png?raw=1)

## A simple fixed effects analysis

When we drag the `ES` variable into the “Effect Size” field, and the `SE` variable into the “Effect Size Standard Error” field, we instantaneously obtain a result in the output window. In order to ensure that this is a Fixed effects analysis, we have to set the “Methods” dropdown to ‘Fixed effects’. The result is shown below.

In the case of a Fixed effects analysis, the output shows 2 tables. The first table summarises the significance of the effect sizes. The first line labelled ‘Omnibus test of the Model Coefficients’ tests the null hypothesis that all the coefficients in the second table are all zero. The second line tests the null hypothesis that all the effect sizes in the studies are all equal (*homogeneity* of effect sizes). In this case, both are highly significant, with p-values below .001, refuting the null hypothesis of no effect, and the hypothesis of homogeneity. The latter implies that the vaccine’s effectiveness differed across studies (*heterogeneity* of the effect sizes). The latter suggests that the Fixed effects model is not an appropriate model to summarise the data, and a better description would result from a random effects model, a model with moderators, or a mixed effects model. However, it’s inference can still be useful, as it indicates that it is safe to conclude that the average of the effect sizes of *the current set of studies* is not equal to zero. (See Hedges & Vevea (1998) for a discussion of fixed vs random effects.)

The second table shows the estimated coefficients for the fixed effects, along with a test of their significance. Here only an intercept is included, the reported coefficient of which is the meta-analytic estimate of the overall (simple) effect size of the combined studies: A weighted average of the effect size estimates in which the weight of each study is the precision of the effect size estimate (the inverse squared standard error). The z-test of the coefficient in this case merely confirms the omnibus test in the first table.

In addition to the tables, by default, a so called forest plot is produced in the output. In a forest plot the effect sizes found for each study are marked with a square mark along the x-axis, one study below the other. The order in which the studies are displayed (currently) follows the order of the data file. The size of the marks is proportional to the weight the study has in determining the combined effect size estimate. In addition to the individual effect size estimates the meta-analytically combined effect size estimate is indicated by the diamond shape at the bottom line of the plot. The width of the diamond indicates the 95% confidence interval for the estimate. The confidence intervals for each study are also indicated in the plot with whiskers. In the case of homogeneity of the effect size, these intervals are expected to cover the combined effect size approximately 95% of the time. The fact that this is not the case here (6 out of these 13 study level confidence intervals do no contain the diamond shape) is yet another indication that the Fixed effects analysis does not give an accurate summary of the effect sizes found in this body of literature. On the right sight of the plot these statistics are also displayed numerically. The left hand side shows the study labels, which by default are just the row numbers if no variable was entered in the “Study Labels” field of the interface. The forest plot displayed below was generated by dragging the `author` variable into the “Study Labels” field and saved in PDF format. The forest plot is probably one of the most insightful summary plots of the data in a meta-analysis, and is highly recommended to include in a publication.

![Simple Fixed effects analysis of the BCG vaccine data](https://www.dropbox.com/s/c9jspqa3lpaz0in/Screenshot%202017-11-07%2016.37.07.png?raw=1)

https://www.dropbox.com/s/i093hwe5gbxy4e8/forestplotBCG_FE.pdf?dl=0

## A simple random effects analysis

Above we concluded that a fixed effects analysis of this set of data does not provide an accurate summary of the current data set. The reason is that the analysis ignores excess variance in the effect sizes that cannot be attributed to sampling error. A (very) simple way to do more justice to the data is to provide an estimate of the excess variance. This variance should be interpreted as systematic unaccounted for differences between studies observed effects. The variance of these differences is often denoted $$\tau^2$$. It turns out that this excess variance also affects the weight that each study should have in the meta-analytically combined effect size estimate. An estimate of $$\tau^2$$ and weight adjusted meta-analytic effect size estimate are obtained in a random effects analysis. 

**Setting the Method**
In JASP a random effects analysis is obtained by choosing any of the methods other than ‘Fixed Effects’ in the ‘Method’ drop down menu. The most commonly used methods are DerSimonian-Laird, Maximum Likelihood, and Restricted Maximum likelihood. When the focus of the analysis is directed to the fixed effects part of the model, Restricted ML has been recommended. When the estimate of $$\tau^2$$ is also of interest, ML has been recommended. The results are displayed below. The key difference with a fixed effects analysis is a third table that gives the estimate of $$\tau^2$$, along with some other metrics that have been found to be useful characterizations of the excess variance. (For instance, $$I^2$$ expresses the excess variance as a percentage of the total variance observed in the effect size estimates across studies, and hence is akin an intraclass correlation; for the other measures, please consult the meta-analysis literature.)

![](https://www.dropbox.com/s/cxr55356dqoy0fk/Screenshot%202017-11-07%2017.53.30.png?raw=1)



## A fixed effects meta-regression analysis

In a situation like the current one with the BCG vaccine data set, the random effects model properly makes explicit the excess variance in an estimate of $$\tau^2$$. The random effects model therefore provides a more truthful summary of the effects found in the literature regarding the effectiveness of the vaccine. However, it would be more insightful if we could explain the excess variance. To this end we may try to explain the differences in the effect between studies, by factors that characterise study specific circumstances and characteristics. Factors that may play a role in the current case are the *geographical latitude* where the vaccine was tested (e.g., because of climatological reasons, economical reasons, or other reasons that are correlated with distance to the equator). Also study design, and in particular the method of *allocation* of study participants among the studies treatment arms, may have significant influence on treatment effects. To see if any of these explain the observed between studies differences, we carry out a meta-regression that includes both the covariate `ablat` as well as the factor `alloc`. We choose the fixed effects method to see if these effect size moderators are sufficient to explain all the excess variance. The results are displayed below.

![](https://www.dropbox.com/s/uknt3jgf50aewvr/Screenshot%202017-11-07%2023.12.01.png?raw=1)


Next to the intercept in the Coefficients table, we now see one coefficient for `ablat`, and two for `alloc`. The omnibus test indicates that not all 3 of these coefficients are equal to zero. The test for residual heterogeneity in the first table indicates that although the moderators do explain the differences between studies to some extend, there still is unexplained excess variance. The Coefficients table itself indicates that only the coefficient for `ablat` is in fact non-zero. Note that the forest plot has changed as well: It now includes grey diamonds for each study separately. These are the predicted effect sizes for these studies computed from the meta-regression model.

## A Mixed effects analysis

As was the case with the simple fixed effects model, because there still is unexplained heterogeneity, the fixed effects meta-regression is not an accurate summary of the evidence of the vaccine effectiveness conveyed by this set of studies. The unexplained variance must again be made explicit in an estimate of this variance $$\tau^2$$. To this end we change the Method to Restricted Maximum Likelihood which gives an unbiased estimate of $$\tau^2$$. Again this affects the weight that each of the studies carry in the compounded meta-analytic estimated effect.

![](https://www.dropbox.com/s/zvw2usg05n5y5vi/Screenshot%202017-11-07%2023.33.49.png?raw=1)

# Other options

The module allows for a number of other common statistics to be computed and displayed. 


- **Model** allows for building models with interactions
- **Statistics** offers a range of statistics and plots that are likely of interest, including statistics commonly used for the assessment of publication bias
- **Diagnostics** offers a range of diagnostics, including robustness analysis tools


## Statistics

Typically confidence intervals are interest, which can be added to the tables by ticking the ‘confidence intervals’ box. To types are available: Confidence intervals based on z statistic (Gaussian approximation), and confidence intervals based on the adjustment proposed in (Knapp & Hartung, 2003). The former are most conventional.

Another feature of likely interest are the various tools often used for publication bias assessment. The first is a funnel plot, which plots study precision (1 / SE) against observed effect size (ES). In general this plot is expected to by symmetrical in the vertical axis around the meta-analytic compounded effect size estimate. Also, the points are expected to lie in a confidence triangle 95% of the time. An excessive amount of points outside the interval can result from heterogeneity in fixed effects models, and non-normal distributed between study differences in random or mixed effects models. Asymmetry of the funnel plot is often interpreted as evidence of publication bias. Funnel plot asymmetry can be formally tested with a non-parametric rank test, or the parametric regression test (also known as “Egger’s test”). In both cases, low p-values are indicative of asymmetry, and by extension, taken as evidence for publication bias.

![](https://www.dropbox.com/s/k2yqia9pbcoi8no/Screenshot%202017-11-08%2000.02.07.png?raw=1)


The running example shows no signs of funnel plot asymmetry. Both visually, as well as according to the rank test.

## Diagnostics

Various diagnostics are available. Besides residuals and influential cases assessment, methods specific to meta-analysis are Fail-safe N and trim-and-fill. 

- Fail-safe N estimates the number of zero finding studies needed to add to the list of studies in order to bring the currently observed significance of the meta-analytic effect size to a significant level of $$\alpha = 0.05$$. 
- Trim-and-fill tries to add ‘missing studies’ by adding points that will make the funnel plot symmetric. The added points are then included in the current analysis to adjust the parameter estimates in an attempt to mitigate the influence of publication bias.

Both these methods have been severely criticised and their usefulness is disputed. Below the results for these analysis for the BCG vaccine data are displayed. Fail-safe N is equal to 608, indicating that 608 studies with study effect size 0 would have to be added to the meta-analysis in order to bring the observed significance back to $$\alpha = 0.05$$. The trim-and-fill analysis doesn’t change any of the results, because the funnel plot is not found to be asymmetric. The plot produced for trim-and-fill analysis includes several subplots: A forest plot with the adjusted effect size estimates, a funnel plot that would include the estimated ‘missing studies’ as white (as opposed to black) dots if there were any (not in this example), a radial version of the funnel plot, and a normal q-q plot that can be used to assess departure from normality.

![](https://www.dropbox.com/s/6jfduxfu0hvndab/Screenshot%202017-11-08%2000.28.32.png?raw=1)

https://www.dropbox.com/s/khykvpiqwiqkqe8/trim-fill-bcg_data.pdf?dl=0



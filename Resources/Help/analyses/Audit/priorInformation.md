Incorporating prior information into the statistical anlaysis
==========================

You can incorporate existing information from various sources into the statistical sampling procedure. Possible options are:

- **None:** This option does not incorporate any information into the statistical analysis and therefore assumes a negligible and conservative prior distribution. 

- **Audit Risk Model:** This option can only be selected when testing against a performance materiality. When selected, it incorporates the information from the assessed risks of material misstatement (inherent risk and control risk) from the audit risk model into the statistical analysis. They are mapped to probabilities according to standards, but can also be mapped according to custom preferences.

- High: 100%
- Medium: 60%
- Low: 36%
- Custom

- **Equal prior probabilities:** This option can only be selected when testing against a performance materiality. When selected, it incorporates the information that tolerable misstatement is equally likely to occur a priori as intolerable misstatement. The prior distribution resulting from this method has 50\% of its probability mass below the performance materiality and 50\% of its probability mass above the performance materiality.

- **Custom prior probabilities:** This option can only be selected when testing against a performance materiality. When selected, you can use your own assessment of the a priori probability that the population contains tolerable misstatement. You may also deduct this value as one minus the probability of intolerable misstatement. 

- **Earlier sample:** This option incorporates the information from an earlier sample in the statistical analysis. 

- **Weighted earlier sample:** This option incorporates the information from an earlier sample in the statistical analysis, weighted by a factor *f*, which determines the weight that you assign to the results of the earlier sample.
# Vovk-Sellke MPR (Maximum p-Ratio)

This document describes the implementation of the Vovk-Sellke MPR into each statistical procedure in JASP. Follow the steps to complete a procedure. The necessary global functions are in the file commonMPR.R in the existing Engine/JASP/R directory. At the bottom of this document is a to-do list for where this still needs to be implemented.

##### Outline

1. In analysis R file
2. In JSON file
3. In AnalysisForms file
4. In Markdown help file (if help file exists)


### 1. In analysis R file
##### Within column definition of each table (footnotes need to be initialised)
```r
if (options$VovkSellkeMPR) {
	.addFootnote(footnotes, symbol = "\u002A", text = "Vovk-Sellke Maximum
	<em>p</em>-Ratio: Based the <em>p</em>-value, the maximum
	possible odds in favor of H\u2081 over H\u2080 equals
	1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37
	(Sellke, Bayarri, & Berger, 2001).")
	fields[[length(fields) + 1]] <- list(name = "VovkSellkeMPR",
										title = "VS-MPR\u002A",
										type = "number",
										format = "sf:4;dp:3")
 }
```
##### Within row for-loop (*p*-value needs to be available)
```r
if (options$VovkSellkeMPR){
  res[["VovkSellkeMPR"]] <- VovkSellkeMPR(p)
}
```

### 2. In JSON file
```JSON
{
  "name": "VovkSellkeMPR",
  "type": "Boolean",
  "default": false
},
```

### 3. In AnalysisForms file
Add a selection box under "additional options" with the text "Sellke Maxmum p-Ratio" and name

    VovkSellkeMPR


### 4. In Markdown help file
##### Under Analysis
```Markdown
- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>. 
For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.
```
- Vovk-Sellke Maximum *p*-Ratio: The bound 1/(-e *p* log(*p*)) is derived from the shape of the *p*-value distribution. Under the null hypothesis (H<sub>0</sub>) it is uniform(0,1), and under the alternative (H<sub>1</sub>) it is decreasing in *p*, e.g., a beta(&#945;, 1) distribution, where 0 < &#945; < 1. The Vovk-Sellke MPR is obtained by choosing the shape &#945; of the distribution under H<sub>1</sub> such that the obtained *p*-value is *maximally diagnostic*. The value is then the ratio of the densities at point *p* under H<sub>0</sub> and H<sub>1</sub>. 
For example, if the two-sided *p*-value equals .05, the Vovk-Sellke MPR equals 2.46, indicating that this *p*-value is at most 2.46 times more likely to occur under H<sub>1</sub> than under H<sub>0</sub>.

##### In References
```Markdown
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of *p* values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
```

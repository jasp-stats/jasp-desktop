Regularized Linear Regression
==========================

Regularized linear regression is an adaptation to linear regression in which the coefficients are shrunken towards 0. This is done by applying a penalty (e.g., ridge, lasso, or elastic net). The parameter λ controls the degree to which parameters are shrunken.

### Assumptions
- The target variable is a continuous variable.
- The predictor variables consist of continuous, nominal, or ordinal variables.

### Input 
-------

#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Predictors: In this box, the variables that provide information about the target variable should be entered. 
- Weights: In this box, an optional variable containing case weights can be entered.

#### Tables  
- Evaluation metrics: shows commonly used regression evaluation metrics such as mean squared error (MSE), root mean squared error (RMSE) and R<sup>2</sup>.
- Regression coefficients: gives the regression coefficient for each predictor.

#### Plots
- Data split: shows how the data is split into training (and validation), and test set. Numbers displayes is the size of each set.
- Predictive performance: shows the selected test set observations against their predicted values.
- Variable trace: shows the development of the coefficients as lambda increases.
- \u03BB evaluation: shows the cross-validated MSE across different values of \u03BB, including an indication for the optimal \u03BB values.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set.

#### Training and Validation Data
- Sample *x*% for validation data: Randomly sample a percentage from the remaining data (after selecting the test set).

### Training Parameters 
#### Algorithmic Settings

- Penalty: specifies which penalty is used in shrinking the regression coefficients. The options are ridge, lasso, and elastic net (see James, Witten, Hastie, & Tibshirani, 2013).
- Fit intercept: specified whether the regression function should include an intercept or not.
- Lambda (\u03BB): specifies the shrinkage parameter. It can be fixed to a specific value, optimized according to which value gives the lowest mean squared error (MSE) in cross-validation, or set to the largest value that was within 1 standard error (SE) of the minimum MSE during cross-validation.
- Scale variables: Scales the continuous variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Add Predicted Values to Data
Generates a new column in your dataset with the values of your regression result. This gives you the option to inspect, cluster, or predict the generated values.

### Output
-------

#### Regularized Linear Regression Model Table
- Penalty: shows the penalty that is applied in the model.
- \u03BB: gives the chosen value for the shrinkage parameter \u03BB.
- n(Train): the number of observations in the training set.
- n(Validation): the number of observations in the validation set (enabled when model is optimized).
- n(Test): the number of observations in the test set.
- Validation MSE: MSE on the validation set (enabled when model is optimized).
- Test set MSE: MSE on the test set.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- glmnet

### Example 
--- 
- For an example data set go to `Open` --> `Data Library` --> `Machine Learning` --> `Student Grades`.  
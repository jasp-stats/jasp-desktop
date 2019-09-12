Boosting Regression
==========================

Boosting works by sequentially adding predictors to an decision tree ensemble, each one correcting its predecessor. Boosting tries to fit the new predictor to the residual errors made by the previous predictor.

### Assumptions
- The target variable is a continuous variable.
- The predictor variables consist of continuous, nominal, or ordinal variables.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Predictors: In this box, the variables that provide information about the target variable should be entered.

#### Tables  
- Evaluation metrics: Shows commonly used classification evaluation metrics like mean squared error (MSE), root mean squared error (RMSE) and R<sup>2</sup>.
- Relative influence: Shows the relative influence of the predictors.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- Out-of-bag improvement: Plots the number of trees against the out-of-bag classification accuracy improvement of the model. Accuracy is assessed for the training set.
- Predictive performance: Shows the selected test set observations against their predicted values.
- Deviance: Shows the prediction error plotted against the number of trees.
- Relative influence: Shows the relative influence of the predictors.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set. This indicator should be a column in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.

#### Training and Validation Data
- Sample *x*% for validation data: Randomly sample a percentage from the remaining data (after selecting the test set).
- K-fold with *k* folds: Partition the remaining data in *k* parts.

### Training Parameters 
#### Algorithmic Settings
- Shrinkage: A shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction 0.001 to 0.1 usually work, but a smaller learning rate typically requires more trees.
- Interaction depth: Integer specifying the maximum depth of each tree (i.e., the highest level of variable interactions allowed. A value of 1 implies an additive model, a value of 2 implies a model with up to 2-way interactions, etc. Default is 1.
- Min. observations in node: Integer specifying the minimum number of observations in the terminal nodes of the trees. Note that this is the actual number of observations, not the total weight.
- Training data used per tree: Select the percentage of training data that is used to train each individual tree.
- Loss function: The loss function used. Can be either *Gaussian*, *Laplace*, or *t*.
- Scale variables: Scales the continuous variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Number of Trees
- Fixed: Enables you to use a user-specified number of decision trees. 
- Optimized: Enables you to optimize the prediction error on a validation data set with respect to the number of trees. 
- Max. number of trees: Sets the maximum number of possible decision trees to be considered. At default, this is set to 100.

#### Add Predicted Values to Data
Generates a new column in your dataset with the values of your regression result. This gives you the option to inspect, cluster, or predict the generated values.

### Output
-------

#### Boosting Regression Model Table
- The first column shows the number of trees.
- Shrinkage: The shrinkage parameter.
- n(Train): The number of observations in the training set.
- n(Validation): The number of observations in the validation set (enabled when model is optimized).
- n(Test): The number of observations in the test set.
- Validation MSE: The MSE on the validation set (enabled when model is optimized).
- Test set MSE: The MSE on the test set.

#### Evaluation Metrics
- MSE: The mean squared error of the model.
- RMSE: The root mean squared error of the model.
- MAE: The mean absolute error of the model.
- MAPE: The mean absolute percentage error of the model.
- R<sup>2<\sup>: The proportion of the variance for a dependent variable that's explained by an independent variable or variables.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- gbm

### Example 
--- 
- For an example data set go to `Open` --> `Data Library` --> `Machine Learning` --> `Student Grades`.  


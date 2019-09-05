K-Nearest Neighbors Regression
==========================

K-nearest neighbors is a method of regression that looks at the *k* number of predictor observations that are most similar to new observations to make a prediction for their values. The number of nearest neighbors is intrinsincly linked to model complexity, as small numbers increase the flexibility of the model.

### Assumptions
- The predictors consist of continuous, nominal, or ordinal variables.
- The target is a continuous variable.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Predictors: In this box the variables that provide information about the target variable should be entered. 

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set.

#### Training and Validation Data
- Sample *x*% for validation data: Randomly sample a percentage from the remaining data (after selecting the test set).
- K-fold with *k* folds: Partition the remaining data in *k* parts.
- Leave-one-out: Partition the remaining data in *n* parts.

### Training Parameters 
#### Algorithmic Settings
- Weights: sets the weighting scheme for the nearest neighbors. The default rectangular option results in standard knn, while the other options expand the algorithm by weighing the nearest neighbors. See also the kknn package.
- Distance: the distance metric to be used when determining the similarity between nearest neighbors. Can be either Euclidean or Manhattan distance.
- Scale variables: scales the variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.

#### Number of Nearest Neighbors
- Fixed: enables you to use a user-specified number of nearest neighbors. 
- Optimized: enables you to optimize the prediction error on a validation data set with respect to the number of nearest neighbors. 
- Max. number of nearest neighbors: sets the maximum number of possible nearest neighbors to be considered. At default, this is set to 10.

#### Tables  
- Evaluation metrics: shows commonly used classification evaluation metrics like mean squared error (MSE), root mean squared error (RMSE) and R<sup>2</sup>.

#### Plots
- Data split: shows how the data is split into training (and validation), and test set. Numbers displayes is the size of each set.
- Mean squared error: plots the number of nearest neighbors against the mean squared error of the model. Accuracy is assessed for the training (and validation) set.
- Predictive performance: shows the selected test set observations against their predicted values.

#### Add Predicted Values to Data
Generates a new column in your dataset with the values of your regression result. This gives you the option to inspect, cluster, or predict the generated values.

### Output
-------

#### K-Nearest Neighbors Regression Model Table
- The first column shows the number of nearest neighbors.
- Weights: The weighting scheme.
- Distance: The distance used.
- n(Train): The number of observations in the training set.
- n(Validation): The number of observations in the validation set (enabled when model is optimized).
- n(Test): The number of observations in the test set.
- Validation MSE: The mean squared error on the validation set (enabled when model is optimized).
- Test set MSE: The mean squared error on the test set.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- kknn
- ROCR

### Example 
--- 
- For an example go to `Open` --> `Data Library` --> `Machine Learning` --> `Student Grades`.  


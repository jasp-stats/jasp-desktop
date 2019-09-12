K-Nearest Neighbors Classification
==========================

K-nearest neighbors is a method of classification that looks at the *k* number of predictor observations that are most similar to new observations to make a prediction for their class assignments. The number of nearest neighbors is intrinsincly linked to model complexity, as small numbers increase the flexibility of the model.

### Assumptions
- The target is a nominal or ordinal variable. 
- The predictor variables consist of continuous, nominal, or ordinal variables.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Predictors: In this box the variables that provide information about the target variable should be entered. 

#### Tables  
- Confusion matrix: Displays a table that shows the observed classes against the predicted classes. Used to assess model accuracy.
- Class proportions: Displays a table that shows the proportions of each class in the data set, training (and validaton), and test set.
- Evaluation metrics: Shows commonly used classification evaluation metrics like precision, recall, the F1-score, support and AUC (area under the ROC curve).

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set.
- Classification accuracy: Plots the number of nearest neighbors against the classification accuracy of the model. Accuracy is assessed for the training (and validation) set.
- ROC curves: Displays ROC curves for each class predicted against all other classes.
- Andrews curves: Is a way to visualize structure in high-dimensional data. Lines that cluster are observations that are more alike. 
- Decision boundary matrix: Creates a *n* x *n* plot that visualizes how every observation would be classified if predicted through the current model. Boundaries between classes are visualized. Can only be made for numeric predictors.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set. This indicator should be a column in your data that consists of only 0 (excluded from the test set) and 1 (included in the test set). The data will then be split into a training (and validation if requested) set (0), and a test set (1) according to your indicator.

#### Training and Validation Data
- Sample *x*% for validation data: Randomly sample a percentage from the remaining data (after selecting the test set).
- K-fold with *k* folds: Partition the remaining data in *k* parts.
- Leave-one-out: Partition the remaining data in *n* parts.

### Training Parameters 
#### Algorithmic Settings
- Weights: Sets the weighting scheme for the nearest neighbors. The default rectangular option results in standard knn, while the other options expand the algorithm by weighing the nearest neighbors. See also the kknn package.
- Distance: The distance metric to be used when determining the similarity between nearest neighbors. Can be either Euclidean or Manhattan distance.
- Scale variables: Scales the continuous variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Number of Nearest Neighbors
- Fixed: Enables you to use a user-specified number of nearest neighbors. 
- Optimized: Enables you to optimize the prediction error on a validation data set with respect to the number of nearest neighbors. 
- Max. number of nearest neighbors: Sets the maximum number of possible nearest neighbors to be considered. At default, this is set to 10.

#### Add Predicted Classes to Data
Generates a new column in your dataset with the class labels of your classification result. This gives you the option to inspect, classify, or predict the generated class labels.

### Output
-------

#### K-Nearest Neighbors Classification Model Table
- The first column shows the number of nearest neighbors.
- Weights: The weighting scheme.
- Distance: The distance used.
- n(Train): The number of observations in the training set.
- n(Validation): The number of observations in the validation set (enabled when model is optimized).
- n(Test): The number of observations in the test set.
- Validation accuracy: The classification accuracy on the validation set (enabled when model is optimized).
- Test set accuracy: The classification accuracy on the test set.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- kknn
- ROCR

### Example 
--- 
- For an example data set go to `Open` --> `Data Library` --> `Machine Learning` --> `Telco Customer Churn`.  


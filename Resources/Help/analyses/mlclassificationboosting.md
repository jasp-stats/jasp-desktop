Boosting Classification
==========================

Boosting works by sequentially adding predictors to an decision tree ensemble, each one correcting its predecessor. However, instead of changing the weights for every incorrect classified observation at every iteration, Boosting method tries to fit the new predictor to the residual errors made by the previous predictor.

### Assumptions
- The target variable is a nominal or ordinal variable.
- The predictor variables consist of continuous, nominal, or ordinal variables.

### Input 
-------
#### Assignment Box 
- Target: In this box, the variable that needs to be predicted should be entered. 
- Predictors: In this box, the variables that provide information about the target variable should be entered. 

#### Tables  
- Confusion matrix: Displays a table that shows the observed classes against the predicted classes. Used to assess model accuracy.
- Class proportions: Displays a table that shows the proportions of each class in the data set, training (and validaton), and test set.
- Evaluation metrics: Shows commonly used classification evaluation metrics like precision, recall, the F1-score, support and AUC (area under the ROC curve).
- Relative influence: Shows the relative influence of the predictors.

#### Plots
- Data split: Shows how the data is split into training (and validation), and test set. Numbers displayes is the size of each set.
- Out-of-bag improvement: Plots the number of trees against the out-of-bag classification accuracy improvement of the model. Accuracy is assessed for the training set.
- ROC curves: Displays ROC curves for each class predicted against all other classes.
- Andrews curves: Is a way to visualize structure in high-dimensional data. Lines that cluster are observations that are more alike. 
- Deviance: Shows the prediction error plotted against the number of trees.
- Relative influence: Shows the relative influence of the predictors.
- Decision boundary matrix: Creates a *n* x *n* plot that visualizes how every observation would be classified if predicted through the current model. Boundaries between classes are visualized. Can only be made for numeric predictors.

### Data Split Preferences
#### Holdout Test Data
- Sample *x*% of all data: Choose a percentage to randomly sample from your data to derive prediction error. Generates an internal indicator variable that indicates whether the observation is included (1) or excluded (0) from the test set.
- Add generated indicator to data: Add the generated test set indicator from the option above to your data set. Requires a column name.
- Test set indicator: Use an indicator variable to select data for the test set.

#### Training and Validation Data
- Sample *x*% for validation data: Randomly sample a percentage from the remaining data (after selecting the test set).
- K-fold with *k* folds: Partition the remaining data in *k* parts.

### Training Parameters 
#### Algorithmic Settings
- Shrinkage: A shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step-size reduction 0.001 to 0.1 usually work, but a smaller learning rate typically requires more trees.
- Interaction depth: Integer specifying the maximum depth of each tree (i.e., the highest level of variable interactions allowed. A value of 1 implies an additive model, a value of 2 implies a model with up to 2-way interactions, etc. Default is 1.
- Min. observations in node: Integer specifying the minimum number of observations in the terminal nodes of the trees. Note that this is the actual number of observations, not the total weight.
- Training data used per tree: Select the percentage of training data that is used to train each individual tree.
- Scale variables: Scales the continuous variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: Gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis. For example, setting a seed makes it possible to re-run analyses with the same data splits.

#### Number of Trees
- Fixed: Enables you to use a user-specified number of decision trees. 
- Optimized: Enables you to optimize the prediction error on a validation data set with respect to the number of trees. 
- Max. number of trees: Sets the maximum number of possible decision trees to be considered. At default, this is set to 100.

#### Add Predicted Classes to Data
Generates a new column in your dataset with the class labels of your classification result. This gives you the option to inspect, classify, or predict the generated class labels.

### Output
-------

#### Boosting Classification Model Table
- The first column shows the number of trees.
- Shrinkage: The shrinkage parameter.
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
- gbm
- ROCR

### Example 
--- 
- For an example data set go to `Open` --> `Data Library` --> `Machine Learning` --> `Telco Customer Churn`.  


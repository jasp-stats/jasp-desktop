Random Forest Classification
==========================

Random Forest is a method of classification that creates a set of decision trees that consists of a large number of individual trees which operate as an ensemble. Each individual tree in the random forest returns a class prediction and the class with the most votes becomes the model’s prediction.

### Assumptions
- The predictors consist of continuous, nominal, or ordinal variables.
- The target is a nominal or ordinal variable

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

### Training Parameters 
#### Algorithmic Settings
- Training data used per tree: select the percentage of training data that is used to train each individual tree.
- Predictors per split: set the number of predictor variables that is used within each split in the decision trees. Auto defaults to  
- Scale variables: scales the variables. Standardization ensures that values of variables from different scales range into a specific similar scale. As a result, standardizing provides numerical stability, which improves the clustering output. JASP uses the Z-score standardization of a mean of 0 and a standard deviation of 1. This option is selected by default.
- Set seed: gives the option to set a seed for your analysis. Setting a seed will exclude random processes influencing an analysis.

#### Number of Trees
- Fixed: enables you to use a user-specified number of decision trees. 
- Optimized: enables you to optimize the prediction error on a validation data set with respect to the number of trees. 
- Max. number of trees: sets the maximum number of possible decision trees to be considered. At default, this is set to 100.

#### Tables  
- Confusion matrix: displays a table that shows the observed classes against the predicted classes. Used to assess model accuracy.
- Class proportions: displays a table that shows the proportions of each class in the data set, training (and validaton), and test set.
- Evaluation metrics: shows commonly used classification evaluation metrics like precision, recall, the F1-score, support and AUC (area under the ROC curve).
- Variable importance: shows the mean decrease in accuracy and total increase in node purity for all predictor variables. These are indicators of the importance of the predictors.

#### Plots
- Data split: shows how the data is split into training (and validation), and test set. Numbers displayes is the size of each set.
- Out-of-bag accuracy: plots the number of trees against the out-of-bag classification accuracy of the model. Accuracy is assessed for the training (and validation) set.
- ROC curves: displays ROC curves for each class predicted against all other classes.
- Andrews curves: is a way to visualize structure in high-dimensional data. Lines that cluster are observations that are more alike. 
- Mean decrease in accuracy: displays the variable mean decrease in accuracy for the model.
- Total increase in node purity: displays the variable total increase in node purity for the model.
- Decision boundary matrix: creates a n x n plot that visualizes how every observation would be classified if predicted through the current model. Boundaries between classes are visualized. Can only be made for numeric predictors.

#### Add Predicted Classes to Data
Generates a new column in your dataset with the class labels of your classification result. This gives you the option to inspect, classify, or predict the generated class labels.

### Output
-------

#### Random Forest Classification Model Table
- The first column shows the number of trees.
- Predictors per split: The number of predictors used at each split in the decision trees.
- n(Train): The number of observations in the training set.
- n(Validation): The number of observations in the validation set (enabled when model is optimized).
- n(Test): The number of observations in the test set.
- Validation accuracy: The classification accuracy on the validation set (enabled when model is optimized).
- Test set accuracy: The classification accuracy on the test set.
- OOB accuracy: The out-of-bag classification accuracy on the test set.

### References
-------
- James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning. Springer New York.

### R-packages 
--- 
- randomForest
- ROCR


### Example 
--- 
- For an example go to `Open` --> `Data Library` --> `Machine Learning` --> `Telco Customer Churn`.  


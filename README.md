## Diabetes Prediction Classifier Analysis
This repository contains the analysis of a dataset aimed at predicting diabetes status using various classification models. The dataset, [diabetes_5050.csv](https://github.com/racxxhel/Diabetes-Predictor-Classifier-Analysis/blob/main/diabetes_5050.csv), consists of survey responses collected in the US in 2015. 

**Overview of the Analysis**

The goal of this project is to identify the best classifier for predicting whether an individual has diabetes, based on survey data. The dataset includes both categorical and continuous features, with the response variable being binary (indicating whether or not the individual has diabetes). In the analysis, several preprocessing steps, exploratory data analysis (EDA), and classification models were applied to determine the best classifier for this prediction task.

**Key Steps in the Analysis**
* Data Exploration and Preprocessing:
* Data Cleaning: Variables that are weakly associated with the response variable were removed to focus on the most relevant predictors.
* Variable Association: Variables were analyzed for their association with the response variable using statistical tests such as Chi-squared (for categorical variables) and correlation coefficients (for continuous variables).
* Classification Models
* Model Evaluation

**Conclusion** 

Logistic Regression emerged as the best performing model overall, with the highest values across key performance metrics (accuracy, precision, recall, F1-score, and AUC).
Decision Tree showed lower recall, potentially due to a high false-negative rate.
Naive Bayes had a high AUC but relatively low accuracy, making it unsuitable when precision is a key concern.
KNN showed the lowest accuracy, possibly due to its sensitivity to outliers and high-dimensional data.

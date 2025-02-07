"This R code uses the lavaaan package to perform Confirmatory Factor Analysis (CFA) on a dataset, 
visualizes the model, and assesses reliability."

#Step 0. Call libraries with used functions
library(psy)      # has cronbach
library(lavaan)
library(semPlot)
library(semTools)
library(MVN)

#Step 1. Provides a summary of the dataset
summary(dataset)

#Step 2. Assign the dataset to a variable for further use
cfa_data <- dataset  

#Step 3. Run Mardia's test to checlk for multivariate normality on the dataset
mvn(data = cfa_data,  mvnTest = "mardia")  

#Step 4. Initiate a variable that specifies your CFA model. Specify your latent
#variables and the items they are measured by, as found in your EFA, using the lavaan syntax.
#e.g., Your_CFA_model <- 'Factor_1 =~ Column_1 + Column_2 + Column_3 + ... + Column_n
#.                        Factor_2 =~ Column_11 + Column_12 + Column_13 + ... + Column_k
#.                        Column_11 ~~ Column_14'
#The "~~" indicates when latent variables are allowed to correlate (calculates covariance).

Your_CFA_Model <- ' '

#Step 5. Fit the CFA model using the "cfa" function from the lavaan package. 
#Arguments for "cfa" function:
#     SE: Your_CFA_model 
#     data = your dataset
#     std.lv = TRUE, Standardize latent variables.
#     test = "satorra.bentler": Use Satorra-Bentler corrected chi-square.
#     missing = "listwise": Handle missing data by listwise deletion.
#     estimator = "MLM": Maximum Likelihood estimation with robust (Huber-White) standard errors.Only for complete data
#                 "MLMVS": maximum likelihood estimation with robust standard errors and a mean- and variance-adjusted test statistic.
#                 "MLR": maximum Likelihood with robust standard errors
#                 Others: https://lavaan.ugent.be/tutorial/est.html

fit <- cfa(Your_CFA_Model, data = cfa_data, estimator = "MLR")


#Step 6. Print a summary of the model fit, including fit measures, standardized estimates, and R-squared values (, modindices = F, rsq = T).
summary(fit, fit.measures = T, standardized = T) 

#Step 7. Print modification indices
modificationIndices(fit)

#Step 8. Print model residuals
residuals(fit, type = "cor")

#Step 9. Calculate reliability measures for the fitted model
reliability(fit) 

#Step 10. Calculate Cronbach's alpha for the subsets of items to assess internal consistency reliability
cronbach() 

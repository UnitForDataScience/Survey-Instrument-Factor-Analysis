"This R code uses the lavaaan package to perform Confirmatory Factor Analysis (CFA) on a dataset, 
visualizes the model, and assesses reliability."

#install.packages("lavaan", dependencies = TRUE)

#Step 0. Load packages with used functions
library(psy)         # Used to calculate Cronbach's alpha
library(lavaan)      # Allows us to fit our model and run a CFA  
library(semPlot)     # Used for model visualization
library(semTools)    # Used to calculate the reliability of our model fit
library(MVN)         # Used to check for multivariate normality    

#Step 1. Assign the data set to a variable for further use
cfa_data <- SAQ_Item_3_Reversed_ 

#Step 2. Provides a summary of the data set
summary(cfa_data)

#Step 3. Run Mardia's test to check for multivariate normality on the dataset
mvn(data = cfa_data,  mvnTest = "mardia")  

#In the following steps, we specify our model, run our CFA, calculate our fit measures, 
#and calculate reliability and internal consistency

#Step 4. Initiate a variable that specifies your CFA model. Specify your latent
#variables and the items they are measured by, as found in your EFA, using the lavaan syntax.
#e.g., Your_CFA_model <- 'Factor_1 =~ Column_1 + Column_2 + Column_3 + ... + Column_n
#.                        Factor_2 =~ Column_11 + Column_12 + Column_13 + ... + Column_k
#.                        Column_11 ~~ Column_14'
#The "~~" indicates when latent variables are allowed to correlate (calculates covariance).

Your_CFA_Model <- 'f1 =~ Q01 + Q03 + Q04+ Q05 + Q12 + Q16 + Q20 + Q21
                   f2 =~ Q06 + Q07 + Q10 + Q13 + Q18  
                   f3 =~ Q08 + Q11 + Q17
                   f4 =~ Q02 + Q09 + Q19 + Q22 + Q23 
'

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

#Step 9. Calculate Cronbach's alpha for the subsets of items to assess internal consistency reliability
cronbach(cfa_data[,c(1, 3, 4, 5, 12, 16, 20, 21)]) 
cronbach(cfa_data[,c(6, 7, 10, 13, 18)]) 
cronbach(cfa_data[,c(8, 11, 17)]) 
cronbach(cfa_data[,c(2, 9, 19, 22, 23)]) 

#Step 10. Visualize your CFA model using semPaths from the semPlot package. 
#Parameters: layout, sizes, inclusion of intercepts and residuals, curve shape, rotation, and colors.

semPaths(fit, what = "path", "std", "lisrel",
         layout = "tree2", sizeMan = 5,
         sizeMan2 = 6,
         intercepts = FALSE,
         residuals = TRUE,
         curve = 1.5,
         rotation = 1,
         color = list(lat = rgb(245, 253, 118, maxColorValue = 255),
                      man = rgb(155, 253, 175, maxColorValue = 255)),
         mar = c(10, 5, 10, 5))

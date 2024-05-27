##########################################
# Stats Lab
# Predicting Staff Costs per Department
# Model - Lasso Regression
##########################################

####### Packages #######
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(car)
library(carData)
library(glmnet)
library(Matrix)
library(caret)
library(lattice)
library(grplasso)
library(grpreg)
#######################
# Note: Want to create a model for each department

# Removing NA rows
cleaned.all.departments.combined.shifted.data <- lapply(all.departments.combined.shifted.data, na.omit)
all.departments.combined.data2 <- lapply(all.departments.combined.data2, na.omit) # Relevant Data, Use this one

# Different lists to perform regression with 
all.departments.combined.data # scaled, raw data
cleaned.all.departments.combined.shifted.data # scaled, log transformed
all.departments.combined.data2 # predictors scaled + raw, staff_costs log transformed, want to use these!

############################### TRAIN-TEST DATA SPLIT ###############################

train.data <- list()
test.data <- list()


for(department in names(all.departments.combined.data2)) { # loop through each department
  
  dept.data <- all.departments.combined.data2[[department]] # select department specific data
  
  unique_muni <- unique(dept.data$GEMEINDE)
  n <- length(unique_muni)
  sample.size <- floor(0.75 * n)
  
  train.muni <- sample(unique_muni, size = sample.size)  # generate random sample of municipalities
  train.ind <- dept.data$GEMEINDE %in% train.muni  # index for training set
  
  train.data[[department]] <- dept.data[train.ind, ]  # training data
  test.data[[department]] <- dept.data[!train.ind, ]  # test data
}



############################# LINEAR REGRESSION ####################################

for(department in names(train.data)) {
  
  # Remove non-predictor columns
  predictors <- colnames(train.data[[department]]) != "LOG.STAFF.COSTS" & colnames(train.data[[department]]) != "GEMEINDE" & colnames(train.data[[department]]) != "STAFF_COSTS"
  
  # prep training variables
  x.train <- train.data[[department]][, predictors]
  y.train <- train.data[[department]]$LOG.STAFF.COSTS
  
  # prep testing variables
  x.test <- test.data[[department]][, predictors]
  y.test <- test.data[[department]]$LOG.STAFF.COSTS
  
  # model fitting per department (linear regression)
  linreg <- lm(y.train ~ ., data = cbind(y.train, x.train))
  summary(linreg)$adj.r.squared  # adjusted R-squared 
  
  # prediction on test data
  y_test_pred <- predict(linreg, newdata = x.test)
  
  # MSE
  mse.test <- mean((y_test_pred - y.test)^2)
  
  print(paste("Department:", department, "MSE:", mse.test, "Adj. R-squared:", summary(linreg)$adj.r.squared))
}

# Warning: rank-deficient fit
# This means there may be issues with: Multicollinearity, Overfitting

############################# LASSO REGRESSION ON TRAINING DATA #####################################################

models.per.department_lasso <- list() 
mse_scores_train_lasso <- list()
r2_scores_train_lasso <- list()

for(department in names(train.data)) {
  
  formula <- as.formula(paste("LOG.STAFF.COSTS ~ . - GEMEINDE - STAFF_COSTS")) # exclude non-predictor columns
  x_train <- model.matrix(formula, data = train.data[[department]])[,-1]  # fits lasso regression, removes intercept column
  y_train <- train.data[[department]]$LOG.STAFF.COSTS
  
  cv_model <- cv.glmnet(x_train, y_train, alpha = 1) # λ = amount of shrinkage
  # cv.glmnet performs cross-validation, picks λ with lowest average prediction error
  
  models.per.department_lasso[[department]] <- cv_model # stores model
  #plot(cv_model) # plots MSE vs log(λ) per department
  
  # extra step, uses fitted model to predict training response
  predictions <- predict(models.per.department_lasso[[department]], newx = x_train, s = "lambda.min")
  
  mse_train <- sum((predictions - y_train)^2) / length(y_train) # calculate MSE
  mse_scores_train_lasso[[department]] <- mse_train
  
  # R-Squared Value
  ss_total <- sum((y_train - mean(y_train))^2)
  ss_res <- sum((y_train - predictions)^2)
  r2_train <- 1 - (ss_res / ss_total)
  r2_scores_train_lasso[[department]] <- r2_train
  
  print(paste("Department:", department, "MSE:", mse_train, "R-squared:", r2_train))
  
  
}

# extracting Coefficients per department
lasso_coefficients_per_department <- list()

for(department in names(models.per.department_lasso)) {
  
  # get coefficients where lambda has minimum mean cross-validated error
  coefficients <- coef(models.per.department_lasso[[department]], s = "lambda.min")
  
  # keep only non-zero coefficients
  non_zero_coefficients <- coefficients[, 1] != 0
  non_zero_coefficient_names <- rownames(coefficients)[non_zero_coefficients]
  
  lasso_coefficients_per_department[[department]] <- non_zero_coefficient_names
}


########################### EVALUATING MODEL ON TEST DATA ############################

mse_scores_lasso <- list()
r2_scores_lasso <- list()


for(department in names(test.data)) {
  
  formula <- as.formula(paste("LOG.STAFF.COSTS ~ . - GEMEINDE - STAFF_COSTS"))
  
  x_test <- model.matrix(formula, data = test.data[[department]])[,-1] # fits lasso regression, removes intercept column
  y_test <- test.data[[department]]$LOG.STAFF.COSTS
  
  predictions <- predict(models.per.department_lasso[[department]], newx = x_test) # predictions based on model
  
  mse_test <- sum((predictions - y_test)^2) / length(y_test) # calculate MSE
  
  mse_scores_lasso[[department]] <- mse_test
  
  # R^2 for test data
  ss_total_test <- sum((y_test - mean(y_test))^2)
  ss_res_test <- sum((y_test - predictions)^2)
  r2_test <- 1 - (ss_res_test / ss_total_test)
  r2_scores_lasso[[department]] <- r2_test
  
  print(paste("Department:", department, "MSE:", mse_test, "R-squared:", r2_test))
  
}

mse_scores_lasso

############################# LASSO REGRESSION USING ALL AVAILABLE DATA #####################################################

# Initialize
models.per.department_lasso <- list() 
mse_scores_lasso <- list()
r2_scores_lasso <- list()


for(department in names(all.departments.combined.data2)) {
  
  dept.data <- all.departments.combined.data2[[department]] 
    formula <- as.formula(paste("LOG.STAFF.COSTS ~ . - GEMEINDE - STAFF_COSTS")) 
  
  # Model matrix
  x <- model.matrix(formula, data = dept.data)[,-1]  # Remove intercept column
  y <- dept.data$LOG.STAFF.COSTS
  
  # Lasso regression with cross-validation
  cv_model <- cv.glmnet(x, y, alpha = 1) # λ = amount of shrinkage
  # cv.glmnet performs cross-validation, picks λ with lowest average prediction error
  models.per.department_lasso[[department]] <- cv_model 
  
  # Predictions
  predictions <- predict(models.per.department_lasso[[department]], newx = x, s = "lambda.min")
  
  # MSE
  mse <- sum((predictions - y)^2) / length(y) 
  mse_scores_lasso[[department]] <- mse
  
  #  R-Squared Value
  ss_total <- sum((y - mean(y))^2)
  ss_res <- sum((y - predictions)^2)
  r2 <- 1 - (ss_res / ss_total)
  r2_scores_lasso[[department]] <- r2
  
  print(paste("Department:", department, "MSE:", mse, "R-squared:", r2))
}

############################## PREDICTING STAFF COSTS BASED ON MODEL WITH ALL DATA ###########################

mse_scores_test_lasso <- list()
r2_scores_test_lasso <- list()
predictions <- list()

for(department in names(all.departments.combined.data2)) {
  
  dept.data <- all.departments.combined.data2[[department]]
  formula <- as.formula(paste("LOG.STAFF.COSTS ~ . - GEMEINDE - STAFF_COSTS"))
  
  # Model Matrix
  x_test <- model.matrix(formula, data = dept.data)[,-1]  # Remove intercept column
  y_test <- dept.data$LOG.STAFF.COSTS
  
  # Predictions
  predictions[[department]] <- predict(models.per.department_lasso[[department]], newx = x_test, s = "lambda.min")
  
  # MSE
  mse_test <- sum((predictions[[department]] - y_test)^2) / length(y_test) 
  mse_scores_test_lasso[[department]] <- mse_test
  
  #  R-Squared Value
  ss_total_test <- sum((y_test - mean(y_test))^2)
  ss_res_test <- sum((y_test - predictions[[department]])^2)
  r2_test <- 1 - (ss_res_test / ss_total_test)
  r2_scores_test_lasso[[department]] <- r2_test
  
  print(paste("Department:", department, "MSE (test):", mse_test, "R-squared (test):", r2_test))
}

################ PREPARING DATA FOR CSV OUTPUT ####################
all.x <- list()
all.y <- list()

for(department in names(all.departments.combined.data2)) {
  dept.data <- all.departments.combined.data2[[department]]
  all.x[[department]] <- dept.data[, -c(251, 252)] # remove STAFF_COSTS and LOG.STAFF.COSTS columns
  all.y[[department]] <- dept.data[, c(1, 2, 252)]  # keep columns year, municipality, and LOG.STAFF.COSTS columns
}

for(department in names(all.y)) {
  if (department %in% names(predictions)) {
    dept.data <- all.y[[department]]
    pred.data <- predictions[[department]]
    
    pred.data <- as.data.frame(pred.data)
    colnames(pred.data) <- "pred_LOG.STAFF.COSTS"
    
    all.y[[department]] <- cbind(dept.data, pred.data)
    
    # Write to CSV
    write.csv(all.y[[department]], paste0(department, "_fabian_pred_true_lassoFINAL.csv"))
  } else {
    print(paste("No predictions available for department:", department))
  }
}

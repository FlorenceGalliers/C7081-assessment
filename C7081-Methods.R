## HEADER ####
## Florence Galliers 
## 2020-11-23
## C7081 Assignment Methods Script

# 0.0 Set wd and libraries ####
setwd("~/Google Drive/Harper/1-C7081/Asssesment/github-C7081/C7081-assessment")

require(knitr)
require(ggplot2)
require(glmnet)
require(pls)
require(leaps)
require(tree)
require(randomForest)
require(gbm)
require(dummies)
require(car)
require(plyr)
require(writexl)
require(psych)
require(ggcorrplot)
require(dummies)
library(openxlsx)
require(ggfortify)
require(lmtest)

# 0.1 Import cleaned data set created after Exploratory Data Analysis ####
data <- read.xlsx("cleaned-data.xlsx")
data$if_basement <- as.factor(data$if_basement)
data$if_renovated <- as.factor(data$if_renovated)
data$city <- as.factor(data$city)
# 0.2 Split Data into Test and Train ####
set.seed(22) # set seed
n <- nrow(data) # create variable with number of rows
train_index <- sample(1:n, size = round(0.8*n), replace=FALSE) 
train <- data[train_index ,] # takes 80% of the data for training set
test <- data[-train_index ,] # remaining 20% for the test set
# As we will be using dummy variables further on, I have also created the 
# test and train data sets containing all the dummy variables 
dummy_data <- dummy.data.frame(data, sep = ".")
names(dummy_data)
dummy_train <- dummy.data.frame(train, sep = ".")
names(dummy_train)
dummy_test <- dummy.data.frame(test, sep = ".")
names(dummy_test)

# 1.0 Simple Linear Regression ####
# Create simple linear model using price as dependent variable and sqft_living
# as independent variable, and training data set
set.seed(2)
simple_lm <- lm(price ~ sqft_living, 
                data = train)
# Make predictions using this model on test data set
simple_pred <- predict(simple_lm, 
                       test) 
# calculate MSE
simple_lm_MSE <- mean((test[, "price"] - simple_pred)^2)
# calculate RMSE
simple_lm_RMSE <- sqrt(simple_lm_MSE)
# print summary of model
summary(simple_lm)

# 2.0 Multiple Linear Regression ####
# Create a multiple linear model using price as dependent variable and all
# other variables, training data set
multiple_lm <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+condition
                  +if_basement+house_age+if_renovated+city,
                  data = train)
# Make predictions on test data set
multiple_pred <- predict(multiple_lm, test)
# calculate MSE 
multiple_lm_MSE <- mean((test[, "price"] - multiple_pred)^2)
# calculate RMSE
multiple_lm_RMSE <- sqrt(multiple_lm_MSE)
# print summary of model
summary(multiple_lm)

# 2.1 Investigate Collinearity of model containing all variables ####
vif(multiple_lm)

# 2.2 Multiple Linear Regression with Selected Variables ####
# Create a linear model with only the variables that showed significance in the previous multiple linear model.
multiple_selective <- lm(price ~ bedrooms+bathrooms+sqft_living+condition+
                           if_basement.1+house_age+city.Bellevue+city.Issaquah+
                           city.Kent+city.Kirkland+city.Medina+city.Mercerisland+
                           city.Redmond+city.Sammamish+city.Seattle+city.Shoreline+
                           city.Woodinville, data = dummy_train)
# Make predictions using this model on test data set
multi_select_pred <- predict(multiple_selective, dummy_test)
# calculate MSE of model
multiple_select_MSE <- mean((dummy_test[, "price"] - multi_select_pred)^2)
# calculate RMSE of model
multiple_select_RMSE <- sqrt(multiple_select_MSE)
# print summary of model
summary(multiple_selective)
# Plot diagnostic plots
par(mfrow=c(2,2))
plot(multiple_selective)
# 1. Assumption of linear relationship holds true as horizontal line
# 2. Residuals follow line but there are some points with high influence, 4296, 4292, 2252
# 3. Variability of residuals increases as fitted values increase
# Breusch Pagen Test for homoscedasticity
bptest(multiple_selective) # p > 0.05 so accept null hypothesis, we have homoscedasticity here
# 4. No points exceeding Cook's distance, point 4296 is just on the borderline.

# Histogram of Residuals
hist(multiple_selective$residuals, breaks = 500)
# It has a tail... not normally distributed

par(mfrow=c(1,1))
# Plot actual vs predicted values for this model
plot(dummy_test[,"price"], multi_select_pred)

multiple_log <- lm(log(price) ~ bedrooms+bathrooms+sqft_living+condition+
                           if_basement.1+house_age+city.Bellevue+city.Issaquah+
                           city.Kent+city.Kirkland+city.Medina+city.Mercerisland+
                           city.Redmond+city.Sammamish+city.Seattle+city.Shoreline+
                           city.Woodinville, data = dummy_train)
# Make predictions using this model on test data set
log_pred <- predict(multiple_log, dummy_test)
# calculate MSE of model
m_log_MSE <- mean((dummy_test[, "price"] - log_pred)^2)
# calculate RMSE of model
m_log_RMSE <- sqrt(m_log_MSE)

# 3.0 Best Subset Selection ####
# Fit subset selection model
bestsub <- regsubsets(price ~ .,
                      data = train,
                      nvmax = 25)
# Create test matrix
test_matrix <- model.matrix(price ~ .,
                            data = test)
# Create loop for finding validation errors for a model of each size
val_errors <- rep(NA, 25)
for(i in 1:25) {
  coefi <- coef(bestsub, id = i)
  pred <- test_matrix[ ,names(coefi)]%*%coefi
  val_errors[i] = mean((test$price - pred)^2)}
val_errors # print validation errors
which.min(val_errors) # print the minimum validation error
plot(val_errors) # plot validation errors 
# shows which number of variables has the lowest validation error
coef(bestsub, 23) # 23 variables is optimal from validation error

# create prediction formula 
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi}

# Choosing among models of different sizes using cross-validation
k <- 5
folds <- sample(1:k, nrow(data), replace = TRUE)
cv.errors <- matrix(NA, k, 25, dimnames = list(NULL, paste(1:25)))

for(j in 1:k){
  best.fit <- regsubsets(price ~ ., 
                         data = data[folds!=j,],
                         nvmax = 25)
  for (i in 1:25) {
    pred <- predict.regsubsets(best.fit, 
                               data[folds==j,], 
                               id = i)
    cv.errors[j, i] = mean((data$price[folds==j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # average cv errors into table
mean.cv.errors # get the mean cv error for models of each size

plot(mean.cv.errors, type = "b")
which.min(mean.cv.errors) #shows that min cv.error is with 19 variables

mean.cv.errors[19] # get mean cv error for 19 variable model

# Create new test and train data sets containing only the best 19 variables as 
# selected by best subset selection
bestsub_summary <- summary(bestsub)
which_bestsub <- bestsub_summary$which
variables_bestsub <- which_bestsub[19,]
best_variables <- which(variables_bestsub == TRUE)

best_train <- dummy_train[ ,best_variables]
best_test <- dummy_test[, best_variables]

# Run linear model using only the best 15 variables selected from best subset selection
final_bestsub <- lm(price ~ .,
                    data = best_train)

summary(final_bestsub)

# make predictions using this model on test data set
final_bestsub_pred <- predict(final_bestsub,
                              best_test)

# calculate MSE of best 15 variable model from subset selection
final_bestsub_MSE <- mean((best_test[, "price"] - final_bestsub_pred)^2)
# calculate RMSE of best 15 variable model from subset selection
final_bestsub_RMSE <- sqrt(final_bestsub_MSE)


# 4.0 Forward Stepwise Selection ####
forward_model <- regsubsets(price ~ ., 
                         data = data, 
                         nvmax = 25,
                         method = "forward")

summary(forward_model)

# Create loop for finding validation errors for a model of each size in forward selection
fwd_errors <- rep(NA, 25)
for(i in 1:25) {
  coefi <- coef(forward_model, id = i)
  pred <- test_matrix[ ,names(coefi)]%*%coefi
  fwd_errors[i] = mean((test$price - pred)^2)
}

fwd_errors
which.min(fwd_errors) # it says minimum is 24, but there seems to be some overfitting
# after around 20 variables, a low point at 15.

plot(fwd_errors)
# 16 variable model also has a low validation error

# Create new test and train data sets containing only the best 16 variables as 
# selected by forward selection
forward_summary <- summary(forward_model)
which_forward <- forward_summary$which
variables_fwd <- which_forward[24,]
fwd_variables <- which(variables_fwd == TRUE)

forward_train <- dummy_train[ ,fwd_variables]
forward_test <- dummy_test[, fwd_variables]

# Run linear model using 15 variables from forward selection
forward_model <- lm(price ~ .,
                    data = forward_train)

summary(forward_model)
# make predictions using this model on test data set
forward_pred <- predict(forward_model,
                        forward_test)
# calculate MSE of best 15 variable model from subset selection
forward_MSE <- mean((forward_test[, "price"] - forward_pred)^2)
# calculate RMSE of best 15 variable model from subset selection
forward_RMSE <- sqrt(forward_MSE)

# 5.0 Backward Stepwise Selection ####
backward_model <- regsubsets(price ~ ., 
                         data = data, 
                         nvmax = 25, 
                         method = "backward")

summary(backward_model)

# Create loop for finding validation errors for a model of each size in backward selection
bwd_errors <- rep(NA, 25)
for(i in 1:25) {
  coefi <- coef(backward_model, id = i)
  pred <- test_matrix[ ,names(coefi)]%*%coefi
  bwd_errors[i] = mean((test$price - pred)^2)
}

bwd_errors
which.min(bwd_errors) # minimum is 22

plot(bwd_errors)

# Create new test and train data sets containing only the best 15 variables as 
# selected by forward selection
backward_summary <- summary(backward_model)
which_backward <- backward_summary$which
variables_bwd <- which_backward[22,]
bwd_variables <- which(variables_bwd == TRUE)

backward_train <- dummy_train[ ,bwd_variables]
backward_test <- dummy_test[, bwd_variables]

# Run linear model using 15 variables from backward selection
backward_model <- lm(price ~ .,
                     data = backward_train)

summary(backward_model)
# make predictions using this model on test data set
backward_pred <- predict(backward_model,
                         backward_test)
# calculate MSE of best 15 variable model from subset selection
backward_MSE <- mean((backward_test[, "price"] - backward_pred)^2)
# calculate RMSE of best 15 variable model from subset selection
backward_RMSE <- sqrt(backward_MSE)

# 6.0 Ridge Regression ####
# Fit a ridge regression, choose lambda by cross validation
set.seed(1)
# Split train and test data into x and y 
x_train <- model.matrix(price ~ ., data = train)[,-1]
x_test <- model.matrix(price ~ ., data = test)[,-1]
y_train <- train$price
y_test <- test$price
# Finding optimal value of lambda using cross validation
cv.ridge <- cv.glmnet(x_train, 
                      y_train, 
                      alpha = 0)
# Assign it to best.lambda variable
best.lambda <- cv.ridge$lambda.min
best.lambda
plot(cv.ridge) 
# Report test error of ridge regression 
ridge.mod <- glmnet(x_train, y_train, alpha = 0)
ridge.pred <- predict(ridge.mod, 
                      s = best.lambda,
                      newx = x_test)
ridge_MSE <- mean((ridge.pred - y_test)^2) 
ridge_RMSE <- sqrt(ridge_MSE) 

# 7.0 LASSO Regression ####
set.seed(1)
# Fit lasso model
lasso_mod <- glmnet(x_train, 
                    y_train, 
                    alpha = 1)
# Shows for a sequence of lambda, the values of the coefficients
# Numbers at the top of the graph show how many variables are included at each
# value of lambda - this is LASSO model selection capacity.
plot(lasso_mod, 
     xvar='lambda')

# Now we need to find which value of lambda minimises the difference between 
# predicted and actual values, and therefore has lowest MSE
set.seed(1)
lasso_cv <- cv.glmnet(x_train, 
                      y_train,
                      alpha = 1)

# Plot values of MSE against lambda, can see it is fairly stable but starts to 
# increase slightly after less than 18 variables are included in the model
plot(lasso_cv)

# Assign the lowest value of lambda to an object
best_lamb <- lasso_cv$lambda.min
# Use this value of lambda to make predictions on test data set
lasso_pred <- predict(lasso_mod, s = best_lamb, 
                      newx = x_test)
# Report test error and number of non zero coefficients for LASSO
lasso_MSE <- mean((lasso_pred - y_test)^2)
lasso_RMSE <- sqrt(lasso_MSE)

out <- glmnet(x_train, y_train, alpha = 1)
lasso_coef <- predict(out, type = "coefficients", 
                      s = best_lamb)
lasso_coef # show coefficients for the model
lasso_mod$lambda # show values of lamba tried
lasso_mod$df[55] # Get number of variables in model with best lambda value
# A 34 variable model

estimates <- as.vector(coef(lasso_mod, s = best_lamb, exact = TRUE))
norm. <- sum(abs(estimates))
plot(lasso_mod, xlim = range(0, norm., as.vector(lasso_mod$beta)))
abline(v = norm., col = "red")

# However from cross validation plot we can see that the error is quite stable
# until
lasso_cv$cvm


# Assign a different value of lambda to an object
alt_lamb <- lasso_cv$lambda[33]
# Use this value of lambda to make predictions on test data set
lasso_pred2 <- predict(lasso_mod, s = alt_lamb, 
                      newx = x_test)
# Report test error and number of non zero coefficients for LASSO
lasso2_MSE <- mean((lasso_pred2 - y_test)^2)
lasso2_RMSE <- sqrt(lasso2_MSE)

lasso2_coef <- predict(out, type = "coefficients", 
                      s = alt_lamb)
lasso2_coef # show coefficients for the model
lasso_mod$df[33] # Get number of variables in model with alternative lambda
# value = 15 variables


estimates <- as.vector(coef(lasso_mod, s = best_lamb, exact = TRUE))
norm. <- sum(abs(estimates))
plot(lasso_mod, xlim = range(0, norm., as.vector(lasso_mod$beta)))
abline(v = norm., col = "red")

# 8.0 Simple Decision Tree ####
set.seed(1)
# Fit a simple decision tree using data with only variables selected by best-subset
# selection as tree() function has a limit on number of variables
tree_house <- tree(price ~ ., 
                   data = best_train)
summary(tree_house)
# Only one variable (sqft_living) is used to construct a tree with 5 nodes
plot(tree_house) 
text(tree_house, 
     pretty = 0, 
     cex = 0.8)
# Tree indicates that sqft_living is the most important variable
cv_tree <- cv.tree(tree_house)
plot(cv_tree$size, cv_tree$dev, type = "b")
# Prune the tree to see if any improvements can be made
prunedtree <- prune.tree(tree_house, best = 5)
plot(prunedtree)
text(prunedtree, 
     pretty = 0,
     cex = 0.8)
# The pruned tree and the unpruned tree have the same 
# cross validation error so we can use either to make predictions 
yhat <- predict(prunedtree, 
                newdata = best_test)
testdata <- best_test[ ,"price"]
plot(yhat, testdata)
abline(0, 1)
# Calculate test error of the tree
tree_MSE <- mean((yhat - testdata)^2)
tree_RMSE <- sqrt(tree_MSE)

# 9.0 Bagging ####
set.seed(1)
# Create a tree using bagging
rf_tree <- randomForest(price ~ ., 
                        data = best_train,
                        mtry = 15, # m = p
                        importance = TRUE)
rf_tree
# Create predictions using this tree and test data
rf_pred <- predict(rf_tree, 
                   newdata = best_test)
# Plot these predictions
plot(rf_pred, testdata)
abline(0,1)
# Calculate MSE and RMSE of these predictions
rf_MSE <- mean((rf_pred - testdata)^2)
rf_RMSE <- sqrt(rf_MSE)
# How many trees has it used by default?
rf_tree$ntree # this used 500 trees

# Plotting the error over number of trees shows that after around 100 trees
# the error is pretty constant 
plot(rf_tree)

# So lets try bagging but with 100 trees
rf_100 <- randomForest(price ~ .,
                       data = best_train,
                       mtry = 15,
                       ntree = 100,
                       importance = TRUE)
# Predictions
rf_100_pred <- predict(rf_100, 
                       newdata = best_test)
# Plot predictions
plot(rf_100_pred, testdata)
abline(0,1)

# Plot error against number of trees
plot(rf_100)

# Calculate MSE and RMSE
rf_100_MSE <- mean((rf_100_pred - testdata)^2)
rf_100_RMSE <- sqrt(rf_100_MSE)

# 10.0 RandomForests ####
# Now going to do RandomForest with m = 3
rf_m3 <- randomForest(price ~ ., 
                      data = best_train,
                      mtry = 3,
                      ntree = 100,
                      importance = TRUE)
# Predictions
yhat_rf <- predict(rf_m3, 
                   newdata = best_test)
# Calculate MSE and RMSE
rf_m3_MSE <- mean((yhat_rf-testdata)^2)
rf_m3_RMSE <- sqrt(rf_m3_MSE)
# Having m = 3 massively reduces the MSE

# Plot error over number of trees 
plot(rf_m3)
# Can see that the error is pretty stable after around 20 trees - lets reduce
# it to 30 trees.
rf_m3_30 <- randomForest(price ~ ., 
                         data = best_train,
                         mtry = 3,
                         ntree = 30,
                         importance = TRUE)
# Predictions
yhat_rf <- predict(rf_m3_30, 
                   newdata = best_test)
# Calculate MSE and RMSE
rf_m3_30_MSE <- mean((yhat_rf-testdata)^2)
rf_m3_30_RMSE <- sqrt(rf_m3_30_MSE)
# This gives a slight reduction in MSE
# Take this forward as the final model for randomForests - rf_m3_30

# Lets look at importance of variance and plot this
importance(rf_m3_30)
varImpPlot(rf_m3_30)
# sqft_living is by far the most influential variable shown here.

# 11.0 Boosting ####
set.seed(1)
boost_house <- gbm(price ~ ., 
                   data = best_train,
                   distribution = "gaussian",
                   n.trees = 5000,
                   interaction.depth = 4)

boost_house$shrinkage #lambda for boost_house is 0.1

summary(boost_house)
# From this summary it is clear the variable that accounts for the highest amount 
# of variance is sqft_living, followed by no of bathrooms.
# The rest of the variables account for a tiny amount of variance in comparison.

# Lets produce some partial dependence plots for sqft_living and no of bathrooms
# these illustrate the marginal effect of the selected variables on the response
# after integrating out the other variables
plot(boost_house, i = "sqft_living")
plot(boost_house, i = "bathrooms")

# Use boosted model to predict price on the test set
boost_pred <- predict(boost_house, 
                      newdata = best_test,
                      n.trees = 5000)

boost_MSE <- mean((boost_pred - testdata)^2)
boost_RMSE <- sqrt(boost_MSE)

# Create new boosted model but change lambda to 0.001
boost_model <- gbm(price ~ .,
                   data = best_train,
                   distribution = "gaussian",
                   n.trees = 5000,
                   interaction.depth = 4, 
                   shrinkage = 0.001, 
                   verbose = F)

boost_pred2 <- predict(boost_model,
                       newdata = best_test, 
                       n.trees = 5000)

boost2_MSE <- mean((boost_pred2 - testdata)^2)
boost2_RMSE <- sqrt(boost2_MSE)
# Changing lambda has greatly improved accuracy, but is not competitive with
# other models.

# 12.0 Results ####
RMSE_comparison <- c(simple_lm_RMSE,
                     multiple_lm_RMSE,
                     final_bestsub_RMSE,
                     backward_RMSE, 
                     forward_RMSE,
                     lasso_RMSE,
                     ridge_RMSE,
                     tree_RMSE,
                     rf_RMSE,
                     rf_100_RMSE,
                     rf_m3_RMSE,
                     rf_m3_30_RMSE,
                     boost_RMSE,
                     boost2_RMSE)

RMSE_names <- c("simple_lm_RMSE",
                "multiple_lm_RMSE",
                "final_bestsub_RMSE",
                "backward_RMSE", 
                "forward_RMSE",
                "lasso_RMSE",
                "ridge_RMSE",
                "tree_RMSE",
                "rf_RMSE",
                "rf_100_RMSE",
                "rf_m3_RMSE",
                "rf_m3_30_RMSE",
                "boost_RMSE",
                "boost2_RMSE")

plot(RMSE_comparison)

RMSE_lowest <- c( multiple_lm_RMSE,
                  final_bestsub_RMSE,
                  backward_RMSE, 
                  forward_RMSE,
                  lasso_RMSE,
                  ridge_RMSE)

plot(RMSE_lowest)

# 13.0 Trialling Linear Regression without Location variables ####
new_train <- train[, -11]
new_test <- test[, -11]

linear_mod <- lm(price ~ ., data = new_train)
# Make predictions on test data set
linear_pred <- predict(linear_mod, new_test)
# calculate MSE 
linear_MSE <- mean((new_test[, "price"] - linear_pred)^2)
# calculate RMSE
linear_RMSE <- sqrt(linear_MSE)
# print summary of model
summary(linear_mod)


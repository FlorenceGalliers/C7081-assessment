## HEADER ####
## Florence Galliers 
## 2020-11-23
## C7081 Assignment Methods Script

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

# Import original data set as downloaded from Kaggle
# https://www.kaggle.com/shree1992/housedata
data <- read.csv(file = "original-data.csv")

set.seed(2)

# Exploratory Data Analysis (EDA)
# This section is intended to get to know the data, select the best variables to 
# take forward in a cleaned data set to use in my analysis and visualise any
# existing patterns or correlations in the data.

# First I am will go through each of the variables from the original data set

#1 Date (not included in analysis)
summary(data$date)
# This is a character variable giving the date of the sale of the house, as I will
# not be looking at price over time, this variable is not going to be included in my
# analysis.

#2 Price (dependent variable for analysis)
summary(data$price)
# Numeric variable giving sale price of house in US dollars
# We can see the max price is well above the 3rd quartile value, this may be due 
# to some outlying values. Will look into this later.
# best to convert this variable into thousands of dollars to make it more manageable
data$price <- data$price/1000
# Look if any values are = 0, assign them to variable
zero_values <- which(data$price == 0)
# Remove these from the data set as house price cannot be $0
data <- data[-zero_values,]

#3 Bedrooms
summary(data$bedrooms)
# There are between 0 and 9 bedrooms in each house, mean of just over 3 bedrooms
hist(data$bedrooms)
# Histogram shows normal distribution of number of bedrooms, can also see they 
# are discrete values.
plot(data$bedrooms) # normal distribution
plot(data$bedrooms, data$price) # price increase with increase in bedrooms?

#4 Bathrooms
summary(data$bathrooms)
#There are between 0 and 8 bathrooms in each house, mean of just over 2 bathrooms
hist(data$bathrooms)
plot(data$bathrooms, data$price) # may be positive correlation between no of 
# bathrooms and price of house
bathrooms <- as.factor(data$bathrooms)
plot(bathrooms, data$price)

#5 sqft_living
summary(data$sqft_living) 
plot(data$sqft_living, data$price)
#can see that as sqft of living area increases, price increases

#6 sqft_lot
summary(data$sqft_lot)
plot(data$sqft_lot, data$price)

#7 Floors
summary(data$floors) #between 1 and 3.5 floors for each house, mean of 1.5
plot(data$floors, data$price)

#8 Waterfront (not included in analysis)
plot(data$waterfront) #not many values for this
data$waterfront <- as.factor(data$waterfront)
count(data$waterfront) #only 33 values as 'YES' for having a waterfront
#lets remove this variable from our dataset

#9 View (not included in analysis)
summary(data$view)
plot(data$view)
data$view <- as.factor(data$view)
count(data$view)
#most houses score 0 for view, or the data is missing
#lets remove this variable from our dataset.

#10 Condition
summary(data$condition) #looks like 5 levelled rating on condition
plot(data$condition)
#lets convert to a factor
#data$condition <- as.factor(data$condition)
#count(data$condition) #majority have condition 3,4,5 which is not suprising 
plot(data$condition, data$price) #looks like price may not change too much with
#condition

#11 sqft_above (not included in analysis)
summary(data$sqft_above)
plot(data$sqft_above, data$price)
#looks to be a correlation, but is this just the same as sqft_living as not 
#all properties have basements (see below) - remove this variable
cor(data$sqft_above, data$sqft_living) #88% correlated with sqft_living

#12 sqft_basement
summary(data$sqft_basement)
plot(data$sqft_basement) #can see a lot of properties have 0 value for this 
#which means they do not have a basement, lets convert this into a binary variable
#instead, with 0 = no basement and 1 = basement
data$sqft_basement <- ifelse(data$sqft_basement == 0, 0, 1)
#change column name from sqft_basement to if_basement
names(data)[names(data) =="sqft_basement"] <- "if_basement"
data$if_basement <- as.factor(data$if_basement)

#13 yr_built
plot(data$yr_built, data$price)
#it is treating this as a numeric continuous variable, lets change it into age
#of house instead of year built.
end_year <- 2014 #year data is from
#calculate age of each house is years
data$yr_built <- end_year - data$yr_built
#change column name from yr_built to house_age
names(data)[names(data)=="yr_built"] <- "house_age"
#converted age into categorical variable with 10 year age groups
#data$house_age <- cut(data$house_age, 
#  breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
# labels = c("0-10", "11-20", "21-30", "31-40", "41-50",
#           "51-60", "61-70", "71-80", "81-90", "91-100", "100+"))
#levels(data$house_age)
#count(data$house_age)
plot(data$house_age, data$price) #doesn't look like age has too much of an effect
#on price

#14 yr_renovated
count(data$yr_renovated) #we can see there are 2706 zero values which means not
#all properties have been renovated. lets change into a binary variable where 
# 0 = no renovation and 1 = has been renovated
data$yr_renovated <- ifelse(data$yr_renovated == 0, 0, 1)
# change column name from yr_renovated to if_renovated
names(data)[names(data) =="yr_renovated"] <- "if_renovated"
count(data$if_renovated) 
boxplot(data$if_renovated, data$price)
data$if_renovated <- as.factor(data$if_renovated)

#15 Street (not included in analysis)
summary(data$street) #character variable giving street name
data$street <- as.factor(data$street) #all unique names - not going to be much
#help in predicting price so lets remove this from the dataset

#16 City
summary(data$city) #character variable giving city that houses are located in
data$city <- as.factor(data$city)
count(data$city) #gives factor with 44 levels, some only have one or two houses in
#lets remove observations for those cities with less than 5 houses.
lowfreq_cities <- c(which(data$city == "Algona"),
                    which(data$city == "Beaux Arts Village"),
                    which(data$city == "Inglewood-Finn Hill"),
                    which(data$city == "Milton"),
                    which(data$city == "Preston"),
                    which(data$city == "Skykomish"),
                    which(data$city == "Snoqualmie Pass"),
                    which(data$city == "Yarrow Point"))
data <- data[-lowfreq_cities,] #left with 4532 observations after removing these
#low frequency cities
data$city <- droplevels(data$city) #drop these unused levels from dataset
plot(data$city, data$price) #definitely looks to be some cities with higher prices
# change factor level names so they do not inlcude spaces
levels(data$city) <- c("Auburn", "Bellevue", "Blackdiamond", "Bothell", "Burien",
                       "Carnation", "Clydehill", "Covington", "Desmoines",
                       "Duvall", "Enumclaw", "Fallcity", "Federalway", 
                       "Issaquah", "Kenmore", "Kent", "Kirkland", 
                       "Lakeforestpark", "Maplevalley", "Medina", 
                       "Mercerisland", "Newcastle", "Normandypark", 
                       "Northbend", "Pacific", "Ravensdale", "Redmond", "Renton",
                       "Sammamish", "SeaTac", "Seattle", "Shoreline", "Snoqualmie",
                       "Tukwila", "Vashon", "Woodinville")

#17 State zipcode
summary(data$statezip)
data$statezip <- as.factor(data$statezip) #73 unique variables? is this too many?
plot(data$statezip, data$price) #definitely some here with higher values than others

#18 Country (not included in analysis)
# constant variable, so lets remove

variables_to_remove <- c(1, 8, 9, 11, 15, 17, 18)
# Date, Waterfront, View, sqft_above, Street, Country
data <- data[ ,-variables_to_remove]

# We are left with 11 variables after the initial EDA

# Remove outliers identified throughout.
outliers <- c(100, 121, 122, 2271, 2387, 2921, 4324, 4325, 4328, 4329)
data <- data[-outliers, ]

# Final number of observations is 4522

set.seed(2) # set seed
n <- nrow(data) # create variable with number of rows
train_index <- sample(1:n, size = round(0.8*n), replace=FALSE) 
train <- data[train_index ,] # takes 80% of the data for training set
test <- data[-train_index ,] # remaining 20% for the test set

dummy_data <- dummy.data.frame(data, sep = ".")
names(dummy_data)
dummy_train <- dummy.data.frame(train, sep = ".")
names(dummy_train)
dummy_test <- dummy.data.frame(test, sep = ".")
names(dummy_test)

set.seed(2)
# create simple linear model using price as dependent variable and sqft_living
# as independent variable, and training data set
simple_lm <- lm(price ~ sqft_living, 
                data = train)
# make predictions using this model on test data set
simple_pred <- predict(simple_lm, 
                       test) 
# calculate MSE of simple linear model
simple_lm_MSE <- mean((test[, "price"] - simple_pred)^2)
# calculate RMSE
simple_lm_RMSE <- sqrt(simple_lm_MSE)

# print summary of model
summary(simple_lm)

# create a multiple linear model using price as dependent variable and all
# other variables, training data set
multiple_lm <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+condition
                  +if_basement+house_age+if_renovated+city,
                  data = train)
# make predictions using this model on test data set
multiple_pred <- predict(multiple_lm,
                         test)
# calculate MSE of multiple linear model
multiple_lm_MSE <- mean((test[, "price"] - multiple_pred)^2)
# calculate RMSE of multiple linear model
multiple_lm_RMSE <- sqrt(multiple_lm_MSE)

# print summary of model
summary(multiple_lm)

vif(multiple_lm)


# Fit subset selection model
bestsub <- regsubsets(price ~ .,
                      data = train,
                      nvmax = 44)

summary(bestsub)

# Create test matrix
test_matrix <- model.matrix(price ~ .,
                            data = test)

# Create loop for finding validation errors for a model of each size
val_errors <- rep(NA, 44)
for(i in 1:44) {
  coefi <- coef(bestsub, id = i)
  pred <- test_matrix[ ,names(coefi)]%*%coefi
  val_errors[i] = mean((test$price - pred)^2)
}

val_errors
which.min(val_errors)
# shows which number of variables has the lowest validation error
coef(bestsub, 34) # 34 variables is optimal from validation error

# create prediction formula 
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi
}

# Choosing among models of different sizes using cross-validation
k <- 5
folds <- sample(1:k, nrow(data), replace = TRUE)
cv.errors <- matrix(NA, k, 44, dimnames = list(NULL, paste(1:44)))

for(j in 1:k){
  best.fit <- regsubsets(price ~ ., 
                         data = data[folds!=j,],
                         nvmax = 44)
  for (i in 1:44) {
    pred <- predict.regsubsets(best.fit, 
                               data[folds==j,], 
                               id = i)
    cv.errors[j, i] = mean((data$price[folds==j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

plot(mean.cv.errors, type = "b")
which.min(mean.cv.errors) #shows that min cv.error is with 16 variables

mean.cv.errors[16]

summary(best.fit)

# Create new test and train data sets containing only the best 15 variables as 
# selected by best subset selection
names(dummy_test)
best_train <- dummy_train[c(1, 2, 3, 4, 7, 9, 10, 14, 19, 
                            26, 28, 29, 32, 33, 39, 41, 43)]

best_test <- dummy_test[c(1, 2, 3, 4, 7, 9, 10, 14, 18, 
                          25, 27, 28, 31, 32, 37, 39, 41)]

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


# Forward Stepwise Selection
regfit.fwd <- regsubsets(price ~ ., 
                         data = data, 
                         nvmax = 44,
                         method = "forward")

summary(regfit.fwd)

# Create loop for finding validation errors for a model of each size in forward selection
fwd_errors <- rep(NA, 44)
for(i in 1:44) {
  coefi <- coef(regfit.fwd, id = i)
  pred <- test_matrix[ ,names(coefi)]%*%coefi
  fwd_errors[i] = mean((test$price - pred)^2)
}

fwd_errors
which.min(fwd_errors) # it says minimum is 44, but there seems to be some overfitting
# after around 20 variables, a low point at 16.

plot(fwd_errors)
# 16 variable model also has a low validation error

# Create new test and train data sets containing only the best 16 variables as 
# selected by forward selection
forward_train <- dummy_train[c(1, 2, 3, 4, 7, 9, 10, 14, 19, 
                               25, 26, 29, 32, 33, 39, 41, 43)]

forward_test <- dummy_test[c(1, 2, 3, 4, 7, 9, 10, 14, 18, 
                             24, 25, 28, 31, 32, 37, 39, 41)]

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

# Backward Stepwise Selection
regfit.bwd <- regsubsets(price ~ ., 
                         data = data, 
                         nvmax = 44, 
                         method = "backward")

summary(regfit.bwd)

# Create loop for finding validation errors for a model of each size in backward selection
bwd_errors <- rep(NA, 44)
for(i in 1:44) {
  coefi <- coef(regfit.bwd, id = i)
  pred <- test_matrix[ ,names(coefi)]%*%coefi
  bwd_errors[i] = mean((test$price - pred)^2)
}

bwd_errors
which.min(bwd_errors) # it says minimum is 43, but there seems to be some overfitting
# after around 20 variables, a low point at 15 shown on plot.

plot(bwd_errors)
# 15 variable model also has a low validation error

# Create new test and train data sets containing only the best 15 variables as 
# selected by forward selection
backward_train <- dummy_train[c(1, 2, 3, 4, 7, 9, 10, 14, 19, 
                                26, 29, 32, 33, 39, 41, 43)]

backward_test <- dummy_test[c(1, 2, 3, 4, 7, 9, 10, 14, 18, 
                              25, 28, 31, 32, 33, 37, 39, 41)]

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
lasso_coef
lasso_variables <- lasso_coef[lasso_coef!=0]
# This gives 45 variables in the best model ... a lot more variables than selected 
# by other methods.

# Plot shows percentage of deviance explained
plot(lasso_mod, xvar="dev", label=TRUE)

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

set.seed(1)
boost_house <- gbm(price ~ ., 
                   data = best_train,
                   distribution = "gaussian",
                   n.trees = 5000,
                   interaction.depth = 4)

boost_house$shrinkage #lambda for boost_house is 0.1

summary(boost_house)
# From this summary it is clear the variable that accounts for the highest amount 
# of variance is sqft_living, followed by no of bathrooms and house_age.]
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

plot(RMSE_comparison)

RMSE_lowest <- c( multiple_lm_RMSE,
                  final_bestsub_RMSE,
                  backward_RMSE, 
                  forward_RMSE,
                  lasso_RMSE,
                  ridge_RMSE)

plot(RMSE_lowest)

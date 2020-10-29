## HEADER ####
## 2020-10-27
## C7081-trees

# Set WD, Read in Data
setwd("~/Google Drive/Harper/1 Statistical Analysis for Data Science/Asssesment/github-C7081/C7081-assessment")
library(openxlsx)
data2 <- read.xlsx("housing_data_assessment.xlsx")

# Split into training and test data sets

data2 <- data2[ ,-11]

set.seed(1)

train <- sample(c(TRUE, FALSE), nrow(data2), rep = TRUE)

test <- (!train)

house.train <- data2[train,]

house.test <- data2[test,]

# Regression Tree

tree_house <- tree(price ~ ., 
                   data = house.train)

summary(tree_house)

plot(tree_house)
text(tree_house, pretty = 0, cex = 0.8)

tree_house

# Tree only uses one variable, and indicates that a higher sqft of living
# space leads to a higher house price.

cv_tree <- cv.tree(tree_house)
plot(cv_tree$size, cv_tree$dev, type = "b")

prunedtree <- prune.tree(tree_house, best = 5)
plot(prunedtree)
text(prunedtree, pretty = 0)


# The pruned tree and the unpruned tree have the same 
# cross validation error so we can use either to make predictions

yhat <- predict(prunedtree, newdata = house.test)
testdata <- house.test[ ,"price"]
plot(yhat, testdata)
abline(0, 1)

mean((yhat-testdata)^2)
sqrt(152564491806)

# The sqrt of the MSE is around 390595, suggesting that this model leads to
# test predictions that are within $390,595 of the true house price.

set.seed(1)

bag_house <- randomForest(price ~ ., 
                          data = house.train,
                          mtry = 9, 
                          importance = TRUE)
bag_house

yhat_bag <- predict(bag_house, newdata = house.test)
plot(yhat_bag, testdata)
abline(0,1)

mean((yhat_bag-testdata)^2)
sqrt(200502311145)
# this suggests that the model leads to test predictions that are within 
# $447,775 of the true house price, worse than above.

rf_house <- randomForest(price ~ ., 
                         data = house.train,
                         mtry = 3,
                         importance = TRUE)

yhat_rf <- predict(rf_house, newdata = house.test)


mean((yhat_rf-testdata)^2)
sqrt(148879274522)
# this suggests that this model using 3 variables leads to test predictions
# that are $385,849 within the true house price. Better than the two trees above

importance(rf_house)

varImpPlot(rf_house)

# This shows that across all trees considered in the random forest, the year 
# a house was built and the sqft of the house are the two most important variables

# Boosting

library(gbm)

boost_house <- gbm(price ~ ., 
                   data = house.train,
                   distribution = "gaussian",
                   n.trees = 5000,
                   interaction.depth = 4)

summary(boost_house)

# sqft of house is the most important variable and then yr_built

# Lets produce some partial dependence plots for sqft_living and yr_built
# these illustrate the marginal effect of the selected variables on the response
# after integrating out the other variables

par(mfrow=c(2, 1))

plot(boost_house, i = "sqft_living")
plot(boost_house, i = "yr_built")

# Use boosted model to predict price on the test set

yhat_boost <- predict(boost_house, 
                      newdata = house.test,
                      n.trees = 5000)

mean((yhat_boost-testdata)^2)
sqrt(218630375438)
# worse performance than the above trees.

# boosting with a different value of lambda
boost_boston <- gbm(price ~ .,
                    data = house.train,
                    distribution = "gaussian",
                    n.trees = 5000,
                    interaction.depth = 4, 
                    shrinkage = 0.2, 
                    verbose = F)

yhat_boost <- predict(boost_boston,
                      newdata = house.test, 
                      n.trees = 5000)

mean((yhat_boost-testdata)^2)
sqrt(2.33353e+11)
# even worse!


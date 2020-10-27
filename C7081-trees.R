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

# Tree only uses one variable, and indicates that a higher sqft of living
# space leads to a higher house price.

cv_tree <- cv.tree(tree_house)
plot(cv_tree$size, cv_tree$dev, type = "b")

prunedtree <- prune.tree(tree_house, best = 4)
plot(prunedtree)
text(prunedtree, pretty = 0)


# Use the pruned tree to make predctions on the test set as this tree
# had the lowest cross validation error

yhat <- predict(prunedtree, newdata = house.test)
testdata <- house.test[ ,"price"]
plot(yhat, testdata)
abline(0, 1)

mean((yhat-testdata)^2)
sqrt(152564491806)

# The sqrt of the MSE is around 390595, suggesting that this model leads to
# test predictions that are within $390,595 of the true house price.
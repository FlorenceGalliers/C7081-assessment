## HEADER ####
## 2020-10-27
## C7081-Assessment

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

# Fit linear model using least squares on train data, report test error

lm.model <- lm(price ~ ., 
               data = house.train)

lm.pred <- predict(lm.model, house.test)

mean((house.test[, "price"] - lm.pred)^2)

# Fit a ridge regression, choose lambda by cross validation

library(glmnet)

set.seed(1)

x <- model.matrix(price ~ ., data = data2)[,-1]

y <- data2$price

cv.ridge <- cv.glmnet(x[train,], y[train], alpha = 0)

plot(cv.ridge)

best.lambda <- cv.ridge$lambda.min

best.lambda

# Report test error

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0)

ridge.pred <- predict(ridge.mod, s = best.lambda,
                      newx = x[test,])

mean((ridge.pred - y[test])^2)

# Fit a lasso model on the training set, choose lambda by C-V

lasso.mod <- glmnet(x[train,], 
                    y[train], 
                    alpha = 1)
plot(lasso.mod)

set.seed(1)

cv.lasso <- cv.glmnet(x[train,], 
                      y[train],
                      alpha = 1)

plot(cv.lasso)

best.lamb <- cv.lasso$lambda.min

lasso.pred <- predict(lasso.mod, s = best.lamb, 
                      newx = x[test,])

# Report test error and number of non zero coefficients

mean((lasso.pred - y[test])^2)

out <- glmnet(x, y, alpha = 1)

lasso.coef <- predict(out, type = "coefficients", 
                      s = best.lamb)
lasso.coef

lasso.coef[lasso.coef!=0]

# Fit PCR Model on training data, M chosen by C-V

library(pls)

set.seed(2)

pcr.fit <- pcr(price ~ ., 
               data = house.train,
               scale = TRUE,
               validation = "CV")

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP") 
# shows the smallest C-V error is when M = 9 which is the same as just 
# performing least squares

names(pcr.fit)


# Report test error, and value of M selected

pcr.pred <- predict(pcr.fit, x[test,], ncomp = 9)

mean((pcr.pred - y[test])^2)

# Fit PLS Model to training data, C-V

pls.fit <- plsr(price ~ .,
                data = house.train,
                scale = TRUE,
                validation = "CV")

summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")

# Report test error, and value of M selected

pls.pred <- predict(pls.fit, x[test,], ncomp = 3)

mean((pls.pred - y[test])^2)

pls.fit <- plsr(price ~ ., data = data2, scale = TRUE, ncomp = 3)

summary(pls.fit)

# Best subset selection

library(leaps)

regsub.fit <- regsubsets(price ~ ., 
                         data = data2[train, ],
                         nvmax = 9)

test.matrix <- model.matrix(price ~ .,
                            data = data2[test,])

val.errors <- rep(NA, 9)
for(i in 1:9) {
  coefi <- coef(regsub.fit, id = i)
  pred <- test.matrix[ ,names(coefi)]%*%coefi
  val.errors[i]=mean((data2$price[test]-pred)^2)
}

val.errors

which.min(val.errors)

coef(regsub.fit, 7)

# create prediction formula 

predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi
}

# Make use of full data set to obtain more accurate coef estimates

regfit.best <- regsubsets(price ~ ., 
                          data = data2, 
                          nvmax = 9)

coef(regfit.best, 7)

reg.summary <- summary(regfit.best)


# Choosing among models of different sizes using cross-validation

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(data2), replace = TRUE)
cv.errors <- matrix(NA, k, 9, dimnames = list(NULL, paste(1:9)))

for(j in 1:k){
  best.fit <- regsubsets(price ~ ., 
                         data = data2[folds!=j,],
                         nvmax = 9)
  for (i in 1:9) {
    pred <- predict(best.fit, data2[folds==j,], id=i)
    cv.errors[j, i]=mean((data2$price[folds==j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

plot(mean.cv.errors, type = "b")

# Cross validation also selects a seven variable model

# Plot graphs of RSS, adjr2, cp and BIC

which.max(reg.summary$adjr2)

which.min(reg.summary$cp)

which.min(reg.summary$bic)


par(mfrow=c(2,2))

plot(reg.summary$rss, 
     xlab="Number of Variables", 
     ylab="RSS",
     type = "l")

plot(reg.summary$adjr2,
     xlab = "Number of Variables",
     ylab = "Adjusted RSq",
     type = "l")

points(7, reg.summary$adjr2[7], col="red", cex=2, pch=20)

plot(reg.summary$cp, 
     xlab = "Number of Variables",
     ylab = "Cp",
     type = "l")

points(7, reg.summary$cp[7], # this is giving the coordinates of the point 
       # on the graph to plot, 10 on the x axis and the 10th value of the cp on
       # y axis
       col = "red",
       cex = 2,
       pch = 20)

plot(reg.summary$bic, 
     xlab = "Number of Variables",
     ylab = "BIC",
     type = "l")

points(5, reg.summary$bic[5],
       col = "red",
       cex = 2,
       pch = 20)

# Forward and Backward Stepwise Selection

regfit.fwd <- regsubsets(price ~ ., 
                         data = data2, 
                         nvmax = 9,
                         method = "forward")

summary(regfit.fwd)

regfit.bwd <- regsubsets(price ~ ., 
                         data = data2, 
                         nvmax = 9, 
                         method = "backward")

summary(regfit.bwd)

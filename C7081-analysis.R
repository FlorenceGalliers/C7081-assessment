## HEADER ####
## 2020-10-08 
## Testing Data for Assessment 
####

# read in data file
setwd("~/Google Drive/Harper/1 Statistical Analysis for Data Science/Asssesment/github-C7081/C7081-assessment")
library(openxlsx)
data <- read.xlsx("housing_data_assessment.xlsx")

# EDA

# look if any values are = 0, assign them to variable
zero_values <- which(data$price == 0)
# remove these from the data set as house price cannot = 0
data <- data[-zero_values,]
# we are now left with 4551 observations

# plot price against sqft_living
plot(data$price, data$sqft_living,
     xlab = "House Price ($)",
     ylab = "Size of House (sqft)",
     main = "House Price - House Size")
# we can see there are some outliers, let's calculate IQR to have some actual 
# values to remove outliers, rather than doing it by eye

# start with price
summary(data$price)
# 1st Q = 326264
# 3rd Q = 657500
657500 - 326264
# IQR = 3rdQ - 1stQ = 331236
331236*3 # = 993708
# subtracting IQR*3 value from the 1stQ does not leave any outstanding values
# adding IQR*3 value to the 3rdQ 
657500+993708 # = 1651208
# anything above $1651208 will be counted as an outlier
which(data$price > 1651208)
# this gives 89 values, lets remove them from the dataset and re-plot the data
outliers_price <- which(data$price > 1651208)
data <- data[-outliers_price, ]
# leaves us with 4462 observations
# plot again
plot(data$price, data$sqft_living,
     xlab = "House Price ($)",
     ylab = "Size of House (sqft)",
     main = "House Price - House Size")

# lets do the same with sqft_living
summary(data$sqft_living)
# 1st Q = 1450
# 3rd Q = 2568
2568 - 1450
# IQR = 3rdQ - 1stQ = 1118
1118*3 # = 3354
# subtracting IQR*3 value from the 1stQ does not leave any outstanding values
# adding IQR*3 value to the 3rdQ 
2568+3354 # = 5922
# anything above 5922 counted as an outlier
outliers_sqft_living <- which(data$sqft_living > 5922) # 8 values
# remove outliers from data set
data <- data[-outliers_sqft_living, ]
# left with 4454 observations

# plot again
plot(data$price, data$sqft_living,
     xlab = "House Price ($)",
     ylab = "Size of House (sqft)",
     main = "House Price - House Size")

# create histogram of number of bedrooms
hist(data$bed,
     main = "Histogram: Number of Bedrooms",
     xlab = "Number of Bedrooms",
     ylab = "Frequency")
# we can see number of bedrooms is discrete values, normally distributed

# create histogram of number of bathrooms
hist(data$bath,
     main = "Histogram: Number of Bathrooms",
     xlab = "Number of Bathrooms",
     ylab = "Frequency")







data_lm <- lm(data$price ~ . -city-basement-yr_built-renovated, 
              data = data)
summary(data_lm)

plot(data_lm)

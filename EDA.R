## HEADER ####
## 2020-10-27
## C7081-Exploratory Data Analysis

#set working directory
setwd("~/Google Drive/Harper/1-C7081/Asssesment/github-C7081/C7081-assessment")

library(plyr)
library(writexl)
library(psych)
library(ggplot2)
library(ggcorrplot)


#import data
data <- read.csv(file = "original-data.csv")

#this script is intended to get to know the data, select the best variables to take
#forward to my analysis and visualise any existing patterns or correlations in 
#the data.

#first I am going to go through each of the variables from the original data set

#1 date (not included in analysis)
summary(data$date)
#this is a character variable giving the date of the sale of the house, as I will
#not be looking at price over time, this variable is not going to be included in my
#analysis.

#2 price (dependent variable for analysis)
summary(data$price)
#numeric variable giving sale price of house in US dollars
#we can see the max price is well above the 3rd quartile value, this may be due 
#to some outlying values. Will look into this later.
#best to convert this variable into thousands of dollars
data$price <- data$price/1000
# look if any values are = 0, assign them to variable
zero_values <- which(data$price == 0)
# remove these from the data set as house price cannot = 0
data <- data[-zero_values,]
# we are now left with 4551 observations


#3 bedrooms
summary(data$bedrooms)
#between 0 and 9 bedrooms in each house, mean of just over 3 bedrooms
hist(data$bedrooms)
#histogram shows normal distribution of number of bedrooms, can also see they 
#are discrete values.
plot(data$bedrooms) #normal distribution
plot(data$bedrooms, data$price) #price increase with increase in bedrooms?

#4 bathrooms
summary(data$bathrooms)
#between 0 and 8 bathrooms in each house, mean of just over 2 bathrooms
hist(data$bathrooms)
plot(data$bathrooms, data$price) #may be positive correlation between no of 
#bathrooms and price of house
bathrooms <- as.factor(data$bathrooms)
plot(bathrooms, data$price)
  

#5 sqft_living
summary(data$sqft_living) 
plot(data$sqft_living, data$price)
#can see that as sqft of living area increases, price increases

#6 sqft_lot
summary(data$sqft_lot)
plot(data$sqft_lot, data$price)

#7 floors
summary(data$floors) #between 1 and 3.5 floors for each house, mean of 1.5
plot(data$floors, data$price)

#8 waterfront (not included in analysis)
plot(data$waterfront) #not many values for this
data$waterfront <- as.factor(data$waterfront)
count(data$waterfront) #only 33 values as 'YES' for having a waterfront
#lets remove this variable from our dataset

#9 view (not included in analysis)
summary(data$view)
plot(data$view)
data$view <- as.factor(data$view)
count(data$view)
#most houses score 0 for view, or the data is missing
#lets remove this variable from our dataset.

#10 condition
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

#15 street (not included in analysis)
summary(data$street) #character variable giving street name
data$street <- as.factor(data$street) #all unique names - not going to be much
#help in predicting price so lets remove this from the dataset

#16 city
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
          
#17 statezip
summary(data$statezip)
data$statezip <- as.factor(data$statezip) #73 unique variables? is this too many?
plot(data$statezip, data$price) #definitely some here with higher values than others

plot(data$sqft_living, data$price)



#18 country (not included in analysis)
#constant variable, lets remove

variables_to_remove <- c(1, 8, 9, 11, 15, 18)
data <- data[ ,-variables_to_remove]

#we are left with 12 variables after the initial EDA and sorting out.

# remove outliers
outliers <- c(100, 121, 122, 2271, 2387, 2921, 4324, 4325, 4328, 4329)
data <- data[-outliers, ]


#final number of observations is 4524

#investigating correlations between price and other variables 
pairs.panels(data, col="red")

corr_variables <- data.frame(data$price, data$bedrooms, data$bathrooms, 
                             data$sqft_living, data$sqft_lot,
                             data$floors, data$condition, data$house_age)

cor_mat <- cor(corr_variables)

p_mat <- cor_pmat(corr_variables)

ggcorrplot(cor_mat, 
           method = "circle", 
           type = "lower",
           p.mat = p_mat,
           lab = T, lab_size = 2.5)
# from this correlation we can see that the house price is most strongly 
# correlated with sqft_living, no of bathrooms, and then no of 
# bedrooms and no of floors. These will be the variables we will focus on in
# in our analysis.
# the correlation between price and house_age is not significant. 



#split into training and testing sets
set.seed(2) 
n = nrow(data) #number of rows
train_index = sample(1:n, size = round(0.8*n), replace=FALSE) 
train = data[train_index ,] #takes 80% of the data for training set
test = data[-train_index ,] #remaining 20% for the test set

#least squares linear regression
lm_model <- lm(price ~ ., 
               data = train)
# make predictions on test set
lm_pred <- predict(lm_model, test) 
# calculate MSE
mean((test[, "price"] - lm_pred)^2)
#28053.99
summary(lm_model)

# Diagnostic plots of the linear regression model
par(mfrow=c(2,2))
plot(lm_model)


data2 <- data[ ,-12 ]
data2 <- data2[ ,-11]

#split into training and testing sets
set.seed(2) 
n = nrow(data2) #number of rows
train_index = sample(1:n, size = round(0.8*n), replace=FALSE) 
train2 = data2[train_index ,] #takes 80% of the data for training set
test2 = data2[-train_index ,] #remaining 20% for the test set

#least squares linear regression
lm_model2 <- lm(price ~ ., 
               data = train2)
# make predictions on test set
lm_pred2 <- predict(lm_model2, test2) 
# calculate MSE
mean((test[, "price"] - lm_pred)^2)
summary(lm_model2)

# Diagnostic plots of the linear regression model
par(mfrow=c(2,2))
plot(lm_model2)

## HEADER ####
## 2020-10-08 
## Testing Data for Assessment 
####

library(openxlsx)

data <- read.xlsx("housing_data_assessment.xlsx")

dim(data)

summary(data)

data.lm <- lm(data$price ~ . -city-basement-yr_built-renovated, 
   data = data)

summary(data.lm)

summary(data$city)

plot(data$sqft_living, data$price)

plot(data$price)

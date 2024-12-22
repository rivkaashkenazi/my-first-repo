#loading cars data and summarizing
# original data: 
cars2 <- read.csv("C:/Users/rivka/Documents/college/intro to data science/final_project/auto-mpg(1).csv")
summary(cars2)

### (diff tools i used in analyzing the data)
sort(cars2$car.name)
table(is.na(cars2$displacement))
str(cars2)

#checking data type of data
str(cars2)

### i decided to convert the column car name to car company since there is not enough data to analyze by car name, its too specific for this small of a dataset
### looking at the car name column, i see that the car company is the first word in the name
# so i need to extract the first word from each car name to determine the car company
cars2$car_company <- sapply(strsplit(as.character(cars2$car.name), " "), `[`, 1)

# convert the car_company column to a factor
cars2$car_company <- as.factor(cars2$car_company)

# view the resulting dataset with the new car company column
head(cars2)
sort(unique(cars2$car_company))

#delete car.name column because no longer needed
cars2 <- subset(cars2, select = -car.name)

### i realized there are spelling mistakes and abbreviations to some car company categories, 
# so combined the categories as appropriate
library(dplyr) #we will need to use "recode" from the library "dplyr"

cars2$car_company <- recode(cars2$car_company,
                            "chevroelt" = "chevrolet",
                            "chevy" = "chevrolet",
                            "mercedes-benz" = "mercedes",
                            "maxda" = "mazda",
                            "toyouta" = "toyota",
                            "vokswagen" = "volkswagen",
                            "vw" = "volkswagen")


# checking unique levels after correction
sort(unique(cars2$car_company))

### looking at the data, i assume origin is a categorical data type, and its acceptable to assume horsepower is a integer type
#changing origin to character type, and horsepower to integer type
cars2$horsepower <- as.integer(cars2$horsepower)
cars2$origin <- as.character(cars2$origin)

### i saw that after converting horsepower to integer, there are NA values in the horsepower variables due to question marks 
# substitute all horsepower NA values with the mean horsepower value
is.na(cars2$horsepower)
cars2$horsepower[is.na(cars2$horsepower)] <- mean(cars2$horsepower, na.rm = TRUE)


### because of a error that arose due to unmatched data levels between training and testing data 
### i gathered all car company categories that come up less than 3 times to a car company category called "other" since they are anyways not significant and cause prediction problems
# convert rare levels into "other" level

# Count the occurrences of each level
level_counts <- table(cars2$car_company)

# Identify rare levels
rare_levels <- names(level_counts[level_counts < 3]) #levels with a frequency below 3 will be grouped

# Replace rare levels with "Other"
cars2$car_company <- as.factor(ifelse(cars2$car_company %in% rare_levels, 
                                      "other", 
                                      as.character(cars2$car_company)))

# Check levels after modification
levels(cars2$car_company)

###splitting the data to training and testing splits before fitting it to regression models

library(rsample) # this library contains the training and testing functions
set.seed(123) ### setting seed so that i can get same results if i do it again in the future
split <- initial_split(cars2, prop = 0.754) ###trying the best to split the data to 300 train and 98 test

train_data <- training(split)
test_data <- testing(split)

#creating a full multiple linear regression
cars_lm = lm(mpg ~ ., data = train_data)
summary(cars_lm)
### equation: mpg ~ cylinder + displacement + horsepower + weight + acceleration + model.year + origin + car_company

#creating stepwise selection model (trying forward selection)
forward_lm <- step(cars_lm, direction = "forward")
summary(forward_lm)
### equation: mpg ~ cylinder + displacement + horsepower + weight + acceleration + model.year + origin + car_company
###did not eliminate any variables

#trying backward selection
backward_lm <- step(cars_lm, direction = "backward")
summary(backward_lm)
### equation: mpg ~ displacement + horsepower + weight + model.year + origin
###adjusted R2, R2 , and residual standard error not improved but worsened
###eliminated car_company , acceleration, cylinder

#trying a stepwise both ways (backward and forward)
step_lm <- step(cars_lm, direction = "both")
summary(step_lm)
### equation: mpg ~ displacement + horsepower + weight + model.year + origin
###stepwise selection eliminated car_company, acceleration, cylinder just like backward selection
###but i will want to test it further with predictions and accuracy with test sample to see if its actually worse

### creating a SIMPLE linear regression with the variable that has the highest correlation to mpg
#checking correlation bet mpg and all other variables.
#install.packages("GGally")
library(GGally)
ggpairs(cars2,cardinality_threshold = 25) 

#the lsr library will help in finding correlation between mpg and the categorical variables like origin and car company
library(lsr)
etaSquared(aov(cars2$mpg ~ cars2$car_company, data = cars2))

### weight seems to have the highest correlation to mpg so ill create a linear regression bet mpg and weight
weight_lm <- lm(mpg ~ weight, data = train_data)
summary(weight_lm)
### the R2 significantly dropped compared to other models, yet we will use it for the sake of testing a simple linear regression

### one more multiple linear regression according to my queses and observations.
### i made some variables as a log() in the equation because i saw there are non linear relationships that needs to be incorporated
### i also eliminated origin since it gave me a higher R2 after its elimination
my_lm <- lm(mpg ~ cylinder + log(displacement) + log(horsepower) + log(weight) + log(acceleration) + model.year + car_company, data = train_data)
summary(my_lm) 
### equation: mpg ~ cylinder + log(displacement) + log(horsepower) + log(weight) + log(acceleration) + model.year + car_company
### i got the best statistics through this regression, lets further assess with the prediction results

# predicting test sample on all five different models
full_pred <- predict(cars_lm, newdata = test_data)
step_pred <- predict(step_lm, newdata = test_data)
weight_pred <- predict(weight_lm, newdata = test_data)
my_pred <- predict(my_lm, newdata = test_data)
backward_pred <- predict(backward_lm, newdata = test_data)


# finding MAE of predictions of the three models 
full_mae <- mean(abs(full_pred - test_data$mpg))
print(paste("Full MAE:", round(full_mae, 2)))

step_mae <- mean(abs(step_pred - test_data$mpg))
print(paste("Step MAE:", round(step_mae, 2)))

weight_mae <- mean(abs(weight_pred - test_data$mpg))
print(paste("Weight MAE:", round(weight_mae, 2)))

my_mae <- mean(abs(my_pred - test_data$mpg))
print(paste("My MAE:", round(my_mae, 2)))

backward_mae <- mean(abs(backward_pred - test_data$mpg))
print(paste("Backward MAE:", round(backward_mae, 2)))


#finding RMSE
full_rmse <- sqrt(mean((full_pred - test_data$mpg)^2))
print(paste("Full RMSE:", round(full_rmse, 2)))

step_rmse <- sqrt(mean((step_pred - test_data$mpg)^2))
print(paste("Step RMSE:", round(step_rmse, 2)))

weight_rmse <- sqrt(mean((weight_pred - test_data$mpg)^2))
print(paste("Weight RMSE:", round(weight_rmse, 2)))

my_rmse <- sqrt(mean((my_pred - test_data$mpg)^2))
print(paste("My RMSE:", round(my_rmse, 2)))

backward_rmse <- sqrt(mean((backward_pred - test_data$mpg)^2))
print(paste("Backward RMSE:", round(backward_rmse, 2)))

# My model shows best results overall, with prediction and model fit
# in both MAE and RMSE this is the order best to worse: my, full, step/backward, weight

# lets bring in some residual plots and histogram to select 1 from the 2 best model which are so far the my_lm and full_lm


# calculate residuals for my_pred (from the model i created) and full_pred (from the full model)
my_residuals <- test_data$mpg - my_pred
full_residuals <- test_data$mpg - full_pred


# Residuals plot
plot(test_data$mpg, full_residuals, 
     xlab = "Actual Values", 
     ylab = "Residuals", 
     main = "Full Model Residuals Plot", 
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)  # Add a horizontal reference line at 0


# Residuals plot
plot(test_data$mpg, my_residuals, 
     xlab = "Actual Values", 
     ylab = "Residuals", 
     main = "My Model Residuals Plot", 
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)  # Add a horizontal reference line at 0


# Plot histogram for full_residuals (full model)
hist(full_residuals, 
     main = "Histogram of Full Model Residuals", 
     xlab = "Residuals", 
     col = "blue", 
     border = "black", 
     breaks = 20)


# plot histogram for my_residuals (my model)
hist(my_residuals, 
     main = "Histogram of My Model Residuals", 
     xlab = "Residuals", 
     col = "blue", 
     border = "black", 
     breaks = 20)

# looking at the histograms and residuals plots we see how the "my model" is performing better 
# (histogram of the "my modle" residuals shows a better normal distribution and the residuals plot have less of a pattern to it that the "full model" plot)

data <-  read.csv('Call_Center.csv')

### first question: "can we predict the call duration and how accurate can it be?"
###(we can apply this model in predicting waiting time on the phone line so that costumers can be informed of approximately how long they will need to wait)"

# check data structure and assign correct data type for each variable
str(data)
data$Channel <- as.factor(data$Channel)
data$Reason <- as.factor(data$Reason)
data$State <- as.factor(data$State)
data$Sentiment <- as.factor(data$Sentiment)
data$Call.Centres.City <- as.factor(data$Call.Centres.City)
data$City <- as.factor(data$City)

sapply(data, function(x) length(unique(x))) 

### because of the complexity of the data, fitting the full data into a linear function is too much for the computer, so ill use the most correlated variables
#identify correlation so we can pick important variables
# calculate the correlation between Call.Duration.In.Minutes and other numeric variables
cor(data$Call.Duration.In.Minutes,data$Csat.Score, use = "complete.obs") #only Csat.Score is another numeric variable after modifications made

# Perform ANOVA for each categorical variable
categorical_columns <- sapply(data, is.factor)  # Identify categorical columns
aov_results <- lapply(data[, categorical_columns], function(x) {
  aov(Call.Duration.In.Minutes ~ x, data = data)
})

# View ANOVA results for each categorical variable
aov_summary <- lapply(aov_results, summary)
print(aov_summary)

### results show that there are no variables with significant effect on duration... 
### though it shows that sentiment, city, state, reason have the best (from best to worse order) correlation with duration.
library(dplyr)

# Select the columns you want to keep
selected_data <- select(data, Call.Duration.In.Minutes, City, Sentiment, State, Reason)

#install.packages("caret")
library(rsample)

set.seed(123)
split <- initial_split(selected_data, prop = 0.7)  # Specify the primary variable for stratification
train_data <- training(split)
test_data <- testing(split)

# fit it into a regression model
full_lm <- lm(Call.Duration.In.Minutes ~ ., data = train_data)
summary(full_lm)

### full model is doing terrible at predicting duration
### lets try step selection regression
step_lm <-  step(full_lm,train_data,direction = "both")
summary(step_lm)

### model is not improving our chance of predicting duration time of the service correctly.
### i think we will get better results if we include ONLY call services of the channel column (because we actually want to predict specifically CALL DURATION!)

# Make sure dplyr is loaded
library(dplyr)

# Filter rows where Channel is "Call-Center"
data_call_center <- filter(data, Channel == "Call-Center")
data_call_center <- subset(data_call_center, select = -Channel)

summary(data_call_center)

### now lets repeat the steps and see correlation between variables using only this call center data

# calculate the correlation between Call.Duration.In.Minutes and other numeric variables
numeric_columns <- sapply(data_call_center, is.numeric)  # Identify numeric columns
correlations <- cor(data_call_center[, numeric_columns], use = "complete.obs")  # Correlation matrix

# extract correlation with Call.Duration.In.Minutes
cor_with_duration <- correlations["Call.Duration.In.Minutes", ]
print(cor_with_duration)


# perform ANOVA for each categorical variable
categorical_columns <- sapply(data_call_center, is.factor)  # Identify categorical columns
aov_results <- lapply(data_call_center[, categorical_columns], function(x) {
  aov(Call.Duration.In.Minutes ~ x, data = data_call_center)
})

# view ANOVA results for each categorical variable
aov_summary <- lapply(aov_results, summary)
print(aov_summary)

### results show that there are variables with significant effect on duration: (from highest correlation to lowest) city, state,sentiment,reason
### now lets repeat the steps and fit the data to regression models just this time with only call center data
library(dplyr)

# Select the columns you want to keep
selected_data <- select(data_call_center, Call.Duration.In.Minutes, City, Sentiment, State, Reason)

# install.packages("caret")
library(rsample)

# split the data to test and train 
set.seed(123)
split <- initial_split(selected_data, prop = 0.7)  # Specify the primary variable for stratification
train_data <- training(split)
test_data <- testing(split)

# fit the call center data to regression model
full_lm <- lm(Call.Duration.In.Minutes ~ ., data = train_data)
summary(full_lm)

# lets try step selection model
step_lm <-  step(full_lm,train_data,direction = "both")
summary(step_lm)

### step regression chose to eliminate all variables

### though we could simply take average of call duration of each reason and use this in predicting total duration of a line, though its not statistically significant or accurate
# Calculate average call duration by reason
library(dplyr)
avg_call_duration <- data %>%
  group_by(Reason) %>%
  summarize(Average_Call_Duration = mean(Call.Duration.In.Minutes, na.rm = TRUE))

# Display the result
avg_call_duration

### this gives a rounded 25 min duration for each reason, which basically says there is no difference in call duration by the reason for calling
 
### therefor answering our first question: using the data we have to predict call duration will not help in predicting time duration. best thing to do will probably be to take average time duration of all calls and times it by the amount of costumers waiting online...
### (i did not bother to use the testing data because it was obvious it would not show us any good results)


### second question: "is there any correlation between channel (email,web,chatbot,call_center) and Csat.score(satisfaction score)?
### is there a way of service that is more liked than others? which way of service will you recommend to improved?"

### looking at the score column, there are a lot of non rated experiences 
table(is.na(data$Csat.Score))      

# picking out only experiences with score rating
rated_data = data[!is.na(data$Csat.Score), ]

#checking if it worked
table(is.na(rated_data$Csat.Score))

#visualize graph between score and channel
plot(rated_data$Channel, rated_data$Csat.Score,
     main = "Channel ~ Score", 
     xlab = "Channel", 
     ylab = "Score")

### looking at the graph generated, we see how call-center and web have a higher mean score than chatbot and email services
### lets further analyze if its significant

# conducting a anova test
anova_result <- aov(Csat.Score ~ Channel, data = rated_data)
summary(anova_result)
### p-value is just over the significance level of 5%

###lets conduct a tukey test to see which category is the reason for the significant mean difference

#tukey test
TukeyHSD(anova_result)
### the biggest difference is between email and call-center services which puts call-services most satisfactory and email services least satisfactory
### maybe this can indicate the need to focus in improving email services...

### we do need to consider the amount of entries per channel category to make sure we have a balanced data set
### looking at ALL frequencies of channel and ONLY rated frequencies of channel, it looks like the sample rated_data represents 
### almost the same RELETIVE frequency of channel categories. this may further support the significance of the score rating analysis.
### seeing the frequency for the full data, this can also mean that people automatically turn to the call-center, then chatbot, then email, and lastly the web.
channel_freq <- table(rated_data$Channel)
channel_freq
barplot(channel_freq, 
        main = "Frequency of Each Category in Channel", 
        xlab = "Channel", 
        ylab = "Frequency")

full_channel_freq <- table(data$Channel)
full_channel_freq
barplot(full_channel_freq, 
        main = "Full Frequency of Each Category in Channel", 
        xlab = "Channel", 
        ylab = "Frequency")


### third question: "what are the most common reasons people call for and can we improve costumer satisfaction through focusing on specific reasons that people call for?"
### we will analyze the reasons that are most frequent, and then see if the reasons have any correlation to the score.

# calculate frequency of each reason level
table(data$Reason)

# bar plot of the most common reasons for calls
library(ggplot2)
ggplot(data, aes(x = Reason)) +
  geom_bar() +
  labs(title = "Reasons for Calls", x = "Reason", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# boxplot to compare Csat.Score across different reasons
ggplot(data, aes(x = Reason, y = Csat.Score)) +
  geom_boxplot() +
  labs(title = "Score by Reason", x = "Reason", y = "Customer Satisfaction Score)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### looking at the box plot display we can see how payments reason services have better satisfaction scores. it may be because payment services are more simple and to the point rather than billing questions and service outage that automatically means there is a problem that needs to be resolved.

library(dplyr)
avg_score <- data %>%
  group_by(Reason) %>%
  summarize(Average_score = mean(Csat.Score, na.rm = TRUE))

# Display the result
avg_score

### looking at the average score per reason, the differences in average score doesnt look that significant.
###lets perform an anova test just to make sure
anova_result <- aov(Csat.Score ~ Reason, data = data)

# Display the summary of the ANOVA result
summary(anova_result)

### according to the anova results, the difference between average score of the three types of reasons isn't significant (at the .05 significance interval)

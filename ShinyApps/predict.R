## Import training data
train <- read.csv("rewards.csv")

# Format training data
train$RR <- train$engaged / train$impressions
train$NCTR <- train$nclicked / train$nshowed

# Linear regression
reward.model <- lm(Redemptions ~ Sessions + NCTR + RR, data=train)

# Display the model
k <- coef(reward.model)
## Model ##
# Redemptions = -66938.87 + 0.006763872(Sessions) + 136197.5(Notification.Click.Through.Rate) + 643754.7(Reward.Redemption.Rate) #

# Test the model
predict(reward.model, test)


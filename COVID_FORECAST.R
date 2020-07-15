covid <- read.csv("C:/Users/User/Desktop/covid19.csv")
View(covid)

attach(covid)
train <- covid[1:56,]
test <- covid[56:69,]

##################################### LINEAR MODEDL ################################

linear_model <- lm(Confirmed ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval ='predict', newdata = test))
mape_linear <- mean(abs(test$Confirmed - linear_pred$fit) / (test$Confirmed))
mape_linear

################################# EXPONENTIAL ###################################

expo_model <- lm(logconfirmed ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
mape_expo <- mean(abs(test$Confirmed-exp(expo_pred$fit)) / (test$Confirmed))
mape_expo

############################### QUADRATIC #####################################

quad_model <- lm(Confirmed ~ t + t2, data = train)
summary(quad_model)
quad_pred <- data.frame(predict(quad_model, interval ='predict', newdata = test))
mape_quad <- mean(abs(test$Confirmed-quad_pred$fit) / (test$Confirmed))
mape_quad

############################### LINEAR TREND + DAILY SEASONALITY #################

lintrendsea <- lm(Confirmed ~ t + sin + cos, data = train)
summary(lintrendsea)
lintrendsea_pred <- data.frame(predict(lintrendsea, newdata = test, interval = 'predict'))
mape_lintrendsea <- mean(abs(test$Confirmed-lintrendsea_pred$fit) / (test$Confirmed))
mape_lintrendsea

############################## EXPONENTIAL TREND + DAILY SEASONALITY ##############

exptrendsea <- lm(logconfirmed ~ t +sin +cos, data = train)
summary(exptrendsea)
exptrendsea_pred <- data.frame(predict(exptrendsea, newdata = test, interval = 'predict'))
mape_exptrendsea <- mean(abs(test$Confirmed-exp(exptrendsea_pred$fit)) / (test$Confirmed))
mape_exptrendsea


############################## LINEAR TREND + DAILY SEASONALITY + LAG ########################

lintrendsealag <- lm(Confirmed ~ t + LagConfirmed + sin+ cos, data = train)
summary(lintrendsealag)
lintrendsealag_pred <- data.frame(predict(lintrendsealag, newdata = test, interval ='predict'))
mape_lintrendsealag <- mean((abs(test$Confirmed - lintrendsealag_pred$fit)) / test$Confirmed)
mape_lintrendsealag

#Preparing table on model and it's RMSE Values

table_mape <- data.frame(c("mape_linear", "mape_expo", "mape_quad", "mape_lintrendsea", "mape_exprendsea", "mape_lintrendsealag"))
colnames(table_mape) <- c("model", "MAPE")
View(table_mape)
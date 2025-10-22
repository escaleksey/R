getwd()
setwd("C:/Users/scana/PycharmProjects/R/task3")
data <- read.csv("household_power_consumption.csv")

library(forecast)
library(FNN)
library(ggplot2)
data$Global_active_power <- as.numeric(gsub(",", ".", data$Global_active_power))

series <- na.omit(data$Global_active_power)


n <- 100
n_tr <- round(n * 0.8)
train <- series[1:n_tr]
test  <- series[(n_tr + 1):n]



source("funcs_regress.r")
res_arima <- forecast_arima(train, test)
res_ma    <- forecast_ma(train, test, window = 10)
res_knn   <- forecast_knn(train, test, k = 3, window = 7)


source("multi_param.r")

train_df <- data.frame(
  Global_active_power = as.numeric(train),
  Voltage = as.numeric(na.omit(data$Voltage)[1:length(train)])
)

test_df <- data.frame(
  Global_active_power = as.numeric(test),
  Voltage = as.numeric(na.omit(data$Voltage)[(length(train)+1):(length(train)+length(test))])
)

forecast_svm(train_df, "Global_active_power", feature_cols = "Voltage", test_df = test_df, filename = "SVM по 1 параметру.png")

forecast_lm_poly(train_df, "Global_active_power", poly_cols = "Voltage", test_df = test_df, filename = "МНК по 1 параметру.png")


train_df <- data.frame(
  Global_active_power = as.numeric(train),
  Voltage = as.numeric(na.omit(data$Voltage)[1:length(train)]),
  Global_intensity = as.numeric(na.omit(data$Global_intensity)[1:length(train)])
)

test_df <- data.frame(
  Global_active_power = as.numeric(test),
  Voltage = as.numeric(na.omit(data$Voltage)[(length(train)+1):(length(train)+length(test))]),
  Global_intensity = as.numeric(na.omit(data$Global_intensity)[(length(train)+1):(length(train)+length(test))])
)

forecast_lm_poly(
  train_df,
  target_col = "Global_active_power",
  poly_cols = c("Voltage", "Global_intensity"),
  test_df = test_df,
  filename = "МНК по 2 параметрам.png"
)
forecast_svm(
  train_df,
  target_col = "Global_active_power",
  feature_cols = c("Voltage", "Global_intensity"),
  test_df = test_df,
  filename = "SVM по 2 параметрам.png"
)



train_df <- data.frame(
  Global_active_power = as.numeric(train),
  Voltage = as.numeric(na.omit(data$Voltage)[1:length(train)]),
  Global_intensity = as.numeric(na.omit(data$Global_intensity)[1:length(train)]),
  Global_reactive_power = as.numeric(na.omit(data$Global_reactive_power)[1:length(train)])
)

test_df <- data.frame(
  Global_active_power = as.numeric(test),
  Voltage = as.numeric(na.omit(data$Voltage)[(length(train)+1):(length(train)+length(test))]),
  Global_intensity = as.numeric(na.omit(data$Global_intensity)[(length(train)+1):(length(train)+length(test))]),
  Global_reactive_power = as.numeric(na.omit(data$Global_reactive_power)[(length(train)+1):(length(train)+length(test))])
)

forecast_lm_poly(
  train_df,
  target_col = "Global_active_power",
  poly_cols = c("Voltage", "Global_intensity", "Global_reactive_power"),
  test_df = test_df,
  filename = "МНК по 3 параметрам.png"
)
forecast_svm(
  train_df,
  target_col = "Global_active_power",
  feature_cols = c("Voltage", "Global_intensity", "Global_reactive_power"),
  test_df = test_df,
  filename = "SVM по 3 параметрам.png"
)

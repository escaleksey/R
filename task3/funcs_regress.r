library(forecast)
library(FNN)
library(ggplot2)

# ---------- 1. ARIMA ----------
forecast_arima <- function(train, test, filename = "arima_full.png") {
  library(forecast)
  library(ggplot2)
  
  # ---------- Прогноз ARIMA ----------
  model <- auto.arima(train)
  preds_test <- forecast(model, h = length(test))$mean
  preds_train <- fitted(model)  # предсказания модели на тренировочной выборке
  
  mse <- mean((test - preds_test)^2)
  cat("MSE (ARIMA):", mse, "\n")
  
  # ---------- Подготовка данных для графика ----------
  df <- data.frame(
    Index = 1:(length(train) + length(test)),
    Fact = c(train, test),
    Train_Fit = c(preds_train, rep(NA, length(test))),
    Forecast = c(rep(NA, length(train)), preds_test)
  )
  
  # ---------- Построение графика ----------
  p <- ggplot(df, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +            
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) + 
    geom_line(aes(y = Forecast, color = "Прогноз"), size = 1.2) +    
    scale_color_manual(values = c("Факт" = "pink", "Обучение" = "black", "Прогноз" = "blue")) +
    labs(title = "ARIMA", y = "Значение параметра", color = "") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(preds_test = preds_test, preds_train = preds_train, mse = mse, model = model))
}




# ---------- 2. Скользящее среднее ----------
forecast_ma <- function(train, test, window = 5, filename = "ma_full.png") {
  library(ggplot2)
  
  # ---------- Прогноз на train ----------
  train_fit <- rep(NA, length(train))
  for (i in seq_len(length(train))) {
    if (i < window) {
      train_fit[i] <- mean(train[1:i])
    } else {
      train_fit[i] <- mean(train[(i-window+1):i])
    }
  }
  
  # ---------- Прогноз на test ----------
  preds_test <- rep(NA, length(test))
  data_all <- c(train, rep(NA, length(test)))
  for (i in seq_along(test)) {
    preds_test[i] <- mean(tail(data_all[1:(length(train)+i-1)], window))
    data_all[length(train)+i] <- preds_test[i]
  }
  
  mse <- mean((test - preds_test)^2)
  cat("MSE (MA):", mse, "\n")
  
  # ---------- Подготовка данных ----------
  df <- data.frame(
    Index = 1:(length(train)+length(test)),
    Fact = c(train, test),
    Train_Fit = c(train_fit, rep(NA, length(test))),
    Forecast = c(rep(NA, length(train)), preds_test)
  )
  
  # ---------- Построение графика ----------
  p <- ggplot(df, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) +
    geom_line(aes(y = Forecast, color = "Прогноз"), size = 1.2) +
    scale_color_manual(values = c("Факт" = "pink", "Обучение" = "black", "Прогноз" = "blue")) +
    labs(title = paste("Плавающее среднее (окно =", window, ")"),
         y = "Значение параметра", color = "") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(preds_test = preds_test, train_fit = train_fit, mse = mse))
}



# ---------- 3. k-NN ----------
forecast_knn <- function(train, test, k = 3, window = 5, filename = "knn_full.png") {
  library(FNN)
  library(ggplot2)
  
  # ---------- Прогноз на train ----------
  train_fit <- numeric(length(train))
  X_train <- embed(train, window + 1)
  y_train <- X_train[, 1]
  X_train <- X_train[, -1]
  
  # Для первых window точек берём реальные значения
  train_fit[1:window] <- train[1:window]
  
  for (i in (window+1):length(train)) {
    last_window <- train[(i-window):(i-1)]
    pred <- knn.reg(train = X_train, test = matrix(last_window, nrow = 1), y = y_train, k = k)$pred
    train_fit[i] <- pred
  }
  
  # ---------- Прогноз на test ----------
  preds_test <- numeric(length(test))
  last_window <- tail(train, window)
  
  for (i in seq_along(test)) {
    pred <- knn.reg(train = X_train, test = matrix(last_window, nrow = 1), y = y_train, k = k)$pred
    preds_test[i] <- pred
    last_window <- c(last_window[-1], pred)
  }
  
  mse <- mean((test - preds_test)^2)
  cat("MSE (kNN):", mse, "\n")
  
  # ---------- Подготовка данных ----------
  df <- data.frame(
    Index = 1:(length(train)+length(test)),
    Fact = c(train, test),
    Train_Fit = c(train_fit, rep(NA, length(test))),
    Forecast = c(rep(NA, length(train)), preds_test)
  )
  
  # ---------- Построение графика ----------
  p <- ggplot(df, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) +
    geom_line(aes(y = Forecast, color = "Прогноз"), size = 1.2) +
    scale_color_manual(values = c("Факт" = "pink", "Обучение" = "black", "Прогноз" = "blue")) +
    labs(title = paste("k-NN: Факт, Fit на тренировке и прогноз (k =", k, ", window =", window, ")"),
         y = "Значение параметра", color = "") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(preds_test = preds_test, train_fit = train_fit, mse = mse))
}



forecast_ma_windows <- function(train, test, windows = c(3, 5, 10), filename = "ma_windows.png") {
  library(ggplot2)
  library(tidyr)
  
  df_plot <- data.frame(Index = 1:length(test), Test = test)
  
  for (w in windows) {
    preds <- rep(NA, length(test))
    data_all <- c(train, rep(NA, length(test)))
    
    for (i in 1:length(test)) {
      preds[i] <- mean(tail(data_all[1:(length(train) + i - 1)], w))
      data_all[length(train) + i] <- preds[i]
    }
    
    df_plot[[paste0("Окно ", w)]] <- preds
  }
  
  # Преобразуем в длинный формат для ggplot
  df_long <- pivot_longer(df_plot, cols = -Index, names_to = "Series", values_to = "Value")
  
  p <- ggplot(df_long, aes(x = Index, y = Value, color = Series)) +
    geom_line(size = 1.2) +
    labs(title = "Прогноз MA с разными окнами", y = "Значение параметра", color = "") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(df_plot)
}


forecast_knn_windows <- function(train, test, windows = c(3, 5), ks = c(1, 3, 5), filename = "knn_windows.png") {
  library(FNN)
  library(ggplot2)
  library(tidyr)
  
  df_plot <- data.frame(Index = 1:length(test), Test = test)
  
  for (w in windows) {
    X_train <- embed(train, w + 1)
    y_train <- X_train[, 1]
    X_train <- X_train[, -1]
    
    for (k_val in ks) {
      preds <- numeric(length(test))
      last_window <- tail(train, w)
      
      for (i in 1:length(test)) {
        pred <- knn.reg(train = X_train, test = matrix(last_window, nrow = 1), y = y_train, k = k_val)$pred
        preds[i] <- pred
        last_window <- c(last_window[-1], pred)
      }
      
      col_name <- paste0("w", w, "_k", k_val)
      df_plot[[col_name]] <- preds
    }
  }
  
  # Преобразуем в длинный формат для ggplot
  df_long <- pivot_longer(df_plot, cols = -Index, names_to = "Series", values_to = "Value")
  
  # Построение графика
  p <- ggplot(df_long, aes(x = Index, y = Value, color = Series)) +
    geom_line(size = 1.2) +
    labs(title = "Прогноз k-NN с разными window и k", y = "Значение параметра", color = "") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 10, height = 6)
  print(p)
  
  return(df_plot)
}

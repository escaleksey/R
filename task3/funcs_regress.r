library(forecast)
library(FNN)
library(ggplot2)
library(Metrics)

# ======================= 1. ARIMA ===========================
forecast_arima <- function(train, test, filename = "arima_full.png") {
  model <- auto.arima(train)
  preds_test <- forecast(model, h = length(test))$mean
  preds_train <- fitted(model)
  
  # --- Метрики ---
  mae <- mae(test, preds_test)
  rmse <- rmse(test, preds_test)
  r2 <- 1 - sum((test - preds_test)^2) / sum((test - mean(test))^2)
  
  cat("ARIMA:\n")
  cat("MAE:", mae, "\nRMSE:", rmse, "\nR2:", r2, "\n\n")
  
  # --- График ---
  df <- data.frame(
    Index = 1:(length(train) + length(test)),
    Fact = c(train, test),
    Train_Fit = c(preds_train, rep(NA, length(test))),
    Forecast = c(rep(NA, length(train)), preds_test)
  )
  
  metrics_text <- sprintf("MAE = %.3f\nRMSE = %.3f\nR² = %.3f", mae, rmse, r2)
  
  p <- ggplot(df, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) +
    geom_line(aes(y = Forecast, color = "Прогноз"), size = 1.2) +
    scale_color_manual(values = c("Факт" = "gray", "Обучение" = "blue", "Прогноз" = "red")) +
    labs(title = "ARIMA", y = "Значение параметра", color = "") +
    annotate("text", x = length(train) * 0.1, y = max(df$Fact, na.rm = TRUE), 
             label = metrics_text, hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(model = model, mae = mae, rmse = rmse, r2 = r2))
}


# ======================= 2. Скользящее среднее ===========================
forecast_ma <- function(train, test, window = 5, filename = "ma_full.png") {
  train_fit <- rep(NA, length(train))
  for (i in seq_len(length(train))) {
    if (i < window) train_fit[i] <- mean(train[1:i])
    else train_fit[i] <- mean(train[(i-window+1):i])
  }
  
  preds_test <- rep(NA, length(test))
  data_all <- c(train, rep(NA, length(test)))
  for (i in seq_along(test)) {
    preds_test[i] <- mean(tail(data_all[1:(length(train)+i-1)], window))
    data_all[length(train)+i] <- preds_test[i]
  }
  
  # --- Метрики ---
  mae <- mae(test, preds_test)
  rmse <- rmse(test, preds_test)
  r2 <- 1 - sum((test - preds_test)^2) / sum((test - mean(test))^2)
  
  cat("Moving Average:\n")
  cat("MAE:", mae, "\nRMSE:", rmse, "\nR2:", r2, "\n\n")
  
  df <- data.frame(
    Index = 1:(length(train)+length(test)),
    Fact = c(train, test),
    Train_Fit = c(train_fit, rep(NA, length(test))),
    Forecast = c(rep(NA, length(train)), preds_test)
  )
  
  metrics_text <- sprintf("MAE = %.3f\nRMSE = %.3f\nR² = %.3f", mae, rmse, r2)
  
  p <- ggplot(df, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) +
    geom_line(aes(y = Forecast, color = "Прогноз"), size = 1.2) +
    scale_color_manual(values = c("Факт" = "gray", "Обучение" = "blue", "Прогноз" = "red")) +
    labs(title = paste("Плавающее среднее (окно =", window, ")"),
         y = "Значение параметра", color = "") +
    annotate("text", x = length(train) * 0.1, y = max(df$Fact, na.rm = TRUE),
             label = metrics_text, hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(mae = mae, rmse = rmse, r2 = r2, preds_test = preds_test))
}


# ======================= 3. kNN ===========================
forecast_knn <- function(train, test, k = 3, window = 5, filename = "knn_full.png") {
  X_train <- embed(train, window + 1)
  y_train <- X_train[, 1]
  X_train <- X_train[, -1]
  
  train_fit <- numeric(length(train))
  train_fit[1:window] <- train[1:window]
  for (i in (window+1):length(train)) {
    last_window <- train[(i-window):(i-1)]
    pred <- knn.reg(train = X_train, test = matrix(last_window, nrow = 1), y = y_train, k = k)$pred
    train_fit[i] <- pred
  }
  
  preds_test <- numeric(length(test))
  last_window <- tail(train, window)
  for (i in seq_along(test)) {
    pred <- knn.reg(train = X_train, test = matrix(last_window, nrow = 1), y = y_train, k = k)$pred
    preds_test[i] <- pred
    last_window <- c(last_window[-1], pred)
  }
  
  # --- Метрики ---
  mae <- mae(test, preds_test)
  rmse <- rmse(test, preds_test)
  r2 <- 1 - sum((test - preds_test)^2) / sum((test - mean(test))^2)
  
  cat("kNN:\n")
  cat("MAE:", mae, "\nRMSE:", rmse, "\nR2:", r2, "\n\n")
  
  df <- data.frame(
    Index = 1:(length(train)+length(test)),
    Fact = c(train, test),
    Train_Fit = c(train_fit, rep(NA, length(test))),
    Forecast = c(rep(NA, length(train)), preds_test)
  )
  
  metrics_text <- sprintf("MAE = %.3f\nRMSE = %.3f\nR² = %.3f", mae, rmse, r2)
  
  p <- ggplot(df, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) +
    geom_line(aes(y = Forecast, color = "Прогноз"), size = 1.2) +
    scale_color_manual(values = c("Факт" = "gray", "Обучение" = "blue", "Прогноз" = "red")) +
    labs(title = paste("k-NN (k =", k, ", окна =", window, ")"),
         y = "Значение параметра", color = "") +
    annotate("text", x = length(train) * 0.1, y = max(df$Fact, na.rm = TRUE),
             label = metrics_text, hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(mae = mae, rmse = rmse, r2 = r2, preds_test = preds_test))
}

library(FNN)       # для knn.reg
library(Metrics)   # для mae, rmse
library(dplyr)

# ---- Функция ----
tune_knn_autoreg <- function(series, 
                             k_values = 1:15, 
                             window_values = 2:10, 
                             test_size = 0.2) {
  
  # Преобразуем в числовой вектор
  series <- as.numeric(series)
  n <- length(series)
  n_train <- floor((1 - test_size) * n)
  
  results <- data.frame(
    k = integer(),
    window = integer(),
    MAE = numeric(),
    RMSE = numeric(),
    R2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  # --- Перебор параметров ---
  for (window in window_values) {
    
    # Формируем обучающие признаки (лаговые значения)
    X <- embed(series, window + 1)[, -1]      # прошлые значения
    y <- embed(series, window + 1)[, 1]       # текущее значение
    
    # Разделяем на train/test
    X_train <- X[1:n_train, ]
    y_train <- y[1:n_train]
    X_test <- X[(n_train+1):nrow(X), ]
    y_test <- y[(n_train+1):length(y)]
    
    for (k in k_values) {
      # Обучаем KNN-регрессию
      model <- knn.reg(train = X_train, test = X_test, y = y_train, k = k)
      preds <- model$pred
      
      # Метрики
      mae_val <- mae(y_test, preds)
      rmse_val <- rmse(y_test, preds)
      r2_val <- cor(y_test, preds)^2
      
      # Сохраняем результат
      results <- rbind(
        results,
        data.frame(k = k, window = window, 
                   MAE = mae_val, RMSE = rmse_val, R2 = r2_val)
      )
    }
  }
  
  # --- Лучшее сочетание ---
  best <- results %>%
    arrange(RMSE, MAE, desc(R2)) %>%
    slice(1)
  
  # Возвращаем лучший набор и всю таблицу
  list(
    best_params = best,
    all_results = results
  )
}

# ==== Пример использования ====

# Допустим, у тебя есть ряд:
# series <- data$Global_active_power

# Подбор параметров
# res <- tune_knn_autoreg(series, k_values = 1:10, window_values = 2:8)

# Лучшая комбинация:
# res$best_params

# Вся таблица:
# View(res$all_results)

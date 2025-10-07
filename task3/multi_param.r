forecast_lm_poly <- function(train_df, target_col, poly_cols = NULL, test_df = NULL, filename = "lm_forecast.png") {
  library(ggplot2)
  library(Metrics)
  
  # ---------- Формируем формулу ----------
  f <- target_col
  terms <- c()
  if (!is.null(poly_cols)) {
    for (col in poly_cols) {
      terms <- c(terms, col, paste0("I(", col, "^2)"))
    }
  }
  formula_str <- paste(f, "~", paste(terms, collapse = "+"))
  formula_lm <- as.formula(formula_str)
  
  # ---------- Обучаем модель ----------
  model <- lm(formula_lm, data = train_df)
  
  # ---------- Прогноз ----------
  train_pred <- predict(model, newdata = train_df)
  test_pred <- if(!is.null(test_df)) predict(model, newdata = test_df) else NULL
  
  # ---------- Метрики ----------
  train_mae <- mae(train_df[[target_col]], train_pred)
  train_rmse <- rmse(train_df[[target_col]], train_pred)
  train_r2 <- 1 - sum((train_df[[target_col]] - train_pred)^2)/sum((train_df[[target_col]] - mean(train_df[[target_col]]))^2)
  
  if(!is.null(test_pred)) {
    test_mae <- mae(test_df[[target_col]], test_pred)
    test_rmse <- rmse(test_df[[target_col]], test_pred)
    test_r2 <- 1 - sum((test_df[[target_col]] - test_pred)^2)/sum((test_df[[target_col]] - mean(test_df[[target_col]]))^2)
  }
  
  cat("Обучение:: MAE =", round(train_mae,3), "RMSE =", round(train_rmse,3), "R2 =", round(train_r2,3), "\n")
  if(!is.null(test_pred)) {
    cat("Прогноз:: MAE =", round(test_mae,3), "RMSE =", round(test_rmse,3), "R2 =", round(test_r2,3), "\n")
  }
  
  # ---------- Подготовка данных для графика ----------
  df_plot <- data.frame(
    Index = 1:nrow(train_df),
    Fact = train_df[[target_col]],
    Train_Fit = train_pred,
    Test_Forecast = NA
  )
  if(!is.null(test_pred)) {
    df_plot <- rbind(df_plot, data.frame(
      Index = (nrow(train_df)+1):(nrow(train_df)+nrow(test_df)),
      Fact = test_df[[target_col]],
      Train_Fit = NA,
      Test_Forecast = test_pred
    ))
  }
  
  metrics_text <- paste0(
    "Обучение:: MAE=", round(train_mae,2), ", RMSE=", round(train_rmse,2), ", R2=", round(train_r2,2)
  )
  if(!is.null(test_pred)) {
    metrics_text <- paste0(metrics_text, "\nTest: MAE=", round(test_mae,2), ", RMSE=", round(test_rmse,2), ", R2=", round(test_r2,2))
  }
  
  # ---------- График ----------
  p <- ggplot(df_plot, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) +
    geom_line(aes(y = Test_Forecast, color = "Прогноз"), size = 1.2) +
    scale_color_manual(values = c("Факт" = "pink", "Обучение" = "black", "Прогноз" = "blue")) +
    annotate("text", x = max(df_plot$Index)*0.3, 
             y = max(df_plot$Fact, na.rm = TRUE), 
             label = metrics_text, hjust = 0, vjust = 1, size = 4, color = "black") +
    labs(title = "Прогноз методом МНК", y = target_col, color = "") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(model = model, train_pred = train_pred, test_pred = test_pred,
              train_metrics = c(MAE=train_mae, RMSE=train_rmse, R2=train_r2),
              test_metrics = if(!is.null(test_pred)) c(MAE=test_mae, RMSE=test_rmse, R2=test_r2) else NULL))
}




forecast_svm <- function(train_df, target_col, feature_cols = NULL, test_df = NULL, filename = "svm_forecast.png") {
  library(e1071)
  library(ggplot2)
  library(Metrics)
  
  if (is.null(feature_cols)) {
    feature_cols <- setdiff(names(train_df), target_col)
  }
  
  formula_svm <- as.formula(paste(target_col, "~", paste(feature_cols, collapse = "+")))
  
  # ---------- Обучение ----------
  model <- svm(formula_svm, data = train_df, type = "eps-regression")
  
  # ---------- Прогноз ----------
  train_pred <- predict(model, newdata = train_df)
  test_pred <- if(!is.null(test_df)) predict(model, newdata = test_df) else NULL
  
  # ---------- Метрики ----------
  train_mae <- mae(train_df[[target_col]], train_pred)
  train_rmse <- rmse(train_df[[target_col]], train_pred)
  train_r2 <- 1 - sum((train_df[[target_col]] - train_pred)^2)/sum((train_df[[target_col]] - mean(train_df[[target_col]]))^2)
  
  if(!is.null(test_pred)) {
    test_mae <- mae(test_df[[target_col]], test_pred)
    test_rmse <- rmse(test_df[[target_col]], test_pred)
    test_r2 <- 1 - sum((test_df[[target_col]] - test_pred)^2)/sum((test_df[[target_col]] - mean(test_df[[target_col]]))^2)
  }
  
  cat("Обучение:: MAE =", round(train_mae,3), "RMSE =", round(train_rmse,3), "R2 =", round(train_r2,3), "\n")
  if(!is.null(test_pred)) cat("Прогноз:: MAE =", round(test_mae,3), "RMSE =", round(test_rmse,3), "R2 =", round(test_r2,3), "\n")
  
  # ---------- Подготовка графика ----------
  df_plot <- data.frame(
    Index = 1:nrow(train_df),
    Fact = train_df[[target_col]],
    Train_Fit = train_pred,
    Test_Forecast = NA
  )
  if(!is.null(test_pred)) {
    df_plot <- rbind(df_plot, data.frame(
      Index = (nrow(train_df)+1):(nrow(train_df)+nrow(test_df)),
      Fact = test_df[[target_col]],
      Train_Fit = NA,
      Test_Forecast = test_pred
    ))
  }
  
  metrics_text <- paste0(
    "Обучение:: MAE=", round(train_mae,2), ", RMSE=", round(train_rmse,2), ", R2=", round(train_r2,2)
  )
  if(!is.null(test_pred)) {
    metrics_text <- paste0(metrics_text, "\nTest: MAE=", round(test_mae,2), ", RMSE=", round(test_rmse,2), ", R2=", round(test_r2,2))
  }
  
  p <- ggplot(df_plot, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) +
    geom_line(aes(y = Test_Forecast, color = "Прогноз"), size = 1.2) +
    scale_color_manual(values = c("Факт" = "pink", "Обучение" = "black", "Прогноз" = "blue")) +
    annotate("text", x = max(df_plot$Index)*0.3, 
             y = max(df_plot$Fact, na.rm = TRUE), 
             label = metrics_text, hjust = 0, vjust = 1, size = 4, color = "black") +
    labs(title = "Прогноз методом SVM", y = target_col, color = "") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(model = model, train_pred = train_pred, test_pred = test_pred,
              train_metrics = c(MAE=train_mae, RMSE=train_rmse, R2=train_r2),
              test_metrics = if(!is.null(test_pred)) c(MAE=test_mae, RMSE=test_rmse, R2=test_r2) else NULL))
}

forecast_svm_caret <- function(train_df, target_col, feature_cols = NULL, test_df = NULL, filename = "svm_forecast.png") {
  library(caret)
  library(ggplot2)
  
  # ---------- Формируем формулу ----------
  if (!is.null(feature_cols)) {
    f <- paste(target_col, "~", paste(feature_cols, collapse = "+"))
  } else {
    f <- paste(target_col, "~ 1")
  }
  formula_svm <- as.formula(f)
  
  # ---------- Обучаем модель SVM ----------
  set.seed(123)
  mod <- train(formula_svm, data = train_df, method = "svmLinear")
  
  # ---------- Прогноз ----------
  train_pred <- predict(mod, newdata = train_df)
  if (!is.null(test_df)) {
    test_pred <- predict(mod, newdata = test_df)
  } else {
    test_pred <- NULL
  }
  
  # ---------- Метрики ----------
  train_metrics <- postResample(pred = train_pred, obs = train_df[[target_col]])
  if (!is.null(test_pred)) {
    test_metrics <- postResample(pred = test_pred, obs = test_df[[target_col]])
  } else {
    test_metrics <- NULL
  }
  
  cat("Обучение: metrics:\n")
  print(train_metrics)
  if (!is.null(test_metrics)) {
    cat("Прогноз: metrics:\n")
    print(test_metrics)
  }
  
  # ---------- Подготовка данных для графика ----------
  df_plot <- data.frame(
    Index = 1:nrow(train_df),
    Fact = train_df[[target_col]],
    Train_Fit = train_pred,
    Test_Forecast = NA
  )
  
  if (!is.null(test_pred)) {
    df_plot <- rbind(df_plot,
                     data.frame(
                       Index = (nrow(train_df)+1):(nrow(train_df)+nrow(test_df)),
                       Fact = test_df[[target_col]],
                       Train_Fit = NA,
                       Test_Forecast = test_pred
                     ))
  }
  
  # ---------- Текст с метриками ----------
  metrics_text <- paste0(
    "Обучение:: MAE=", round(train_metrics["MAE"],3),
    ", RMSE=", round(train_metrics["RMSE"],3),
    ", R2=", round(train_metrics["Rsquared"],3)
  )
  if (!is.null(test_metrics)) {
    metrics_text <- paste0(metrics_text,
                           "\nTest: MAE=", round(test_metrics["MAE"],3),
                           ", RMSE=", round(test_metrics["RMSE"],3),
                           ", R2=", round(test_metrics["Rsquared"],3))
  }
  
  # ---------- График ----------
  p <- ggplot(df_plot, aes(x = Index)) +
    geom_line(aes(y = Fact, color = "Факт"), size = 1) +
    geom_line(aes(y = Train_Fit, color = "Обучение"), size = 1.2) +
    geom_line(aes(y = Test_Forecast, color = "Прогноз"), size = 1.2) +
    scale_color_manual(values = c("Факт" = "pink", "Обучение" = "black", "Прогноз" = "blue")) +
    annotate("text", x = max(df_plot$Index)*0.3, 
             y = max(df_plot$Fact, na.rm = TRUE), 
             label = metrics_text, hjust = 0, vjust = 1, size = 4, color = "black") +
    labs(title = "Прогноз методом SVM (caret)", y = target_col, color = "") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 8, height = 5)
  print(p)
  
  return(list(model = mod, train_pred = train_pred, test_pred = test_pred, train_metrics = train_metrics, test_metrics = test_metrics))
}


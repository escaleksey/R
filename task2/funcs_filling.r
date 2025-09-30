library(ggplot2)
library(dplyr)

means <- function(data, data_na) {
    
    data_mean_filled <- data_na
    mean_val <- mean(data$Monthly.beer.production, na.rm = TRUE)
    data_mean_filled$Monthly.beer.production[is.na(data_mean_filled$Monthly.beer.production)] <- mean_val

    # Добавляем номер четверти
    n <- nrow(data)
    quarter <- rep(1:4, each = ceiling(n/4))[1:n]
    data$Quarter <- quarter
    data_mean_filled$Quarter <- quarter

    # Преобразуем Month в дату (если формат "YYYY-MM")
    data$MonthDate <- as.Date(paste0(data$Month, "-01"))
    data_mean_filled$MonthDate <- as.Date(paste0(data_mean_filled$Month, "-01"))

    # Создаём список графиков
    plots <- list()
    for(q in 1:4){
        df_q <- data.frame(
            Month = data$MonthDate[data$Quarter == q],
            Original = data$Monthly.beer.production[data$Quarter == q],
            MeanFilled = data_mean_filled$Monthly.beer.production[data$Quarter == q]
        )
        
        plots[[q]] <- ggplot(df_q, aes(x = Month)) +
            geom_line(aes(y = Original, color = "Исходные"), group = 1) +
            geom_point(aes(y = Original, color = "Исходные")) +
            geom_line(aes(y = MeanFilled, color = "Заполнено средним"), group = 1) +
            geom_point(aes(y = MeanFilled, color = "Заполнено средним")) +
            labs(
                title = paste("Четверть", q),
                x = "Месяц",
                y = "Производство",
                color = "Тип данных"
            ) +
            theme_minimal()
    }

    # Выводим графики один под другим с patchwork
    library(patchwork)
    plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
    ggsave("Средний.png", plot=plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]], width = 8, height = 10, dpi = 300)

}



median_f <- function(data, data_na) {
    
    data_mean_filled <- data_na
    mean_val <- median(data$Monthly.beer.production, na.rm = TRUE)
    data_mean_filled$Monthly.beer.production[is.na(data_mean_filled$Monthly.beer.production)] <- mean_val

    # Добавляем номер четверти
    n <- nrow(data)
    quarter <- rep(1:4, each = ceiling(n/4))[1:n]
    data$Quarter <- quarter
    data_mean_filled$Quarter <- quarter

    # Преобразуем Month в дату (если формат "YYYY-MM")
    data$MonthDate <- as.Date(paste0(data$Month, "-01"))
    data_mean_filled$MonthDate <- as.Date(paste0(data_mean_filled$Month, "-01"))

    # Создаём список графиков
    plots <- list()
    for(q in 1:4){
        df_q <- data.frame(
            Month = data$MonthDate[data$Quarter == q],
            Original = data$Monthly.beer.production[data$Quarter == q],
            MeanFilled = data_mean_filled$Monthly.beer.production[data$Quarter == q]
        )
        
        plots[[q]] <- ggplot(df_q, aes(x = Month)) +
            geom_line(aes(y = Original, color = "Исходные"), group = 1) +
            geom_point(aes(y = Original, color = "Исходные")) +
            geom_line(aes(y = MeanFilled, color = "Заполнено медианой"), group = 1) +
            geom_point(aes(y = MeanFilled, color = "Заполнено медианой")) +
            labs(
                title = paste("Четверть", q),
                x = "Месяц",
                y = "Производство",
                color = "Тип данных"
            ) +
            theme_minimal()
    }

    # Выводим графики один под другим с patchwork
    library(patchwork)
    plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
    ggsave("Медиана.png", plot=plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]], width = 8, height = 10, dpi = 300)

}


fill_previous_mean <- function(data, data_thinned_na, n_prev = 3) {
  
  data_filled <- data_thinned_na
  
  # ---- Заполнение пропусков скользящим средним по n_prev предыдущим значениям ----
  for(i in seq_len(nrow(data_filled))) {
    if(is.na(data_filled$Monthly.beer.production[i])) {
      prev_vals <- data_filled$Monthly.beer.production[max(1, i - n_prev):(i - 1)]
      prev_vals <- prev_vals[!is.na(prev_vals)]
      if(length(prev_vals) > 0) {
        data_filled$Monthly.beer.production[i] <- mean(prev_vals)
      }
    }
  }
  
  # ---- Добавляем номер четверти ----
  n <- nrow(data)
  quarter <- rep(1:4, each = ceiling(n/4))[1:n]
  data$Quarter <- quarter
  data_filled$Quarter <- quarter
  
  # ---- Преобразуем Month в дату ----
  data$MonthDate <- as.Date(paste0(data$Month, "-01"))
  data_filled$MonthDate <- as.Date(paste0(data_filled$Month, "-01"))
  
  # ---- Создаём список графиков по четвертям ----
  plots <- list()
  for(q in 1:4) {
    df_q <- data.frame(
      Month = data$MonthDate[data$Quarter == q],
      Original = data$Monthly.beer.production[data$Quarter == q],
      Filled = data_filled$Monthly.beer.production[data$Quarter == q]
    )
    
    plots[[q]] <- ggplot(df_q, aes(x = Month)) +
      geom_line(aes(y = Original, color = "Исходные"), group = 1) +
      geom_point(aes(y = Original, color = "Исходные")) +
      geom_line(aes(y = Filled, color = "Заполнено скользящим средним"), group = 1, linetype = "dashed") +
      geom_point(aes(y = Filled, color = "Заполнено скользящим средним"), shape = 1) +
      labs(
        title = paste("Четверть", q),
        x = "Месяц",
        y = "Производство",
        color = "Тип данных"
      ) +
      theme_minimal()
  }
  
  # ---- Выводим графики один под другим с patchwork ----
  library(patchwork)
  combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
  
  # ---- Сохраняем в PNG ----
  ggsave("СкользящееСреднее.png", plot = combined_plot, width = 8, height = 10, dpi = 300)
  
  # ---- Возвращаем комбинированный график ----
  return(combined_plot)
}


fill_lagrange_quarters <- function(data, data_thinned_na, degree = 2) {
  library(pracma)
  library(ggplot2)
  library(patchwork)

  data_filled <- data_thinned_na

  # ---- Заполнение пропусков методом Лагранжа (через polyfit/polyval) ----
  x <- 1:nrow(data_filled)
  y <- data_filled$Monthly.beer.production

  for(i in seq_along(y)) {
    if(is.na(y[i])) {
      # Берем до 3 точек слева и справа
      left <- max(1, i-3)
      right <- min(length(y), i+3)

      xi <- x[left:right]
      yi <- y[left:right]

      # Оставляем только известные значения
      xi <- xi[!is.na(yi)]
      yi <- yi[!is.na(yi)]

      if(length(xi) >= 2) {
        # Ограничиваем степень полинома
        d <- min(degree, length(xi)-1)
        p <- polyfit(xi, yi, d)
        y[i] <- polyval(p, x[i])
      }
    }
  }

  data_filled$Monthly.beer.production <- y

  # ---- Добавляем номер четверти ----
  n <- nrow(data)
  quarter <- rep(1:4, each = ceiling(n/4))[1:n]
  data$Quarter <- quarter
  data_filled$Quarter <- quarter

  # ---- Преобразуем Month в дату ----
  data$MonthDate <- as.Date(paste0(data$Month, "-01"))
  data_filled$MonthDate <- as.Date(paste0(data_filled$Month, "-01"))

  # ---- Строим графики ----
  plots <- list()
  for(q in 1:4) {
    df_q <- data.frame(
      Month = data$MonthDate[data$Quarter == q],
      Original = data$Monthly.beer.production[data$Quarter == q],
      Filled = data_filled$Monthly.beer.production[data$Quarter == q]
    )

    plots[[q]] <- ggplot(df_q, aes(x = Month)) +
      geom_line(aes(y = Original, color = "Исходные"), group = 1) +
      geom_point(aes(y = Original, color = "Исходные")) +
      geom_line(aes(y = Filled, color = "Заполнено Лагранжем"), group = 1, linetype = "dashed") +
      geom_point(aes(y = Filled, color = "Заполнено Лагранжем"), shape = 1) +
      labs(
        title = paste("Четверть", q),
        x = "Месяц",
        y = "Производство",
        color = "Тип данных"
      ) +
      theme_minimal()
  }

  combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
  ggsave("Лагранж.png", plot = combined_plot, width = 8, height = 10, dpi = 300)

  return(combined_plot)
}



fill_spline_quarters <- function(data, data_thinned_na) {
  # Копируем "прореженные" данные
  data_filled <- data_thinned_na
  
  # ---- Заполнение пропусков методом кубических сплайнов ----
  x <- 1:nrow(data_filled)
  y <- data_filled$Monthly.beer.production
  
  # Опорные точки (без NA)
  xi <- x[!is.na(y)]
  yi <- y[!is.na(y)]
  
  # Создаём функцию-сплайн
  spline_fun <- splinefun(xi, yi, method = "natural")
  
  # Заполняем NA
  for(i in seq_along(y)) {
    if(is.na(y[i])) {
      y[i] <- spline_fun(x[i])
    }
  }
  
  data_filled$Monthly.beer.production <- y
  
  # ---- Добавляем номер четверти ----
  n <- nrow(data)
  quarter <- rep(1:4, each = ceiling(n/4))[1:n]
  data$Quarter <- quarter
  data_filled$Quarter <- quarter
  
  # ---- Преобразуем Month в дату ----
  data$MonthDate <- as.Date(paste0(data$Month, "-01"))
  data_filled$MonthDate <- as.Date(paste0(data_filled$Month, "-01"))
  
  # ---- Создаём список графиков ----
  plots <- list()
  for(q in 1:4) {
    df_q <- data.frame(
      Month = data$MonthDate[data$Quarter == q],
      Original = data$Monthly.beer.production[data$Quarter == q],
      Filled = data_filled$Monthly.beer.production[data$Quarter == q]
    )
    
    plots[[q]] <- ggplot(df_q, aes(x = Month)) +
      geom_line(aes(y = Original, color = "Исходные"), group = 1) +
      geom_point(aes(y = Original, color = "Исходные")) +
      geom_line(aes(y = Filled, color = "Заполнено сплайнами"), group = 1, linetype = "dashed") +
      geom_point(aes(y = Filled, color = "Заполнено сплайнами"), shape = 1) +
      labs(
        title = paste("Четверть", q),
        x = "Месяц",
        y = "Производство",
        color = "Тип данных"
      ) +
      theme_minimal()
  }
  
  # ---- Выводим и сохраняем ----
  library(patchwork)
  combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
  ggsave("Сплайны.png", plot = combined_plot, width = 8, height = 10, dpi = 300)
  
  return(combined_plot)
}





compare_all_methods <- function(data, data_na) {
  library(pracma)
  library(zoo)
  library(ggplot2)
  library(patchwork)

  ## ---- Базовые данные ----
  n <- nrow(data)
  quarter <- rep(1:4, each = ceiling(n/4))[1:n]
  data$Quarter <- quarter
  data$MonthDate <- as.Date(paste0(data$Month, "-01"))
  data_na$Quarter <- quarter
  data_na$MonthDate <- as.Date(paste0(data_na$Month, "-01"))

  x <- 1:n
  y <- data_na$Monthly.beer.production

  ## ---- Среднее ----
  mean_val <- mean(data$Monthly.beer.production, na.rm = TRUE)
  y_mean <- ifelse(is.na(y), mean_val, y)

  ## ---- Медиана ----
  median_val <- median(data$Monthly.beer.production, na.rm = TRUE)
  y_median <- ifelse(is.na(y), median_val, y)

  ## ---- Скользящее среднее (3 предыдущих) ----
  y_prevmean <- y
  for(i in seq_len(n)) {
    if(is.na(y_prevmean[i])) {
      prev_vals <- y_prevmean[max(1, i-3):(i-1)]
      prev_vals <- prev_vals[!is.na(prev_vals)]
      if(length(prev_vals) > 0) {
        y_prevmean[i] <- mean(prev_vals)
      }
    }
  }

  ## ---- Лагранж ----
  y_lagrange <- y
  for(i in seq_along(y)) {
    if(is.na(y[i])) {
      left <- max(1, i-3)
      right <- min(n, i+3)
      xi <- x[left:right]
      yi <- y[left:right]
      xi <- xi[!is.na(yi)]
      yi <- yi[!is.na(yi)]
      if(length(xi) >= 2) {
        d <- min(2, length(xi)-1)
        p <- polyfit(xi, yi, d)
        y_lagrange[i] <- polyval(p, x[i])
      }
    }
  }

  ## ---- Сплайны ----
  y_spline <- y
  xi <- x[!is.na(y)]
  yi <- y[!is.na(y)]
  spline_fun <- splinefun(xi, yi, method = "natural")
  y_spline[is.na(y)] <- spline_fun(x[is.na(y)])

  ## ---- Объединяем всё ----
  df <- data.frame(
    Month = data$MonthDate,
    Quarter = quarter,
    Original = data$Monthly.beer.production,
    Mean = y_mean,
    Median = y_median,
    PrevMean = y_prevmean,
    Lagrange = y_lagrange,
    Spline = y_spline
  )

  ## ---- Строим графики по кварталам ----
  plots <- list()
  for(q in 1:4) {
    df_q <- df[df$Quarter == q, ]
    plots[[q]] <- ggplot(df_q, aes(x = Month)) +
      geom_line(aes(y = Original, color = "Исходные"), linewidth = 1) +
      geom_line(aes(y = Mean, color = "Среднее"), linetype = "dashed") +
      geom_line(aes(y = Median, color = "Медиана"), linetype = "dotted") +
      geom_line(aes(y = PrevMean, color = "Скользящее ср."), linetype = "dotdash") +
      geom_line(aes(y = Lagrange, color = "Лагранж"), linetype = "twodash") +
      geom_line(aes(y = Spline, color = "Сплайны"), linetype = "longdash") +
      labs(
        title = paste("Четверть", q),
        x = "Месяц",
        y = "Производство",
        color = "Метод"
      ) +
      theme_minimal()
  }

  combined_plot <- plots[[1]] / plots[[2]] / plots[[3]] / plots[[4]]
  ggsave("Сравнение_все.png", plot = combined_plot, width = 10, height = 12, dpi = 300)

  return(combined_plot)
}

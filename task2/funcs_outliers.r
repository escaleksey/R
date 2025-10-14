sigma_out <- function(data){
    mean_val <- mean(data$Monthly.beer.production, na.rm = TRUE)
    sd_val <- sd(data$Monthly.beer.production, na.rm = TRUE)

    # Нижняя и верхняя границы по правилу 3 сигм
    lower <- mean_val - 3 * sd_val
    upper <- mean_val + 3 * sd_val

    # Найдём выбросы
    outliers_3sigma <- data$Monthly.beer.production[data$Monthly.beer.production < lower |
                                                    data$Monthly.beer.production > upper]

    cat("Среднее =", mean_val, "\nСтандартное отклонение =", sd_val, "\n")
    cat("Нижняя граница =", lower, "\nВерхняя граница =", upper, "\n")
    cat("Количество выбросов (3σ):", length(outliers_3sigma), "\n")
    print(outliers_3sigma)
    return(outliers_3sigma)
}


iqr_out <- function(data){
    # ---- Поиск выбросов методом IQR ----
    Q1 <- quantile(data$Monthly.beer.production, 0.25, na.rm = TRUE)
    Q3 <- quantile(data$Monthly.beer.production, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1

    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR

    outliers <- data$Monthly.beer.production[data$Monthly.beer.production < lower | 
                                            data$Monthly.beer.production > upper]

    cat("Q1 =", Q1, "\nQ3 =", Q3, "\nIQR =", IQR, "\n")
    cat("Нижняя граница =", lower, "\nВерхняя граница =", upper, "\n")
    cat("Найдено выбросов:", length(outliers), "\n")
    print(outliers)
    return(outliers)

}





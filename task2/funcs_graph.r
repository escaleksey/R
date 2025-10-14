library(ggplot2)



dist_graph <- function(data){ 
    ggplot(data, aes(x = `Monthly.beer.production`)) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "black", alpha = 0.6) + # nolint
    stat_function(fun = dnorm, 
                args = list(mean = mean(data$Monthly.beer.production, na.rm = TRUE), 
                            sd = sd(data$Monthly.beer.production, na.rm = TRUE)), 
                color = "red", size = 1) +
    labs(title = "Гистограмма производства пива с кривой нормального распределения",
        x = "Производство", y = "Плотность") +
    theme_minimal()

    ggsave("Диаграмма распределения.png", plot = gr1, width = 6, height = 4, dpi = 300)
}

box_plot_f <- function(data, name){
    boxplot <- ggplot(data, aes(y = Monthly.beer.production)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", outlier.colour = "red", outlier.shape = 8) +
    labs(
        title = "Boxplot: производство пива",
        y = "Monthly beer production"
    ) +
    theme_minimal()

    ggsave(name, plot = boxplot, width = 6, height = 4, dpi = 300) 

}


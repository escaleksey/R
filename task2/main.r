
#getwd()
#setwd("D:/My Code/R/R/task2")
library(dplyr)
data <- read.csv("monthly-beer-production-in-austr.csv")


source("funcs_graph.r")
source("funcs_outliers.r")

y <- iqr_out(data)


data <- data %>%
  filter(!Monthly.beer.production %in% y)

#box_plot_f(data, "После удаления.png")

data_thinned_na <- data
data_thinned_na$Monthly.beer.production[seq(2, nrow(data), by = 2)] <- NA

source("funcs_filling.r")

#means(data, data_thinned_na)
#median_f(data, data_thinned_na)

#fill_previous_mean(data, data_thinned_na)
#fill_lagrange_quarters(data, data_thinned_na)
#fill_spline_quarters(data, data_thinned_na)

compare_all_methods(data, data_thinned_na)





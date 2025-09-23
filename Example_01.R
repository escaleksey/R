
##########################
### Доп графики

library(dplyr)
library(rvest)
library(tidyverse)
library(ggplot2)
library(readxl)

library("readxl")
db_reg <- read_excel("TGV/db_reg.xlsx", sheet="Sheet1")

ggplot(db_reg, aes(x = anyshare, y = anygrowth)) +
  geom_point(color = "black", size = 3) +
  
  geom_point(data = filter(db_reg, reg == "Россия"), color = "black", size = 4) +
  geom_text(data = filter(db_reg, reg == "Россия"), aes(label = "Россия"), vjust = -1, size=6, color = "black") +
  
  geom_point(data = filter(db_reg, reg == "Пермский край"), color = "dark red", size = 4) +
  geom_text(data = filter(db_reg, reg == "Пермский край"), aes(label = "Пермский край"), vjust = 1.5, size=6, color = "dark red") +
  
  geom_point(data = filter(db_reg, reg == "Свердловская область"), color = "blue", size = 4) +
  geom_text(data = filter(db_reg, reg == "Свердловская область"), aes(label = "Свердловская область"), hjust = 1.0, vjust = 1.5, size=6, color = "blue") +
  
  geom_point(data = filter(db_reg, reg == "Башкортостан (Республика)"), color = "dark green", size = 4) +
  geom_text(data = filter(db_reg, reg == "Башкортостан (Республика)"), aes(label = "Башкортостан"), vjust = -1, size=6, color = "dark green") +
  
  geom_hline(yintercept = db_reg$anygrowth[db_reg$reg == "Россия"], linetype = "dashed") +
  geom_vline(xintercept = db_reg$anyshare[db_reg$reg == "Россия"], linetype = "dashed") +
  
  xlim(0, 40) + # Установите границы по оси X
  ylim(-4, 8) + # Установите границы по оси Y
  
  labs(title = "Общий уровень поддержки МСП в регионах за 2019-2023 гг.", x = "Средняя доля МСП, получивших поддержку любого типа, %", y = "Среднегодовой темп прироста доли поддержанных МСП, %") +
  theme(plot.title = element_text(size = 18, hjust = 0.5), # Larger title, centered
        axis.title.x = element_text(size = 15, vjust = -1), # Larger x-axis title
        axis.title.y = element_text(size = 15, angle = 90), # Larger y-axis title
        axis.text.x = element_text(size = 15), # Smaller x-axis labels
        axis.text.y = element_text(size = 15, vjust = 0, hjust=-1))  # Smaller y-axis labels

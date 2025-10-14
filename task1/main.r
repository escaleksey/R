library(ggplot2)
library(dplyr)
library(patchwork)
library(fmsb)
install.packages("ggradar")
library(ggplotify)

data <- read.csv("youtube_shorts_tiktok_trends_2025.csv")

table(data$platform)
#  Круговая диаграмма просмотров по платформам 
platform_views <- aggregate(views ~ platform, data, sum)
platform_views$percent <- round(100 * platform_views$views / sum(platform_views$views), 1)
platform_views$label <- paste0(platform_views$platform, " (", platform_views$percent, "%)")

graph1 <- ggplot(platform_views, aes(x = "", y = views, fill = platform)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Процентное соотношение просмотров по платформам") +
  theme_void() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4)

# Линейная диаграмма просмотров по месяцам 
data$month_number <- ceiling(data$week_of_year / 4.3)

monthly_platform_views <- data %>%
  group_by(month_number, platform) %>%
  summarise(total_views = sum(views, na.rm = TRUE)/1e6) %>%
  ungroup()

graph2 <- ggplot(monthly_platform_views, aes(x = month_number, y = total_views, color = platform, group = platform)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:9) +
  labs(
    title = "Динамика просмотров по платформам по месяцам (приблизительно)",
    x = "Месяц (приблизительно)",
    y = "Просмотры, млн.",
    color = "Платформа"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"))

# ---- График 3: Просмотры по жанрам TikTok ----
genre_views <- data %>%
  filter(platform == "TikTok") %>%
  group_by(genre) %>%
  summarise(total_views = sum(views, na.rm = TRUE)/100) %>%
  arrange(desc(total_views))

graph3 <- ggplot(genre_views, aes(x = reorder(genre, -total_views / 100), y = total_views/ 100, fill = genre)) +
  geom_col() +
  coord_cartesian(ylim = c(0, max(genre_views$total_views/ 100) * 1.2)) +  # немного выше максимума
  labs(
    title = "Просмотры по жанрам видео в TikTok",
    x = "Жанр",
    y = "Просмотры, тыс."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ---- График 4: Количество видео по жанрам TikTok ----
genre_count <- data %>%
  filter(platform == "TikTok") %>%
  group_by(genre) %>%
  summarise(video_count = n()) %>%
  arrange(desc(video_count))

graph4 <- ggplot(genre_count, aes(x = reorder(genre, -video_count), y = video_count, fill = genre)) +
  geom_col() +
  labs(
    title = "Количество видео по жанрам в TikTok",
    x = "Жанр",
    y = "Количество видео"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed")
  )



tiktok_data <- data %>% filter(platform == "TikTok")

Q1 <- quantile(tiktok_data$views, 0.25, na.rm = TRUE)
Q3 <- quantile(tiktok_data$views, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

tiktok_data_clean <- tiktok_data %>%
  filter(views >= lower & views <= upper)

graph5 <- ggplot(data %>% filter(platform == "TikTok"),
                 aes(x = genre, y = views/1000, fill = genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Распределение просмотров по жанрам в TikTok",
    x = "Жанр",
    y = "Просмотры, тыс."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


graph6 <- ggplot(tiktok_data_clean, aes(x = genre, y = views/1000, fill = genre)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(
    title = "Распределение просмотров по жанрам в TikTok (без выбросов)",
    x = "Жанр",
    y = "Просмотры, тыс."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Лепестковая


# ---- Средние показатели по платформам ----
platform_summary <- data %>%
  filter(platform %in% c("TikTok", "YouTube")) %>%
  group_by(platform) %>%
  summarise(
    avg_views = mean(views, na.rm = TRUE),
    avg_likes = mean(likes, na.rm = TRUE),
    avg_comments = mean(comments, na.rm = TRUE),
    avg_shares = mean(shares, na.rm = TRUE)
  ) %>%
  ungroup()

# ---- Подготовка данных для fmsb::radarchart ----
max_vals <- apply(platform_summary[,-1], 2, max)
min_vals <- apply(platform_summary[,-1], 2, min)
radar_plot_data <- rbind(max_vals, min_vals, platform_summary[,-1])
rownames(radar_plot_data) <- c("Max", "Min", platform_summary$platform)

# ---- Строим радарный график ----
fmsb::radarchart(
  radar_plot_data,
  pcol = c("red", "blue"),
  pfcol = scales::alpha(c("red","blue"), 0.3),
  plwd = 2,
  cglcol = "grey", cglty = 1, cglwd = 0.8,
  axistype = 1,
  vlcex = 1,
  title = "Сравнение средних показателей: TikTok и YouTube"
)
legend(
  "topright",
  legend = platform_summary$platform,
  col = c("red","blue"),
  lty = 1,
  bty = "n",
  cex = 0.2
)
# ---- Вывод всех графиков сразу ----
#((graph1 | graph2) / (graph3 | graph4)) / graph5 / graph6
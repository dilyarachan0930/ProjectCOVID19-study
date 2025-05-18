# Загрузка библиотек
library(tidyverse)
library(lubridate)
library(readr)
library(patchwork)
library(ggcorrplot)

# Датасет COVID-19
data <- read_csv("papers.csv")
# Проверка вида датасета
head(data)

# Преобразуем дату публикации в формат Date
data$published_at <- as.Date(data$published_at)

# Извлекаем год
data$year <- year(data$published_at)

# Проверка
summary(data$year)
summary(data$citation_count)

# Создание переменной successful
median_citation <- median(data$citation_count, na.rm = TRUE)
data$successful <- ifelse(data$citation_count > median_citation, 1, 0)

# Создание переменной collaboration_level
data$collaboration_level <- str_count(data$authors, ",") + 1  # +1, так как авторов больше на 1, чем количество запятых в списке
summary(data$successful)
summary(data$collaboration_level)

# Видно, что максимальное количество авторов 1065, это либо выброс, либо большие коллаборации. Сократим до 75 авторов максимум.
rows_to_remove <- data[!is.na(data$collaboration_level) & data$collaboration_level > 75, ]

# Выведем строки которые хотим удалить для проверки
print(rows_to_remove)

# Фильтруем данные, удаляя строки с collaboration_level > 75
data_clean <- data[!is.na(data$collaboration_level) & data$collaboration_level <= 75, ]

# Удаляем строки с NA в столбце collaboration_level
data_clean <- data_clean[!is.na(data_clean$collaboration_level), ]

# Проверим количество удалённых строк
cat("Удалено строк:", nrow(rows_to_remove), "\n")

# Проверим summary после удаления NA
summary(data_clean$collaboration_level)

# Для постройки гистограммы извлекаем месяц и год, т.к. все публикации 2020 и 2021 года.
data_clean <- data_clean %>%
  arrange(published_at) %>%
  mutate(month_year = format(published_at, "%b %Y"), # %b - месяц, %Y – год
         month_year = factor(month_year, levels = unique(format(published_at, "%b %Y")))) # месяцы в хронологическом порядке

# Гистограмма количества публикаций по месяцам и годам
ggplot(data_clean, aes(x = month_year)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Распределение публикаций по месяцам и годам", x = "Месяц и год", y = "Количество публикаций") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Повернем подписи оси X для удобства


# Гистограмма по количеству авторов с лимитом в 75.
ggplot(data_clean, aes(x = collaboration_level)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Распределение количества авторов", x = "Число авторов", y = "Количество публикаций") +
  theme_minimal()

# Доля успешных публикаций
mean(data_clean$successful, na.rm = TRUE)

# Средняя и медианная цитируемость по стране публикации, т.к. частично авторы заполнены неверно, а переменных отдела нет в датасете, было решено сделать такие гистограммы и сравнить их
# Среднее
p1 <- data_clean %>%
  group_by(country) %>%
  summarise(
    mean_citations = mean(citation_count, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count >= 5) %>%
  arrange(desc(mean_citations)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(country, mean_citations), y = mean_citations)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Средняя цитируемость по странам (топ-15)",
       x = "Страна", y = "Среднее число цитирований") +
  theme_minimal()

# Медиана
p2 <- data_clean %>%
  group_by(country) %>%
  summarise(
    median_citations = median(citation_count, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count >= 5) %>%
  arrange(desc(median_citations)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(country, median_citations), y = median_citations)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Медианная цитируемость по странам (топ-15)",
       x = "Страна", y = "Медианное число цитирований") +
  theme_minimal()
# Сравнение
p1 + p2


# Дополнительное задание (портрет успешного автора). Сперва найдем когда были сделаны наиболее успешные публикации.

# График распределения успешных публикаций по времени
ggplot(data_clean, aes(x = month_year, fill = as.factor(successful))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("gray70", "darkgreen"), labels = c("Неуспешные", "Успешные")) +
  labs(title = "Доля успешных публикаций по месяцам",
       x = "Месяц и год", y = "Доля публикаций", fill = "Успешность") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Сверив с графиком количества публикаций, самым успешным месяцем можно считать декабрь 2020 и январь 2021 года.

# Отбираем данные за декабрь 2020 и январь 2021
jan2021_data <- data_clean %>% filter(month_year == "янв 2021")
dec2020_data <- data_clean %>% filter(month_year == "дек 2020")
# Успешные публикации за декабрь 2020 и январь 2021
jan2021_successful <- jan2021_data %>% filter(successful == 1)
dec2020_successful <- dec2020_data %>% filter(successful == 1)
# Объединяем данные за декабрь 2020 и январь 2021
winter_data <- bind_rows(dec2020_data, jan2021_data)

# График распределения числа авторов в публикациях за декабрь 2020 и январь 2021(далее закоментировано, долгое выполнение, но график есть в отчете)

# ggplot(winter_data, aes(x = factor(successful, labels = c("Неуспешные", "Успешные")),
#                         y = collaboration_level)) +
#   geom_jitter(width = 0.2,
#               alpha = 0.6,
#               color = c("red", "green")[winter_data$successful + 1],
#               size = 2) +
#   labs(title = "Распределение числа авторов в публикациях (дек 2020 и янв 2021)",
#        x = "Тип публикации", y = "Количество авторов") +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

# Т.к. всего статей в декабре 29436 и в январе 23664, то при построение графика по странам добавим фильтр в минимум 100 публикаций.

# Считаем общее число публикаций по странам
country_totals <- winter_data %>%
  group_by(country) %>%
  summarise(total_count = n(), .groups = "drop") %>%
  filter(total_count >= 100)

# Оставляем только страны с минимумом публикаций
country_success_counts <- winter_data %>%
  filter(country %in% country_totals$country) %>%
  group_by(country, successful) %>%
  summarise(count = n(), .groups = "drop")

# Преобразуем successful в фактор с понятными метками (1-успешные, 0-неуспешные)
country_success_counts$successful <- factor(country_success_counts$successful, levels = c(0,1), labels = c("Неуспешные", "Успешные"))

# Строим столбчатую диаграмму
ggplot(country_success_counts, aes(x = reorder(country, -count), y = count, fill = successful)) +
  geom_col(position = "dodge") +
  labs(title = "Количество успешных и неуспешных публикаций по странам (декабрь 2020/январь 2021, минимум 100 публикаций)",
       x = "Страна", y = "Количество публикаций", fill = "Тип публикации") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Создание корреляционной матрицы для проверки связи между показателями
# Преобразуем month_year в числовой фактор
data_clean <- data_clean %>%
  mutate(month_year_num = as.numeric(factor(month_year, levels = unique(month_year))))

# Выбираем нужные столбцы и убираем NA
df_corr <- data_clean %>%
  select(collaboration_level, citation_count, successful, month_year_num) %>%
  na.omit()

# Считаем корреляционную матрицу
cor_matrix <- cor(df_corr)

# Строим график корреляций
ggcorrplot(cor_matrix, 
           method = "circle",
           lab = TRUE,
           title = "Корреляционная матрица: авторы, цитаты, успех, месяц")



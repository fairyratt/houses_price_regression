# Установка и загрузка необходимых пакетов
install.packages("readr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("car")
install.packages("caret")
install.packages("corrplot")
install.packages("GGally")
install.packages("fastDummies")

library(readr)
library(tidyr)
library(ggplot2)
library(car)
library(caret)
library(corrplot)
library(GGally)
library(fastDummies)

# Чтение данных из CSV файла
file_path <- "C:/Users/natal/OneDrive/Desktop/курсовая2.csv"
data <- read_csv(file_path)

# Просмотр первых строк данных
head(data)

# Проверка нормальности числовых переменных
numeric_columns <- sapply(data, is.numeric)
shapiro_results <- lapply(data[, numeric_columns], shapiro.test)
print(shapiro_results)


log_transformed_data <- data
log_transform <- function(x) {
  if (min(x) <= 0) {
    x <- x - min(x) + 1
  }
  log(x)
}

# Применение логарифмической трансформации к числовым столбцам
log_transformed_data[, numeric_columns] <- lapply(data[, numeric_columns], log_transform)

# Проверка нормальности числовых переменных после логарифмической трансформации
shapiro_results_log <- lapply(log_transformed_data[, numeric_columns], shapiro.test)
print(shapiro_results_log)


# Корреляционный анализ

# Рассчитываем корреляцию Спирмена
correlation_matrix_spearman <- cor(data[, numeric_columns], method = "spearman")
print(correlation_matrix_spearman)

# Визуализация корреляционной матрицы с помощью тепловой карты
corr <- as.data.frame(correlation_matrix_spearman)

ggcorr(corr,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")


# Преобразование переменной statezip в фиктивные переменные
dummy_vars <- dummy_cols(data, select_columns = "statezip", remove_first_dummy = TRUE)


# Расчет корреляционной матрицы для всех числовых переменных, включая фиктивные
correlation_matrix <- cor(dummy_vars[, sapply(dummy_vars, is.numeric)], use = "complete.obs")

# Извлечение корреляций для price_rub и фиктивных переменных statezip
cor_price_statezip <- correlation_matrix["price_rub", grep("statezip_", colnames(correlation_matrix))]

# Вывод корреляций
print(cor_price_statezip)


top_20_statezip <- unique(data$statezip)[1:20]

# Фильтрация данных по первым 20 районам
filtered_data <- data[data$statezip %in% top_20_statezip, ]

# Построение диаграммы для первых 20 регионов
ggplot(filtered_data, aes(x = statezip, y = price_rub)) +
  geom_boxplot() +
  labs(title = "Распределение цен по первым 20 категориям statezip",
       x = "statezip",
       y = "price_rub") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#Регрессия

# Построение обобщенной линейной модели (GLM) без учета statezip
glm_model_without_statezip <- glm(price_rub ~ sqrtmetres_living + sqrtmetres_lot + bedrooms + bathrooms + floors + view + condition + yr_built + sqrtmetres_above + sqrtmetres_basement, 
                                  data = data, 
                                  family = gaussian())

# Оценка модели без учета statezip
summary(glm_model_without_statezip)
coefficients(glm_model_without_statezip)


# Построение обобщенной линейной модели (GLM) с учетом statezip
glm_model_with_statezip <- glm(price_rub ~ statezip + sqrtmetres_living + sqrtmetres_lot + bedrooms + bathrooms + floors + view + condition + yr_built + sqrtmetres_above + sqrtmetres_basement, 
                               data = data, 
                               family = gaussian())

# Оценка модели с учетом statezip
summary(glm_model_with_statezip)
coefficients(glm_model_with_statezip)


# Визуализация результатов модели без учета statezip
predictions_without_statezip <- predict(glm_model_without_statezip, type = "response")
scatter_plot_predicted_without_statezip <- ggplot(data, aes(x = price_rub, y = predictions_without_statezip)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # добавляем линию идеального соответствия
  labs(x = "Реальная цена (руб.)", y = "Прогнозируемая цена (руб.)") +
  ggtitle("Соответствие реальных и прогнозируемых цен (без учета местоположения)") 
print(scatter_plot_predicted_without_statezip)


# Визуализация результатов модели с учетом statezip
predictions_with_statezip <- predict(glm_model_with_statezip, type = "response")
scatter_plot_predicted_with_statezip <- ggplot(data, aes(x = price_rub, y = predictions_with_statezip)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # добавляем линию идеального соответствия
  labs(x = "Реальная цена (руб.)", y = "Прогнозируемая цена (руб.)") +
  ggtitle("Соответствие реальных и прогнозируемых цен (с учетом местоположения)") 
print(scatter_plot_predicted_with_statezip)

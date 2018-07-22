# Загрузка пакетов
library(dplyr)
library(woeBinning)

# Загрузка данных
dataset <- readRDS(file.path("data", "dataset.rds"))

# Выбираем столбцы для биннинга
temp_data <- dataset %>%
  select(matches("_(count|ratio|sum|min|max|mean|month_payment)$"), applicant_income, application_sum_term, application_sum_cred, age_at_application_date, target) %>%
  as.data.frame()

# Сохраняем названия столбцов
var_names <- names(temp_data)[-ncol(temp_data)]

# Проводим автоматический биннинг
binning <- woe.binning(temp_data, "target", var_names, min.perc.total = 0.1, stop.limit = 0.1)

# Извлекаем категоризованные данные
binned_data <- woe.binning.deploy(temp_data, binning, min.iv.total = 0.01) %>%
  # Оставляем только категоризованные переменные
  select(ends_with(".binned")) %>%
  # Преобразуем в объект `tbl_df`
  as_data_frame() %>%
  # Убираем суффикс '.binned'
  rename_all(~ gsub(".binned", "", .))

# Заменяем переменные на категоризованные
dataset <- dataset %>%
  select(-one_of(var_names)) %>%
  bind_cols(binned_data)

dataset %>%
  saveRDS(file.path("data", "binned.rds"))

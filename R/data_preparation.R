# Загрузка пакетов
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(stringr)
library(forcats)
library(caret)

# Настроки импорта
loc <- locale(
  date_format = "%d.%m.%Y",
  time_format = "%H:%M",
  encoding = "cp1251"
)

# Загрузка данных
apps <- file.path("data", "ankets_for_tests.zip") %>%
  read_tsv(locale = loc, na = c("", "!"))
bki <- file.path("data", "bki_data_for_tests.zip") %>%
  read_tsv(locale = loc, na = c("", "!"))

# Препроцессинг анкетных данных
apps <- apps %>%
  # Переименовываем переменную `npl_90_at_12MB+` в `target`
  rename(target = `npl_90_at_12MB+`) %>%
  # Имена столбцов в нижний регистр
  rename_all(tolower) %>%
  # Извлекаем числа из срока кредита
  mutate(application_sum_term = parse_number(application_sum_term)) %>%
  # Рассчитываем ежемесячный платёж по кредиту (буз учёта процентной ставки)
  mutate(application_month_payment = application_sum_cred / application_sum_term) %>%
  # Рассчитываем долговоую нагрузку кредита
  mutate(application_debt_ratio = applicant_income / application_month_payment) %>%
  # Преобразуем в нижний регистр
  mutate(applicant_region = str_to_lower(applicant_region)) %>%
  # Объединяем категории с количеством наблюдений < 1% в категорию «Другой»
  mutate(applicant_region = fct_lump(applicant_region, prop = 0.01, other_level = "Другой")) %>%
  # Убираем низкочатотные варианты
  mutate(applicant_kind_of_living = recode(applicant_kind_of_living,
                                           "Не выбрано" = "Иное")) %>%
  # Убираем низкочатотные варианты
  mutate(applicant_info_about_work_type = recode(applicant_info_about_work_type,
                                                 "Постоянный контракт" = "Официально")) %>%
  # Убираем низкочатотные варианты
  mutate(applicant_type_of_position = recode(applicant_type_of_position,
                                             "Воспитатель" = "Не выбрано",
                                             "плотник" = "Не выбрано")) %>%
  # Заменяем NA на значение "Не указано"
  mutate(applicant_doc_income = coalesce(applicant_doc_income, "Отсутствует")) %>%
  mutate(applicant_doc_income = fct_lump(applicant_doc_income, prop = 0.01, other_level = "Другой")) %>%
  mutate(applicant_work_occupation = fct_lump(applicant_work_occupation, prop = 0.01, other_level = "Иное")) %>%
  mutate(application_sales_chanel = fct_lump(application_sales_chanel, prop = 0.01, other_level = "Иное")) %>%
  mutate(applicant_work_kind_of_activity = recode(applicant_work_kind_of_activity,
                                                  "Не выбрано" = "Иное")) %>%
  mutate(applicant_work_kind_of_activity = fct_lump(applicant_work_kind_of_activity, prop = 0.01, other_level = "Иное")) %>%
  # Преобразуем логические переменные в 0, 1
  mutate_at(vars(applicant_perecreditaciya, starts_with("assets_"), target),
            funs(as.integer(as.logical(.)))) %>%
  # Преобразуеем строковые переменные в факторы
  mutate_if(is.character, as.factor)

# Препроцессинг данных bki
bki <- bki %>%
  # Переименовываем столбцы в нижний регистр
  rename_all(tolower) %>%
  # Парсим дату
  mutate(date_update = as_date(dmy_hm(date_update))) %>%
  # Преобразуем finance_type в целое число
  mutate(finance_type = parse_integer(finance_type)) %>%
  # Заменяем значение "2099-12-31" в датах на NA
  mutate_if(is.Date, funs(replace(., which(. == as.Date("2099-12-31")), NA))) %>%
  # Является ли запись кредитом или займом
  mutate(is_credit = finance_type %in% c(1, 2, 3, 4, 5))

# репроцессинг данных по кредитам
credits <- bki %>%
  # Кредиты и займы
  filter(is_credit) %>%
  # Переименовываем `sum_loan` в `loan_amount`
  rename(loan_amount = sum_loan) %>%
  # Срок займа в месяцах
  mutate(loan_term = interval(date_open, date_close_plane) %/% period(1L, "month")) %>%
  # Считаем минимальный срок займа равный 1 месяцу
  mutate(loan_term = if_else(loan_term == 0, 1, loan_term)) %>%
  # Статус займа (открыт / закрыт)
  mutate(status = if_else(!is.na(date_close) & account_payment_status_str %in% c("Своевременно", "Списан"), "Закрыт", "Открыт")) %>%
  # Расчёт суммы займа с учётом процентной ставки (без учёта перерасчёта при погашении)
  mutate(total_loan_amount = (prc / 100) * (loan_term / 12)) %>%
  mutate(total_loan_amount = total_loan_amount * loan_amount + loan_amount) %>%
  # Исключаем записи, где сумма займа равна 0
  filter(loan_amount != 0)

#' @title Функция для агрегации данных по кредитам
#' @param data Объект типа `data.frame`. Таблица данных.
#' @param prefix Префикс, который будет добавлен к названиями столбцов.
#' @return Таблица агрегированных данных.
credits_summary <- function(data, prefix) {
  # Общая статистика по займам
  loans <- data %>%
    group_by(id_anket) %>%
    summarise(
      # Сумма займов
      loans_amount_sum = sum(loan_amount),
      loans_amount_mean = round(mean(loan_amount)),
      loans_amount_min = min(loan_amount),
      loans_amount_max = max(loan_amount),
      # Количество займов
      loans_count = length(id_anket),
      # Срок займа
      loans_term_sum = sum(loan_term, na.rm = TRUE),
      loans_term_mean = round(mean(loan_term, na.rm = TRUE)),
      loans_term_max = max(loan_term, na.rm = TRUE),
      loans_term_min = min(loan_term, na.rm = TRUE),
      # Сумма непросроченной задолженности
      debt_not_overvude_amount_sum = sum(saldo),
      debt_not_overvude_amount_min = min(saldo),
      debt_not_overvude_amount_max = max(saldo),
      # Сумма просроченной задолженности
      debt_overdue_amount_sum = sum(saldo_arrears),
      debt_overdue_amount_min = min(saldo_arrears),
      debt_overdue_amount_max = max(saldo_arrears)
    ) %>%
    ungroup()
  
  loans <- loans %>%
    # Округляем все суммы
    mutate_at(vars(contains("_amount_")), ~ round(.))
  
  # Количество займов с разными статусами
  loan_status_count <- data %>%
    count(id_anket, status = current_payment_status_agregated) %>%
    mutate(status = str_pad(status, 2, pad = "0")) %>%
    mutate(status = paste("payment_status", status, "count", sep = "_")) %>%
    spread(status, n, fill = 0L)
  
  # Добавляем флаги
  loan_status_flag <- loan_status_count %>%
    transmute_at(vars(ends_with("_count")), ~ as.integer(. > 0L)) %>%
    rename_all(~ str_replace(., "_count", "_flag"))
  
  loan_status <- bind_cols(loan_status_count, loan_status_flag)
  
  # Количество займов разного типа
  loan_type_count <- data %>%
    count(id_anket, type = finance_type) %>%
    mutate(type = str_pad(type, 2, pad = "0")) %>%
    mutate(type = paste("loan_type", type, "count", sep = "_")) %>%
    spread(type, n, fill = 0L)
  
  # Добавляем флаги
  loan_type_flag <- loan_type_count %>%
    transmute_at(vars(ends_with("_count")), ~ as.integer(. > 0L)) %>%
    rename_all(~ str_replace(., "_count", "_flag"))
  
  loan_type <- bind_cols(loan_type_count, loan_type_flag)
  
  # Собираем данные вместе
  loans <- loans %>%
    left_join(loan_status, by = "id_anket") %>%
    left_join(loan_type, by = "id_anket")
  
  # Добавляем префикс к названиям столбцов
  if (!missing(prefix)) {
    loans <- loans %>%
      rename_at(vars(-id_anket), ~ paste(prefix, ., sep = "_"))
  }
  
  return(loans)
}

# Агрегация данных по закрытым кредитам
loans_closed <- credits %>%
  filter(status == "Закрыт") %>%
  credits_summary("closed")

# Агрегация данных по текущим кредитам
loans_opened <- credits %>%
  filter(status == "Открыт") %>%
  credits_summary("opened") %>%
  # Рассчитываем приблизительную сумму ежемесячного платежа по открытым займам
  mutate(opened_loans_month_payment = round(opened_loans_amount_sum / opened_loans_term_sum))

credits <- left_join(loans_closed, loans_opened, by = "id_anket")
credits <- credits %>%
  mutate_all(function(x) replace(x, is.na(x), 0))

# Агрегируем данным по кредитным картам и счетам
cards <- bki %>%
  # Запись не является кредитом
  filter(!is_credit) %>%
  # Счёт не закрыт
  filter(is.na(date_close)) %>%
  group_by(id_anket) %>%
  summarise(overdraft_card_flag = as.integer(any(finance_type == 26)),
            credit_card_flag = as.integer(any(finance_type == 23)))

# Собираем все данные в один датасет
dataset <- apps %>%
  left_join(credits, by = "id_anket") %>%
  left_join(cards, by = "id_anket")

dataset <- dataset %>%
  # Удаляем константные столбцы
  select_if(~ length(unique(.)) > 1L) %>%
  # Заменяем пропущенные значения на 0
  mutate_at(vars(matches("_(count|flag|mean|min|max|amount|sum|month_payment)$")),
            function(x) replace(x, is.na(x), 0))

# Удаляем переменные с околонулевой дисперсией
to_drop <- dataset %>%
  select(-id_anket) %>%
  nearZeroVar(names = TRUE, freqCut = 100 / 1)
dataset <- dataset %>%
  select(-one_of(to_drop))

# Удаляем коррелирующие переменные
to_drop <- dataset %>%
  select(-id_anket) %>%
  select_if(is.numeric) %>%
  cor() %>%
  findCorrelation(cutoff = 0.95, names = TRUE)
dataset <- dataset %>%
  select(-one_of(to_drop))

dataset %>%
  saveRDS(file.path("data", "dataset.rds"))

# Загрузка пакетов
library(dplyr)
library(caret)
library(MLmetrics)
library(fbroc)

# Загрузка данных
dataset <- readRDS(file.path("data", "dataset.rds")) %>%
  select(-id_anket)

target_lab <- dataset$target

# One Hot Encoded data
dummy <- dummyVars(target ~ ., dataset)
dataset <- as.data.frame(predict(dummy, dataset))

# Генерируем индексы для разбиения данных на тестовую и обучающую выборки
ind <- sample(seq_len(nrow(dataset)), nrow(dataset) * 0.7)

# Разбиваем исходные даннные на тестовую и оббучающую выборки
train_lab <- target_lab[ind]
train_data <- dataset[ind, ]
test_lab <- target_lab[-ind]
test_data <- dataset[-ind, ]

glm_fit <- glm(train_lab ~ ., train_data, family = binomial())

glm_pred <- predict.glm(glm_fit, test_data, type = "response")
# Оценка модели
AUC(glm_pred, test_lab)
Gini(glm_pred, test_lab)
KS_Stat(glm_pred, test_lab)

roc <- boot.roc(glm_pred, test_lab == 1)
print(roc)
plot(roc)
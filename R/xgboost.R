# Загрузка пакетов
library(dplyr)
library(ROSE)
library(Matrix)
library(xgboost)
library(MLmetrics)
library(fbroc)

# Загрузка данных
dataset <- readRDS(file.path("data", "dataset.rds")) %>%
  select(-id_anket)

# Генерируем индексы для разбиения данных на тестовую и обучающую выборки
ind <- sample(seq_len(nrow(dataset)), nrow(dataset) * 0.7)

# Разбиваем исходные даннные на тестовую и обучающую выборки
train_data <- dataset[ind, ]
test_data <- dataset[-ind, ]

# Балансируем обучающую выборки по зависимой переменной
balanced_train_data <- ovun.sample(target ~ ., train_data)$data
balanced_train_data <- na.omit(balanced_train_data)

train_lab <- balanced_train_data$target
train_mat <- sparse.model.matrix(target ~ . -1, balanced_train_data)
test_lab <- test_data$target
test_mat <- sparse.model.matrix(target ~ . -1, test_data)

# Преобразуем данные
train_mat <- xgb.DMatrix(data = train_mat, label = train_lab)
test_mat <- xgb.DMatrix(data = test_mat, label = test_lab)

# Сохранение данных
xgb.DMatrix.save(train_mat, "data/xgb_train.data")
xgb.DMatrix.save(test_mat, "data/xgb_test.data")
watchlist <- list(train = train_mat, test = test_mat)

# Параметры модели (нужно настраивать)
params <- list(max_depth = 5,
               eta = 0.03,
               gamma = 1,
               colsample_bytree = 0.5,
               min_child_weight = 1,
               subsample = 0.5,
               objective = "binary:logistic",
               eval_metric = "auc")

# Обучение модели
bst <- xgb.train(data = train_mat,
                 params = params,
                 nrounds = 200,
                 watchlist = watchlist,
                 print_every_n = 1)

# Сохраняем модель
xgb.save(bst, "data/xgboost.model")

# Предсказвыем тестовые данные
xgb_pred <- predict(bst, newdata = test_mat)

# Оценка модели
AUC(xgb_pred, test_lab == 1)
Gini(xgb_pred, test_lab == 1)
KS_Stat(xgb_pred, test_lab == 1)

roc <- boot.roc(xgb_pred, getinfo(test_mat, "label") == 1)
print(roc)
plot(roc)
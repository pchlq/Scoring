# Загрузка пакетов
library(dplyr)
library(rpart)
library(partykit)
library(MLmetrics)

# Загрузка данных
dataset <- readRDS(file.path("data", "dataset.rds")) %>%
  select(-id_anket)

target_lab <- dataset$target

ctree_fit <- ctree(
  formula = as.factor(target) ~ .,
  data = dataset,
  control = ctree_control(
    mincriterion = 0.9,
    minbucket = 100,
    maxdepth = 3L,
    cores = parallel::detectCores(logical = FALSE)
  )
)
ctree_pred <- predict(ctree_fit, dataset, type = "response")
AUC(ctree_pred, target_lab)
plot(ctree_fit)

glmtreee_fit <- glmtree(
  formula = as.factor(target) ~ .,
  data = dataset,
  family = binomial,
  minsize = 100,
  prune = "AIC",
  cores = parallel::detectCores(logical = FALSE)
)

glmtreee_pred <- predict(glmtreee_fit, dataset, type = "response")
AUC(glmtreee_pred, target_lab)
plot(glmtreee_fit)

rpart_fit <- rpart(
  formula = as.factor(target) ~ .,
  data = dataset,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    maxdepth = 3L
  ))
plot(as.party(rpart_fit))

dataset %>%
  mutate(rule = as.integer(applicant_perecreditaciya == 1 & application_sum_cred >= 90000)) %>%
  # mutate(rule = 1) %>%
  group_by(rule) %>%
  summarise(count = n(),
            bad_Rate = sum(target) / n() * 100)

rpart_pred <- predict(rpart_fit, dataset, type = "prob")[, "1"]
AUC(rpart_pred, target_lab)

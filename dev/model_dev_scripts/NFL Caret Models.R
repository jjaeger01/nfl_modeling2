library(caret)
library(doParallel)
set.seed(825)

modeldata <- create_modeldata("result")
# model_formula <- build_formula("result")

indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
train <- modeldata %>% na.omit() %>% dplyr::slice(indices)
test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)


cl <- makePSOCKcluster(10)
registerDoParallel(cl)

# # SVM Linear ####
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 5,
#   ## repeated ten times
#   repeats = 5)
#
# # Fit model
# svmLinear3_fit <- train(result ~ ., data = train[-1],
#                 method = "svmLinear3",
#                 trControl = fitControl,
# )
# svmLinear3_fit

# # NEURAL NET####
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
#
# # Fit model
# nn_fit <- train(result ~ ., data = train[-1],
#                         method = "neuralnet",
#                         trControl = fitControl,
# )
# nn_fit

# # RANDOM FOREST ####
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# # Fit model
# rf_fit <- train(home_score ~ ., data = train[-1],
#                  method = "rf",
#                  trControl = fitControl,
# )
# rf_fit

# BOOSTED LM ####
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# # Fit model
# BstLm_fit <- train(result ~ ., data = train[-1],
#                 method = "BstLm",
#                 trControl = fitControl,
# )
# BstLm_fit

# GLMNET ####
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# # Fit model
# glmnet_fit <- train(result ~ ., data = train[-1],
#                    method = "glmnet",
#                    trControl = fitControl,
# )
# glmnet_fit

# RVMLINEAR ####
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
# Fit model
rvmLinear_fit <- train(result ~ ., data = train[-1],
                    method = "rvmLinear",
                    trControl = fitControl,
)
rvmLinear_fit


# penalized ####
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# # Fit model
# penalized_fit <- train(result ~ ., data = train[-1],
#                     method = "penalized",
#                     trControl = fitControl,
# )
# penalized_fit

# Outcomes ####
outcomes %>%
  filter(game_id %in% test$game_id) %>%
  mutate(
         # pred_rf = predict(rf_fit , test) ,
         # pred_gbm = predict(gbmFit1 , test ) ,
         # pred_svml3 = predict(svmLinear3_fit , test) ,
         # pred_BstLm = predict(BstLm_fit , test) ,
         pred = predict(rvmLinear_fit , test) ,
         linedif = result + spread_line ,
         pred_linedif = pred + spread_line ,
         home_win = ifelse(result > 0  , 1 , 0) ,
         home_cover = ifelse(linedif > 0 , 1 , 0) ,
         pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
         model_win = ifelse(pred_cover ==   home_cover  , 1 , 0)
  )  %>%
  select(game_id , spread_line , pred , pred_linedif , home_score , away_score , result , linedif , model_win)  %>%
  tabyl(model_win)

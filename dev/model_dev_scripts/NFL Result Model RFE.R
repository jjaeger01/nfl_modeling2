library(caret)
library(doParallel)
set.seed(10)

modeldata <- create_modeldata("result" , drop_cols_ = T)

indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
train <- modeldata %>% na.omit() %>% dplyr::slice(indices)
test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)

train.int <- data.frame(model.matrix( ~ .^2 , train[-1]))[-1]

cl <- makePSOCKcluster(10)
registerDoParallel(cl)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = T)
lmProfile <- rfe(result ~ . , data = train.int ,
                 rfeControl = ctrl)

stopCluster(cl)

outcomes %>%
  filter(game_id %in% test$game_id) %>%
  mutate(pred = predict(lmProfile$fit , data.frame(model.matrix( ~ .^2 , test[-1]))[-1]) ,
         linedif = result + spread_line ,
         pred_linedif = pred + spread_line ,
         home_win = ifelse(result > 0  , 1 , 0) ,
         home_cover = ifelse(linedif > 0 , 1 , 0) ,
         pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
         model_win = ifelse(pred_cover ==   home_cover  , 1 , 0)
  )  %>%
  select(game_id , spread_line , pred , pred_linedif , result , linedif , model_win) %>%
  tabyl(model_win)

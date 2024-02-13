modeldata <- create_modeldata("home_score")
# model_formula <- build_formula("home_score")

# modeldata <- create_modeldata("home_win")
# model_formula <- build_formula("home_win")

indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
train <- modeldata %>% na.omit() %>% dplyr::slice(indices)
test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)

nfl_model <- lm(home_score ~ . , data = train[-1] )
summary(nfl_model)
# nfl_model <- glm(data = train[-1] , model_formula)
# randomForest::randomForest(data = train[-1] , home_win ~ .)
# nfl_model <- train %>%
#   mutate(home_win = as.factor(home_win)) %>%
#   select(-1) %>%
#   randomForest::randomForest(data = . , home_win ~ . , type = "classification")
# nfl_model$confusion
# nfl_model$importance
#
# nfl_model <- MASS::stepAIC(nfl_model)
# summary(nfl_model)

outcomes %>%
  filter(game_id %in% test$game_id) %>%
  mutate(pred = predict(nfl_model , test) ,
         linedif = result + spread_line ,
         pred_linedif = pred + spread_line ,
         home_win = ifelse(result > 0  , 1 , 0) ,
         home_cover = ifelse(linedif > 0 , 1 , 0) ,
         pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
         model_win = ifelse(pred_cover ==   home_cover  , 1 , 0)
  )  %>%
  select(game_id , spread_line , home_score , away_score , pred , pred_linedif , result , linedif , model_win) %>%
tabyl(model_win)

# outcomes %>%
#   filter(game_id %in% test$game_id) %>%
#   mutate(pred = predict.glm(nfl_model , test , type = "response") ,
#          linedif = result + spread_line ,
#          pred_linedif = pred + spread_line ,
#          home_win = ifelse(result > 0  , 1 , 0) ,
#          home_cover = ifelse(linedif > 0 , 1 , 0) ,
#          pred_cover = ifelse(pred > 0.5 , 1 , 0) ,
#          model_win = ifelse(pred_cover == home_cover , 1 , 0)
#   ) %>% tabyl(model_win)

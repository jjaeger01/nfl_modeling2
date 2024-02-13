modeldata <- create_modeldata("result")

indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
train <- modeldata %>% na.omit() %>% dplyr::slice(indices)
test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)

# train.int <- data.frame(model.matrix( ~ .^2 , train[-1]))[-1]

# nfl_model <- lm(data = train.int , result ~ .)
nfl_model <- lm(data = train[-1] , result ~ .^2)
nfl_model <- MASS::stepAIC(nfl_model)
summary(nfl_model)

save(nfl_model , "stepAIC_result_model_v1.r")

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
  select(game_id , spread_line , pred , pred_linedif , result , linedif , model_win) %>%
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

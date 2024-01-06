library(caret)
library(doParallel)
set.seed(825)

modeldata <- create_modeldata(c("home_score" , "away_score"))

indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
train_ <- modeldata %>% na.omit() %>% dplyr::slice(indices)
test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)


home_fit <- model_outcome(outcome_ = "home_score" , method_ = "xgbTree" , train__ = train_ %>% select(-away_score))
away_fit <- model_outcome(outcome_ = "away_score" , method_ = "xgbTree" , train__ = train_ %>% select(-home_score))


# Outcomes ####
outcomes %>%
  filter(game_id %in% test$game_id) %>%
  mutate(
    pred_home_score = predict(home_fit , test) ,
    pred_away_score = predict(away_fit , test) ,
    pred = pred_home_score - pred_away_score ,
    linedif = result + spread_line ,
    pred_linedif = pred + spread_line ,
    home_win = ifelse(result > 0  , 1 , 0) ,
    home_cover = ifelse(linedif > 0 , 1 , 0) ,
    pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
    pred_win = ifelse(pred > 0 , 1 , 0) ,
    model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
    model_win_ns = ifelse(pred_win == home_win , 1 , 0)
  )  %>%
  select(game_id , season , spread_line ,
         home_score , pred_home_score ,
         away_score , pred_away_score ,
         result , pred ,  linedif , model_win , model_win_ns)  %>%
  tabyl(model_win_ns)



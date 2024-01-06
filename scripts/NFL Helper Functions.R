# HELPER FUNCTIONS ####

# Create modeling dataset, contains previous-average variables , ATS and wins, user specified outcome, defaults to result
create_modeldata <- function(outcome_ = "result" , drop_cols_ = T){
  modeldata <- alldata %>%
    select("game_id" , all_of(outcome_) ,
           "div_game" , "spread_line" ,
           contains("prev")
           ) %>%
    rename_with(~str_replace(.,".prev_avg" , ".avg") , contains(".prev_avg"))
  if(drop_cols_ == T){
    drop_cols <- c("prev_wins" , "prev_wins.away" ,  "prev_points" , "prev_points.away" ,
                   "prev_cover" , "prev_cover.away" , "prev_game" , "prev_game.away"  ,
                   "epa_play.avg" , "epa_play.avg.away" , "pass.comp.avg.away" , "pass.comp.avg" ,
                   "pass_plays.avg.away" , "pass_plays.avg" , "prev_home_cover" , "prev_away_cover" ,
                   "prev_home_fav_cover" , "prev_away_fav_cover" , "prev_home_dog_cover" , "prev_away_dog_cover" ,
                   "prev_home_cover.away" , "prev_away_cover.away" , "prev_home_fav_cover.away" , "prev_away_fav_cover.away" ,
                   "prev_home_dog_cover.away" , "prev_away_dog_cover.away" )
    modeldata <- modeldata %>% select(-drop_cols)
  }
  return(modeldata)
}

# For current week of season, pull post-game-average variable from previous week
build_current_week <- function(season_ , week_ , outcome_ = "result"){
  pull_season <- ifelse(week_ == 1 , season_ - 1 , season_)
  pull_week_ <- ifelse(week_ == 1 , ifelse(season_ > 2020 , 18  , 17), week_ - 1)
  current_week <- alldata %>%
    select(season, week , game_id , home_team , away_team ) %>%
    mutate(pull_week = pull_week_) %>%
    filter(season == season_ , week == week_) %>%
    left_join(team_agg %>% filter(season == pull_season , week == pull_week_) %>%
                select("game_id" , outcome_ , "div_game" , "spread_line" ,  contains("post") , starts_with("prev_"))  ,
              by = join_by(home_team == posteam) , suffix = c("" , ".y")) %>%
    select(-ends_with(".y")) %>%
    left_join(team_agg %>% filter(season == pull_season , week == pull_week_) %>%
                select("game_id" , outcome_ , "div_game" ,  "spread_line" ,  contains("post") , starts_with("prev_"))  ,
              by = join_by(away_team == posteam) , suffix = c("" , ".away")) %>%
    rename_with(~ str_remove(., "post_"), everything())
  return(current_week)
}

# Build a formula using a user-chosen outcome and the columns in modeldata
build_formula <- function(outcome_){
  formula_ <- paste(outcome_ , "~ div_game" , sep = " ")
  # print(formula_)
  for(var in colnames(modeldata)){
    if(var != outcome_ & var != "game_id"  & var != "div_game"){
      formula_ <- paste(formula_ , var , sep = " + ")
    }
  }
  # formula_ <- formula(formula_)
  return(formula_)
}

# Apply caret model training using a user-chosen outcome and modeling method
model_outcome <- function(outcome_ ,
                          method_ = "lm" ,
                          train__ = train_  ,
                          formula__ = "simple" ,
                          resample_ = 5 ,
                          repeats_ = 5 ,
                          clusters_ = 10){

  cl <- makePSOCKcluster(clusters_)
  registerDoParallel(cl)

  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = resample_,
    ## repeated ten times
    repeats = resample_)

  if(formula__ == "simple"){
    formula_ <- formula(paste(outcome_ , " ~ ."))
  }
  if(formula__ == "int"){
    formula_ <- formula(paste(outcome_ , " ~ .^2"))
  }

  # Fit model
  fit_ <- train(formula_ , data = train__[-1],
                method = method_ ,
                trControl = fitControl,
  )
  stopCluster(cl)
  return(fit_)
}

game_outcomes <- function(game_data_){
  game_outcomes_ <- outcomes %>%
    filter(game_id %in% game_data_$game_id)
  return(game_outcomes_)
}

pred_outcomes <- function(test , outcome_ = "result"){
  test_outcomes <- game_outcomes(test)
  if(outcome_ == "result"){
    test_outcomes <- test_outcomes %>% mutate(
      pred = predict(result_fit , test) ,
      pred_linedif = pred + spread_line ,
      pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
      pred_win = ifelse(pred > 0 , 1 , 0) ,
      model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
      model_win_ns = ifelse(pred_win == home_win , 1 , 0)
    )
  }
  if(outcome_ == "linedif"){
    test_outcomes <- test_outcomes %>% mutate(
      pred = predict(result_fit , test) ,
      pred_linedif = pred  ,
      pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
      pred_win = ifelse((pred - spread_line) > 0 , 1 , 0) ,
      model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
      model_win_ns = ifelse(pred_win == home_win , 1 , 0)
    )
  }
  if(outcome_ == "spread_line"){
    test_outcomes <- test_outcomes %>% mutate(
      pred = predict(result_fit , test) ,
      pred_linedif = pred + spread_line ,
      pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
      pred_win = ifelse(pred > 0 , 1 , 0) ,
      model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
      model_win_ns = ifelse(pred_win == home_win , 1 , 0)
    )
  }
  if(outcome_ == "home_away_score"){
    test_outcomes <- test_outcomes %>% mutate(
      pred_home_score = predict(home_fit , test) ,
      pred_away_score = predict(away_fit , test) ,
      pred = pred_home_score - pred_away_score ,
      pred_linedif = pred + spread_line ,
      pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
      pred_win = ifelse(pred > 0 , 1 , 0) ,
      model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
      model_win_ns = ifelse(pred_win == home_win , 1 , 0)
    )
  }
  if(outcome_ == "home_cover"){
    test_outcomes <- test_outcomes %>% mutate(
      pred = as.numeric(as.character(predict(home_cover_fit , test))) ,
      pred_cover = pred ,
      model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
    )
  }
  return(test_outcomes)
}

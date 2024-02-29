# For current week of season, pull post-game-average variable from previous week
build_current_week <- function(season_ , week_ , outcome_ = "result"){
  pull_season <- ifelse(week_ == 1 , season_ - 1 , season_)
  pull_week_ <- ifelse(week_ == 1 , ifelse(season_ > 2020 , 18  , 17), week_ - 1)
  pull_week_2 <- ifelse(week_ == 1 , ifelse(season_ > 2020 , 18  , 17), week_ - 2)
  outcome__ <- outcome_
  if(outcome_ == "home_away_score"){
    outcome__ <-  c("home_score" , "away_score")
  }
  current_week <-   alldata %>%
    dplyr::select("season", "week" , "game_id" ,
                  "home_team" , "away_team" , outcome__ ,
                  "div_game" ,  "spread_line" , week_) %>%
    mutate(pull_week = pull_week_) %>%
    filter(season == season_ , week == week_) %>%
    inner_join(team_agg %>% filter(season == pull_season , week == pull_week_) %>%
                 dplyr::select("game_id" , contains("post") , starts_with("prev_"))  ,
               by = join_by(home_team == posteam )) %>%
    inner_join(team_agg %>% filter(season == pull_season , week == pull_week_) %>%
                 dplyr::select("game_id" , contains("post") , starts_with("prev_"))  ,
               by = join_by(away_team == posteam) , suffix = c("" , ".away"))  %>%
    rename_with(~ str_remove(., "post_"), everything()) %>%
    select(pull_week , colnames(modeldata))

  missing_rows <- alldata %>%
    dplyr::select("season", "week" , "game_id" ,
                  "home_team" , "away_team" , outcome__ ,
                  "div_game" ,  "spread_line" , week_) %>%
    mutate(pull_week = pull_week_) %>%
    filter(season == season_ , week == week_) %>%
    anti_join(team_agg %>% filter(season == pull_season , week == pull_week_) %>%
                dplyr::select("game_id" , contains("post") , starts_with("prev_"))  ,
              by = join_by(home_team == posteam ) ) %>%
    inner_join(team_agg %>% filter(season == pull_season , week == pull_week_2) %>%
                 dplyr::select("game_id" , contains("post") , starts_with("prev_"))  ,
               by = join_by(home_team == posteam) , suffix = c("" , ".y"))  %>%
    left_join(team_agg %>% filter(season == pull_season , week == pull_week_2) %>%
                dplyr::select("game_id" , contains("post") , starts_with("prev_"))  ,
              by = join_by(away_team == posteam) , suffix = c("" , ".away")) %>%
    rename_with(~ str_remove(., "post_"), everything()) %>%
    dplyr::select(-ends_with(".y"))  %>%
    add_row(
      alldata %>%
        dplyr::select("season", "week" , "game_id" ,
                      "home_team" , "away_team" , outcome__ ,
                      "div_game" ,  "spread_line" ,) %>%
        mutate(pull_week = pull_week_)  %>%
        filter(season == season_ , week == week_)  %>%
        anti_join(team_agg %>% filter(season == pull_season , week == pull_week_) %>%
                    dplyr::select("game_id" , contains("post") , starts_with("prev_"))  ,
                  by = join_by(away_team == posteam))  %>%
        dplyr::select(-ends_with(".y")) %>%
        inner_join(team_agg %>% filter(season == pull_season , week == pull_week_2) %>%
                     dplyr::select("game_id" , contains("post") , starts_with("prev_"))  ,
                   by = join_by(home_team == posteam) , suffix = c("" , ".y")) %>%
        left_join(team_agg %>% filter(season == pull_season , week == pull_week_2) %>%
                    dplyr::select("game_id" , contains("post") , starts_with("prev_"))  ,
                  by = join_by(away_team == posteam) , suffix = c("" , ".away")) %>%
        select(-c("season.away" , "game_id.away")) %>%
        rename_with(~ str_remove(., "post_"), everything()) %>%
        dplyr::select(-ends_with(".y"))
    ) %>%
    distinct(game_id , .keep_all = T) %>%
    select(pull_week , colnames(modeldata))

  pred_week_ <- bind_rows(current_week , missing_rows)

  return(pred_week_)
}

# Apply caret model training using a user-chosen outcome and modeling method
model_outcome <- function(outcome_ ,
                          method_ = "lm" ,
                          train__ ,
                          outcome_type_ = "cont" ,
                          formula__ = "simple" ,
                          resample_ = 5 ,
                          repeats_ = 5 ,
                          clusters_ = 10){

  cl <- makePSOCKcluster(clusters_)
  registerDoParallel(cl)
  if(outcome_type_ == "cat"){
    train__[[outcome_]] <- factor(train__[[outcome_]])
  }
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = resample_,
    ## repeated ten times
    repeats = resample_)

  if(formula__ == "simple"){
    model_formula <- formula(paste(outcome_ , " ~ ."))
  }
  if(formula__ == "int"){
    model_formula <- formula(paste(outcome_ , " ~ .^2"))
  }

  # Fit model
  fit_ <- train(model_formula ,
                data = train__[-1],
                method = method_ ,
                trControl = fitControl,

  )
  stopCluster(cl)
  return(fit_)
}




# current_week %>%
#   inner_join(pull_week ,
#             by = join_by(home_team == posteam) ,
#             suffix = c("" , ".y")) %>%
#   dplyr::select(-ends_with(".y")) %>%
# add_row(
# current_week %>%
#   anti_join(pull_week ,
#              by = join_by(home_team == posteam)) %>%
#   select(-pull_week) %>%
#   inner_join(pull_week_2 ,
#              by = join_by(home_team == posteam) ,
#              suffix = c("" , ".y")) %>%
#   dplyr::select(-ends_with(".y"))
# )

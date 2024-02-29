# HELPER FUNCTIONS ####

# Create modeling dataset, contains previous-average variables , ATS and wins, user specified outcome, defaults to result
create_modeldata <- function(outcome_ = "result" , drop_cols_ = T){
  modeldata <- alldata %>%
    dplyr::select("game_id" , all_of(outcome_) ,
           "div_game" , "spread_line" ,
           contains("prev")
           ) %>%
    rename_with(~str_replace(.,".prev_avg" , ".avg") , contains(".prev_avg"))
  modeldata %>% filter(!is.na(outcome_[1])) %>% dplyr::select(all_of(outcome_)) %>% unique()
  if(drop_cols_ == T){ # Remove highly correlated columns
    drop_cols <- c("prev_wins" , "prev_wins.away" ,  "prev_points" , "prev_points.away" ,
                   "prev_cover" , "prev_cover.away" , "prev_game" , "prev_game.away"  ,
                   "epa_play.avg" , "epa_play.avg.away" , "pass.comp.avg.away" , "pass.comp.avg" ,
                   "pass_plays.avg.away" , "pass_plays.avg" , "prev_home_cover" , "prev_away_cover" ,
                   "prev_home_fav_cover" , "prev_away_fav_cover" , "prev_home_dog_cover" , "prev_away_dog_cover" ,
                   "prev_home_cover.away" , "prev_away_cover.away" , "prev_home_fav_cover.away" , "prev_away_fav_cover.away" ,
                   "prev_home_dog_cover.away" , "prev_away_dog_cover.away" )
    modeldata <- modeldata %>% dplyr::select(-drop_cols)
  }
  return(modeldata)
}


pull_pred_week <- function(season_ , week_ , outcome_){

  pull_season <- ifelse(week_ == 1 , season_ - 1 , season_)
  pull_week_ <- ifelse(week_ == 1 , ifelse(season_ > 2020 , 18  , 17), week_ - 1) # Last week
  pull_week_2_ <- ifelse(week_ == 1 , ifelse(season_ > 2020 , 18  , 17), week_ - 2) # Two weeks ago
  outcome__ <- outcome_
  if(outcome_ == "home_away_score"){
    outcome__ <-  c("home_score" , "away_score")
  }

  # Base df for pred_week
  current_week <-   alldata %>%
    dplyr::select("season", "week" , "game_id" ,
                  "home_team" , "away_team" , outcome__ ,
                  "div_game" ,  "spread_line") %>%
    filter(season == season_ , week == week_ ) %>%
    mutate(pull_week = pull_week_)

  # Pull post-average features from previous week
  pull_week <-  team_agg %>%
    dplyr::select("season", "week" , "game_id" , "posteam" ,
                  "home_team" , "away_team" , outcome__ ,
                  "div_game" ,  "spread_line" ,
                  contains(".post") , contains("prev")) %>%
    filter(season == season_ , week == pull_week_ ) %>%
    mutate(pull_week = pull_week_) %>%
    rename_with(~ str_remove(., "post_"), everything())

  # Pull post-average features from two weeks ago
  pull_week_2 <- team_agg %>%
    dplyr::select("season", "week" , "game_id" , "posteam" ,
                  "home_team" , "away_team" , outcome__ ,
                  "div_game" ,  "spread_line" ,
                  contains(".post") , contains("prev")) %>%
    filter(season == season_ , week == pull_week_2_ ) %>%
    mutate(pull_week = pull_week_2_) %>%
    rename_with(~ str_remove(., "post_"), everything())

  # Add last week post-avg features to pred_week base
  pred_week_ <- current_week %>%
    inner_join(pull_week %>% select(posteam , season , contains(".avg")) ,
               by = join_by(home_team == posteam , season == season)) %>%
    inner_join(pull_week %>% select(posteam , season , contains(".avg")) ,
               by = join_by(away_team == posteam , season == season) ,
               suffix = c("" , ".away")) %>%
  ## For pred_week rows in which either team had a bye the previous week: pull from two weeks ago
  add_row(
    bind_rows(current_week %>%
              anti_join(pull_week ,
                        by = join_by(away_team == posteam))
              ,
            current_week %>%
              anti_join(pull_week ,
                        by = join_by(home_team == posteam))
            ) %>% select(-pull_week) %>%
    inner_join(pull_week_2 %>% select(posteam , season , pull_week ,  contains(".avg")) ,
               by = join_by(home_team == posteam , season == season)) %>%
    inner_join(pull_week_2 %>% select(posteam , season , contains(".avg")) ,
               by = join_by(away_team == posteam , season == season) ,
               suffix = c("" , ".away"))
  ) %>% distinct(game_id , .keep_all = T)
  return(pred_week_)
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

game_outcomes <- function(game_data_){
  game_outcomes_ <- outcomes %>%
    filter(game_id %in% game_data_$game_id)
  return(game_outcomes_)
}

pred_week_base <- function(game_data_ , outcome_ = NULL){
  pred_week_base_ <- alldata %>%
    filter(game_id %in% game_data_$game_id) %>%
    select(game_id , season , week , home_team , away_team , spread_line , outcome_)
  return(pred_week_base_)
}

pred_outcomes <- function(test , # Generate predictions for a test dataset. User supplies test data, model, and outcome
                          outcome_ = "result" ,
                          outcome_type = "cont" ,
                          fit_ = NULL,
                          eval_pred = T ,
                          make_predictions = T ,
                          home_fit_ = NA ,
                          away_fit_ = NA ,
                          ensemble_ = F ,
                          ensemble_threshold = 0.5){
  method___ <- fit_$method__
  if(make_predictions == T){
    if(outcome_type == "cont" & ensemble_ == F){
      pred_base <- pred_week_base(test) %>%
        mutate(pred = predict(fit_ , test))
      if(outcome_ == "home_away_score"){
        pred_base <- pred_week_base(test) %>%
          mutate(pred_home_score = predict(home_fit_ , test) ,
                 pred_away_score = predict(away_fit_ , test) ,
                 pred = pred_home_score - pred_away_score)
      }
    }
    if(outcome_type == "cat" & ensemble_ == F){
      pred_base <- pred_week_base(test) %>%
        mutate(raw_pred = predict(fit_ , test[-1]) ,
   #            pred_prob = predict(fit_ , test[-1] , type = "prob") ,
               pred = as.numeric(as.character(predict(fit_ , test[-1] )))
               )
    }
    if(ensemble_ == T){
      ###########
      # insert lines 207-210 from run_nfl_model
      ###########
      if(outcome_type == "cat"){
        pred_base <- pred_week_base(test) %>%
          mutate(avg_pred = rowMeans(test %>% dplyr::select(matches("pred"))) ,
                 pred = ifelse(avg_pred >  ensemble_threshold , 1 , 0) ,
                 # does_count = ifelse(avg_pred > 0.8 , T , F) ,
          )
      }
      if(outcome_type == "cont"){
        test_outcomes <- pred_week_base(test) %>%
          mutate(pred = rowMeans(test %>% dplyr::select(matches("pred"))) ,
          )
      }
    }
    pred_base <- pred_base %>% mutate(method = method___ ,
                         outcome = outcome_ ,
                         outcome_type = outcome_type ,
                         # model_runs = model_runs__ ,
                         pred_time = as.character(lubridate::as_datetime(Sys.time())))
  }
  if(eval_pred == T){
    if(!exists("pred_base")){
      pred_base <- test
    }
    test_outcomes <- pred_base %>% left_join(game_outcomes(pred_base) , by = join_by(game_id, season, week, home_team, away_team) , suffix = c("" , ".close"))

    if(outcome_ == "result"){
      test_outcomes <- test_outcomes %>%  mutate(
        linedif = spread_line + result ,
        pred_home_score = NA ,
        pred_away_score = NA ,
        pred_linedif = pred + spread_line ,
        pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
        pred_cat_ = pred_cover ,
        outcome_ = home_cover ,
        pred_win = ifelse(pred > 0 , 1 , 0) ,
        model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
        model_win_ns = ifelse(pred_win == home_win , 1 , 0)
      )
    }
    if(outcome_ == "linedif"){
      test_outcomes <- test_outcomes %>% mutate(
        pred_home_score = NA ,
        pred_away_score = NA ,
        pred_linedif = pred  ,
        pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
        pred_cat_ = pred_cover ,
        outcome_ = home_cover ,
        pred_win = ifelse((pred - spread_line) > 0 , 1 , 0) ,
        model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
        model_win_ns = ifelse(pred_win == home_win , 1 , 0)
      )
    }
    if(outcome_ == "spread_line"){
      test_outcomes <- test_outcomes %>% mutate(
        pred_home_score = NA ,
        pred_away_score = NA ,
        pred_linedif = pred + spread_line ,
        pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
        pred_cat_ = pred_cover ,
        outcome_ = home_cover ,
        pred_win = ifelse(pred > 0 , 1 , 0) ,
        model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
        model_win_ns = ifelse(pred_win == home_win , 1 , 0)
      )
    }
    if(outcome_ == "home_away_score"){
      test_outcomes <- test_outcomes %>% mutate(
        pred_linedif = pred + spread_line ,
        pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
        pred_cat_ = pred_cover ,
        outcome_ = home_cover ,
        pred_win = ifelse(pred > 0 , 1 , 0) ,
        model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
        model_win_ns = ifelse(pred_win == home_win , 1 , 0)
      )
    }
    if(outcome_ == "home_cover"){
      test_outcomes <- test_outcomes %>% mutate(
        pred_home_score = NA ,
        pred_away_score = NA ,
        pred_cover = pred ,
        pred_cat_ = pred_cover ,
        outcome_ = home_cover ,
        pred_win = NA ,
        model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
        model_win_ns = NA ,
        outcome_ = home_cover
      )
    }
    if(outcome_ == "home_win"){
      test_outcomes <- test_outcomes %>% mutate(
        pred_home_score = NA ,
        pred_away_score = NA ,
        pred_win = pred ,
        pred_cat_ = pred_win ,
        outcome_ = home_win ,
        pred_cover = NA ,
        model_win = ifelse(pred_win ==   home_win  , 1 , 0) ,
        model_win_ns = ifelse(pred_win ==   home_win  , 1 , 0) ,
        outcome_ = home_win
        )
    }
  }
  if(eval_pred == F){
    test_outcomes <- pred_base
  }
  return(test_outcomes)
}


log_model_run <- function(model_run_ ,
                          con_ = NULL ,
                          table_ = "model_runs_v2"){
  library(DBI)
  if(is.null(con_)){
    con_ <- dbConnect(RSQLite::SQLite() , "data/model_runs_db.db")
  }
  dbWriteTable(con_ , table_ , model_run_$model_info , append = T)
  dbDisconnect(con_)
  print(paste("Model run logged in table " , table_))
}

log_prediction <- function(predictions_df_ ,
                          con_ = NULL ,
                          table_ = "predictions_v0"){
  library(DBI)
  if(is.null(con_)){
    con_ <- dbConnect(RSQLite::SQLite() , "data/predictions.db")
  }
  dbWriteTable(con_ , table_ , predictions_df_ , append = T)
  dbDisconnect(con_)
  print(paste("Predictions logged in table " , table_))
}

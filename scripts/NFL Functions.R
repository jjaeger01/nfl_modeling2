# DATA SETUP FUNCTIONS ####

# Create modeling dataset, contains previous-average variables , ATS and wins, user specified outcome, defaults to result
## Scrape play by play data for desired seasons #####

update_pbp <- function(update = FALSE , first_season , current_season){
  pbp_date <- date(file.info("data-raw/pbp.rdata")$mtime)
  print(paste("Play-by-play data last updated on" , file.info("data-raw/pbp.rdata")$mtime))
  if(pbp_date > max(games$gameday[!is.na(games$result)]) & update == FALSE){
    print("LOADING PBP DATA FROM LOCAL")
    load("data-raw/pbp.rdata")
  } else {
    print("UPDATING PLAY-BY-PLAY DATA")
    pbp <- load_pbp(first_season_:current_season_)
    save(games, file = "data-raw/games.rdata")
    save(pbp, file = "data-raw/pbp.rdata")
  }
  return(pbp)
}

create_modeldata <- function(outcome_ = "result" ,
                             drop_cols_ = NULL ,
                             drop_cor_cols = T ,
                             na.omit_ = T ,
                             seasons = c(2000:2023)){
  if(outcome_ == "home_away_score"){
    outcome_ <- c("home_score" , "away_score")
  }
  modeldata <- alldata %>%
    dplyr::select("game_id" , all_of(outcome_) ,
                  "div_game" , "spread_line" ,
                  "home_elo" , "away_elo" ,
                  contains("prev")
    ) %>%
    mutate(elo_dif = home_elo - away_elo) %>%
    rename_with(~str_replace(.,".prev_avg" , ".avg") , contains(".prev_avg")) %>%
    filter(substr(game_id , 1 , 4) %in% seasons)

  if(na.omit_ == T){
    modeldata <- modeldata %>% na.omit()
  }

  modeldata %>% filter(!is.na(outcome_[1])) %>% dplyr::select(all_of(outcome_)) %>% unique()
  if(drop_cor_cols == T){ # Remove highly correlated columns
    drop_cols <- c("prev_wins" , "prev_wins_away" ,  "prev_points" , "prev_points_away" ,
                   "prev_cover" , "prev_cover_away" , "prev_game" , "prev_game_away"  ,
                   "epa_play.avg" , "epa_play.avg_away" , "pass.comp.avg_away" , "pass.comp.avg" ,
                   "pass_plays.avg_away" , "pass_plays.avg" , "prev_home_cover" , "prev_away_cover" ,
                   "prev_home_fav_cover" , "prev_away_fav_cover" , "prev_home_dog_cover" , "prev_away_dog_cover" ,
                   "prev_home_cover_away" , "prev_away_cover_away" , "prev_home_fav_cover_away" , "prev_away_fav_cover_away" ,
                   "prev_home_dog_cover_away" , "prev_away_dog_cover_away"  ,
                   "prev_times_home_fav" , "prev_times_away_fav" , "prev_times_home_dog" , "prev_times_away_dog" ,
                   "prev_times_home_fav_away" , "prev_times_away_fav_away" , "prev_times_home_dog_away" , "prev_times_away_dog_away")

    modeldata <- modeldata %>% dplyr::select(-drop_cols )
  }
  if(length(outcome_) < 2){
    if(length(unique(modeldata[[outcome_]])) <= 3){
      modeldata[[outcome_]] <- as.factor(modeldata[[outcome_]])
    }
  }

  if(!is.null(drop_cols_)){
    modeldata <- modeldata %>% select(-drop_cols_)
  }

  return(modeldata)
}

game_outcomes <- function(game_data_){
  game_outcomes_ <- outcomes %>%
    filter(game_id %in% game_data_$game_id)
  return(game_outcomes_)
}

game_info <- function(game_data_){
  game_outcomes_ <- games %>%
    filter(game_id %in% game_data_$game_id) %>%
    select(game_id , gameday , gametime , season , week , home_team , away_team , spread_line , spread_quint , total_line, home_moneyline,  away_moneyline) %>%
    mutate(as_of_time = as.character(lubridate::as_datetime(Sys.time())))
  return(game_outcomes_)
}



ATS_prior_info <- function(game_data_){
  alldata %>%
    filter(game_id %in% game_data_$game_id) %>%
    # dplyr::select("season", "week" , "game_id" ,
    #               "home_team" , "away_team" ,
    #               "div_game" , "prev_game" ,  "spread_line" ,
    #               "home_moneyline" , "away_moneyline" ,
    #               "prev_cover" , "prev_cover_away" ,
    #               "prev_home_cover" , "prev_away_cover_away" ,
    #               "prev_home_dog_cover" ,  "prev_away_dog_cover_away" ,
    #               "prev_home_fav_cover" , "prev_away_fav_cover_away"
    #               ) %>%
    mutate(home_fav = ifelse(spread_line < 0 , 1 , 0) ,
           home_dog = ifelse(spread_line > 0 , 1 , 0) ,
           cover_pct = prev_cover / prev_game ,
           cover_pct_away = prev_cover_away / prev_game_away ,
           home_cover_pct = prev_home_cover / home_games_played ,
           away_cover_pct = prev_away_cover_away / away_games_played_away ,
           home_cover_fav_pct = prev_home_fav_cover / prev_times_home_fav ,
           away_cover_fav_pct = prev_away_fav_cover_away / prev_times_away_fav_away ,
           home_cover_dog_pct = prev_home_dog_cover / prev_times_home_dog ,
           away_cover_dog_pct = prev_away_dog_cover_away / prev_times_away_dog_away ) %>%
    select(game_id , home_team , away_team ,
           home_fav , home_dog , prev_cover , prev_cover_away ,
           cover_pct , cover_pct_away ,
           home_cover_pct , away_cover_pct ,
           home_cover_fav_pct , away_cover_fav_pct ,
           home_cover_dog_pct , away_cover_dog_pct ,
           prev_home_fav_cover , prev_times_home_fav ,
           prev_away_fav_cover , prev_times_away_fav ,
           prev_home_dog_cover , prev_times_home_dog ,
           prev_away_dog_cover , prev_times_away_dog)
}

ATS_record <- function(game_data_){
  team_agg %>%
    filter(game_id %in% game_data_$game_id) %>%
    group_by(posteam) %>%
    summarise(wins = sum(win) ,
              losses = sum(loss) ,
              ties = sum(tie) ,
              ats_covers = sum(ats_cover) ,
              ats_losses = sum(ats_loss) ,
              ats_push = sum(ats_push) ,
              home_ats_covers_home = sum(home_cover) ,
              away_ats_covers_away = sum(away_cover) ,
              ats_home_dog_cover = sum(ats_home_dog_cover) ,
              ats_home_fav_cover = sum(ats_home_fav_cover) ,
              ats_away_dog_cover = sum(ats_away_dog_cover) ,
              ats_away_fav_cover = sum(ats_away_fav_cover)
              )
}

pull_pred_week2 <- function(season_ , week_ , outcome_){

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
    filter(season == pull_season , week == pull_week_ ) %>%
    mutate(pull_week = pull_week_) %>%
    rename_with(~ str_remove(., "post_"), everything())

  # Pull post-average features from two weeks ago
  pull_week_2 <- team_agg %>%
    dplyr::select("season", "week" , "game_id" , "posteam" ,
                  "home_team" , "away_team" , outcome__ ,
                  "div_game" ,  "spread_line" ,
                  contains(".post") , contains("prev")) %>%
    filter(season == pull_season , week == pull_week_2_ ) %>%
    mutate(pull_week = pull_week_2_) %>%
    rename_with(~ str_remove(., "post_"), everything())

  # Add last week post-avg features to pred_week base
  pred_week_ <- current_week %>%
    inner_join(pull_week %>% select(posteam , season ,  contains(".avg")) ,
               by = join_by(home_team == posteam)) %>%
    inner_join(pull_week %>% select(posteam , season ,  contains(".avg")) ,
               by = join_by(away_team == posteam) ,
               suffix = c("" , "_away")) %>%
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
        inner_join(pull_week_2 %>% select(posteam , pull_week , season ,  contains(".avg")) ,
                   by = join_by(home_team == posteam)) %>%
        inner_join(pull_week_2 %>% select(posteam , season ,  contains(".avg")) ,
                   by = join_by(away_team == posteam) ,
                   suffix = c("" , "_away"))
    ) %>%
    distinct(game_id , .keep_all = T) %>%
    mutate(season = season.x)
  return(pred_week_)
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
    filter(season == pull_season , week == pull_week_ ) %>%
    mutate(pull_week = pull_week_) %>%
    rename_with(~ str_remove(., "post_"), everything())

  # Pull post-average features from two weeks ago
  pull_week_2 <- team_agg %>%
    dplyr::select("season", "week" , "game_id" , "posteam" ,
                  "home_team" , "away_team" , outcome__ ,
                  "div_game" ,  "spread_line" ,
                  contains(".post") , contains("prev")) %>%
    filter(season == pull_season , week == pull_week_2_ ) %>%
    mutate(pull_week = pull_week_2_) %>%
    rename_with(~ str_remove(., "post_"), everything())

  # Add last week post-avg features to pred_week base
  pred_week_ <- current_week %>%
    inner_join(pull_week %>% select(posteam , season ,  contains(".avg")) ,
               by = join_by(home_team == posteam)) %>%
    add_row(current_week %>%
              anti_join(pull_week ,
                        by = join_by(home_team == posteam)) %>%
              select(-pull_week) %>%
              inner_join(pull_week_2 %>%
                           select(posteam , pull_week , season ,  contains(".avg")) ,
                         by = join_by(home_team == posteam)
              )
    ) %>% left_join(
      current_week %>%
        inner_join(pull_week %>% select(posteam , season ,  contains(".avg")) ,
                   by = join_by(away_team == posteam)) %>%
        rename_at(vars(contains(".avg")) , function(x){paste(x,"_away" , sep = "")}) %>%
        ## For pred_week rows in which either team had a bye the previous week: pull from two weeks ago
        add_row(current_week %>%
                  anti_join(pull_week ,
                            by = join_by(away_team == posteam)) %>%
                  select(-pull_week) %>%
                  inner_join(pull_week_2 %>% select(posteam , pull_week , season ,  contains(".avg"))  ,
                             by = join_by(away_team == posteam)) %>%
                  rename_at(vars(contains(".avg")) , function(x){paste(x,"_away" , sep = "")})

        ) %>%
        distinct(game_id , .keep_all = T) %>%
        mutate(season = season.x) ,
      by = join_by(game_id) ,
      suffix = c("" , ".y")
    ) %>% select(-ends_with(".y"))
  return(pred_week_)
}

pull_pred_season <- function(season_){
  pull_season_ <- season_ - 1
  final_week <- ifelse(season_ > 2020 , 18  , 17)

  pred_season <-   alldata %>%
    dplyr::select("season", "week" , "game_id" ,
                  "home_team" , "away_team" ,
                  "div_game" ,  "spread_line") %>%
    filter(season == season_)

  pull_season <-  team_agg %>%
    dplyr::select("season", "week" , "game_id" , "posteam" ,
                  "home_team" , "away_team" ,
                  "div_game" ,  "spread_line" ,
                  contains(".post") , contains("prev")) %>%
    filter(season == pull_season_ , week == final_week) %>%
    rename_with(~ str_remove(., "post_"), everything())

  pred_season <- pred_season %>%
    inner_join(pull_season %>% select(posteam , season , contains(".avg")) ,
               by = join_by(home_team == posteam)) %>%
    inner_join(pull_season %>% select(posteam , season , contains(".avg")) ,
               by = join_by(away_team == posteam) ,
               suffix = c("" , "_away"))
  return(pred_season)
}








# MODELING FUNCTIONS ####
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

# Build a formula using a user-chosen outcome and the columns in modeldata
build_formula <- function(outcome_ , method_list = NULL , full_cols = T ,  other_cols = NULL , drop_cols = NULL){
  modeldata <- create_modeldata(outcome_ , seasons = 2022 , drop_cols_ = drop_cols)
  formula_ <- paste(outcome_ , "~ div_game" , sep = " ")
  # print(formula_)
  if(is.null(method_list)){
    if(full_cols == T){
      for(var in colnames(modeldata)){
        if(var != outcome_ & var != "game_id" & var != "div_game"){
          formula_ <- paste(formula_ , var , sep = " + ")
        }
      }
    }
  } else{
    formula_ <- paste(outcome_ , "~ " ,  method_list[1], sep = " ")
    for(var in method_list[-1]){
      if(var != outcome_ & var != "game_id" ){
        formula_ <- paste(formula_ , var , sep = " + ")
      }
    }
  }
  if(!is.null(other_cols)){
    for(col_ in other_cols){
      formula_ <- paste(formula_ , col_ , sep = " + ")
    }
  }
  # formula_ <- formula(formula_)
  return(formula_)
}
#
# # Build a formula using a user-chosen outcome and the columns in modeldata
# build_formula <- function(outcome_ , method_list = NULL , full_cols = T ,  other_cols = NULL , drop_cols = NULL){
#   modeldata <- create_modeldata(outcome_ , seasons = 2022 , drop_cols_ = drop_cols)
#   formula_ <- paste(outcome_ , "~ div_game" , sep = " ")
#   # print(formula_)
#   if(is.null(method_list)){
#     if(full_cols == T){
#       for(var in colnames(modeldata)){
#         if(var != outcome_ & var != "game_id"  & var != "div_game"){
#           formula_ <- paste(formula_ , var , sep = " + ")
#         }
#       }
#     }
#   } else{
#     formula_ <- paste(outcome_ , "~ " ,  method_list[1], sep = " ")
#     for(var in method_list[-1]){
#       if(var != outcome_ & var != "game_id"  & var != "div_game"){
#         formula_ <- paste(formula_ , var , sep = " + ")
#       }
#     }
#   }
#   if(!is.null(other_cols)){
#     for(col_ in other_cols){
#       formula_ <- paste(formula_ , col_ , sep = " + ")
#     }
#   }
#   # formula_ <- formula(formula_)
#   return(formula_)
# }

# PREDICTION FUNCTIONS ####
make_predictions <- function(test_data ,
                             fit_ = NA,
                             home_fit_ = NA ,
                             away_fit_ = NA ,
                             use_model_scaling = F ,
                             outcome_ = "result",
                             standard_predict = F ,
                             custom_method = NULL ,
                             sparse_df = F){
  # print(test_data)
  if(is.null(fit_$method)){
    method_ = custom_method
  } else{
    method_ = fit_$method
  }
  if(is.factor(test_data[[outcome_]]) == F){
    outcome_type <- "cont"
  } else{
    outcome_type <- "cat"
  }

  pred_base <- test_data %>%
    mutate(pred = NA , pred_cover = NA , pred_win = NA , pred_linedif = NA , pred_home_score = NA , pred_away_score = NA) %>%
    left_join(test_data %>% game_info() %>% select(game_id , point_spread = spread_line , point_spread_quint = spread_quint)  , by = join_by(game_id))

  # print(test_data)
  if(use_model_scaling == T){
    test_data <- predict(fit_$preproc , pred_base)
  }
  # print(test_data)
  if(standard_predict == T){
    pred_base <- pred_base %>%
      mutate(pred = round(predict(fit_ , test_data) , digits = 1))
  } else{
    if(outcome_type == "cont"){
      if(outcome_ == "home_away_score"){
        pred_base <- pred_base %>%
          mutate(pred_home_score = predict.train(home_fit_ , test_data) ,
                 pred_away_score = predict.train(away_fit_ , test_data) ,
                 pred = round(pred_home_score - pred_away_score , digits = 1)) # %>%
        # select(-c(pred_home_score , pred_away_score))
      } else{
        pred_base <- pred_base %>%
          mutate(pred = round(predict.train(fit_ , test_data) , digits = 1))
      }
      if(outcome_ %in% c("result" , "home_away_score")){
        pred_base <- pred_base %>%
          mutate(pred_linedif = pred + point_spread )
      }
      if(outcome_ == "linedif"){
        pred_base <- pred_base %>%
          mutate(pred_linedif = pred)
      }
      pred_base <- pred_base %>%
        mutate(pred_cover = ifelse(pred_linedif > 0 , 1 , 0))
      }

    if(outcome_type == "cat"){
      pred_base <- pred_base %>%
        mutate(raw_pred = predict.train(fit_ , test_data[-1]) ,
               # pred_prob = predict.train(fit_ , test_data[-1] , type = "prob") ,
               pred = as.numeric(as.character(predict.train(fit_ , test_data[-1] )))
        )
      if(outcome_ == "home_cover"){
        pred_base <- pred_base %>%
          mutate(pred_cover = pred)
      } else {
        pred_base <- pred_base %>%
          mutate(pred_cover = NA)
      }
    }
  }
  predicted_data <- pred_base %>% mutate(method = method_ ,
                                         outcome = outcome_ ,
                                         model_formula = fit_$formula_type ,
                                         feature_selection = ifelse(fit_$used_RFE == 1 , "RFE" , "") ,
                                         spread_quint = fit_$quintile ,
                                         outcome_type = outcome_type ,
                                         # model_runs = model_runs__ ,
                                         pred_time = as.character(lubridate::as_datetime(Sys.time())))


  # predicted_data <- predicted_data %>% left_join(predicted_data %>% game_info() %>% select(game_id , point_spread = spread_line)) # This line re-adds the unscaled point spread

  if(sparse_df == T){
    predicted_data <- predicted_data %>%  select(game_id , point_spread , pred , pred_home_score , pred_away_score , pred_cover ,  pred_linedif ,
                                                 method , outcome , model_formula , feature_selection , spread_quint , pred_time)
    #   predicted_data <- predicted_data %>% left_join(pred_base %>% select(game_id , pred_prob))
    # }
  }

  return(predicted_data)
}


eval_pred <- function(predicted_data ,
                      sparse_df = F){
  outcome_ <- first(predicted_data$outcome)
  # print(game_outcomes(predicted_data))
  # print(outcome_)
  test_outcomes <- game_outcomes(predicted_data) %>%
    select(-c("spread_line")) %>%
    left_join(predicted_data %>%
                select(game_id , pred , pred_home_score , pred_away_score , method , outcome , model_formula , feature_selection ,
                       spread_line = point_spread) , # USES POINTSPREAD FROM TIME OF PREDICTION
              by = join_by(game_id)  , suffix = c("" , ".close")
    )
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
      model_win_ns = ifelse(pred_win == home_win , 1 , 0) ,
      residual = result - pred
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
      model_win_ns = ifelse(pred_win == home_win , 1 , 0) ,
      residual = linedif - pred

    )
  }
  if(outcome_ == "spread_line"){
    test_outcomes <- test_outcomes %>% mutate(
      pred_home_score = NA ,
      pred_away_score = NA ,
      pred_linedif = (pred) + spread_line ,
      pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
      pred_cat_ = pred_cover ,
      outcome_ = home_cover ,
      pred_win = ifelse(pred < 0 , 1 , 0) ,
      model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
      model_win_ns = ifelse(pred_win == home_win , 1 , 0) ,
      residual = spread_line - pred

    )
  }
  if(outcome_ == "spread_line_neg"){
      test_outcomes <- test_outcomes %>% mutate(
        pred = pred*-1 ,
        pred_home_score = NA ,
        pred_away_score = NA ,
        pred_linedif = pred + spread_line ,
        pred_cover = ifelse(pred_linedif > 0 , 1 , 0) ,
        pred_cat_ = pred_cover ,
        outcome_ = home_cover ,
        pred_win = ifelse(pred > 0 , 1 , 0) ,
        model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
        model_win_ns = ifelse(pred_win == home_win , 1 , 0) ,
        residual = spread_line - pred

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
      model_win_ns = ifelse(pred_win == home_win , 1 , 0) ,
      residual = result - pred

    )
  }
  if(outcome_ == "home_cover"){
    test_outcomes <- test_outcomes %>% mutate(
      pred_linedif = NA ,
      pred_home_score = NA ,
      pred_away_score = NA ,
      pred_cover = pred ,
      pred_cat_ = pred_cover ,
      outcome_ = home_cover ,
      pred_win = NA ,
      model_win = ifelse(pred_cover ==   home_cover  , 1 , 0) ,
      model_win_ns = NA ,
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
    )
  }
  if(sparse_df == T){
    test_outcomes <- test_outcomes %>%
      select(game_id , outcome ,
             home_team , away_team ,
             result , spread_line ,
             pred , pred_home_score , pred_away_score ,
             pred_linedif , pred_cover ,
             home_cover , model_win)
  }
  return(test_outcomes)
}

eval_ensemble_pred <- function(ensemble_data_){

  df_list <- list()
  for(modeled_outcome in unique(ensemble_data_$outcome)){
    df_list[[modeled_outcome]] <- ensemble_data_ %>%
      filter(outcome == modeled_outcome) %>% left_join(eval_pred(.))
  }
  full_evaluated_df <- bind_rows(df_list)

}

calc_ATS <- function(eval_pred){
  ATS_accuracy <- eval_pred %>% pull(model_win) %>% mean()
  print(paste("ATS Accuracy:" , ATS_accuracy ))
  return(ATS_accuracy)
}


# SQL FUNCTIONS ####

log_model_run <- function(model_run_ ,
                          db_file = NULL ,
                          table_ = "model_runs_v5"){
    library(DBI)
    con_ <- dbConnect(RSQLite::SQLite() , db_file)
    if(is.null(db_file)){
      con_ <- dbConnect(RSQLite::SQLite() , "data/model_runs2_db.db")
    }
    dbWriteTable(con_ , table_ , model_run_$model_info , append = T)
    dbDisconnect(con_)
    print(paste("Model run logged in table " , table_))
  }

log_prediction <- function(predictions_df_ ,
                           db_file = NULL ,
                           table_ = "predictions_v1"){
  library(DBI)
  con_ <- dbConnect(RSQLite::SQLite() , db_file)
  if(is.null(db_file)){
    con_ <- dbConnect(RSQLite::SQLite() , "data/predictions2.db")
  }
  dbWriteTable(con_ , table_ , predictions_df_ , append = T)
  dbDisconnect(con_)
  print(paste("Predictions logged in table " , table_))
}

log_preseason_prediction <- function(predictions_df_ ,
                           db_file = NULL ,
                           table_ = "predictions_v1"){
  library(DBI)
  con_ <- dbConnect(RSQLite::SQLite() , db_file)
  if(is.null(db_file)){
    con_ <- dbConnect(RSQLite::SQLite() , "data/predictions2.db")
  }
  predictions_df_ <- predictions_df_ %>%
    select(all_of(c("home_team" , "home_wins" , "away_wins" , "WINS" , "LOSSES" , "RECORD" , "METHOD" , "OUTCOME" , "formula" , "run_time")))
  dbWriteTable(con_ , table_ , predictions_df_ , append = T)
  dbDisconnect(con_)
  print(paste("Predictions logged in table" , table_))
}



pull_quintile_tables <- function(db_file){
  con_ <- dbConnect(RSQLite::SQLite() , db_file)
  quint_table_out <- tibble()
  for(table_ in dbListTables(con_)){
    # print(table_)
    quint_table <- dbGetQuery(con_ , paste("select * from" , table_)) %>%
      mutate(quint = substr(table_ , str_length(table_) , str_length(table_)) )
    # dbGetQuery(con_ , paste("select * from" , table_)) %>% select(-features)
    quint_table_out <- bind_rows(quint_table_out , quint_table)
  }
  return(quint_table_out)
}

# GAMBLING ####

bets <- function(bets , amt , win_pct){
  wins <- round(bets * win_pct)
  losses <- bets - wins
  winnings <- wins * (amt*0.9)
  lost <- losses * amt
  net <- winnings - lost
  # print(paste("Won " , wins , " games" ,  sep = ""))
  # print(paste("Won $" , net ,  sep = ""))
  return(net)
}



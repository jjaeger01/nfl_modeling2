# HELPER FUNCTIONS ####
# Create modeling dataset, contains previous-average variables , ATS and wins, user specified outcome, defaults to result
create_modeldata <- function(outcome_ = "result"){
  modeldata <- alldata %>%
    select("game_id" , all_of(outcome_) ,
           "div_game" , "spread_line" ,
           contains("prev")
           ) %>%
    rename_with(~str_replace(.,".prev_avg" , ".avg") , contains(".prev_avg"))
}


build_current_week <- function(season_ , week_ , outcome_ = "result"){ # For current week of season, pull post-game-average variable from previous week
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

build_formula <- function(outcome_){ # Build a formula using a user-chosen outcome and the columns in modeldata
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

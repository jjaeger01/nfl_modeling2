# LIBRARIES #####
setwd("~/Projects/nfl_modeling2")
library(tidyverse)
library(devtools)
library(nflfastR)
library(psych)
library(lme4)
library(mice)
library(broom)
library(mitools)
library(lubridate)
library(progress)
library(glue)
library(janitor)
library(parallel)
library(doParallel)
library(caret)
options(scipen = 9999)
set.seed(666)

print("Loading NFL Helper Functions")
source("~/Projects/nfl_modeling2/Scripts/NFL Functions.R")

# IMPORT GAMEDATA #####

first_season_ <- 1999
current_season_ <- 2024

# Import Game Data
gamesfull <- readRDS(url("http://www.habitatring.com/games.rds"))
games <- gamesfull %>%
  filter(season >= first_season_ ) %>%
  mutate(spread_line = spread_line * -1) %>%
  select(game_id , season , game_type , week , gametime ,
         gameday , away_team , home_team , away_score , home_score , result , total ,
         spread_line , total_line , home_moneyline , away_moneyline ,
         div_game , roof, surface , temp, wind) %>%
         mutate(spread_quint =  cut(spread_line ,
                                     quantile(spread_line , probs = seq(0 , 1 , .20) , na.rm = T) ,
                                     include.lowest = T ,
                                     labels = c(1:5) ,
                                     ordered_result = T) ,
                spread_quart =  cut(spread_line ,
                                    quantile(spread_line , probs = seq(0 , 1 , .25) , na.rm = T) ,
                                    include.lowest = T ,
                                    labels = c(1:4) ,
                                    ordered_result = T) , )


## Relabel teams that have moved #####
games$home_team[games$home_team=="STL"]<-"LA"
games$home_team[games$home_team=="SD"]<-"LAC"
games$home_team[games$home_team=="OAK"]<-"LV"
games$away_team[games$away_team=="STL"]<-"LA"
games$away_team[games$away_team=="OAK"]<-"LV"
games$away_team[games$away_team=="SD"]<-"LAC"
# IMPORT PBP DATA ###################################################


pbp <- update_pbp(first_season = first_season_ , current_season = current_season_ ) ## Update season  ####


pbp$pen_yds <- ifelse((pbp$penalty == 1) & pbp$posteam == pbp$penalty_team, ifelse(!is.na(pbp$penalty_yards), pbp$penalty_yards, 0), 0)
pbp$yac <- ifelse(pbp$complete_pass == 1, ifelse(!is.na(pbp$yards_after_catch), pbp$yards_after_catch, 0), 0)
# pbp$passyds <- ifelse(pbp$play_type_nfl == "PASS", ifelse(!is.na(pbp$passing_yards), pbp$passing_yards, 0) , 0)
pbp$passyds <- ifelse(pbp$play_type_nfl == "PASS", ifelse(!is.na(pbp$yards_gained), pbp$yards_gained, 0) , 0)
pbp$rushyds <- ifelse(pbp$play_type_nfl == "RUSH", ifelse(!is.na(pbp$rushing_yards), pbp$rushing_yards, 0) , 0)
pbp$first_1 <- ifelse(pbp$first_down == 1 & pbp$down == 1 , 1 , 0)
pbp$first_2 <- ifelse(pbp$first_down == 1 & pbp$down == 2 , 1 , 0)
pbp$first_3 <- ifelse(pbp$first_down == 1 & pbp$down == 3 , 1 , 0)
pbp$first_4 <- ifelse(pbp$first_down == 1 & pbp$down == 4 , 1 , 0)
pbp$pass_epa <- ifelse(pbp$pass == 1 , pbp$epa , NA)
pbp$rush_epa <- ifelse(pbp$rush == 1 , pbp$epa , NA)

# Create a dataset of the last few weeks of the previous season. Use these average values in first few weeks
last_season_final_weeks <- games %>%
  filter( (season==2019 & ( week==15 | week==16 |week==17)) |(season == 2019 & (away_team =="CHI" & week == 11)))

# AGGREGATE TO GAME-TEAM LEVEL #####

## Aggregate pbp stats to games-teams within teams within seasons #####
### Offense ####
agg_off <- pbp %>%
  group_by(season, posteam, game_id) %>%
  summarise(# passyds = sum(passyds , na.rm = TRUE),
            passyds = sum(passyds , na.rm = TRUE)  +  sum(ifelse(sack == 1 , yards_gained , 0) , na.rm = T) ,
            # sack_yds = sum(ifelse(sack == 1 , yards_gained , 0) , na.rm = T) ,
            pass.comp = sum(complete_pass , na.rm = TRUE),
            pass.att = sum(pass_attempt, na.rm = TRUE) ,
            pass_plays = sum(pass , na.rm = T) ,
            pass.comp.pct = (sum(complete_pass , na.rm = TRUE) / sum(pass_attempt[sack == 0], na.rm = TRUE) ) ,
            pass.ints = sum(interception , na.rm = TRUE),
            pass.tds = sum(pass_touchdown , na.rm = TRUE),
            rushyds = sum(rushyds , na.rm = TRUE) ,
            rush.att = sum(rush_attempt , na.rm = TRUE) ,
            rush.td = sum(rush_touchdown , na.rm = TRUE) ,
            fumble.lost = sum(fumble_lost , na.rm = TRUE) ,
            sacks_taken = sum(sack , na.rm = T) ,
            first.down = sum(first_down , na.rm = TRUE) ,
            yac = sum(yac , na.rm = TRUE),
            pass_epa_play = mean(pass_epa , na.rm = T) ,
            rush_epa_play = mean(rush_epa , na.rm = T) ,
            epa_play = mean(epa , na.rm = T) ,
  ) %>% filter(!is.na(posteam))
offense <- colnames(agg_off)
### Defense ####
agg_def <- pbp %>%
  group_by(season , defteam , game_id) %>%
  summarise(passyds.allowed = sum(passyds , na.rm = TRUE) ,
            pass.comp.pct.allowed = (sum(complete_pass , na.rm = TRUE) / sum(pass_attempt[sack == 0],na.rm = TRUE) ) ,
            def.pass.ints = sum(interception , na.rm = TRUE) ,
            pass.tds.allowed = sum(pass_touchdown , na.rm = TRUE) ,
            fumble.forced = sum(fumble_forced , na.rm = TRUE) ,
            rushyds.allowed = sum(rushyds , na.rm = TRUE) ,
            rush.td.allowed = sum(rush_touchdown , na.rm = TRUE) ,
            first.down.allowed = sum(first_down , na.rm = TRUE) ,
            yac.allowed = sum(yac , na.rm = TRUE),
            sacks = sum(sack , na.rm = TRUE) ,
            def_pass_epa = mean(epa[play_type_nfl == "PASS"] , na.rm = T)*-1 ,
            def_rush_epa = mean(epa[play_type_nfl == "RUSH"] , na.rm = T)*-1
  ) %>% filter(!is.na(defteam))
defense <- colnames(agg_def)
### Down %s ####
agg_downs <- pbp %>%
  group_by(season, posteam, game_id , down) %>%
  summarize_at(c("first_1" , "first_2" , "first_3" ,"first_4") , mean, na.rm = T) %>%
  group_by(season, posteam, game_id) %>%
  summarize_at(c("first_1" , "first_2" , "first_3" ,"first_4") , sum, na.rm = T) %>%
  filter(!is.na(posteam))
first_down_rates <- colnames(agg_downs)
### Penalty Yds ####
pen_yds <- pbp %>%
  group_by(season, posteam, game_id) %>%
  summarise(pen_yds = sum(penalty_yards , na.rm = TRUE)) %>%
  filter(!is.na(posteam))
penalties <- colnames(pen_yds)
### Combine into team_agg base ####
team_agg_base <-  left_join(agg_off, pen_yds) %>%
  left_join(agg_downs) %>%
  dplyr::left_join(agg_def , join_by(season, game_id , posteam == defteam))

# AVG FEATURES ####
prev_avg <- function(x) (cumsum(x) - x )/(row_number() - 1 )
curr_avg <- function(x) (cumsum(x)/(row_number()))

team_agg <- team_agg_base %>%
  left_join(
    team_agg_base %>% group_by(season , posteam) %>%
      mutate_at(vars(-group_cols() & -game_id), prev_avg) %>%
      rename_at(vars(-group_cols() & -game_id), function(x)paste0(x, ".prev_avg")) ## Avgs up to now ####
            ) %>%
  left_join(
    team_agg_base %>% group_by(season , posteam) %>%
      mutate_at(vars(-group_cols() & -game_id), curr_avg) %>%
      rename_at(vars(-group_cols() & -game_id), function(x)paste0(x, ".post_avg")) ## Avgs after game ####
            ) %>%
  left_join(games %>% select(season, game_id , div_game , week ,
                             home_team , away_team , home_score , away_score ,
                             result , spread_line) ## Add score & line variables ####
            ) %>%
  mutate(team_ = posteam ,
         home_away = ifelse(home_team == team_ , "home" , "away") ,
         team_spread = ifelse(home_away == "home" , spread_line , spread_line * -1) ,
         team_result = ifelse(home_away == "home" , result , result * -1) ,
         home_dog = ifelse(home_away == "home" & team_spread > 0 , 1 , 0) ,
         home_fav = ifelse(home_away == "home" & team_spread < 0 , 1 , 0) ,
         away_dog = ifelse(home_away == "away" & team_spread > 0 , 1 , 0) ,
         away_fav = ifelse(home_away == "away" & team_spread < 0 , 1 , 0) ,
         win = ifelse(team_result > 0 , 1 , 0) ,
         loss = ifelse(team_result < 0 , 1 , 0) ,
         tie = ifelse(team_result == 0 , 1 , 0) ,
         points = ifelse(home_away == "home" , home_score , away_score) ,
         linedif = team_spread + team_result ,
         ats_cover = ifelse(linedif > 0 , 1 , 0) ,
         ats_loss = ifelse(linedif < 0 , 1 , 0) ,
         ats_push = ifelse(linedif == 0 , 1 , 0) ,
         home_win = ifelse(home_away == "home" & win == 1 , 1 , 0) ,
         home_cover = ifelse(home_away == "home" & ats_cover == 1 , 1 , 0)  ,
         home_push = ifelse(home_away == "home" &  linedif == 0 , 1 , 0) ,
         away_win = ifelse(home_away == "away" & win == 1 , 1 , 0) ,
         away_cover = ifelse(home_away == "away" &  ats_cover == 1 , 1 , 0) ,
         away_push = ifelse(home_away == "away" & linedif == 0  , 1 , 0) ,
         ats_home_cover = ifelse(home_away == "home" & ats_cover == 1 , 1 , 0) ,
         ats_away_cover = ifelse(home_away == "away" & ats_cover == 1 , 1 , 0) ,
         ats_home_dog_cover = ifelse(home_away == "home" & home_dog == 1 & ats_cover == 1 , 1 , 0) ,
         ats_home_fav_cover = ifelse(home_away == "home" & home_fav == 1 & ats_cover == 1 , 1 , 0) ,
         ats_away_dog_cover = ifelse(home_away == "away" & away_dog == 1 & ats_cover == 1 , 1 , 0) ,
         ats_away_fav_cover = ifelse(home_away == "away" & away_fav == 1 & ats_cover == 1 , 1 , 0)
  ) %>%
  group_by(posteam, season) %>%
  mutate(total_points = cumsum(points) ,
         home_games_played = cumsum(ifelse(home_away == "home" , 1 , 0)) - 1,
         away_games_played = cumsum(ifelse(home_away == "away" , 1 , 0)) - 1 ,
         prev_times_home_fav = ifelse(is.na(home_fav), sum(home_fav, na.rm = TRUE),
                                      ifelse(home_fav ==1, cumsum(home_fav) - 1, cumsum(home_fav))) ,
         prev_times_away_fav = ifelse(is.na(away_fav), sum(away_fav, na.rm = TRUE),
                                      ifelse(away_fav ==1, cumsum(away_fav) - 1, cumsum(away_fav))) ,
         prev_times_home_dog = ifelse(is.na(home_dog), sum(home_dog, na.rm = TRUE),
                                      ifelse(home_dog ==1, cumsum(home_dog) - 1, cumsum(home_dog))) ,
         prev_times_away_dog = ifelse(is.na(away_dog), sum(away_dog, na.rm = TRUE),
                                      ifelse(away_dog ==1, cumsum(away_dog) - 1, cumsum(away_dog))) ,
         new_avg.points = total_points/row_number() ,
         prev_points = total_points - points ,
         prev_game = row_number() - 1 ,
         points.avg = prev_points / prev_game ,
         prev_wins = ifelse(is.na(win), sum(win, na.rm = TRUE),
                            ifelse(win ==1, cumsum(win) - 1, cumsum(win))) ,
         prev_cover = ifelse(is.na(ats_cover), sum(ats_cover, na.rm = TRUE),
                            ifelse(ats_cover ==1, cumsum(ats_cover) - 1, cumsum(ats_cover))) ,
         prev_home_cover = ifelse(is.na(ats_home_cover), sum(ats_home_cover, na.rm = TRUE),
                                  ifelse(ats_home_cover ==1, cumsum(ats_home_cover) - 1, cumsum(ats_home_cover))) ,
         prev_away_cover = ifelse(is.na(ats_away_cover), sum(ats_away_cover, na.rm = TRUE),
                                  ifelse(ats_away_cover ==1, cumsum(ats_away_cover) - 1, cumsum(ats_away_cover))) ,
         prev_home_fav_cover = ifelse(is.na(ats_home_fav_cover), sum(ats_home_fav_cover, na.rm = TRUE),
                                  ifelse(ats_home_fav_cover ==1, cumsum(ats_home_fav_cover) - 1, cumsum(ats_home_fav_cover))) ,
         prev_away_fav_cover = ifelse(is.na(ats_away_fav_cover), sum(ats_away_fav_cover, na.rm = TRUE),
                                  ifelse(ats_away_fav_cover ==1, cumsum(ats_away_fav_cover) - 1, cumsum(ats_away_fav_cover))) ,
         prev_home_dog_cover = ifelse(is.na(ats_home_dog_cover), sum(ats_home_dog_cover, na.rm = TRUE),
                                      ifelse(ats_home_dog_cover ==1, cumsum(ats_home_dog_cover) - 1, cumsum(ats_home_dog_cover))) ,
         prev_away_dog_cover = ifelse(is.na(ats_away_dog_cover), sum(ats_away_dog_cover, na.rm = TRUE),
                                      ifelse(ats_away_dog_cover ==1, cumsum(ats_away_dog_cover) - 1, cumsum(ats_away_dog_cover))) ,
         prev_wins_adj.avg = prev_wins * (prev_game/22) ,
         prev_cover_adj.avg = prev_cover * (prev_game/22)
         ) %>%
  arrange(game_id) # %>%
# select(game_id , team_ , points , prev_points , new_avg.points , avg.points) %>%
# View()


## Join aggregated home and away player-game data with information about games #####
alldata <- games %>%
  left_join(team_agg , by = join_by(game_id  , season , home_team == posteam) , suffix = c("" , ".y")) %>% select(-ends_with(".y")) %>%
  left_join(team_agg , by = join_by(game_id  , season , away_team == posteam) , suffix = c("" , "_away")) # %>%
  # select(any_of(colnames(games)) ,
  #        contains("avg") ,
  #        contains("prev")
  #        )
outcomes <- games %>%
  select(game_id , season , week , home_team , away_team , home_score , away_score , result , spread_line , spread_quint) %>%
  mutate(linedif = result + spread_line ,
         home_win = ifelse(result > 0  , 1 , 0) ,
         home_cover = ifelse(linedif > 0 , 1 , 0)
         )
save(games, file = "data/alldata.rdata")
save(games, file = "data/games.rdata")


# source("~/Projects/nfl_modeling2/scripts/NFL Helper Functions.R")
#
# # RESULT model pre-processing
# modeldata <- create_modeldata("result") %>% na.omit()

# Find correlated predictors

# full_cor <- cor(modeldata[2:ncol(modeldata)])
# findCorrelation(full_cor , names = T , cutoff =  .8)
# full_cor[ , findCorrelation(full_cor , names = T , cutoff =  .7)] %>% View()

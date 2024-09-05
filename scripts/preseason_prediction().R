source("~/Projects/nfl_modeling2/scripts/run_NFL_model().R")

preseason_prediction <- function(outcome ,
                                 method ,
                                 season ,
                                 model_test_season ,
                                 model_formula = "simple" ,
                                 log_in_table = "preseason_pred2"){

  trained_model <- run_NFL_model(outcome_ = outcome ,
                                 method__ = method ,
                                 test_season = model_test_season ,
                                 formula__ = model_formula)

  features <- trained_model$model_info$features

  pred_season <- pull_pred_season(season)

  pred_season$spread_line <- ifelse(is.na(pred_season$spread_line) , mean(pred_season$spread_line , na.rm = T) , pred_season$spread_line)

  predicted_season_ <- make_predictions(test_data = pred_season ,
                                       outcome_ = outcome ,
                                       fit_ = trained_model$model_objects$model ,
                                       home_fit_ = trained_model$model_objects$home_fit_ ,
                                       away_fit_ = trained_model$model_objects$away_fit_)

  # predicted_season_ <- predicted_season_ %>% mutate(outcome = if_else(outcome == "spread_line" , "spread_line_neg" , outcome))

  predicted_season <- predicted_season_ %>%
    eval_pred() %>%
    select(game_id , season , home_team , away_team , pred_win)

  PROJECTION  <-  predicted_season %>%
                    group_by(home_team) %>%
                    summarise(home_wins = sum(as.numeric(as.character(pred_win)))) %>%
                    left_join(
                      predicted_season %>%
                        group_by(away_team) %>%
                        summarise(away_wins = sum(if_else(as.numeric(as.character(pred_win)) == 0 , 1 , 0))) ,
                      by = c("home_team" = "away_team")
                    ) %>%
                    mutate(WINS = home_wins + away_wins ,
                           LOSSES = 17 - WINS ,
                           RECORD = paste(WINS , LOSSES , sep = " - ") ,
                           METHOD = method ,
                           OUTCOME = outcome ,
                           formula = model_formula ,
                           features = features ,
                           run_time = as.character(Sys.time())
                           )

  # print(PROJECTION)
  log_preseason_prediction(PROJECTION , db_file = "data/predictions2.db" , table_ = log_in_table)

  pred_season_table <- function(team_win_projection_ , combo = F){
    afc_east <- c("BUF" , "MIA" , "NYJ" , "NE")
    afc_north <- c("PIT" , "BAL" , "CLE" , "CIN")
    afc_west <- c("DEN" , "LAC" , "LV" , "KC")
    afc_south <- c("TEN" , "IND" , "HOU" , "JAX")
    afc <- list("AFC East" = afc_east  , "AFC South" = afc_south, "AFC West" = afc_west , "AFC North" = afc_north)

    nfc_east <- c("DAL" , "PHI" , "WAS" , "NYG")
    nfc_north <- c("CHI" , "GB" , "DET" , "MIN")
    nfc_west <- c("SF" , "SEA" , "ARI" , "LA")
    nfc_south <- c("ATL" , "TB" , "NO" , "CAR")
    nfc <- list("NFC East" =  nfc_east  , "NFC South" =  nfc_south, "NFC West" =  nfc_west , "NFC North" =  nfc_north)

    i <- 1
    conf_table <- tibble()
    for(div in afc){
      div_table <- cbind(Division =  rep("" , 4) , team_win_projection_ %>% dplyr::filter(home_team %in% div) %>% arrange(desc(WINS)))
      label_row <- c(names(afc[i]) , rep("" , length(div_table) - 1))
      div_table <- rbind(label_row , div_table)
      conf_table <- rbind(conf_table , div_table)
      i <- i + 1
    }
    afc_table <- conf_table

    i <- 1
    conf_table <- tibble()
    for(div in nfc){
      div_table <- cbind( Division = rep("" , 4) , team_win_projection_ %>% dplyr::filter(home_team %in% div) %>% arrange(desc(WINS)))
      label_row <- c(names(nfc[i]) , rep("" , length(div_table) - 1))
      div_table <- rbind(label_row , div_table)
      conf_table <- rbind(conf_table , div_table)
      i <- i + 1
    }
    nfc_table <- conf_table

    if(combo == F){
      preseason_projection <- rbind(afc_table , nfc_table) %>% select(-c(RECORD , METHOD , OUTCOME , formula , features , run_time))
    } else {
      preseason_projection <- rbind(afc_table , nfc_table)
    }


    return(preseason_projection)
  }
  pred_season_table(PROJECTION)
}

# preseason_prediction("spread_line" , "glmboost" , 2024 , 2023)




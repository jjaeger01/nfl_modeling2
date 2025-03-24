source("~/Projects/nfl_modeling2/scripts/run_NFL_model().R")
# load("~/Projects/nfl_modeling2/data/PRED_MODELS.R")

# model <- run_NFL_model("home_away_score" , method__ = "knn" , test_season = 2023 , formula__ = "simple" , scale_ = T ,use_RFE = 1)

predict_week <- function(season_ , week_ , outcome , NFL_MODEL){
  # season_ <- 2023
  # week_ <- 15
  PREDICTED_WEEK <- tibble()

  outcome_ <- outcome
  pred_week_ <- pull_pred_week(season_ , week_ , outcome_) %>% left_join(game_info(.) %>% select(game_id , spread_quint) , by = join_by(game_id))
  # print(pred_week)
  if(nrow(pred_week_) > 0){
    if(outcome_ == "home_away_score"){
      predicted_week <-   make_predictions(pred_week_ ,
                                           outcome_ = outcome_ ,
                                           fit_ = NFL_MODEL$model_objects$model ,
                                           home_fit_ = NFL_MODEL$model_objects$home_fit_ ,
                                           away_fit_ = NFL_MODEL$model_objects$away_fit_ ,
                                           sparse_df = T ,
                                           use_model_scaling = T) %>%
        mutate(pred = as.numeric(as.character(pred)))
    }else{
      predicted_week <-   make_predictions(pred_week_ ,
                                           outcome_ = outcome_ ,
                                           fit_ = NFL_MODEL$model_objects$model ,
                                           sparse_df = T ,
                                           use_model_scaling = T) %>%
        mutate(pred = as.numeric(as.character(pred)))
    }

    PREDICTED_WEEK <- bind_rows(PREDICTED_WEEK , predicted_week)
  }


  PREDICTED_WEEK_full <- PREDICTED_WEEK %>%
    mutate(pred_cover = ifelse(outcome == "home_cover" , pred , pred_cover) ,
           pred = ifelse(outcome == "home_cover" , NA , pred) ,
           # pred_home_score = ifelse(spread_quint == 1 ,
           #                            predict.train(MODELS[["qnt_1_scores"]]$model_objects$home_fit_ , pred_week %>% filter(spread_quint == 1)) ,
           #                          ifelse(spread_quint == 2 ,
           #                                   predict.train(MODELS[["qnt_2_scores"]]$model_objects$home_fit_ , pred_week %>% filter(spread_quint == 2)) ,
           #                                 ifelse(spread_quint == 5 ,
           #                                          predict.train(MODELS[["qnt_5_scores"]]$model_objects$home_fit_ , pred_week %>% filter(spread_quint == 5)) ,
           #                          pred_home_score))) ,
           # pred_away_score = ifelse(spread_quint == 1 ,
           #                          predict.train(MODELS[["qnt_1_scores"]]$model_objects$away_fit_ , pred_week %>% filter(spread_quint == 1)) ,
           #                          ifelse(spread_quint == 2 ,
           #                                 predict.train(MODELS[["qnt_2_scores"]]$model_objects$away_fit_ , pred_week %>% filter(spread_quint == 2)) ,
           #                                 ifelse(spread_quint == 5 ,
           #                                        predict.train(MODELS[["qnt_5_scores"]]$model_objects$away_fit_ , pred_week %>% filter(spread_quint == 5)) ,
           #                                        pred_away_score)))
    )

  PRED_WEEK <- PREDICTED_WEEK_full %>%
    left_join(game_info(.) %>%
                select(game_id , home_team , away_team ) , by = join_by("game_id")
    ) %>%
    select(game_id , home_team , away_team , method , model_formula , feature_selection ,
           modeled_outcome = outcome ,
           point_spread ,
           pred = pred_cover ,
    ) %>%
    mutate(outcome = "home_cover")
  # print(PRED_WEEK)
  OUTLIST <- list()
  OUTLIST[["FULL_PRED"]] <- PREDICTED_WEEK_full
  OUTLIST[["PRED_WEEK_DISPLAY"]] <- PRED_WEEK
  # print(PRED_WEEK)
  return(OUTLIST)
}

# predicted_week <- predict_week(2023 , 5 , "home_away_score" , model)

# predicted_week$FULL_PRED %>% log_prediction(db_file = "test_week_pred.db" , table_ = "test_table")

# predict_week(current_season ,
#              current_week ,
#              model_$model_info$outcome ,
#              model_)



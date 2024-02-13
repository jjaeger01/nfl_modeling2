source("~/Projects/nfl_modeling2/scripts/run_NFL_model().R")
pred_week <- function(season_ ,
                      week_ ,
                      outcome__ ,
                      outcome_type_ ,
                      method_  ,
                      ensemble_ ){

  prediction_model <- run_NFL_model(outcome__ ,
                                    model_runs_ = 1 ,
                                    outcome_type_ = outcome_type_ ,
                                    method__  = method_,
                                    resample_method_ = "repeatedcv",
                                    resample__  = 5 ,
                                    repeats__  = 5 ,
                                    clusters_ = 10 ,
                                    test_season = season_ ,
                                    ensemble = F ,
                                    formula__ = "simple" ,
                                    log_model_run_ = F ,
                                    pre_process_ = NA ,
                                    avg_vars_ = "full")



  pred_week <- build_current_week(season_ , week_ , outcome_ = outcome__)
  print(outcome__)
  out_ <- pred_outcomes(pred_week ,
                outcome_ = outcome__ ,
                outcome_type =  outcome_type_ ,
                fit_ = prediction_model$model_objects[[1]]$model) %>%
    mutate(method = method_ ,
           ensemble = ensemble_ ,
           outcome = as.character(outcome__)) %>%
    select(game_id , season , week , home_team , away_team ,
           home_score , away_score , result ,
           spread_line , linedif , home_cover ,
           pred , pred_cover , pred_win , pred_home_score , pred_away_score ,
           model_win ,
           outcome , method , ensemble)
  return(out_)
}

for(model_ in c("glm" , "glmnet" , "rf" , "xgbTree" , "glmboost" , "LogitBoost")){
  print(pred_week(2023,22, "home_cover" , "cat" , model_ , ensemble_ = F))
}

for(model_ in c("lm" , "glmnet" , "rf" , "xgbTree" , "glmboost")){
  print(pred_week(2023,22, "result" , "cont" , model_ , ensemble_ = F))
}

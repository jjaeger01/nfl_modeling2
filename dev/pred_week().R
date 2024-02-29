pred_week <- function(outcome__ = "result"  ,
                      outcome_type__ = "cont"  ,
                      model_runs__ = 1  ,
                      method___ = "lm"  ,
                      test_season_ = 2023  ,
                      pred_season_ = 2023  ,
                      week__ = 10 ,
                      ensemble_ = F ,
                      log_model_run__ = F ,
                      eval_pred_ = T ,
                      log_prediction_ = T){

  trained_model <- run_NFL_model(
    outcome_ = outcome__ ,
    outcome_type_ = outcome_type__ ,
    model_runs_ = model_runs__ ,
    method__ = method___ ,
    test_season = test_season_ ,
    ensemble = ensemble_ ,
    log_model_run_ = log_model_run__)

  if(outcome__ == "home_away_score"){
    home_fit <- trained_model$model_objects[[1]]$home_fit_
    away_fit <- trained_model$model_objects[[1]]$away_fit_
  }

  pred_week <- pull_pred_week(season_ = pred_season_ ,
                              week_ = week__ ,
                              outcome_ = outcome__)

  # predict(trained_model$model_objects[[1]]$model , pred_week , type = "prob")

  predictions_df <- pred_outcomes(pred_week ,
                                  outcome_ = outcome__ ,
                                  outcome_type = outcome_type__ ,
                                  fit_ = trained_model$model_objects[[1]]$model ,
                                  eval_pred = F)
  if(outcome__ == "home_away_score"){
    predictions_df <- pred_outcomes(pred_week ,
                                    outcome_ = outcome__ ,
                                    outcome_type = outcome_type__ ,
                                    home_fit_ = home_fit ,
                                    away_fit_ = away_fit ,
                                    eval_pred = F)
  }
  if(log_prediction_ == T){
    log_prediction(predictions_df_ = predictions_df ,
                   table_ = "predictions_v0")
  }

  out_ <- predictions_df

  if(eval_pred_ == T){
    predictions_results_df <-  pred_outcomes(predictions_df ,
                                             fit_ = NULL ,
                                             outcome_ = outcome__ ,
                                             outcome_type = outcome_type__ ,
                                             eval_pred = eval_pred_ ,
                                             make_predictions = F)
    out_ <- predictions_results_df
  }
  return(out_)
}

# pred_week(outcome__ = "home_cover" ,
#           outcome_type__ = "cat" ,
#           model_runs__ = 5 ,
#           method___ = "glm" ,
#           test_season_ = 2022 ,
#           pred_season_ = 2023 ,
#           week__ = 10 ,
#           log_model_run__ = T ,
#           log_prediction_ = T ,
#           ensemble_ = T) %>% pull(model_win)  %>% mean()
#
#
# pred_week(outcome__ = "home_away_score" ,
#           outcome_type__ = "cont" ,
#           model_runs__ = 5 ,
#           method___ = "lm" ,
#           test_season_ = 2022 ,
#           pred_season_ = 2023 ,
#           week__ = 10 ,
#           log_model_run__ = T ,
#           log_prediction_ = T ,
#           ensemble_ = T) %>% pull(model_win)  %>% mean()

outcome__ = "home_away_score"
outcome_type__ = "cont"
model_runs__ = 1
method___ = "lm"
test_season_ = 2022
pred_season_ = 2023
week__ = 10
log_model_run__ = T
log_prediction_ = T
ensemble_ = F







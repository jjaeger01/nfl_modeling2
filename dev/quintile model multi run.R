source("~/Projects/nfl_modeling2/scripts/run_NFL_model().R")
quintile_models_multi <- function(outcomes ,
                                  methods_ = NULL,
                                  seasons = 2020:2023 ,
                                  formula_ = "simple" ,
                                  custom_formula_ = NULL ,
                                  log_db_file_ = NULL,
                                  log_ = F ,
                                  log_in_table_ = NULL ,
                                  log_pred_db_file_ = NULL ,
                                  log_pred_ = F ,
                                  log_pred_in_table_ = NULL){
  class_outcomes <- c("home_cover" , "home_win")
  reg_outcomes <- c("result" , "linedif" , "spread_line" , "home_away_score")
  class_methods <- c("bayesglm" , "glm" ,  "glmnet" , "LogitBoost" , "glmboost" , "bstTree" , "xgbTree") # , "xgbDART")
  reg_methods <- c("bayesglm" , "lm" , "lmStepAIC" ,"BstLm" , "glmnet"  , "glmboost" , "xgbTree" , "xgbLinear") # , "xgbDART" , "svmLinear3" , "svmPoly")

  for(season_ in seasons){
    for(oc in outcomes){
      if(is.null(methods_)){
        if(oc %in% class_outcomes){
          methods <- class_methods
        }
        if(oc %in% reg_outcomes){
          methods <- reg_methods
        }
      }else{
        methods <- methods_
      }
      print(paste("Running models for" , oc , "using" , paste(methods , collapse = " , ")))
      for(model_ in methods){
        # print(i)
        use_rfe_ = ifelse(model_ %in% c("lm" , "glm" , "bayesglm" , "svmLinear3" , "svmPoly") , T , F)
        resample <- 10 # ifelse(model_ %in% c("xgbTree") , 10 , 5)
        repeat_ <- 10 # ifelse(model_ %in% c("xgbTree") , 10 , 5)

        train_ <- games %>% select(game_id , spread_quint) %>%
          left_join(create_modeldata(oc)) %>% na.omit() %>%
          filter(substr(game_id , 1 , 4) < season_)

        test_ <- games %>% select(game_id , spread_quint) %>%
          left_join(create_modeldata(oc)) %>% na.omit() %>%
          filter(substr(game_id , 1 , 4) == season_)
        output <- list()
        model_outlist <- list()
        TEST_RESULTS_DF <- tibble()
        for(i in 1:5){
          # print("Train quintiles")
          # print(quantile(train_$spread_line , probs = seq(0 , 1 , .20) , na.rm = T))
          # print("Test quintiles")
          # print(quantile(test_$spread_line , probs = seq(0 , 1 , .20) , na.rm = T))

          quant_bounts <- train_ %>%
            filter(spread_quint == i) %>%
            summarise(lower = min(spread_line) ,
                      upper = max(spread_line))

          cat(paste(paste("Quintile #" , i , sep = "") ,
                    paste("\n Spread_line min:" , quant_bounts$lower , "\n" , "Spread_line max:" , quant_bounts$upper) , "\n")
              )
          outmodel<- run_NFL_model(outcome_ = oc ,
                                    method__  = model_,
                                    test_season = season_ ,
                                    formula__ = formula_ ,
                                    log_model_run_ = log_ ,
                                    log_in_table = paste(log_in_table_ , i , sep = "_") ,
                                    log_db_file = log_db_file_ ,
                                    log_pred = log_pred_ ,
                                    log_pred_in_table = paste(log_pred_in_table_ , i , sep = "_") ,
                                    log_pred_db_file = log_pred_db_file_ ,
                                    train_data_ = train_ %>% filter(spread_quint == i) ,
                                    test_data_ = test_ %>% filter(spread_quint == i)
                                  )
          quant <- paste("q" , i , sep = "")
          model_outlist[[quant]] <- outmodel
          TEST_RESULTS_DF <- bind_rows(TEST_RESULTS_DF , outmodel$test_result_df)
        }
      print(paste("OVERALL ATS TEST ACCURACY:" , TEST_RESULTS_DF %>% calc_ATS()))
      }
    }
  }
}

# quintile_models_multi("result")

for(oc_ in c("result" , "linedif" , "home_away_score" , "home_cover" , "home_win")){
  print(oc_)
  quintile_models_multi(oc_ ,
                        methods_ = NULL,
                        seasons = 2020:2023 ,
                        formula_ = "simple" ,
                        custom_formula_ = NULL ,
                        log_db_file_ = "data/quintile_model_runs.db" ,
                        log_ = T ,
                        log_in_table_ = paste("quintile_" , oc_ , "_2024_05_20" ,  sep = "") ,
                        log_pred_db_file_ = "data/quintile_model_preds.db" ,
                        log_pred_ = T ,
                        log_pred_in_table_ = paste("quintile_pred_" , oc_ , "_2024_05_20" ,  sep = "")
                        )
}


for(oc_ in c("home_cover")){
  print(oc_)
  quintile_models_multi(oc_ ,
                        methods_ = c("bayesglm" , "glm" ,  "glmnet" , "LogitBoost" , "glmboost") ,
                        seasons = 2020:2023 ,
                        formula_ = "simple" ,
                        custom_formula_ = NULL ,
                        log_db_file_ = "data/quintile_model_runs.db" ,
                        log_ = T ,
                        log_in_table_ = paste("quintile_" , oc_ , "_2024_05_20" ,  sep = "") ,
                        log_pred_db_file_ = "data/quintile_model_preds.db" ,
                        log_pred_ = T ,
                        log_pred_in_table_ = paste("quintile_pred_" , oc_ , "_2024_05_20" ,  sep = "")
  )
}


# paste("QUINTLE_" , toupper(oc) , "_2024_05_20_" , i , sep = "") ,
# paste("QUINTLE_", toupper(oc) ,"_PRED_2024_05_20_" , i , sep = "")

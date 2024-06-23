source("~/Projects/nfl_modeling2/scripts/run_NFL_model().R")

multi_run_NFL <- function(outcomes ,
                          methods_ = NULL,
                          seasons = 2020:2021 ,
                          formula_ = "simple" ,
                          custom_formula_ = NULL ,
                          log_ = F ,
                          log_in_table_ = NULL ,
                          log_pred_ = F ,
                          log_pred_in_table_ = NULL
                          ){

  class_outcomes <- c("home_cover" , "home_win")
  reg_outcomes <- c("result" , "linedif" , "spread_line" , "home_away_score")
  class_methods <- c("bayesglm" , "glm" ,  "glmnet" , "LogitBoost" , "glmboost" , "bstTree" , "xgbTree") # , "xgbDART")
  reg_methods <- c("bayesglm" , "lm" , "lmStepAIC" ,"BstLm" , "glmnet"  , "glmboost" , "xgbTree" , "xgbLinear") # , "xgbDART" , "svmLinear3" , "svmPoly")

  # Categorical ####
  for(i in seasons){
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
        model_run <- run_NFL_model(oc ,
                                   clusters_ = 10 ,
                                   method__ = model_ ,
                                   test_season = i ,
                                   resample__  = resample ,
                                   repeats__  = repeat_ ,
                                   formula__ = formula_ ,
                                   custom_formula = custom_formula_ ,
                                   log_model_run_ = log_ ,
                                   use_RFE =   use_rfe_ ,
                                   scale_ = T ,
                                   log_in_table = log_in_table_ ,
                                   log_pred = log_pred_ ,
                                   log_pred_in_table = log_pred_in_table_ )
        model_name <- as.character(paste(i , "model" , sep = "_"))
        # model_runs[[model_name]] <- model_run
      }
    }
  }
}


# multi_run_NFL(outcomes = c("result" , "linedif") , formula_ = "custom_int" , log_ = F , log_pred_ = F ,
#               seasons = 2019:2023 ,
#               methods_ = c("bayesglm" , "lm" , "lmStepAIC" ,"BstLm" ,
#                            "glmnet"  , "glmboost" , "xgbTree" , "xgbLinear")
#               )







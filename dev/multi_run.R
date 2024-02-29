source("~/Projects/nfl_modeling2/scripts/run_NFL_model().R")

# # Continuous outcomes ####
# for(i in 2018:2023){
#   for(oc in c("home_away_score" , "result" , "linedif")){
#     for(model_ in c("lm" , "lmStepAIC" ,"BstLm" , "glmnet"  , "glmboost"  , "bstTree" , "xgbTree" , "xgbDART")){
#       print(i)
#       model_run <- run_NFL_model(oc ,
#                                  clusters_ = 10 ,
#                                  model_runs_ =  1,
#                                  outcome_type_ = "cont" ,
#                                  method__ = model_ ,
#                                  test_season = i ,
#                                  resample__  = 5 ,
#                                  repeats__  = 5 ,
#                                  ensemble = F ,
#                                  formula__ = "simple" ,
#                                  log_model_run_ = T)
#       model_name <- as.character(paste(i , "model" , sep = "_"))
#       # model_runs[[model_name]] <- model_run
#     }
#   }
# }
#
# # Categorical ####
# for(i in 2018:2023){
#   for(oc in c("home_cover" , "home_win")){
#     for(model_ in c("glm" , "glmStepAIC" ,"rf" , "glmnet" , "LogitBoost" , "glmboost" , "xgbTree" ,  "bstTree" , "xgbDART")){
#       print(i)
#       model_run <- run_NFL_model(oc ,
#                                  clusters_ = 10 ,
#                                  model_runs_ =  1,
#                                  outcome_type_ = "cat" ,
#                                  method__ = model_ ,
#                                  test_season = i ,
#                                  resample__  = 5 ,
#                                  repeats__  = 5 ,
#                                  ensemble = F ,
#                                  formula__ = "simple" ,
#                                  log_model_run_ = T)
#       model_name <- as.character(paste(i , "model" , sep = "_"))
#       # model_runs[[model_name]] <- model_run
#     }
#   }
# }

# Categorical Ensemble

for(i in 2018:2023){
  for(oc in c("home_cover" , "home_win")){
    for(j in c(5, 10 , 20 , 50 , 100)){
      for(model_ in c("xgbTree" ,  "bstTree")){
        print(i)
        model_run <- run_NFL_model(oc ,
                                   clusters_ = 10 ,
                                   model_runs_ =  j ,
                                   outcome_type_ = "cat" ,
                                   method__ = model_ ,
                                   test_season = i ,
                                   resample__  = 5 ,
                                   repeats__  = 5 ,
                                   ensemble = F ,
                                   formula__ = "simple" ,
                                   log_model_run_ = T)
        model_name <- as.character(paste(i , "model" , sep = "_"))
        # model_runs[[model_name]] <- model_run
      }
    }
  }
}



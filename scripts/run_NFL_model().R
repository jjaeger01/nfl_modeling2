if(!exists("pbp")){
  source("~/Projects/nfl_modeling2/scripts/NFL Data Set Up_full.R")
}
source("~/Projects/nfl_modeling2/Scripts/NFL_rfe().R")
source("~/Projects/nfl_modeling2/Scripts/NFL Functions.R")


run_NFL_model <- function(outcome_ = "result" ,
                          method__  = "lm",
                          test_season = 2021 ,
                          formula__ = "simple" ,
                          custom_formula = NULL ,
                          log_model_run_ = F ,
                          log_in_table = "model_runs_v5" ,
                          log_db_file = NULL ,
                          log_pred = F ,
                          log_pred_in_table = "pred_table" ,
                          log_pred_db_file = NULL ,
                          resample_method_ = "repeatedcv",
                          resample__  = 5 ,
                          repeats__  = 5 ,
                          clusters_ = 10 ,
                          train_seasons = NULL ,
                          scale_ = F ,
                          avg_vars_ = "full" ,
                          use_RFE = F ,
                          funcs = NULL ,
                          train_data_ = NULL ,
                          test_data_ = NULL){
  library(caret)
  library(doParallel)
  set.seed(825)
  outlist <- list()

  outcome__ <- outcome_
  if(outcome_ == "home_away_score"){
    outcome__ <-  c("home_score" , "away_score")
  }
  modeldata <- create_modeldata(c(outcome_)) %>% na.omit() # Model data created####

  # Model run loop ####
  start_time <- Sys.time()
  cat(paste("Running models, starting at" , start_time , "\n" , "Outcome:" , outcome_ , "\n" ,  "Method:" , method__ , "\n" ,"Season:" , test_season , "\n"))

  if(!is.null(train_seasons)){
    train__ <- modeldata %>%  filter(substr(game_id , 1 , 4) %in% train_seasons)
  }
  train__ <- modeldata %>%  filter(substr(game_id , 1 , 4) < min(test_season))
  if(!is.null(train_data_)){
    train__ <- train_data_
  }
  # train__.x <- train__ %>% select(-game_id , -outcome_) %>% as.data.frame()
  # train__.y <- train__[[outcome_]]
  test_ <- modeldata %>% na.omit() %>% filter(substr(game_id , 1 , 4) %in% test_season)
  if(!is.null(test_data_)){
    test_ <- test_data_
  }
  if(scale_ == T){
    preProcValues <- preProcess(train__ %>% select(-c("game_id" , "spread_line" , outcome__)),
                                method = c("center", "scale")
                                )
    train__ <- predict(preProcValues , train__)
    test_ <- predict(preProcValues , test_)
  }else{
    preProcValues <- NULL
  }

  # Set up clusters ####
  cl <- makePSOCKcluster(clusters_)
  registerDoParallel(cl)

  # Set caret parameters ####
  fitControl <- trainControl(## 10-fold CV
    method = resample_method_,
    number = resample__,
    savePredictions = T ,
    ## repeated ten times
    repeats = repeats__)
  # Create model formula based on outcome
  home_fit <- NA
  away_fit <- NA
  j <- 2
  train_ <- train__
  for(outcome___ in outcome__){
    if(length(outcome__) > 1){
      train__ <- train_ %>% select(-outcome__[j])
    }
    if(is.factor(modeldata[[outcome___]]) == F){
      outcome_type_ <- "cont"
    } else{
      outcome_type_ <- "cat"
    }
    if(formula__ == "simple"){
      model_formula_ <- paste(outcome___ , " ~ .")
      model_formula <- formula(model_formula_)
    }
    if(formula__ == "int"){
      model_formula_ <- paste(outcome___ , " ~ .^2")
      model_formula <- formula(model_formula_)
    }
    if(formula__ == "custom_int"){
      source("~/Projects/nfl_modeling2/dev/build int formula.R")
      model_formula_ <- build_formula(outcome___ ,  other_cols = c(home_int , away_int) )
      custom_formula <- formula(model_formula_)
    }
    if(!is.null(custom_formula)){
      model_formula <- formula(custom_formula)
    }
    if(use_RFE == T){
      if(is.factor(train__[[outcome___]]) == F ){
        outcome_type_ <- "cont"
        if(is.null(funcs)){
          rfe_funcs <- lmFuncs
          funcs <- "lm"
        }
      } else{
        outcome_type_ <- "cat"
        if(is.null(funcs)){
          rfe_funcs <- lrFuncs
          funcs <- "lr"
        }
      }
      if(!is.null(funcs)){
        if(funcs == "rf"){
          rfe_funcs <- rfFuncs
        }
        if(funcs == "nb"){
          rfe_funcs <- nbFuncs
        }
      }
      print("Running RFE...")
      rfe_ctrl <-rfeControl(functions = rfe_funcs,
                            allowParallel = T ,
                            method = "repeatedcv" ,
                            verbose = F ,
                            number = 10)
      rfe_run <- rfe(x = train__[-c(1:2)] ,
                     y = train__ %>% pull(outcome___) ,
                     verbose = T ,
                     rfeControl = rfe_ctrl ,
                     sizes = 1:ncol(train__[-1]))



      model_formula_ <- build_formula(outcome___ ,
                                     other_cols = rfe_run$optVariables ,
                                     full_cols = F)



      model_formula <- formula(model_formula_)
    }
    if(formula__ == "custom_int"){
      model_formula <- formula(str_replace_all(model_formula_ , "\\.avg\\." , ".avg:"))
    }

    print(model_formula)

    # Fit model ####
    fit_ <- train(model_formula,
                  data = train__[-1] ,
                  method = method__ ,
                  trControl = fitControl)
    if(outcome_ == "home_away_score"){
      if(outcome___ == "home_score"){
        home_fit <- fit_
      }
      if(outcome___ == "away_score"){
        away_fit <- fit_
      }
    }
    j <- j - 1
  }
  stopCluster(cl)
  # Use pred_outcome() to apply model to test dataset, generate predictions, and evaluate
  results_df.train <- eval_pred(
    make_predictions(test_data = train__ ,
                     outcome_ = outcome_ ,
                     fit_ = fit_ ,
                     home_fit_ = home_fit ,
                     away_fit_ = away_fit
                     )
    )
  results_df <- eval_pred(
    make_predictions(test_data = test_ ,
                     outcome_ = outcome_ ,
                     fit_ = fit_ ,
                     home_fit_ = home_fit ,
                     away_fit_ = away_fit)
    )
  # print(results_df)
  accuracy <- mean(results_df$model_win)
  accuracy_ns <- mean(results_df$model_win_ns)
  accuracy.train <- mean(results_df.train$model_win)
  accuracy_ns.train <- mean(results_df.train$model_win_ns)

  if(log_pred == T){
    pred_df <-     make_predictions(test_data = test_ ,
                                    outcome_ = outcome_ ,
                                    fit_ = fit_ ,
                                    home_fit_ = home_fit ,
                                    away_fit_ = away_fit ,
                                    sparse_df = T)

    log_prediction(pred_df ,
                   db_file = log_pred_db_file ,
                   table_ = log_pred_in_table)
  }

  NFL_model <- list(model = fit_,
                    results_df = results_df ,
                    results_df.train = results_df.train ,
                    accuracy = accuracy ,
                    accuracy_ns = accuracy_ns ,
                    accuracy.train = accuracy.train ,
                    accuracy_ns.train = accuracy_ns.train ,
                    home_fit_ = home_fit ,
                    away_fit_ = away_fit ,
                    pre_process = preProcValues)

  stop_time <- Sys.time()
  runtime <- round(as.numeric(difftime(stop_time , start_time , units = "mins")) , 2)
  print(paste("Total Runtime" , runtime , "mins"))
  print(paste("ATS Accuracy predicting " , outcome_ , "using " , method__ , "is " , round(accuracy , digits = 3) ))

  # Extract model info ####
  model_info <- bind_cols(outcome = outcome_ ,
                          method = method__ ,
                          accuracy_ATS = accuracy ,
                          accuracy_W_L = accuracy_ns ,
                          accuracy_ATS.train = accuracy.train ,
                          accuracy_W_L.train = accuracy_ns.train ,
                          runtime = runtime ,
                          test_season = test_season ,
                          run_date = as.character(start_time) ,
                          used_RFE = use_RFE ,
                          formula = model_formula_ ,
                          features = paste(colnames(NFL_model$model$trainingData[-1]) , collapse = " , ") ,
                          resample_method = resample_method_ ,
                          cv_folds = resample__ ,
                          repeats = repeats__ ,
                          scale_ = scale_ ,
                          avg_vars_ = "full"
  )
  # print(model_info$features)
  result_list <- list(model_objects = NFL_model ,
                      formula = model_formula ,
                      train_data = train__ ,
                      test_data = test_ ,
                      test_result_df = NFL_model$results_df ,
                      results_df.train = NFL_model$results_df.train

  )
  model_metric_result_df <- results_df

  # print(model_metric_result_df)

  confusion_matrix <- confusionMatrix(factor(model_metric_result_df$pred_cat_) ,
                                      factor(model_metric_result_df$outcome_) , positive = "1")
  model_metrics <- data.frame(as.list(c(confusion_matrix$overall , confusion_matrix$byClass))) %>%
    select(Accuracy , Kappa , AccuracyLower ,
           AccuracyUpper , Sensitivity , Specificity ,
           Precision , Recall , Prevalence ,
           Detection.Rate , Detection.Prevalence)

  if(outcome_type_ == "cat"){
    model_metrics <- model_metrics %>%
      mutate(RMSE = NA ,
             Rsquared = NA ,
             MAE = NA)
  }

  if(outcome_type_ == "cont"){
    if(outcome_ == "home_away_score"){
      outcome_ <- "result"
    }
    model_metrics <- bind_cols(model_metrics ,
                               data.frame(as.list(postResample(pred = model_metric_result_df$pred , obs = model_metric_result_df[[outcome_]]))) )
  }

  result_list[["model_info"]] <- bind_cols(model_info , model_metrics)
  # Log model run ####
  if(log_model_run_ == T){
    log_model_run(result_list , db_file = log_db_file ,
                  table_ = log_in_table)
  }
  # print(model_info$)
  return(result_list)
}

# # Test ####
# model_run <- run_NFL_model(outcome_ = "home_away_score" ,
#               method__ = "xgbTree" ,
#               use_RFE = T ,
#               funcs = "lm" ,
#               formula__ = "simple" ,
#               test_season = 2023 ,
#               scale_ = T )


# custom_formula =  build_formula("linedif" , drop_cols = "spread_line" , other_cols = c(home_int , away_int) ))

# # Stacked model
#
# stacked_result_model <-  run_NFL_model(outcome_ = "result" ,
#                                         method__  = "glmnet",
#                                         resample_method_ = "repeatedcv",
#                                         resample__  = 5 ,
#                                         repeats__  = 5 ,
#                                         clusters_ = 5 ,
#                                         train_seasons = "2019:2021" ,
#                                         test_season = "2022:2023" ,
#                                         formula__ = "simple" ,
#                                         custom_formula = ensemble_formula ,
#                                         log_model_run_ = F ,
#                                         log_in_table = "stacked_models_v0_1" ,
#                                         scale_ = F ,
#                                         avg_vars_ = "full" ,
#                                         log_pred = F ,
#                                         log_pred_in_table = "stacked_models_results_v1" ,
#                                         use_RFE = F ,
#                                         train_data_ = ensemble_data %>% filter(substr(game_id , 1 , 4) %in% 2019:2021) ,
#                                         test_data_ = ensemble_data %>% filter(substr(game_id , 1 , 4) %in% 2022)
#                                        )


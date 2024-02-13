if(!exists("pbp")){
  source("~/Projects/nfl_modeling2/scripts/NFL Data Set Up_full.R")
}

# Turn multi-runs into a function ?

# outcome_ = "home_away_score"
# clusters_ = 10
# model_runs_ =  1
# outcome_type_ = "cont"
# method__ = "lm"
# test_season = 2023
# resample__  = 5
# repeats__  = 5
# ensemble = F
# formula__ = "simple"
# log_model_run_ = F
# resample_method_ = "repeatedcv"
# pre_process_ = NA
# avg_vars_ = "full"

run_NFL_model <- function(outcome_ = "home_away_score" ,
                          model_runs_ = 10 ,
                          outcome_type_ = "cont" ,
                          method__  = "lm",
                          resample_method_ = "repeatedcv",
                          resample__  = 5 ,
                          repeats__  = 5 ,
                          clusters_ = 10 ,
                          test_season = 2021 ,
                          ensemble = T ,
                          formula__ = "simple" ,
                          log_model_run_ = T ,
                          pre_process_ = NA ,
                          avg_vars_ = "full"
                          ){
  library(caret)
  library(doParallel)
  set.seed(825)
  outlist <- list()

  outcome__ <- outcome_
  if(outcome_ == "home_away_score"){
    outcome__ <-  c("home_score" , "away_score")
  }
  modeldata <- create_modeldata(c(outcome__)) %>% na.omit() # Model data created####

  # Model run loop ####
  start_time <- Sys.time()
  cat(paste("Running models, starting at" , start_time , "\n" , "Method:" , method__ , "\n" ,"Season:" , test_season , "\n" , "Number of runs:" , model_runs_ , "\n\n"))
  for(i in 1:model_runs_){
    print(paste("Model Run:" , i , sep = ""))

    ## Test / train split ####

    # indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
    # train__ <- modeldata %>% na.omit() %>% dplyr::slice(indices)
    # test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)

    train__ <- modeldata %>%  filter(substr(game_id , 1 , 4) < test_season)
    if(outcome_type_ == "cat"){ #
      train__[[outcome_]] <- factor(train__[[outcome_]])
    }
    # train__.x <- train__ %>% select(-game_id , -outcome_) %>% as.data.frame()
    # train__.y <- train__[[outcome_]]

    test_ <- modeldata %>% na.omit() %>% filter(substr(game_id , 1 , 4) == test_season)
    # Set up clusters ####
    cl <- makePSOCKcluster(clusters_)
    registerDoParallel(cl)


    # Set caret parameters ####
    fitControl <- trainControl(## 10-fold CV
      method = resample_method_,
      number = resample__,
      ## repeated ten times
      repeats = repeats__)
    # Create model formula based on outcome
    home_fit <- NA
    away_fit <- NA
    j <- 2
    for(outcome___ in outcome__){
      if(formula__ == "simple"){
        model_formula_ <- paste(outcome___ , " ~ .")
        model_formula <- formula(model_formula_)
      }
      if(formula__ == "int"){
        model_formula_ <- paste(outcome___ , " ~ .^2")
        model_formula <- formula(model_formula_)
      }
      if(length(outcome__) > 1){
        train__ <- modeldata %>%  filter(substr(game_id , 1 , 4) < test_season)
        train__ <- train__ %>% select(-outcome__[j])
      }

    # Fit model ####
      fit_ <- train(# x = train__.x ,
                    # y = train__.y ,
                    model_formula,
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
    results_df <- pred_outcomes(test = test_ , outcome_ = outcome_ , outcome_type = outcome_type_ , fit_ = fit_ , home_fit_ = home_fit , away_fit_ = away_fit)
    accuracy <- mean(results_df$model_win)
    accuracy_ns <- mean(results_df$model_win_ns)

    out <- list(model = fit_, results_df = results_df , accuracy = accuracy , accuracy_ns = accuracy_ns)
    outlist[[i]] <-  out
  }
  stop_time <- Sys.time()
  runtime <- round(as.numeric(difftime(stop_time , start_time , units = "mins")) , 2)
  print(paste("Total Runtime" , runtime , "mins"))

  # Extract accuracies from model run(s) ####
  accuracies <- c()
  accuracies_ns <- c()
  for(i in 1:model_runs_){
    accuracies <- c(accuracies , outlist[[i]]$accuracy)
    accuracies_ns <- c(accuracies_ns , outlist[[i]]$accuracy_ns)

  }
  average_accuracy_ <- mean(accuracies)
  average_accuracy_ns <- mean(accuracies_ns)
  print(paste("Average accuracy predicting " , outcome_ , "using " , method__ , "across " , model_runs_, "model run(s) is " , average_accuracy_))

  # Extract model info ####
  model_info <- bind_cols(outcome = outcome_ ,
                      method = method__ ,
                      runs = model_runs_ ,
                      accuracy_ = average_accuracy_ ,
                      accuracy_ns = average_accuracy_ns ,
                      runtime = runtime ,
                      test_season = test_season ,
                      ensemble_threshold = NA ,
                      ensemble_test_accuracy = NA ,
                      run_date = as.character(start_time) ,
                      formula = model_formula_ ,
                      features = paste(colnames(train__[3:ncol(train__)]) , collapse = ",") ,
                      resample_method = resample_method_ ,
                      cv_folds = resample__ ,
                      repeats = repeats__ ,
                      pre_process = pre_process_ ,
                      avg_vars_ = "full"
                      )

  result_list <- list(model_objects = outlist ,
                      formula = model_formula ,
                      train_data = train__ ,
                      test_result_df = outlist[[1]]$results_df
                      )
  # accuracies_w_l <- c()
  # for(i in 1:15){
  #   accuracies_w_l <- c(accuracies_w_l , mean(outlist[[i]]$results_df$model_win_ns))
  # }

  # Generate ensemble predictions ####
  if(ensemble == T){ ## Extract predictions from each model run,  compile into one dataframe ####
    ensemble_predictions <- tibble(train__ %>% game_outcomes() %>% dplyr::select(game_id , result , spread_line , outcome_))
    for(i in 1:model_runs_){
      pred <- ensemble_predictions %>% mutate(pred = as.numeric(as.character(predict(outlist[[i]]$model , train__)))) %>% dplyr::select(game_id , pred)
      ensemble_predictions <- ensemble_predictions %>% left_join(pred , by = "game_id" , suffix = c("",i))
    }
    ## Find optimal threshold ####
    threshold_mean_model_wins <- tibble()
    i <- 0
    if(outcome_type_ == "cat"){
      while(i < 1){ # Find optimal threshold for predicting 1 based on the average of all ensemble_predictions [0 , 1]
        threshold_mean_model_wins <- bind_rows(threshold_mean_model_wins,
                                               bind_cols( threshold = i ,
                                                          model_win = ensemble_predictions %>%
                                          mutate(linedif = spread_line + result ,
                                                 avg_pred = rowMeans(ensemble_predictions %>% dplyr::select(matches("pred"))) ,
                                                 pred_ = ifelse(avg_pred > i , 1 , 0) ,
                                                 does_count = ifelse(avg_pred >= 0.8 , T , F) ,
                                                 model_win = ifelse(pred_ == .[outcome_] , 1 , 0)
                                          )  %>%
                                        pull(model_win) %>% mean()
                                               )
          # View()
          # filter(avg_pred == 0.8) %>%
          # View(".8")
          # tabyl(model_win , pred_) %>% adorn_totals("row")
        )
        i <- i + 0.01
      }

      colnames(threshold_mean_model_wins) <- c("threshold" , "model_win")
      best_thresh <- min(threshold_mean_model_wins$threshold[threshold_mean_model_wins$model_win == max(threshold_mean_model_wins$model_win)])
      print(paste("Optimal threshold for ensemble prediction is " , best_thresh))
    }
    ## Apply models to test data using pred_outcomes() ####
    ensemble_predictions_test <- tibble(test_ %>% game_outcomes() %>% dplyr::select(game_id , result , spread_line , outcome_))
    for(i in 1:model_runs_){
      pred <- test_ %>% mutate(pred = as.numeric(as.character(predict(outlist[[i]]$model , test_)))) %>% dplyr::select(game_id , pred)
      ensemble_predictions_test <- ensemble_predictions_test %>% left_join(pred , by = "game_id" , suffix = c("",i))
    }

    ensemble_results_df <- pred_outcomes(ensemble_predictions_test ,
                                         ensemble_ = T ,
                                         outcome_ = outcome_ ,
                                         outcome_type = outcome_type_ ,
                                         ensemble_threshold = best_thresh)
    ensemble_test_accuracy <- ensemble_results_df %>% pull(model_win) %>% mean()
    print(paste("Accuracy predicting" , outcome_ , "using ensemble of" , model_runs_ ,  method__ , "model run(s) is" , ensemble_test_accuracy))
    ensemble_confusion_matrix <- confusionMatrix(factor(ensemble_results_df$pred_cover) , factor(ensemble_results_df$home_cover) , positive = "1")

    ## Add ensemble stuff to result list ####
    result_list[["ensemble_threshold"]] <- ifelse(exists("best_thresh") , best_thresh , NA)
    result_list[["ensemble_predictions"]] <-  ensemble_predictions_test
    result_list[["ensemble_result_df"]] <- ensemble_results_df
    result_list[["ensemble_test_accuracy"]] <- ifelse(exists("ensemble_test_accuracy") , ensemble_test_accuracy , NA)
    result_list[["thesholds"]] <- threshold_mean_model_wins
    model_info[["ensemble_threshold"]] <- ifelse(exists("best_thresh") , best_thresh , NA)
    model_info[["ensemble_test_accuracy"]] <- ifelse(exists("ensemble_test_accuracy") , ensemble_test_accuracy , NA)

  }
    # View()
    # tabyl(model_win , pred_)
  model_metric_result_df <- results_df
  if(ensemble == T){
    model_metric_result_df <- ensemble_results_df
  }
  confusion_matrix <- confusionMatrix(factor(model_metric_result_df$pred_cat_) , factor(model_metric_result_df$outcome_) , positive = "1")
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
    model_metrics <- bind_cols(model_metrics , data.frame(as.list(postResample(pred = model_metric_result_df$pred , obs = model_metric_result_df[[outcome_]]))) )
  }
  result_list[["model_info"]] <- bind_cols(model_info , model_metrics)
  # Log model run ####
  if(log_model_run_ == T){
    log_model_run(result_list)
  }
  return(result_list)
}


# model_run <- run_NFL_model("home_win" ,
#                           clusters_ = 10 ,
#                           model_runs_ =  1 ,
#                           outcome_type_ = "cat" ,
#                           method__ = "glm" ,
#                           test_season = 2023,
#                           resample__  = 5 ,
#                           repeats__  = 5 ,
#                           ensemble = F,
#                           formula__ = "simple" ,
#                           log_model_run_ = F)

# confusionMatrix(model_run$test_result_df$raw_pred , factor(model_run$test_result_df[[model_run$model_info$outcome]]) , positive = "1")

# model_run_log1 <- enframe(test_run) %>% pivot_wider()
# test_run2 <- run_NFL_model("home_cover" , model_runs_ =  5 , outcome_type_ = "cat" , method__ = "glm" , test_season = 2023 , ensemble = T)
# model_run_log2 <- enframe(test_run2) %>% pivot_wider()
# data_base <- model_run_log1 %>% bind_rows(model_run_log2)



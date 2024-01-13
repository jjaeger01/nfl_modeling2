# Turn multi-runs into a function ?

run_NFL_model <- function(outcome__ = "result" ,
                          model_runs_ = 10 ,
                          outcome_type = "cont" ,
                          method__  = "lm",
                          resample__  = 5 ,
                          repeats__  = 5 ,
                          clusters_ = 10 ,
                          test_season = 2021 ,
                          ensemble = T
                          ){
  library(caret)
  library(doParallel)
  set.seed(825)
  outlist <- list()
  modeldata <- create_modeldata(c(outcome__))
  if(outcome_type == "cat"){
    modeldata[[outcome__]] <- factor(modeldata[[outcome__]])
  }
  start_time <- Sys.time()
  cat(paste("Running models, starting at" , start_time , "\n" , "Method:" , method__ , "\n" ,"Season:" , test_season , "\n" , "Number of runs:" , model_runs_ , "\n\n"))

  for(i in 1:model_runs_){
    print(paste("Model Run:" , i , sep = ""))
    # indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
    # train_ <- modeldata %>% na.omit() %>% dplyr::slice(indices)
    # test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)

    train_ <- modeldata %>% na.omit() %>% filter(substr(game_id , 1 , 4) < test_season)
    test_ <- modeldata %>% na.omit() %>% filter(substr(game_id , 1 , 4) == test_season)

    fit__ <- model_outcome(outcome_ = outcome__ ,
                          train__ = train_ ,
                          method_ = method__ ,
                          resample_ = resample__ ,
                          repeats_ = repeats__)

    results_df <- pred_outcomes(test = test_ , outcome_ = outcome__ , fit_ = fit__)

    accuracy <- mean(results_df$model_win)
    out <- list(model = fit__, results_df = results_df , accuracy = accuracy)
    outlist[[i]] <-  out
  }
  stop_time <- Sys.time()
  runtime <- round(as.numeric(difftime(stop_time , start_time , units = "mins")) , 2)
  print(paste("Total Runtime" , runtime , "mins"))

  accuracies <- c()
  for(i in 1:model_runs_){
    accuracies <- c(accuracies , outlist[[i]]$accuracy)
  }
  average_accuracy_ <- mean(accuracies)
  print(paste("Average accuracy predicting " , outcome__ , "using " , method__ , "across " , model_runs_, "model run(s) is " , average_accuracy_))

  result_list <- list(model_objects = outlist ,
                      average_accuracy = average_accuracy_
                      )

  # accuracies_w_l <- c()
  # for(i in 1:15){
  #   accuracies_w_l <- c(accuracies_w_l , mean(outlist[[i]]$results_df$model_win_ns))
  # }
  if(ensemble == T){ # For each game, extract the prediction from each model run, compile into one dataframe

    ensemble_predictions <- tibble(train_ %>% game_outcomes() %>% select(game_id , result , spread_line , home_cover))
    for(i in 1:model_runs_){
      pred <- ensemble_predictions %>% mutate(pred = as.numeric(as.character(predict(outlist[[i]]$model , train_)))) %>% select(game_id , pred)
      ensemble_predictions <- ensemble_predictions %>% left_join(pred , by = "game_id" , suffix = c("",i))
    }
    threshold_mean_model_wins <- tibble()
    i <- 0
    while(i <= 1){ # Find optimal threshold for predicting 1 based on the average of all ensemble_predictions [0 , 1]
      threshold_mean_model_wins <- bind_rows(threshold_mean_model_wins,
                                             bind_cols( threshold = i ,
                                                        model_win = ensemble_predictions %>%
                                        mutate(linedif = spread_line + result ,
                                               avg_pred = rowMeans(ensemble_predictions %>% select(matches("pred"))) ,
                                               pred_ = ifelse(avg_pred > i , 1 , 0) ,
                                               does_count = ifelse(avg_pred >= 0.8 , T , F) ,
                                               model_win = ifelse(pred_ == home_cover , 1 , 0)
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

    ensemble_predictions_test <- tibble(test_ %>% game_outcomes() %>% select(game_id , result , spread_line , home_cover))
    for(i in 1:model_runs_){
      pred <- test_ %>% mutate(pred = as.numeric(as.character(predict(outlist[[i]]$model , test_)))) %>% select(game_id , pred)
      ensemble_predictions_test <- ensemble_predictions_test %>% left_join(pred , by = "game_id" , suffix = c("",i))
    }

    ensemble_result_df <-   ensemble_predictions_test %>%
      game_outcomes() %>%
      mutate(linedif = spread_line + result ,
             avg_pred = rowMeans(ensemble_predictions_test %>% select(matches("pred"))) ,
             pred_ = ifelse(avg_pred >  best_thresh , 1 , 0) ,
             # does_count = ifelse(avg_pred > 0.8 , T , F) ,
             model_win = ifelse(pred_ == home_cover , 1 , 0)
      )
    ensemble_test_accuracy <- ensemble_result_df %>% pull(model_win) %>% mean()
    # View()
    # tabyl(model_win , pred_)
    print(paste("Accuracy predicting" , outcome__ , "using ensemble of" , model_runs_ ,  method__ , "model run(s) is" , ensemble_test_accuracy))
  }
  result_list[["ensemble_threshold"]] <- best_thresh
  result_list[["ensemble_predictions"]] <- ensemble_result_df
  result_list[["ensemble_test_accuracy"]] <- ensemble_test_accuracy
  result_list[["thesholds"]] <- threshold_mean_model_wins
  return(result_list)
}

test_run <- run_NFL_model("home_cover" , model_runs_ =  25 , outcome_type = "cat" , method__ = "xgbTree" , test_season = 2023)




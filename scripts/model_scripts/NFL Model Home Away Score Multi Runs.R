library(caret)
library(doParallel)
set.seed(825)

outlist <- list()
start_time <- Sys.time()
modeldata <- create_modeldata(c("home_score" , "away_score")) # %>% mutate(home_cover = as.factor(home_cover))

for(i in 1:15){
  print(paste("Model Run:" , i , sep = ""))
  # indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
  # train_ <- modeldata %>% na.omit() %>% dplyr::slice(indices)
  # test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)

  train_ <- modeldata %>% na.omit() %>% filter(substr(game_id , 1 , 4) < 2023)
  test_ <- modeldata %>% na.omit() %>% filter(substr(game_id , 1 , 4) >= 2023)

  home_fit <- model_outcome(outcome_ = "home_score" ,
                            method_ = "xgbTree" ,
                            train__ = train_ %>% select(-away_score) ,
                            resample_ = 5 ,
                            repeats_ = 5 ,
                            clusters_ = 5)
  away_fit <- model_outcome(outcome_ = "away_score" ,
                            method_ = "xgbTree" ,
                            train__ = train_ %>% select(-home_score) ,
                            resample_ = 5 ,
                            repeats_ = 5 ,
                            clusters_ = 5)
  results_df <- pred_outcomes(test = test_ , outcome_ = "home_away_score")

  accuracy <- mean(results_df$model_win)
  out <- list(model = home_cover_fit, results_df = results_df , accuracy = accuracy)
  outlist[[i]] <-  out
}
stop_time <- Sys.time()
runtime <- round(as.numeric(difftime(stop_time , start_time , units = "mins")) , 2)
print(paste("Total Runtime" , runtime , "mins"))

accuracies <- c()
for(i in 1:15){
  accuracies <- c(accuracies , outlist[[i]]$accuracy)
}
psych::describe(accuracies)

# accuracies_w_l <- c()
# for(i in 1:15){
#   accuracies_w_l <- c(accuracies_w_l , mean(outlist[[i]]$results_df$model_win_ns))
# }

predictions <- tibble(outlist[[1]]$results_df %>% select(game_id , result , spread_line))
for(i in 1:15){
  pred <- outlist[[i]]$results_df %>% select(game_id , pred)
  predictions <- predictions %>% left_join(pred , by = "game_id" , suffix = c("",i))
}

predictions %>% game_outcomes() %>%
  mutate(linedif = spread_line + result ,
         avg_pred = rowMeans(predictions %>% select(matches("pred"))) ,
         pred_ = avg_pred ,
         pred_cover = ifelse((pred_ + spread_line) > 0 , 1  , 0) ,
         model_win = ifelse(pred_cover == home_cover , 1 , 0)
  )  %>%
  pull(model_win) %>%
  mean()

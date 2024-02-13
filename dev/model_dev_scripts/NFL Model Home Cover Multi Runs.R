library(caret)
library(doParallel)
set.seed(825)

outlist <- list()
start_time <- Sys.time()
modeldata <- create_modeldata(c("home_cover")) %>% mutate(home_cover = as.factor(home_cover))

for(i in 1:15){
  print(paste("Model Run:" , i , sep = ""))
  # indices <- sample(1:nrow(modeldata), size = 0.75 * nrow(modeldata))
  # train_ <- modeldata %>% na.omit() %>% dplyr::slice(indices)
  # test <- modeldata %>% na.omit() %>% dplyr::slice(-indices)

  train_ <- modeldata %>% na.omit() %>% filter(substr(game_id , 1 , 4) < 2022)
  test_ <- modeldata %>% na.omit() %>% filter(substr(game_id , 1 , 4) >= 2022)

  home_cover_fit <- model_outcome(outcome_ = "home_cover" ,
                                  method_ = "xgbTree" ,
                                  resample_ = 5 ,
                                  repeats_ = 5)

  results_df <- pred_outcomes(test = test_ , outcome_ = "home_cover")

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

predictions <- tibble(outlist[[3]]$results_df %>% select(game_id , result , spread_line))
for(i in 1:15){
  pred <- outlist[[i]]$results_df %>% select(game_id , pred)
  predictions <- predictions %>% left_join(pred , by = "game_id" , suffix = c("",i))
}

i <- 0
while(i <= 1){
  print(i)
  print(
    predictions %>% game_outcomes() %>%
      mutate(linedif = spread_line + result ,
             avg_pred = rowMeans(predictions %>% select(matches("pred"))) ,
             pred_ = ifelse(avg_pred > i , 1 , 0) ,
             does_count = ifelse(avg_pred >= 0.8 , T , F) ,
             model_win = ifelse(pred_ == home_cover , 1 , 0)
      )  %>% pull(model_win) %>% mean()#View()
      # filter(avg_pred == 0.8) %>%
      # View(".8")
      # tabyl(model_win , pred_) %>% adorn_totals("row")
  )
i <- i + 0.01
}

predictions %>% game_outcomes() %>%
  mutate(linedif = spread_line + result ,
         avg_pred = rowMeans(predictions %>% select(matches("pred"))) ,
         pred_ = ifelse(avg_pred > .8 , 1 , 0) ,
         does_count = ifelse(avg_pred > 0.8 , T , F) ,
         model_win = ifelse(pred_ == home_cover , 1 , 0)
  )  %>% pull(model_win) %>% mean()
   # View()
  # tabyl(model_win , pred_)





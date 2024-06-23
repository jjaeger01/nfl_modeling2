# STANDARD BASE MODELS - regression outcomes ####
source("~/Projects/nfl_modeling2/scripts/multi_run_NFL().R")

multi_run_NFL(outcomes = "result" ,
              seasons = 2020:2023 ,
              log_ = T ,
              formula_ = "simple" ,
              log_in_table_ = "BASE_MODELS_RESULT_SIMPLE_20240520" ,
              log_pred_in_table_ = "BASE_PRED_RESULT_SIMPLE_20240520" ,
              log_pred_ = T)

multi_run_NFL(outcomes = "linedif" ,
              seasons = 2020:2023 ,
              log_ = T ,
              formula_ = "simple" ,
              log_in_table_ = "BASE_MODELS_LINEDIF_SIMPLE_20240520" ,
              log_pred_in_table_ = "BASE_PRED_LINEDIF_SIMPLE_20240520" ,
              log_pred_ = T)

multi_run_NFL(outcomes = "home_away_score" ,
              seasons = 2020:2023 ,
              log_ = T ,
              formula_ = "simple" ,
              log_in_table_ = "BASE_MODELS_SCORES_SIMPLE_20240520" ,
              log_pred_in_table_ = "BASE_PRED_SCORES_SIMPLE_20240520" ,
              log_pred_ = T)

# STANDARD BASE MODELS - class outcomes ####

multi_run_NFL("home_win" ,
              seasons = 2020:2023 ,
              formula_ = "simple" ,
              log_ = T ,
              log_in_table_ = "BASE_MODELS_WIN_SIMPLE_20240520" ,
              log_pred_ = T ,
              log_pred_in_table_ = "BASE_PRED_WIN_SIMPLE_20240520")

multi_run_NFL("home_cover" ,
              seasons = 2020:2023 ,
              formula_ = "simple" ,
              log_ = T ,
              log_in_table_ = "BASE_MODELS_COVER_SIMPLE_20240520" ,
              log_pred_ = T ,
              log_pred_in_table_ = "BASE_PRED_COVER_SIMPLE_20240520")


# INTERACTION BASE MODELS - regression outcomes ####

multi_run_NFL(outcomes = "result" ,
              seasons = 2020:2023 ,
              log_ = T ,
              formula_ = "custom_int" ,
              log_in_table_ = "BASE_MODELS_RESULT_INT_20240520" ,
              log_pred_in_table_ = "BASE_PRED_RESULT_INT_20240520" ,
              log_pred_ = T)

multi_run_NFL(outcomes = "linedif" ,
              seasons = 2020:2023 ,
              log_ = T ,
              formula_ = "custom_int" ,
              log_in_table_ = "BASE_MODELS_LINEDIF_INT_20240520" ,
              log_pred_in_table_ = "BASE_PRED_LINEDIF_INT_20240520" ,
              log_pred_ = T)

multi_run_NFL(outcomes = "home_away_score" ,
              seasons = 2020:2023 ,
              log_ = T ,
              formula_ = "custom_int" ,
              log_in_table_ = "BASE_MODELS_SCORES_INT_20240520" ,
              log_pred_in_table_ = "BASE_PRED_SCORES_INT_20240520" ,
              log_pred_ = T)

# INTERACTION BASE MODELS - class outcomes ####

multi_run_NFL("home_win" ,
              seasons = 2020:2023 ,
              methods_ = c("bayesglm" , "glm" ,  "glmnet" , "LogitBoost" , "glmboost" , "xgbTree") ,
              formula_ = "custom_int" ,
              log_ = T ,
              log_in_table_ = "BASE_MODELS_WIN_INT_20240520" ,
              log_pred_ = T ,
              log_pred_in_table_ = "BASE_PRED_WIN_INT_20240520")

multi_run_NFL("home_cover" ,
              seasons = 2020:2023 ,
              methods_ = c("bayesglm" , "glm" ,  "glmnet" , "LogitBoost" , "glmboost"  , "xgbTree") ,
              formula_ = "custom_int" ,
              log_ = T ,
              log_in_table_ = "BASE_MODELS_COVER_INT_20240520" ,
              log_pred_ = T ,
              log_pred_in_table_ = "BASE_PRED_COVER_INT_20240520")

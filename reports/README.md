<<<<<<< HEAD
`{r , include=FALSE , message=F} library(tidyverse) # library(tidylog) library(DBI) library(RSQLite) source("~/Projects/nfl_modeling2/scripts/NFL Functions.R", echo=F) source("~/Projects/nfl_modeling2/scripts/NFL Data Set Up_full.R", echo=F)`

\`\`\`{r , eval = F , include=FALSE , message=F} \# STANDARD BASE
MODELS - regression outcomes \####
source(“~/Projects/nfl_modeling2/scripts/multi_run_NFL().R”)

multi_run_NFL(outcomes = “result” , seasons = 2020:2023 , log\_ = T ,
formula\_ = “simple” , log_in_table\_ =
“BASE_MODELS_RESULT_SIMPLE_20240520” , log_pred_in_table\_ =
“BASE_PRED_RESULT_SIMPLE_20240520” , log_pred\_ = T)

multi_run_NFL(outcomes = “linedif” , seasons = 2020:2023 , log\_ = T ,
formula\_ = “simple” , log_in_table\_ =
“BASE_MODELS_LINEDIF_SIMPLE_20240520” , log_pred_in_table\_ =
“BASE_PRED_LINEDIF_SIMPLE_20240520” , log_pred\_ = T)

multi_run_NFL(outcomes = “home_away_score” , seasons = 2020:2023 , log\_
= T , formula\_ = “simple” , log_in_table\_ =
“BASE_MODELS_SCORES_SIMPLE_20240520” , log_pred_in_table\_ =
“BASE_PRED_SCORES_SIMPLE_20240520” , log_pred\_ = T)

# STANDARD BASE MODELS - class outcomes

multi_run_NFL(“home_win” , seasons = 2020:2023 , formula\_ = “simple” ,
log\_ = T , log_in_table\_ = “BASE_MODELS_WIN_SIMPLE_20240520” ,
log_pred\_ = T , log_pred_in_table\_ = “BASE_PRED_WIN_SIMPLE_20240520”)

multi_run_NFL(“home_cover” , seasons = 2020:2023 , formula\_ = “simple”
, log\_ = T , log_in_table\_ = “BASE_MODELS_COVER_SIMPLE_20240520” ,
log_pred\_ = T , log_pred_in_table\_ =
“BASE_PRED_COVER_SIMPLE_20240520”)

# INTERACTION BASE MODELS - regression outcomes

multi_run_NFL(outcomes = “result” , seasons = 2020:2023 , log\_ = T ,
formula\_ = “custom_int” , log_in_table\_ =
“BASE_MODELS_RESULT_INT_20240520” , log_pred_in_table\_ =
“BASE_PRED_RESULT_INT_20240520” , log_pred\_ = T)

multi_run_NFL(outcomes = “linedif” , seasons = 2020:2023 , log\_ = T ,
formula\_ = “custom_int” , log_in_table\_ =
“BASE_MODELS_LINEDIF_INT_20240520” , log_pred_in_table\_ =
“BASE_PRED_LINEDIF_INT_20240520” , log_pred\_ = T)

multi_run_NFL(outcomes = “home_away_score” , seasons = 2020:2023 , log\_
= T , formula\_ = “custom_int” , log_in_table\_ =
“BASE_MODELS_SCORES_INT_20240520” , log_pred_in_table\_ =
“BASE_PRED_SCORES_INT_20240520” , log_pred\_ = T)

# INTERACTION BASE MODELS - class outcomes

multi_run_NFL(“home_win” , seasons = 2020:2023 , methods\_ =
c(“bayesglm” , “glm” , “glmnet” , “LogitBoost” , “glmboost” , “xgbTree”)
, formula\_ = “custom_int” , log\_ = T , log_in_table\_ =
“BASE_MODELS_WIN_INT_20240520” , log_pred\_ = T , log_pred_in_table\_ =
“BASE_PRED_WIN_INT_20240520”)

multi_run_NFL(“home_cover” , seasons = 2020:2023 , methods\_ =
c(“bayesglm” , “glm” , “glmnet” , “LogitBoost” , “glmboost” , “xgbTree”)
, formula\_ = “custom_int” , log\_ = T , log_in_table\_ =
“BASE_MODELS_COVER_INT_20240520” , log_pred\_ = T , log_pred_in_table\_
= “BASE_PRED_COVER_INT_20240520”)



    ```{r , include=FALSE , message=F}
    ## CONNECT TO DATABASE ####
    con_ <- dbConnect(RSQLite::SQLite() , "C:/Users/jaege/OneDrive/Documents/Projects/nfl_modeling2/data/model_runs2_db.db")
    ## LIST TABLES ####
    dbListTables(con_)


    base_model_bar_graph <- function(BASE_MODELS , title_ = NA){
      model_means <- BASE_MODELS %>% group_by(method) %>% summarise(mean_accuracy = mean(accuracy_ATS))
      overall_mean <- BASE_MODELS %>% pull(accuracy_ATS) %>% mean()
      BASE_MODELS %>%  
        ggplot(aes(x = method , y = accuracy_ATS ,  fill = as.factor(test_season))) +
        geom_bar(position = "dodge" , 
                 stat="identity") +
        coord_cartesian(ylim =c(0.4 , 0.6))  + 
        geom_hline(yintercept=0.523, linetype="dashed", color = "red") + 
        geom_hline(yintercept=overall_mean, linetype="dashed", color = "blue") + 
        geom_errorbar(data=model_means , 
                      aes(method, ymax = mean_accuracy, ymin = mean_accuracy) ,
                      linewidth=0.5 ,
                      linetype = "longdash" ,
                      inherit.aes = F ,
                      width = 1) +
        guides(fill=guide_legend(title="Season")) + 
        labs(x = "Method" , y = "ATS Accuracy" , title = title_)
    }

    base_model_overall_ats <- function(BASE_MODELS , title_ = NA){
      # model_means <- BASE_MODELS %>% group_by(method) %>% summarise(mean_accuracy = mean(accuracy_ATS))
      # overall_mean <- BASE_MODELS %>% pull(accuracy_ATS) %>% mean()
      BASE_MODELS %>%  
        ggplot(aes(x = method , y = accuracy_ATS  )) +
        geom_bar(position = "dodge" , 
                 stat="identity") +
        coord_cartesian(ylim =c(0.4 , 0.6))  + 
        geom_hline(yintercept=0.523, linetype="dashed", color = "red") +
        # geom_hline(yintercept=overall_mean, linetype="dashed", color = "blue") +
        # geom_errorbar(data=model_means , 
        #               aes(method, ymax = mean_accuracy, ymin = mean_accuracy) ,
        #               linewidth=0.5 ,
        #               linetype = "longdash" ,
        #               inherit.aes = F ,
        #               width = 1) +
        guides(fill=guide_legend(title="Season")) + 
        labs(x = "Method" , y = "ATS Accuracy" , title = title_)
    }

## Definitions:

*H**o**m**e**S**c**o**r**e* : Points scored by the home team  
*A**w**a**y**S**c**o**r**e* : Points scored by the away team  
*R**e**s**u**l**t* = *H**o**m**e**S**c**o**r**e* − *A**w**a**y**S**c**o**r**e*  
*P**o**i**n**t**S**p**r**e**a**d* : The result (point differential) set
by odds-makers  
*L**i**n**e**d**i**f* = *R**e**s**u**l**t* + *P**o**i**n**t**S**p**r**e**a**d*  
*H**o**m**e**C**o**v**e**r* = *L**i**n**e**d**i**f* \> 0  
*H**o**m**e**W**i**n* = *R**e**s**u**l**t* \> 0  

## Simple feature set:

*O**u**t**c**o**m**e* = *D**i**v**i**s**i**o**n**a**l**M**a**t**c**h**U**p* + *P**o**i**n**t**S**p**r**e**a**d* + *H**o**m**e**T**e**a**m**A**g**g**r**e**g**a**t**e**d**S**t**a**t**i**s**t**i**c**s* + *A**w**a**y**T**e**a**m**A**g**g**r**e**g**a**t**e**d**S**t**a**t**i**s**t**i**c**s*

\`\``{r , echo=F , message=F , warning=F} home_features <- colnames(create_modeldata())[!str_detect(colnames(create_modeldata()) , "away")][-c(1:4)]  away_features <- colnames(create_modeldata())[str_detect(colnames(create_modeldata()) , "away")] bind_cols(`Game
Features`=  c("div_game" , "spread_line" , rep(NA , times = length(home_features) - 2)) ,`Home
Team Features`= home_features ,`Away Team Features\` = away_features)


    ```{r , echo = F , message = F}
    viz_title <- "Results model using simple feature set"
    RESULT_BASE_MODELS <- dbGetQuery(con_ , "SELECT * FROM BASE_MODELS_RESULT_SIMPLE_20240520")
    base_model_bar_graph(RESULT_BASE_MODELS , viz_title)

\`\`\`{r , echo = F , message = F} viz_title \<- “Linedif model using
simple feature set” LINEDIF_BASE_MODELS \<- dbGetQuery(con\_ , “SELECT
\* FROM BASE_MODELS_LINEDIF_SIMPLE_20240520”)
base_model_bar_graph(LINEDIF_BASE_MODELS , viz_title)


    ```{r , echo = F , message = F}
    viz_title <- "Scores model using simple feature set"
    SCORES_BASE_MODELS <- dbGetQuery(con_ , "SELECT * FROM BASE_MODELS_SCORES_SIMPLE_20240520")
    base_model_bar_graph(SCORES_BASE_MODELS , viz_title)

\`\`\`{r , echo = F , message = F} viz_title \<- “Home cover model using
simple feature set” COVER_BASE_MODELS \<- dbGetQuery(con\_ , “SELECT \*
FROM BASE_MODELS_COVER_SIMPLE_20240520”)
base_model_bar_graph(COVER_BASE_MODELS , viz_title)


    ## Interaction feature set:

    $Outcome = Divisional Match Up + PointSpread + Home Team Stats + Away Team Stats + (Home*Away) + (Away*Home)$

    ```{r , echo=F ,  message=F}
    source('C:/Users/jaege/OneDrive/Documents/Projects/nfl_modeling2/dev/build int formula.R')
    home_features <- colnames(create_modeldata())[!str_detect(colnames(create_modeldata()) , "away")][-c(1:4)] 
    away_features <- colnames(create_modeldata())[str_detect(colnames(create_modeldata()) , "away")]

    bind_cols(`Game Features` =  c("div_game" , "spread_line" , rep(NA , times = length(home_features) - 2)) ,       
              `Home Team Features` = home_features , 
              `Away Team Features` = away_features , 
              `Home-Away Interactions` = c(home_int , rep(NA , time = length(home_features) - length(home_int))) , 
              `Away-Home Interactions` = c(away_int , rep(NA , time = length(home_features) - length(away_int))) 
              )

`{r , echo = F , message = F} viz_title <- "Result model using home-away interactions feature set" RESULT_BASE_MODELS <- dbGetQuery(con_ , "SELECT * FROM BASE_MODELS_RESULT_INT_20240520") base_model_bar_graph(RESULT_BASE_MODELS , viz_title)`

`{r , echo = F , message = F} viz_title <- "Linedif model using home-away interactions feature set" LINEDIF_BASE_MODELS <- dbGetQuery(con_ , "SELECT * FROM BASE_MODELS_LINEDIF_INT_20240520") base_model_bar_graph(LINEDIF_BASE_MODELS , viz_title)`

`{r , echo = F , message = F} viz_title <- "Scores model using home-away interactions feature set" SCORES_BASE_MODELS <- dbGetQuery(con_ , "SELECT * FROM BASE_MODELS_SCORES_INT_20240520") base_model_bar_graph(SCORES_BASE_MODELS , viz_title)`

`{r , echo = F , message = F} viz_title <- "Cover model using home-away interactions feature set" COVER_BASE_MODELS <- dbGetQuery(con_ , "SELECT * FROM BASE_MODELS_COVER_INT_20240520") base_model_bar_graph(COVER_BASE_MODELS , viz_title)`

\`\`\`{r} ALL_BASEMODELS \<- tibble() for(sql_table in
dbListTables(con\_)){ table\_ \<- dbGetQuery(con\_ , paste(“SELECT \*
FROM” , sql_table , sep = ” “)) ALL_BASEMODELS \<-
bind_rows(ALL_BASEMODELS , table\_) }

Overall_model_data \<- ALL_BASEMODELS %\>% filter(outcome != “home_win”)
%\>%  
group_by(method , outcome) %\>% summarise(accuracy_ATS =
mean(accuracy_ATS))

Overall_model_data %\>% ggplot(aes(x = method , y = accuracy_ATS , fill
= as.factor(outcome))) + geom_bar(position = “dodge” ,
stat=“identity”) + coord_cartesian(ylim =c(0.4 , 0.6)) +
geom_hline(yintercept=0.523, linetype=“dashed”, color = “red”) + \#
geom_hline(yintercept=overall_mean, linetype=“dashed”, color = “blue”) +
\# geom_errorbar(data=model_means , \# aes(method, ymax = mean_accuracy,
ymin = mean_accuracy) , \# linewidth=0.5 , \# linetype = “longdash” , \#
inherit.aes = F , \# width = 1) +
guides(fill=guide_legend(title=“Modeled Outcome”)) + labs(x = “Method” ,
y = “ATS Accuracy” , title = “Overall Model Accuracy”) +
theme(axis.text.x = element_text(angle = 45, hjust=1)) \`\`\`

\`\`\`
=======

>>>>>>> efb521d7312251c766b42c77a7950b104b6e45cb

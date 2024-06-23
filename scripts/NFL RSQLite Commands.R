library(tidyverse)
# library(tidylog)
library(DBI)
library(RSQLite)

# MODEL RUNS ####

## CONNECT TO DATABASE ####
con_ <- dbConnect(RSQLite::SQLite() , "data/model_runs2_db.db")

## LIST TABLES ####
dbListTables(con_)

## DISPLAY ALL TABLES
# for(table_ in dbListTables(con_)){
#   dbGetQuery(con_ , paste("select * from" , table_)) %>% View(table_)
# }

# %>% select(-features)

## PULL MODEL RUNS ####
model_runs <- dbGetQuery(con_ , "select * from BASE_MODELS_LINEDIF_SIMPLE_20240430") # %>% select(-features)
# print(model_runs)
model_runs %>% View("model_runs")

## PULL RFE RUNS ####
# model_runs <- dbGetQuery(con_ , "select * from RFE_runs_v0_4") # %>% select(-features)
# # print(model_runs)
# model_runs %>% View("RFE_runs")

## DELETE TABLE ####
#### dbExecute(con_ , "DROP TABLE BASE_MODELS_20240331")

## DELETE TABLE LOOP
drop_tables <- c()
for(table_ in drop_tables){
  dbExecute(con_ , paste("DROP TABLE" , table_) )
}

## WRITE TABLE ####
# dbWriteTable(con_ , "test_table" , games , append = T)

dbDisconnect(con_)
rm(con_)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# PREDICTIONS ####

con_ <- dbConnect(RSQLite::SQLite() , "data/predictions2.db")

## LIST TABLES ####
dbListTables(con_)

## PULL MODEL RUNS ####
predictions <- dbGetQuery(con_ , "select * from BASE_MODELS_PRED_20240411") # %>% select(-features)
# print(model_runs)
predictions %>% View("predictions")

## DELETE TABLE ####
#### dbExecute(con_ , "DROP TABLE BASE_PRED_RESULT_SIMPLE_20240517")

## WRITE TABLE ####
# dbWriteTable(con_ , "test_table" , games , append = T)

dbDisconnect(con_)
rm(con_)

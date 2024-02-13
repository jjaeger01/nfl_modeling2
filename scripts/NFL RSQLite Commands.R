library(tidyverse)
# library(tidylog)
library(DBI)
library(RSQLite)

con_ <- dbConnect(RSQLite::SQLite() , "data/model_runs_db.db")
# con_ <- dbConnect(RSQLite::SQLite() , "data/predictions.db")

# dbWriteTable(con_ , "test_table" , games)

dbListTables(con_)
model_runs <- dbGetQuery(con_ , "select * from model_runs_v2") %>% select(-features)
model_runs %>% View("model_runs")

# dbExecute(con_ , "DROP TABLE model_runs_v2")
# dbWriteTable(con_ , "test_table" , games , append = T)

dbDisconnect(con_)
rm(con_)

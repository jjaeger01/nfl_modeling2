# library(tidyverse)
# library(tidylog)
library(DBI)
library(RSQLite)

con_ <- dbConnect(RSQLite::SQLite() , "data/model_runs_db.db")

# dbWriteTable(con_ , "test_table" , games)

dbListTables(con_)
dbGetQuery(con_ , "select * from model_runs_v1") %>% select(-features)

# dbExecute(con_ , "DROP TABLE model_runs_v1")
# dbWriteTable(con_ , "test_table" , games , append = T)

dbDisconnect(con_)
rm(con_)

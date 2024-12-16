# Load libraries
library(RSQLite)
library(DBI)
library(dplyr)
library(dbplyr)

con <- dbConnect(RSQLite::SQLite(), "T:/09_TECHNICAL_PROJECTS/CRITICAL_COASTAL_HABITAT_ASSESSMENT/DATA/ccha-database.db")

dbListTables(con)

keys <- tbl(con, "Keys")
tranveg <- tbl(con, "Transect_Vegetation")

vegdat <- left_join(tranveg, keys, by = 'Reference') %>% 
  collect()

dbDisconnect(con)

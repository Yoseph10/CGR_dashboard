library(DBI)
library ('plyr')
library(dplyr)
library(tidyverse)
library(haven)


setwd("C:/Users/Yoseph/Documents/GitHub/CGR_dashboard")

#------------------------------
# Output: raw_historical_claims table
#------------------------------

# Obtaining the values of the environment variables
db <- Sys.getenv("db_algorithm")  #database name
db_host <- Sys.getenv("db_host") #localhost
db_port <- Sys.getenv("db_port") #port
db_user <- Sys.getenv("db_user") #credentials: user name
db_pass <- Sys.getenv("db_pass") #credentials: user password


#Connecting to the Postgres database
conn <- dbConnect(
        RPostgres::Postgres(),
        dbname = db,
        host = db_host,
        port = db_port,
        user = db_user,
        password = db_pass
)

## --- INPUT ---- 
# import historical claims table 
raw_historical_claims = dbReadTable(conn, "raw_historical_claims")

#drop first observation
raw_historical_claims <- raw_historical_claims %>% slice(-1) 


## --- OUTPUT ---- 
save(raw_historical_claims, file = "out/raw_historical_claims.RData")








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


#TODO_: VALIDATE 

# Recategorization for FUR status (dummy)
raw_historical_claims$FUR_decision <- mutate(raw_historical_claims, FUR_decision =
                                   case_when(estado_rac == "Calificado" | estado_rac == "Concluido" ~ "Yes",
                                             estado_rac == "Anulado" | estado_rac == "Derivado" | estado_rac == "En Proceso" | estado_rac == "Pendiente" ~ "No" ) )

# Recategorization for PDE status (dummy)
raw_historical_claims$PDE_decision<- ifelse(raw_historical_claims$numerohe != "" & (raw_historical_claims$productoaprobado == "Carpeta Atención de Denuncia" | raw_historical_claims$productoaprobado == "Desestimado" | raw_historical_claims$productoaprobado == "En proceso") , "Yes", "No")


# Recategorization for CAD status (dummy)
raw_historical_claims$CAD_decision<- ifelse(raw_historical_claims$numerohe != "" &  raw_historical_claims$productoaprobado == "Carpeta Atención de Denuncia" , "Yes", "No")


## --- OUTPUT ---- 
save(raw_historical_claims, file = "out/raw_historical_claims.RData")


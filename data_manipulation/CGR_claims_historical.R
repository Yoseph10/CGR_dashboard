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


#TODO_: VALIDATE 

# Recategorization for FUR status (dummy)
#raw_historical_claims$FUR_decision <- mutate(raw_historical_claims, FUR_decision =
#                                   case_when(estado_rac == "Calificado" | estado_rac == "Concluido" ~ "Yes",
#                                             estado_rac == "Anulado" | estado_rac == "Derivado" | estado_rac == "En Proceso" | estado_rac == "Pendiente" ~ "No" ) )

# Recategorization for PDE status (dummy)
#raw_historical_claims$PDE_decision<- ifelse(raw_historical_claims$numerohe != "" & raw_historical_claims$productoaprobado != "" , "Yes", "No")


# Recategorization for CAD status (dummy)
#raw_historical_claims$CAD_decision<- ifelse(raw_historical_claims$numerohe != "" &  raw_historical_claims$productoaprobado == "Carpeta Atención de Denuncia" , "Yes", "No")



#Claims' status
raw_historical_claims  <- mutate(raw_historical_claims,
                                     accept_claim =
                                     case_when( estado_hecho == "A.T. c/subsanación" | estado_hecho == "Aceptado a trámite" | estado_hecho == "Admitido" |
                                                estado_hecho == "Trámite de oficio" | estado_hecho == "T.O. c/subsanación" | estado_hecho == "Verificado"  ~ "Aceptado",
                                                
                                                estado_hecho == "Derivado" ~ "Derivado",
                                                
                                                estado_hecho == "Concluir Ini. Cont. Simultáneo" | estado_hecho == "Desestimado" | estado_hecho ==  "Desvirtuado" |
                                                estado_hecho ==  "Devuelto" | estado_hecho == "En subsanación" | estado_hecho == "Insubsanable" | estado_hecho == "Materia no denunciable" |
                                                estado_hecho == "No aceptado a trámite" | estado_hecho == "No admitido" ~ "Rechazado",  
                                                
                                                estado_hecho == "En evaluación" | estado_hecho == "Pendiente"  | estado_hecho == "Por derivar" ~ "Pendiente" ) )                                            
                                            

## --- OUTPUT ---- 
write.csv(raw_historical_claims, file = "out/raw_historical_claims.csv", row.names = FALSE)


#------------------------------
# Output: raw_historical_claims table for dashboard
#------------------------------

raw_historical_claims_dash <- raw_historical_claims %>% select(annio_sicgr, accept_claim, tipo_de_entidad, tipodedenuncia_primario, uo_ara)

## --- OUTPUT ---- 
write.csv(raw_historical_claims_dash, file = "out/raw_historical_claims_dash.csv", row.names = FALSE)


#------------------------------
# Output: PDE_categories_df table
#------------------------------

##Aprobado, rechazado, pendiente categories

#FUR categories
#FUR_categories_df <- mutate(subset(raw_historical_claims, FUR_decision == "Yes"), FUR_decision_categories =
#                                    case_when( PDE_decision == "Yes" ~ "Aprobado",
#                                               PDE_decision == "No" & (estado_pde == "No admitido" | estado_pde == "No aceptado a trámite") ~ "Rechazado",
#                                               TRUE  ~ "Pendiente") )

#PDE categories
#PDE_categories_df  <- mutate(subset(raw_historical_claims, PDE_decision == "Yes"), PDE_decision_categories =
#                                     case_when( CAD_decision == "Yes" ~ "Aprobado",
#                                                CAD_decision == "No" & productoaprobado == "En proceso" ~ "Pendiente",
#                                                TRUE ~ "Rechazado") )

#PDE_categories_df <- PDE_categories_df %>% select(year,PDE_decision_categories)

## --- OUTPUT ---- 
#write.csv(PDE_categories_df, file = "out/PDE_categories_df_historical.csv")


#------------------------------
# Output: primary_class_count table
#------------------------------

# number and percentage of denuncias per type of primary class
primary_class_count <- subset(raw_historical_claims, tipodedenuncia_primario!= "" ) %>% 
        count(tipodedenuncia_primario, sort = TRUE) %>% 
        mutate( percentage = round( (n/sum(n) )*100, 1))


## --- OUTPUT ---- 
write.csv(primary_class_count, file = "out/primary_class_count_historical.csv", row.names = FALSE)


#------------------------------
# Output: uo_ara_count historical table
#------------------------------

uo_ara_count <- subset(raw_historical_claims, uo_ara!= "" ) %>%
        group_by(uo_ara) %>%
        summarise(
                n = n(),
                percentage = NaN
        )  %>%
        mutate( percentage = round( (n/sum(n) )*100, 1)
        )


## --- OUTPUT ---- 
write.csv(uo_ara_count, file = "out/uo_ara_count_historical.csv", row.names = FALSE)






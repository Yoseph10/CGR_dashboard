library(DBI)
library ('plyr')
library(dplyr)
library(tidyverse)
library(haven)


setwd("C:/Users/Yoseph/Documents/GitHub/CGR_dashboard")

#------------------------------
# Output: raw_claims_feb21 table
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
# import raw claims 21 table 
raw_claims_feb21 = dbReadTable(conn, "raw_claims_feb21")


# Recategorization for fur status (dummy)
raw_claims_feb21 <- mutate(raw_claims_feb21, FUR_decision =
                                case_when(estado_hec == "Calificado" | estado_hec == "Concluido" ~ "Yes",
                                        estado_hec == "Anulado" | estado_hec == "En Proceso" | estado_hec == "Pendiente" ~ "No" ) )
        
# Recategorization for fud status (dummy)
raw_claims_feb21$FUD_decision<- ifelse(raw_claims_feb21$FUR_decision == "Yes" & raw_claims_feb21$numero_fud != "", "Yes", "No")
        
        
# Recategorization for PDE status (dummy)
raw_claims_feb21$PDE_decision<- ifelse(raw_claims_feb21$numero_he != "" & (raw_claims_feb21$producto_aprobado == "Carpeta Atención de Denuncia" | raw_claims_feb21$producto_aprobado == "Desestimado" | raw_claims_feb21$producto_aprobado == "En proceso") , "Yes", "No")
        
        
# Recategorization for CAD status (dummy)
raw_claims_feb21$CAD_decision<- ifelse(raw_claims_feb21$numero_he != "" &  raw_claims_feb21$producto_aprobado == "Carpeta Atención de Denuncia" , "Yes", "No")


## --- OUTPUT ---- 
save(raw_claims_feb21, file = "out/raw_claims_feb21.RData")


#------------------------------
# Output: claims 2021 table
#------------------------------

## --- INPUT ---- 
# import clean_claims_21 table 
clean_claims_21 <- get(load('out/clean_claims_21.Rdata'))


## MERGE RAW_CLAIMS_FEB21 & CLEAN_CLAIMS_21

#convert all vars to string
clean_claims_21_ch <- clean_claims_21 %>%
        mutate(across(everything(), as.character))

raw_claims_feb21_ch <- raw_claims_feb21 %>%
        mutate(across(everything(), as.character))

#deleting white spaces
clean_claims_21_ch$expediente <- gsub(" ", "", clean_claims_21_ch$expediente) 
clean_claims_21_ch$fur <- gsub(" ", "", clean_claims_21_ch$fur) 
clean_claims_21_ch$numero_hec <- gsub(" ", "", clean_claims_21_ch$numero_hec) 
clean_claims_21_ch$numero_hecho <- gsub(" ", "", clean_claims_21_ch$numero_hecho) 
clean_claims_21_ch$numero_fud  <- gsub(" ", "", clean_claims_21_ch$numero_fud ) 

raw_claims_feb21_ch$expediente <- gsub(" ", "", raw_claims_feb21_ch$expediente) 
raw_claims_feb21_ch$fur <- gsub(" ", "", raw_claims_feb21_ch$fur) 
raw_claims_feb21_ch$numero_hec <- gsub(" ", "", raw_claims_feb21_ch$numero_hec) 
raw_claims_feb21_ch$numero_hecho <- gsub(" ", "", raw_claims_feb21_ch$numero_hecho) 
raw_claims_feb21_ch$numero_fud  <- gsub(" ", "", raw_claims_feb21_ch$numero_fud ) 

#from lowercase to uppercase
clean_claims_21_ch <- mutate_each(clean_claims_21_ch, funs(toupper))
raw_claims_feb21_ch <- mutate_each(raw_claims_feb21_ch, funs(toupper))

#solving issues with numero fud var
clean_claims_21_ch$numero_fud <- gsub('NAN', '', clean_claims_21_ch$numero_fud)

#creating variable that will help us with the merge
clean_claims_21_ch$clean_claims_21 <- "1"
raw_claims_feb21_ch$raw_claims_feb21 <- "1"

#solving issues with expediente var (deleting leading zeros)

clean_claims_21_ch$expediente <- sub("^0+", "", clean_claims_21_ch$expediente) 
raw_claims_feb21_ch$expediente <- sub("^0+", "", raw_claims_feb21_ch$expediente) 

#Merge claims 21 with codified claims (todo: solve issues)

claims_21 <- merge(x = raw_claims_feb21_ch, y = clean_claims_21_ch, by = c("expediente", "fur", "numero_hec", "numero_hecho", "numero_fud"), all.x = TRUE)

## --- OUTPUT ---- 
save(claims_21, file = "out/claims_21.RData")


#------------------------------
# Output: df entity's type 2021
#------------------------------

#delete special characters from tipo_de_entidad
claims_21$tipo_de_entidad <- iconv(claims_21$tipo_de_entidad, from="UTF-8",to="ASCII//TRANSLIT")

#Solving some issues with tipo_de_entidad 
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE LA PRODUCCION', 'DIRECCION REGIONAL DE PRODUCCION', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE SALUD DE AREQUIPA', 'DIRECCION REGIONAL DE SALUD AREQUIPA', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE SALUD DE CALLAO', 'DIRECCION REGIONAL DE SALUD CALLAO', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE SALUD DE UCAYALI', 'DIRECCION REGIONAL DE SALUD UCAYALI', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE SALUD DE LA LIBERTAD', 'DIRECCION REGIONAL DE SALUD LA LIBERTAD', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE SALUD DE SAN MARTIN', 'DIRECCION REGIONAL DE SALUD SAN MARTIN', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE TRABAJO Y PROMOCION DE EMPLEO', 'DIRECCION REGIONAL DE TRABAJO Y PROMOCION DEL EMPLEO', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE TRABAJO Y PROMOCION DE EMPLEO', 'DIRECCION REGIONAL DE TRABAJO Y PROMOCION DEL EMPLEO', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE TRANSPORTES', 'DIRECCION REGIONAL DE TRANSPORTE Y COMUNICACIONES', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE TRANSPORTES Y COMUNICACIONES', 'DIRECCION REGIONAL DE TRANSPORTE Y COMUNICACIONES', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('DIRECCION REGIONAL DE TRANSPORTES Y COMUNICACIONES', 'DIRECCION REGIONAL DE TRANSPORTE Y COMUNICACIONES', claims_21$tipo_de_entidad)
claims_21$tipo_de_entidad <- gsub('MINISTERIO DE PRODUCCION', 'MINISTERIO DE LA PRODUCCION', claims_21$tipo_de_entidad)

# number and percentage of denuncias per type of entity

type_entity_count <- subset(claims_21, tipo_de_entidad!= "" ) %>%
        group_by(tipo_de_entidad) %>%
        summarise(
                n = n(),
                percentage = NaN,
                n_hec = sum(FUR_decision == "NO" & FUD_decision == "NO" & PDE_decision == "NO" & CAD_decision == "NO"),
                n_fur = sum(FUR_decision == "YES" & FUD_decision == "NO" & PDE_decision == "NO" & CAD_decision == "NO"),
                n_fud = sum(FUD_decision == "YES" & PDE_decision == "NO" & CAD_decision == "NO" ),
                n_pde = sum(PDE_decision == "YES" & CAD_decision == "NO"),
                n_cad = sum(CAD_decision == "YES"),
        )

type_entity_count$percentage <- round( (type_entity_count$n/sum(type_entity_count$n) )*100, 1)
type_entity_count$n_hec <- round( (type_entity_count$n_hec/type_entity_count$n )*100, 1)
type_entity_count$n_fur <- round( (type_entity_count$n_fur/type_entity_count$n )*100, 1)
type_entity_count$n_fud <- round( (type_entity_count$n_fud/type_entity_count$n )*100, 1)
type_entity_count$n_pde <- round( (type_entity_count$n_pde/type_entity_count$n )*100, 1)
type_entity_count$n_cad <- round( (type_entity_count$n_cad/type_entity_count$n )*100, 1)


## --- OUTPUT ---- 
save(type_entity_count, file = "out/type_entity_count_claims_21.RData")









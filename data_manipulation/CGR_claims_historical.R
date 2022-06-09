library(DBI)
library ('plyr')
library(dplyr)
library(tidyverse)
library(haven)
library(readr)

setwd("C:/Users/Yoseph/Documents/GitHub/CGR_dashboard")

#------------------------------
# Output: raw_historical_claims table
#------------------------------

# Obtaining the values of the environment variables
#db <- Sys.getenv("db_algorithm")  #database name
#db_host <- Sys.getenv("db_host") #localhost
#db_port <- Sys.getenv("db_port") #port
#db_user <- Sys.getenv("db_user") #credentials: user name
#db_pass <- Sys.getenv("db_pass") #credentials: user password


#Connecting to the Postgres database
#conn <- dbConnect(
 #       RPostgres::Postgres(),
 #       dbname = db,
 #       host = db_host,
 #       port = db_port,
 #       user = db_user,
 #       password = db_pass
#)

## --- INPUT ---- 
# import historical claims table 
#raw_historical_claims = dbReadTable(conn, "raw_historical_claims")


raw_historical_claims = read_stata("data/updated_historical_claims.dta")

# column names from upper to lower case
for( i in 1:ncol(raw_historical_claims)){
        colnames(raw_historical_claims)[i] <- tolower(colnames(raw_historical_claims)[i])
}


#raw_historical_claims = read_stata("data/raw_historical_claims.dta")
#raw_historical_claims = raw_historical_claims %>% slice(-1)


# Recategorization for FUR/FUD/PDE/CAD
raw_historical_claims$FUR_decision = ifelse(raw_historical_claims$estado_rac != "Anulado" & raw_historical_claims$estado_rac != "Derivado", "Yes", "No" )

raw_historical_claims$FUD_decision = ifelse(raw_historical_claims$FUR_decision == "Yes" & raw_historical_claims$estadofcero == "Asociado" ,"Yes", "No" )

raw_historical_claims$PDE_decision = ifelse(raw_historical_claims$productopropuesto=="Carpeta Atención de Denuncia" | raw_historical_claims$productopropuesto=="Control simultáneo" |
                                            raw_historical_claims$productopropuesto=="Alerta control", "Yes", "No")

raw_historical_claims$CAD_decision = ifelse(raw_historical_claims$productoaprobado=="Carpeta Atención de Denuncia" | raw_historical_claims$productoaprobado=="Control simultáneo" |
                                            raw_historical_claims$productoaprobado=="Alerta control", "Yes", "No")
                                     
                                            
## --- OUTPUT ---- 
write.csv(raw_historical_claims, file = "out/raw_historical_claims.csv", row.names = FALSE)


#------------------------------
# Output: raw_historical_claims table for dashboard
#------------------------------

#raw_historical_claims_dash <- raw_historical_claims %>% select(annio_sicgr, accept_claim, tipo_de_entidad, tipodedenuncia_primario, uo_ara)
raw_historical_claims_dash <- raw_historical_claims %>% select(year, tipo_de_entidad, tipodedenuncia_primario, uo_ara, FUR_decision, FUD_decision, PDE_decision, CAD_decision )

## --- OUTPUT ---- 
write.csv(raw_historical_claims_dash, file = "out/raw_historical_claims_dash.csv", row.names = FALSE)



#------------------------------
# Output: PDE_categories_df table
#------------------------------

##Aprobado, rechazado, pendiente categories

#FUR categories
#FUR_categories_df <- mutate(subset(raw_historical_claims, FUR_decision == "Yes"), FUR_decision_categories =
 #                                   case_when( PDE_decision == "Yes" ~ "Aprobado",
  #                                             PDE_decision == "No" & (estado_pde == "No admitido" | estado_pde == "No aceptado a trámite") ~ "Rechazado",
   #                                            TRUE  ~ "Pendiente") )

#PDE categories
PDE_categories_df  <- mutate(subset(raw_historical_claims, PDE_decision == "Yes"), PDE_decision_categories =
                                     case_when( CAD_decision == "Yes" ~ "Aprobado",
                                                CAD_decision == "No" & productoaprobado == "En proceso" ~ "Pendiente",
                                                TRUE ~ "Rechazado") )

PDE_categories_df <- PDE_categories_df %>% select(year,PDE_decision_categories)

## --- OUTPUT ---- 
write.csv(PDE_categories_df, file = "out/PDE_categories_df_historical.csv")


#------------------------------
# Output: df claims per region
#------------------------------

hechos_dep <- raw_historical_claims %>% select(departamento, year) 
hechos_dep <- subset(mutate_each(hechos_dep, funs(toupper)), departamento!= "" )


#delete special characters from departamento
hechos_dep$departamento <- iconv(hechos_dep$departamento, from="UTF-8",to="ASCII//TRANSLIT")

#Solving some issues with departamento (temporal)
hechos_dep$departamento[hechos_dep$departamento == 'AMAZONA'] <- 'AMAZONAS'
hechos_dep$departamento[hechos_dep$departamento == 'CALLAO (LA PERLA,C.LEGUA,BELLAV.,L.PUNTA,VENTANILLA)'] <- 'CALLAO'
hechos_dep$departamento[hechos_dep$departamento == 'HUANCAYO'] <- 'JUNIN'
hechos_dep$departamento[hechos_dep$departamento == 'GALVEZ'] <- 'CAJAMARCA'
hechos_dep$departamento[hechos_dep$departamento == 'HUACAYBAMBA'] <- 'ANCASH'
hechos_dep$departamento[hechos_dep$departamento == 'LORETO, SAN MARTIN'] <- 'LORETO'
hechos_dep$departamento[hechos_dep$departamento == 'MADRE DIOS'] <- 'MADRE DE DIOS'
hechos_dep$departamento[hechos_dep$departamento == 'QUICACHA'] <- 'AREQUIPA'
hechos_dep$departamento[hechos_dep$departamento == 'SULLANA'] <- 'PIURA'
hechos_dep$departamento[hechos_dep$departamento == 'VICTORIA'] <- 'LIMA'


hechos_dep_year <- hechos_dep %>%
        group_by(departamento, year) %>%
        summarise( n_year = n() ) 


## --- INPUT ---- 
census_dep <-read_stata("data/departments_census2017.dta")


#claims per department, per 100k inhabitants
hechos_dep_census <- merge(x = hechos_dep_year, y = census_dep, by.x = "departamento", by.y = "dep", all.y = TRUE )

hechos_dep_census <- hechos_dep_census %>% 
                        select(departamento, year, n_year, pop17)

## --- OUTPUT ---- 
write.csv(hechos_dep_census, file = "out/hechos_dep_census_claims_historical.csv", row.names = FALSE)


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


#------------------------------
# Output: df for plot, claims per region 
#-----------------------------

library(rgdal)
library(broom)
library(rgeos)
library(maptools)
library(viridis)
library(scales)

# The black and white polygon map
departamentos<-readOGR(dsn="maps/DEPARTAMENTO/DEPARTAMENTO_27_04_2015.shp", layer="DEPARTAMENTO_27_04_2015")

departamentos$NOMBDEP

#We must transform the data to be able to make a graph with ggplot
departamentos_fortified <- tidy(departamentos, region =  "NOMBDEP")

#We leave only the id and n columns
hechos_dep_census$id <- hechos_dep_census$departamento
hechos_dep_census <- hechos_dep_census[,c("id", "year", "n_year", "pop17")]
colnames(hechos_dep_census) <- c("id", "year", "n_claims_year", "pop17")
        
#left join
departamentos_fortified_ <-merge(x = departamentos_fortified, y = hechos_dep_census, by = c("id"), all.x = TRUE)


#------------------------------
# Output: df corruption claims
#-----------------------------

# creating a variable that identifies corruption claims
raw_historical_claims$corruption_denunc<- ifelse(raw_historical_claims$tipodedenuncia_primario=='Lucro ilegal'|
                                                   raw_historical_claims$tipodedenuncia_primario=='Uso inapropiado de recursos no finacieros'| 
                                                   raw_historical_claims$tipodedenuncia_primario== 'Favoritismo' |
                                                   raw_historical_claims$tipodedenuncia_primario== 'Colusion'| 
                                                   raw_historical_claims$tipodedenuncia_primario== 'Abuso de autoridad o extorsion', 1, 0)

raw_historical_claims$corruption_denunc<- ifelse(raw_historical_claims$tipodedenuncia_primario=='', '', raw_historical_claims$corruption_denunc)

# number and percentage of corruption claims 
corruption_claims_count <- subset(raw_historical_claims, corruption_denunc!= "" ) %>% 
        group_by(year) %>% 
        count(corruption_denunc) 

#corruption_claims_count$percentage <- round( (corruption_claims_count$n/sum(corruption_claims_count$n) )*100, 1)


## --- OUTPUT ---- 
write.csv(corruption_claims_count, file = "out/corruption_claims_count_claims_historical.csv", row.names = FALSE)


#------------------------------
# Output: df corruption per region
#-----------------------------

corruption_dep <- raw_historical_claims %>% select(corruption_denunc, departamento, year) 
corruption_dep <- subset(mutate_each(corruption_dep[corruption_dep$corruption_denunc==1,], funs(toupper)), departamento!= "" )

#delete special characters from departamento
corruption_dep$departamento <- iconv(corruption_dep$departamento, from="UTF-8",to="ASCII//TRANSLIT")

#Solving some issues with departamento (temporal)
corruption_dep$departamento[corruption_dep$departamento == 'AMAZONA'] <- 'AMAZONAS'
corruption_dep$departamento[corruption_dep$departamento == 'CALLAO (LA PERLA,C.LEGUA,BELLAV.,L.PUNTA,VENTANILLA)'] <- 'CALLAO'
corruption_dep$departamento[corruption_dep$departamento == 'HUANCAYO'] <- 'JUNIN'
corruption_dep$departamento[corruption_dep$departamento == 'GALVEZ'] <- 'CAJAMARCA'
corruption_dep$departamento[corruption_dep$departamento == 'HUACAYBAMBA'] <- 'ANCASH'
corruption_dep$departamento[corruption_dep$departamento == 'LORETO, SAN MARTIN'] <- 'LORETO'
corruption_dep$departamento[corruption_dep$departamento == 'MADRE DIOS'] <- 'MADRE DE DIOS'
corruption_dep$departamento[corruption_dep$departamento == 'QUICACHA'] <- 'AREQUIPA'
corruption_dep$departamento[corruption_dep$departamento == 'SULLANA'] <- 'PIURA'
corruption_dep$departamento[corruption_dep$departamento == 'VICTORIA'] <- 'LIMA'


corruption_dep <- corruption_dep %>%
        group_by(departamento, year) %>%
        summarise( n_year = n() )


#claims per department, per 100k inhabitants
corruption_dep_census <-merge(x = corruption_dep, y = census_dep, by.x = "departamento", by.y = "dep", all.y = TRUE )

corruption_dep_census <- corruption_dep_census %>% 
        select(departamento, year, n_year, pop17)


## --- OUTPUT ---- 
write.csv(corruption_dep_census, file = "out/corruption_dep_census_claims_historical.csv", row.names = FALSE)


#------------------------------
# Output: df for plot, corruption per region 
#-----------------------------

#We leave only the id and n columns
corruption_dep_census$id <- corruption_dep_census$departamento
corruption_dep_census <- corruption_dep_census[,c("id", "n_year", "year")]
colnames(corruption_dep_census) <- c("id", "n_corruption_year", "year")

departamentos_fortified_2 <-merge(x = departamentos_fortified_, y = corruption_dep_census, by = c("id", "year"), all.x = TRUE)


## --- OUTPUT ---- 
write.csv(departamentos_fortified_2, file = "out/departamentos_fortified_claims_historical.csv", row.names = FALSE)


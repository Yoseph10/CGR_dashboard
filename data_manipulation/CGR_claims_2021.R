library(DBI)
library ('plyr')
library(dplyr)
library(tidyverse)
library(haven)


setwd("C:/Users/Yoseph/Documents/GitHub/CGR_dashboard")

#------------------------------
# Output: clean claims table
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
# import clean claims 21 table 
#clean_claims_21 = dbReadTable(conn, "clean_claims_21")
clean_claims_21 = dbReadTable(conn, "claims_21_dataset_algoritmo")


#keep the last version
clean_claims_21 <- clean_claims_21[ clean_claims_21$version == max(clean_claims_21$version), ]


#Cleaning data
clean_claims_21[ clean_claims_21 == "NAN" | clean_claims_21 == "nan" |  clean_claims_21 == "." |  clean_claims_21 == "," ] <- ""


## --- OUTPUT ---- 
save(clean_claims_21, file = "out/clean_claims_21.RData")


#------------------------------
# Output: df missings
#------------------------------

df_missings <- ldply(clean_claims_21, function(var_value) sum(var_value =="")) 
colnames(df_missings) <- c("variable", "n_missings")

#adding a new column: percentage of missings obs 
df_missings$percentage_missing = round( (df_missings$n_missings/ nrow(clean_claims_21))*100 , 1)

## --- OUTPUT ---- 
save(df_missings, file = "out/df_missings_claims_21.RData")


#------------------------------
# Output: df plot error encoders
#------------------------------

#keeping just claims reviewed by Jaime
tie_breaking_append <- clean_claims_21[ clean_claims_21$tiebraker_name == "Jaime", ] %>%
                       select(chosen_encoder, date_batch)


names_encoders <- c("andrea", "edwar", "gannen")

#Encoders' number of errors
for(name in names_encoders) {                                                              
        
        #creates columns with zeros 
        n_error <- rep(0, nrow(tie_breaking_append))                                       # Create new column
        tie_breaking_append[ , ncol(tie_breaking_append) + 1] <- n_error                   # Append new column
        colnames(tie_breaking_append)[ncol(tie_breaking_append)] <- paste0("error_", name) # Rename column name
        
        #fills colums with one when there's an error
        if(name == "andrea"){
                
                tie_breaking_append$error_andrea[tie_breaking_append$chosen_encoder != "Andrea" & tie_breaking_append$chosen_encoder != "Andrea/Edwar"] <- 1
                
        } else if(name == "edwar"){
                
                tie_breaking_append$error_edwar[tie_breaking_append$chosen_encoder != "Edwar" & tie_breaking_append$chosen_encoder != "Andrea/Edwar"] <- 1
          
          # for gannen     
        } else{ 
        
               tie_breaking_append$error_gannen[tie_breaking_append$chosen_encoder != "Gannen" & tie_breaking_append$chosen_encoder != "Gannen/Edwar"] <- 1

        }
}


#Number of observations per batches
df2<- tie_breaking_append[!(tie_breaking_append$date_batch ==""),] %>%
        group_by(date_batch) %>%
        summarise(obs = n()) 


#Number of errors per encoder and batch
for(name in names_encoders) {
        
        df <- aggregate(tie_breaking_append %>% select( paste0("error_", name) ), by=list(date_batch=tie_breaking_append$date_batch), FUN=sum)
        assign( paste0("df_", name) , df)

        
}

#Corrections for number of error: 
#Andrea is not working since 2022-03-08 
#Gannen is working since 2022-03-08 

names_df <- c("df_andrea", "df_edwar", "df_gannen")

for(name_df in names_df) {
        
        df <- get(name_df)
        df <- df[!(df$date_batch ==""),]
        
        if(name_df == "df_andrea"){
                
                df[(which(df$date_batch == "2022-03-08")):nrow(df),]$error_andrea <- NA
                
        } else if(name_df == "df_gannen"){
                
                df[1:(which(df$date_batch == "2022-02-15")),]$error_gannen <- NA
        }
                
        assign(name_df, df)

}



#Error rate per encoder and batch  
#df_list <- list(df2, df_andrea, df_edwar)        #put all data frames into list
df_list <- list(df2, df_andrea, df_edwar, df_gannen) 

data_plot <- df_list %>%
        reduce(full_join, by='date_batch') #merge all data frames in list

data_plot$error_rate_andrea <- round( (data_plot$error_andrea/data_plot$obs)*100 , 1) 
data_plot$error_rate_edwar <- round( (data_plot$error_edwar/data_plot$obs)*100 , 1)
data_plot$error_rate_gannen <- round( (data_plot$error_gannen/data_plot$obs)*100 , 1)


## --- OUTPUT ---- 
save(data_plot, file = "out/data_plot_claims_21.RData")


#------------------------------
# Output: df claims per region
#------------------------------

hechos_dep <- subset(mutate_each(clean_claims_21, funs(toupper)), departamento!= "" )

#delete special characters from departamento
hechos_dep$departamento <- iconv(hechos_dep$departamento, from="UTF-8",to="ASCII//TRANSLIT")

#Solving some issues with departamento (temporal)
hechos_dep$departamento <- gsub('AREQUIPA, LIMA', 'AREQUIPA', hechos_dep$departamento)
hechos_dep$departamento <- gsub('LIMA, AYACUCHO', 'LIMA', hechos_dep$departamento)
hechos_dep$departamento <- gsub('MINISTERIO DE DEFENSA', 'LIMA', hechos_dep$departamento)
hechos_dep$departamento <- gsub('PIURA, LIMA', 'LIMA', hechos_dep$departamento)
hechos_dep$departamento <- gsub('TUMBES, PIURA, LAMBAYEQUE, CALLAO, TACNA, AREQUIPA', 'TUMBES', hechos_dep$departamento)
hechos_dep$departamento <- gsub('MONQUEGUA', 'MOQUEGUA', hechos_dep$departamento)
hechos_dep$departamento <- gsub('TRUJILLO', 'LA LIBERTAD', hechos_dep$departamento)
hechos_dep$departamento <- gsub('URUBAMBA', 'CUSCO', hechos_dep$departamento)
hechos_dep$departamento <- gsub('SAN AGUSTIN', 'AREQUIPA', hechos_dep$departamento)
hechos_dep$departamento <- gsub('CELENDIN', 'CAJAMARCA', hechos_dep$departamento)
hechos_dep$departamento <- gsub('CARHUAZ', 'ANCASH', hechos_dep$departamento)
hechos_dep$departamento <- gsub('AOURIMAC', 'APURIMAC', hechos_dep$departamento)


hechos_dep <- hechos_dep %>%
        group_by(departamento) %>%
        summarise( n = n() ) 

hechos_dep$percentage <- round( (hechos_dep$n/sum(hechos_dep$n) )*100, 1)


## --- INPUT ---- 
census_dep <-read_stata("data/departments_census2017.dta")


#claims per department, per 100k inhabitants
hechos_dep_census <- merge(x = hechos_dep, y = census_dep, by.x = "departamento", by.y = "dep", all.y = TRUE )
hechos_dep_census$N_dpt_claims100Kpop <- round( hechos_dep_census$n/(hechos_dep_census$pop17/100000) ,1)


## --- OUTPUT ---- 
save(hechos_dep_census, file = "out/hechos_dep_census_claims_21.RData")


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

#summary(departamentos)

departamentos$NOMBDEP

#head(departamentos@data)

#plot(departamentos)

#We must transform the data to be able to make a graph with ggplot
departamentos_fortified <- tidy(departamentos, region =  "NOMBDEP")
#unique(departamentos_fortified$id)


#We leave only the id and n columns
hechos_dep_census$id <- hechos_dep_census$departamento
hechos_dep_census <- hechos_dep_census[,c("id","N_dpt_claims100Kpop")]

#left join
departamentos_fortified_ <-merge(x = departamentos_fortified, y = hechos_dep_census, by = c("id"), all.x = TRUE)


## --- OUTPUT ---- 
#save(departamentos_fortified_, file = "out/departamentos_fortified_claims_21.RData")



#------------------------------
# Output: df corruption claims
#-----------------------------

# creating a variable that identifies corruption claims
clean_claims_21$corruption_denunc<- ifelse(clean_claims_21$tipo_denuncia_primario_homologado=='lucro ilegal'|
                                                   clean_claims_21$tipo_denuncia_primario_homologado=='uso inapropiado de recursos no finacieros'| 
                                                   clean_claims_21$tipo_denuncia_primario_homologado== 'favoritismo' |
                                                   clean_claims_21$tipo_denuncia_primario_homologado== 'colusion'| 
                                                   clean_claims_21$tipo_denuncia_primario_homologado== 'abuso de autoridad o extorsion', 1, 0)

#clean_claims_21$corruption_denunc<- ifelse(clean_claims_21$tipo_denuncia_primario_homologado=='', '', clean_claims_21$corruption_denunc)


# number and percentage of corruption claims 
corruption_claims_count <- subset(clean_claims_21, corruption_denunc!= "" ) %>% 
        count(corruption_denunc, sort = TRUE)

corruption_claims_count$percentage <- round( (corruption_claims_count$n/sum(corruption_claims_count$n) )*100, 1)


## --- OUTPUT ---- 
save(corruption_claims_count, file = "out/corruption_claims_count_claims_21.RData")


#------------------------------
# Output: df corruption per region
#-----------------------------

corruption_dep <- subset(mutate_each(clean_claims_21[clean_claims_21$corruption_denunc==1,], funs(toupper)), departamento!= "" )

#delete special characters from departamento
corruption_dep$departamento <- iconv(corruption_dep$departamento, from="UTF-8",to="ASCII//TRANSLIT")


#Solving some issues with departamento (temporal) 
corruption_dep$departamento <- gsub('AREQUIPA, LIMA', 'AREQUIPA', corruption_dep$departamento)
corruption_dep$departamento <- gsub('LIMA, AYACUCHO', 'LIMA', corruption_dep$departamento)
corruption_dep$departamento <- gsub('MINISTERIO DE DEFENSA', 'LIMA', corruption_dep$departamento)
corruption_dep$departamento <- gsub('PIURA, LIMA', 'LIMA', corruption_dep$departamento)
corruption_dep$departamento <- gsub('TUMBES, PIURA, LAMBAYEQUE, CALLAO, TACNA, AREQUIPA', 'TUMBES', corruption_dep$departamento)
corruption_dep$departamento <- gsub('MONQUEGUA', 'MOQUEGUA', corruption_dep$departamento)

corruption_dep$departamento <- gsub('TRUJILLO', 'LA LIBERTAD', corruption_dep$departamento)
corruption_dep$departamento <- gsub('URUBAMBA', 'CUSCO', corruption_dep$departamento)
corruption_dep$departamento <- gsub('SAN AGUSTIN', 'AREQUIPA', corruption_dep$departamento)
corruption_dep$departamento <- gsub('CELENDIN', 'CAJAMARCA', corruption_dep$departamento)
corruption_dep$departamento <- gsub('CARHUAZ', 'ANCASH', corruption_dep$departamento)
corruption_dep$departamento <- gsub('AOURIMAC', 'APURIMAC', corruption_dep$departamento)


corruption_dep <- corruption_dep %>%
        group_by(departamento) %>%
        summarise( n = n() )

corruption_dep$percentage <- round( (corruption_dep$n/sum(corruption_dep$n) )*100, 1)


#claims per department, per 100k inhabitants
corruption_dep_census <-merge(x = corruption_dep, y = census_dep, by.x = "departamento", by.y = "dep", all.y = TRUE )
corruption_dep_census$N_dpt_claims100Kpop_corruption <- round( corruption_dep_census$n/(corruption_dep_census$pop17/100000) ,1)


## --- OUTPUT ---- 
save(corruption_dep_census, file = "out/corruption_dep_census_claims_21.RData")


#------------------------------
# Output: df for plot, corruption per region 
#-----------------------------

#We leave only the id and n columns
corruption_dep_census$id <- corruption_dep_census$departamento
corruption_dep_census <- corruption_dep_census[,c("id","N_dpt_claims100Kpop_corruption")]

#left join
departamentos_fortified_ <-merge(x = departamentos_fortified_, y = corruption_dep_census, by = c("id"), all.x = TRUE)


## --- OUTPUT ---- 
save(departamentos_fortified_, file = "out/departamentos_fortified_claims_21.RData")




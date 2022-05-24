library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(reactable)
library(htmltools)
library(ggplot2)
library(viridis)
library(mapproj)
#library(openxlsx) #new

#uploading data

clean_claims_21 <- read.csv('out/clean_claims_21_dash.csv')
raw_claims_feb21 <- read.csv('out/raw_claims_feb21_dash.csv')
raw_historical_claims <- read.csv('out/raw_historical_claims_dash.csv')

#raw_historical_claims <-read.xlsx('out/raw_historical_claims_dash.xlsx')

## FUNCTION FOR CUSTOMIZING NEXT TABLES

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
        bar <- div(style = list(background = fill, width = width, height = height))
        chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
        div(style = list(display = "flex", alignItems = "center"), label, chart)
}

##################
## DENUNCIAS 2021
##################

n_claims <- nrow(raw_claims_feb21)
n_encoded <- nrow(clean_claims_21)
percent_encoded <- paste( round(n_encoded/n_claims,2)*100, "%")



##Aprobado, rechazado, pendiente categories
#TODO_ hacer más eficiente esta parte del codigo
#FUR categories
FUR_categories_df <- mutate(subset(raw_claims_feb21, FUR_decision == "Yes"), FUR_decision_categories =
                                    case_when( FUD_decision == "Yes" ~ "Aprobado",
                                               FUD_decision == "No" & (estado_fur == "No admitido" | estado_fur == "No aceptado a trámite") ~ "Rechazado",
                                               TRUE  ~ "Pendiente") )

#FUD categories
FUD_categories_df <- mutate(subset(raw_claims_feb21, FUD_decision == "Yes"), FUD_decision_categories =
                                    case_when( PDE_decision == "Yes" ~ "Aprobado",
                                               PDE_decision == "No" & (estado_fud == "Sin atender" | estado_fud == "Devuelto")  ~ "Pendiente",
                                               TRUE ~ "Rechazado") )

#PDE categories
PDE_categories_df  <- mutate(subset(raw_claims_feb21, PDE_decision == "Yes"), PDE_decision_categories =
                                     case_when( CAD_decision == "Yes" ~ "Aprobado",
                                                CAD_decision == "No" & producto_aprobado == "En proceso" ~ "Pendiente",
                                                TRUE ~ "Rechazado") )

#FUD/FUD/PDE/CAD
fur_claims <- sum(raw_claims_feb21$FUR_decision == "Yes")
percent_fur <-  paste( round(fur_claims/n_claims,2)*100, "%")

fud_claims <- sum(raw_claims_feb21$FUD_decision == "Yes")
percent_fud <-  paste( round(fud_claims/n_claims,2)*100, "%")

pde_claims <- sum(raw_claims_feb21$PDE_decision == "Yes")
percent_pde <-  paste( round(pde_claims/n_claims,2)*100, "%")

cad_claims <- sum(raw_claims_feb21$CAD_decision == "Yes")
percent_cad <-  paste( round(cad_claims/n_claims,2)*100, "%")



departamentos_fortified_ <- read.csv('out/departamentos_fortified_claims_21.csv')


corruption_claims_count <- read.csv('out/corruption_claims_count_claims_21.csv')


##################
## DENUNCIAS HISTÓRICAS
##################

#departamentos_fortified_historical <- read.csv('out/departamentos_fortified_claims_historical.csv')

corruption_claims_count_historical <- read.csv('out/corruption_claims_count_claims_historical.csv')



#https://rdrr.io/cran/flexdashboard/src/R/gauge.R
gaugeSectors <- function(success = NULL, warning = NULL, danger = NULL,
                         colors = c("success", "warning", "danger")) {
        list(success = success,
             warning = warning,
             danger = danger,
             colors = colors)
}



#HEADER
header <- dashboardHeader(title = "Denuncias CGR")

#SIDEBAR
sidebar <- dashboardSidebar(width = 280,
                            sidebarMenu(
                                    menuItem("Información general", tabName = "info", icon=icon("home")),
                                    menuItem("Denuncias 2021", tabName = "claims2021", icon = icon("th")),
                                    menuItem("Denuncias Históricas", tabName = "claims_hist", icon = icon("th"))
                            ) 
)

#BODY
body <- dashboardBody(
        tags$head(tags$style(HTML('
                      .main-header .logo {
                        font-family: "Georgia", Times, "Times New Roman", serif;
                        font-weight: bold;
                        font-size: 24px;
                      }
                    '))),
        
        #Estilo de los tabBox
        tags$style(".nav-tabs {
                                background-color: transparent;   
                                font-family:Georgia;
                                }
                                                
                                .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                                background-color: #F2F1F1;
                                border-color: transparent;
                                }
                                                
                                .nav-tabs-custom .nav-tabs li.active {
                                border-top-color: red;
                                }"),
        
        #Estilo de los Box
        tags$style(HTML("

                    .box.box-solid.box-primary{
                    border-bottom-color:transparent;
                    border-left-color:transparent;
                    border-right-color:transparent;
                    border-top-color:transparent;
                    background:#F2F1F1
                    }

                    ")),
        
        tabItems(
                ###################
                #Información general
                ###################
                
                tabItem(tabName = "info", 
                        
                        fluidRow(
                                
                                box( title = "Información General",
                                     status = "danger", solidHeader = FALSE,
                                     height = "300px",  width = 12, 
                                     
                                     p(br(),"¡¡Bienvenidos!!", br(),br(),
                                       "La idea de esta aplicación web es presentar de manera ordenada y fácil de entender el progreso en la codificación de denuncias, 
                                              así como estadísticos que permitan comprender los tipos de casos que han sido identificados y el progreso de la Contraloría General de la República (CGR)
                                              en la evaluación de las denuncias recibidas. Si bien el desarrollo inicial ha estado en manos del equipo académico, se espera que mediante una estrecha coordinación
                                              con la CGR, parte de los estadísticos que incorporará el Dashboard sean recomendados por especialistas internos de la mencionada institución"
                                       , style = "text-align:justify; color: #14505B ; font-size: 18px")
                                )
                                
                        ),
                        
                        fluidRow(
                                
                                
                                box( title = "Sobre la CGR",
                                     status = "danger", solidHeader = FALSE,
                                     height = "300px",  width = 6, 
                                     
                                     p(br(),"La CGR verifica la correcta aplicación de las políticas públicas y el uso de los recursos y bienes del Estado, 
                                              a través de gerencias regionales de control, los Órganos de Control Institucional (OCI) y las Sociedades de Auditorías (SOA)"
                                       , style = "text-align:justify; color: #14505B ; font-size: 18px")
                                ),
                                
                                
                                box( title = "Sobre la Gerencia de Control Social y Denuncia",
                                     status = "danger", solidHeader = FALSE,
                                     height = "300px",  width = 6, 
                                     
                                     p(br(),"La Gerencia de Control Social y Denuncias busca promover el control social y el fortalecimiento del Sistema Nacional de Control
                                              mediante normas que regulen la atención de la demanda imprevisible de control proveniente de denuncias recibidas y/o autogeneradas,
                                              mecanismos de participación ciudadana, mecanismos de análisis, así como de los servicios de control resultantes"
                                       , style = "text-align:justify; color: #14505B ; font-size: 18px")
                                )
                                
                                
                                
                        )
  
                                

                ),
                ####################
                # Denuncias 2021
                ####################
                tabItem(tabName = "claims2021",
                        
                        fluidRow(
                                
                                tabBox(
                                        id = "claims2021_tabs", width = 12, height = "650px",
                                        
                                        
                                        #Informacion General       
                                        tabPanel("Panorama General", icon = icon("briefcase-medical"),
                                                 
                                                 
                                                 
                                                 fluidRow(
                                                         
                                                         valueBox(value = paste(format(n_claims, big.mark = ","), "", sep = " "), 
                                                                  "Hechos totales",
                                                                  icon = icon("fa-solid fa-database"),             #https://fontawesome.com/v4/icons/
                                                                  color = "purple",
                                                                  width = 6
                                                         ),
                                                         
                                                         valueBox(value = paste(format(n_encoded, big.mark = ","), "/", percent_encoded  , sep = " "), 
                                                                  "Hechos codificados",
                                                                  icon = icon("fa-solid fa-arrow-circle-up"),
                                                                  color = "purple",
                                                                  width = 6
                                                         )
                                                         
                                                 ),
                                                 
                                                 
                                                 fluidRow(
                                                         
                                                         valueBox(value = paste(format(fur_claims, big.mark = ","), "/", percent_fur  , sep = " "),
                                                                  "Hechos FUR (respecto a hechos totales)",
                                                                  icon = icon("fa-solid fa-folder-open"),            
                                                                  color = 'aqua',
                                                                  width = 3),
                                                         
                                                         valueBox(value = paste(format(fud_claims, big.mark = ","), "/", percent_fud  , sep = " "),
                                                                  "Hechos FUD (respecto a hechos totales)",
                                                                  icon = icon("fa-solid fa-folder-open"),             
                                                                  color = 'aqua',
                                                                  width = 3),
                                                         
                                                         valueBox(value = paste(format(pde_claims, big.mark = ","), "/", percent_pde  , sep = " "),
                                                                  "Hechos PDE (respecto a hechos totales)",
                                                                  icon = icon("fa-solid fa-folder-open"),             
                                                                  color = 'aqua',
                                                                  width = 3),
                                                         
                                                         valueBox(value = paste(format(cad_claims, big.mark = ","), "/", percent_cad , sep = " "),
                                                                  "Hechos CAD (respecto a hechos totales)",
                                                                  icon = icon("fa-solid fa-folder-open"),             
                                                                  color = 'aqua',
                                                                  width = 3)
                                                         
                                                 ),
                                                 
                                                 
                                                 fluidRow(
                                                         column(4,
                                                                
                                                                p("Estado FUR",
                                                                  style = "text-align:center; color: black ; font-size: 16px; font-weight: bold"),
                                                                
                                                                plotlyOutput("fur_status_2021")
                                                         ),
                                                         column(4,
                                                                
                                                                p("Estado FUD",
                                                                  style = "text-align:center; color: black ; font-size: 16px; font-weight: bold"),
                                                                
                                                                plotlyOutput("fud_status_2021")
                                                         ),
                                                         column(4,
                                                                
                                                                p("Estado PDE",
                                                                  style = "text-align:center; color: black ; font-size: 16px; font-weight: bold"),
                                                                
                                                                plotlyOutput("pde_status_2021")
                                                         )
                                                 )
                                                 
                                                 
                                        ),
                                        
                                        #Hechos codificados: Monitoreo   
                                        tabPanel("Monitoreo", icon = icon("briefcase-medical"),    
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                
                                                                br(),
                                                                p("Hechos codificados: valores faltantes",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_missings")
                                                         ),
                                                         column(6,
                                                                
                                                                br(),
                                                                p("Hechos codificados: tasa de error por codificador",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                plotlyOutput("plot_tasa_error")
                                                                
                                                         )
                                                 )
                                                 
                                                 
                                        ),
                                        
                                        #Hechos codificados: Estadísticos 
                                        tabPanel("Estadísticos Generales", icon = icon("briefcase-medical"),
                                                 
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: hechos por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_hechos_dep_2021")
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: hechos por 100 mil habitantes por departamento - mapa",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                plotOutput("map_dep_2021")
                                                                
                                                         )
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por tipología primaria",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_tipologia_2021")
                                                                
                                                                ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos por entidad",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_entidad_2021")
                                                                )
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por unidad orgánica del analista",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_uo_ara_2021")
                                                                )
                                                 )
                                                 
   
                                        ),
                                        
                                        #Hechos codificados: Estadísticos corrupción
                                        tabPanel("Estadísticos Corrupción", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         
                                                         box(strong("Hechos de corrupción (Total)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                                 
                                                             flexdashboard::gaugeOutput("corruption_count", width = "90%", height = "100px")
                                                         ),
                                                         
                                                         box(strong("Hechos de corrupción (%)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_perc", width = "90%", height = "100px")
                                                         )

                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: hechos de corrupción por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_corruption_2021")
                                                                
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: hechos de corrupción por 100 mil habitantes por departamento - mapa",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                plotOutput("map_corruption_2021")
                                                         )
                                                 )
                                        )
                                )
                        )
                ),
                #####################
                # Denuncias históricas
                ####################
                tabItem(tabName = "claims_hist",
                        
                        fluidRow(
                                
                                selectInput("year_selection", label = "Selecciona el año",
                                            choices = c("Todos los años", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
                                ),
                                
                                tabBox(
                                        id = "claims_hist_tabs", width = 12, height = "650px",
                                        
                                        #Informacion General       
                                        tabPanel("Panorama General", icon = icon("briefcase-medical"),
                                                 
                                                 
                                                 fluidRow(
                                                         valueBoxOutput("historical_total", width = 12)
                                                 ),
                                                 
                                                 fluidRow(
                                                         valueBoxOutput("fur_historical", width = 4),
                                                         
                                                         valueBoxOutput("pde_historical", width = 4),
                                                         
                                                         valueBoxOutput("cad_historical", width = 4)
    
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(4,
                                                         ),
                                                         
                                                         column(4,
                                                                
                                                                p("Estado PDE",
                                                                  style = "text-align:center; color: black ; font-size: 16px; font-weight: bold"),
                                                                
                                                                plotlyOutput("pde_status_historical")
                                                         )
                                                 )
                                                 
                                        ),
                                        
                                        #Hechos Estadísticos 
                                        tabPanel("Estadísticos Generales", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_hechos_dep_historical")
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos por tipología primaria",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_tipologia_historical")
                                                                
                                                                #plotOutput("map_dep_historical")
                                                         )
                                                 ),
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos por entidad",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_entidad_historical")
                                                                
                                                         ),
                                                         column(6,
                                                                br(),
                                                                p("Hechos por unidad orgánica del analista",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_uo_ara_historical")
                                                         )
                                                 )
 
                                        ),
                                        
                                        # Estadísticos corrupción
                                        tabPanel("Estadísticos Corrupción", icon = icon("briefcase-medical"),
                                                 
                                                 fluidRow(
                                                         
                                                         box(strong("Hechos de corrupción (Total)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_count_historical", width = "90%", height = "100px")
                                                         ),
                                                         
                                                         box(strong("Hechos de corrupción (%)"), width = 6, status = "primary", solidHeader = TRUE, 
                                                             style = "font-family: Georgia;text-align:center;color:#14505B ;font-size: 18px; padding-top: 20px",
                                                             
                                                             flexdashboard::gaugeOutput("corruption_perc_historical", width = "90%", height = "100px")
                                                         )
                                                         
                                                 ),
                                                 
                                                 fluidRow(
                                                         column(6,
                                                                br(),
                                                                p("Hechos codificados: hechos de corrupción por departamento - estadísticos",
                                                                  style = "text-align:left; color: black ; font-size: 16px; font-weight: bold"), br(),
                                                                
                                                                reactableOutput("table_corruption_historical")
                                                                
                                                         )
                                                 )
                                        )
                                                 
                                                 
                                                 
                                                 
                                                 

                                )
                                        
                                        
                                        
                                        
                        )
                                
                )
                        
                        
 
        )
)




shinyApp(
        ui = dashboardPage(skin = "red",
                           header, sidebar, body),
        
        server = function(input, output) {
                
                
                ###################
                # Denuncias 2021
                ##################
                
                output$fur_status_2021 <- renderPlotly({
                        
                        fur_status_df <- as.data.frame(table(FUR_categories_df$FUR_decision_categories))
                        
                        colnames(fur_status_df) <- c("fur_status", "n")
                        
                        fur_status_df %>% 
                                plot_ly(labels = ~fur_status, values = ~n, marker = list(colors = c('#00aa7f', '#ccccc7', '#ff0000'))) %>% 
                                add_pie(hole = 0.55, textfont = list(size = 13)) %>% 
                                layout(
                                        #title = "Estado FUR",
                                       showlegend = T,
                                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
                })
                
                
                
                output$fud_status_2021 <- renderPlotly({ 
                        #Donut chart: fud status  - categories
                        fud_status_df <- as.data.frame(table(FUD_categories_df$FUD_decision_categories))
                        
                        colnames(fud_status_df) <- c("fud_status", "n")
                        
                        fud_status_df %>% 
                                plot_ly(labels = ~fud_status, values = ~n, marker = list(colors = c('#00aa7f', '#ccccc7', '#ff0000'))) %>% 
                                add_pie(hole = 0.55, textfont = list(size = 13)) %>% 
                                layout(
                                        #title = "Estado FUD",
                                       showlegend = T,
                                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                        
                })
                
                output$pde_status_2021 <- renderPlotly({ 
                        #Donut chart: PDE status - categories
                        pde_status_df <- as.data.frame(table(PDE_categories_df$PDE_decision_categories))
                        
                        colnames(pde_status_df) <- c("pde_status", "n")
                        
                        pde_status_df %>% 
                                plot_ly(labels = ~pde_status, values = ~n, marker = list(colors = c('#00aa7f', '#ccccc7', '#ff0000'))) %>%
                                add_pie(hole = 0.55, textfont = list(size = 13)) %>% 
                                layout(
                                        #title = "Estado PDE",
                                       showlegend = T,
                                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                        
                })
                
                output$table_missings <- renderReactable({
                        
                        #dataframe with two columns: variable and number of missings
                        df_missings_claims_21 <- read.csv('out/df_missings_claims_21.csv')
                        
                        reactable(
                                df_missings_claims_21,
                                pagination = TRUE,
                                defaultSorted = "percentage_missing",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        variable = colDef(name = "Variable codificada"
                                                          
                                        ),
                                        n_missings = colDef(
                                                name = "N° de valores faltantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(df_missings_claims_21$n_missing), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#39a7ad")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage_missing = colDef(
                                                name = "% de valores faltantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#ff0000", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                
                output$plot_tasa_error <- renderPlotly({ 
                        
                        ### Create a plot with error rate per encoder (Andrea/Edwar) (x axis is for number of batch, y for the error rate)
                        
                        data_plot <- read.csv('out/data_plot_claims_21.csv')
                        
                        #barplot: a plot with error rate per encoder (Andrea/Edwar)
                        plot_ly(data_plot, x = ~date_batch, y = ~error_rate_andrea, type = 'bar',  name = 'Andrea',
                                marker = list(color = '#ccccc7') ) %>% 
                                add_trace(y = ~error_rate_edwar, name = 'Edwar',  marker = list(color = '#00aa7f') ) %>% 
                                add_trace(y = ~error_rate_gannen, name = 'Gannen',  marker = list(color = '#cca1ed') ) %>% 
                                layout(title = "",
                                       xaxis = list(title = "Fecha del batch",
                                                    zeroline = FALSE),
                                       yaxis = list(title = "Tasa de error (%)",
                                                    zeroline = FALSE))
                        
                })
                
                
                output$table_hechos_dep_2021 <- renderReactable({
                        
                        hechos_dep_census <- read.csv('out/hechos_dep_census_claims_21.csv')
                        
                        reactable(
                                hechos_dep_census[,c("departamento", "n", "percentage", "N_dpt_claims100Kpop")],
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        departamento = colDef(name = "departamento"
                                                              
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(hechos_dep_census$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        N_dpt_claims100Kpop = colDef(
                                                name = "N° de hechos por 100 mil habitantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(hechos_dep_census$N_dpt_claims100Kpop), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#00aa7f")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                
                output$map_dep_2021 <- renderPlot({
                        
                        ggplot() +
                                geom_polygon(data = departamentos_fortified_, aes(fill = N_dpt_claims100Kpop, x = long, y = lat, group = group), colour= "gray", size=0.05, alpha=0.9) +
                                theme_void() +
                                scale_fill_viridis(option = 'D',  direction = -1, trans = "log2",  name="N° de hechos") +
                                #scale_fill_viridis(option = 'D', trans = "log2",  breaks = trans_breaks("log2", function(x) 2^x), name="N° de hechos") +
                                theme(  
                                        text = element_text(size= 12, color = "#22211d"),
                                        
                                        plot.title = element_text(size= 14, hjust=0.01, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
                                        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
                                        plot.caption = element_text( size=10, color = "black", margin = margin(b = 0.3, r=-99, unit = "cm") ),
                                ) +
                                coord_map()
                })
                
                
                output$table_entidad_2021 <- renderReactable({
                        
                        type_entity_count <- read.csv('out/type_entity_count_claims_21.csv')
                        
                        reactable(
                                type_entity_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        tipo_de_entidad = colDef(name = "Tipo de entidad", width = 180),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(type_entity_count$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        n_hec = colDef(
                                                name = "HEC", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_fur = colDef(
                                                name = "FUR", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_fud = colDef(
                                                name = "FUD", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_pde = colDef(
                                                name = "PDE", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_cad = colDef(
                                                name = "CAD", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                })
                                )
                        )
                })       
                
                
                output$table_tipologia_2021 <- renderReactable({
                        
                        primary_class_count <- read.csv("out/primary_class_count_claims_21.csv")
                        
                        reactable(
                                primary_class_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        tipo_denuncia_primario_homologado = colDef(name = "Tipo de denuncia"
                                                                                   
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(primary_class_count$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                output$table_uo_ara_2021 <- renderReactable({
                        
                        uo_ara_count <- read.csv("out/uo_ara_count_claims_21.csv")
                        
                        reactable(
                                uo_ara_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        uo_ara = colDef(name = "Unidad Orgánica", width = 180),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(uo_ara_count$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        n_hec = colDef(
                                                name = "HEC", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_fur = colDef(
                                                name = "FUR", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_fud = colDef(
                                                name = "FUD", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_pde = colDef(
                                                name = "PDE", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                }),
                                        n_cad = colDef(
                                                name = "CAD", width = 60,
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                })
                                )
                        )
                        
                })
                
                
                output$corruption_count <- flexdashboard::renderGauge({
                        
                        max = sum(corruption_claims_count$n)
                        
                        flexdashboard::gauge(corruption_claims_count$n[corruption_claims_count$corruption_denunc == 1], min = 0, max = max , gaugeSectors(
                                         success = c(round((2/3)*max) + 1 , max ), warning = c(round(max/3) + 1, round((2/3)*max) ), danger = c(0, round(max/3) ))
                                      , abbreviate = FALSE, abbreviateDecimals = 1)
                })
                
                output$corruption_perc <- flexdashboard::renderGauge({
                        
                        flexdashboard::gauge(corruption_claims_count$percentage[corruption_claims_count$corruption_denunc == 1], min = 0, max = 100, symbol = '%', gaugeSectors(
                                success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                        ))
                }) 
                
                output$table_corruption_2021 <- renderReactable({
                        
                        corruption_dep_census <- read.csv('out/corruption_dep_census_claims_21.csv')
                        
                        reactable(
                                corruption_dep_census[,c("departamento", "n", "percentage", "N_dpt_claims100Kpop_corruption")],
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        departamento = colDef(name = "departamento"
                                                              
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(corruption_dep_census$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        N_dpt_claims100Kpop_corruption = colDef(
                                                name = "N° de hechos por 100 mil habitantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(corruption_dep_census$N_dpt_claims100Kpop), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#00aa7f")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                output$map_corruption_2021 <- renderPlot({
                        
                        ggplot() +
                                geom_polygon(data = departamentos_fortified_, aes(fill = N_dpt_claims100Kpop_corruption, x = long, y = lat, group = group), colour= "gray", size=0.05, alpha=0.9) +
                                theme_void() +
                                scale_fill_viridis(option = 'D',  direction = -1, trans = "log2",  name="N° de hechos") +
                                #scale_fill_viridis(option = 'D', trans = "log2",  breaks = trans_breaks("log2", function(x) 2^x), name="N° de hechos") +
                                theme(  
                                        text = element_text(size= 7, color = "#22211d"),
                                        
                                        plot.title = element_text(size= 14, hjust=0.01, color = "black", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
                                        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
                                        plot.caption = element_text( size=10, color = "black", margin = margin(b = 0.3, r=-99, unit = "cm") ),
                                ) +
                                coord_map()
                })
                
                
                ###################
                # Denuncias Históricas
                ##################
                
                output$historical_total <- renderValueBox({
                        
                        
                        #creating n_claims_historical variable
                        if (input$year_selection == "Todos los años") {
                                n_claims_historical <- nrow(raw_historical_claims)
                                
                        } else {
                                n_claims_historical <- nrow(subset(raw_historical_claims, year== input$year_selection ))
                                
                        }
                        
                        valueBox(value = paste(format(n_claims_historical, big.mark = ","), "", sep = " "),
                                 "Hechos totales",
                                 icon = icon("fa-solid fa-database"),             #https://fontawesome.com/v4/icons/
                                 color = "purple"
                                 )
                })
                
                output$fur_historical <- renderValueBox({
                        
                        if (input$year_selection == "Todos los años") {
                                fur_claims <- sum(raw_historical_claims$FUR_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_fur <-  paste( round(fur_claims/n_claims_historical,2)*100, "%")
                        } else {
                                #dataframe with specific year
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection)
                                
                                fur_claims <- sum(raw_historical_year$FUR_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_fur <-  paste( round(fur_claims/n_claims_historical,2)*100, "%")
                        }
                        
                        valueBox(value = paste(format(fur_claims, big.mark = ","), "/", percent_fur  , sep = " "),
                                 "Hechos FUR (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                output$pde_historical <- renderValueBox({
                        
                        if (input$year_selection == "Todos los años") {
                                pde_claims <- sum(raw_historical_claims$PDE_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_pde <-  paste( round(pde_claims/n_claims_historical,2)*100, "%")
                        } else {
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection)
                                
                                pde_claims <- sum(raw_historical_year$PDE_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_pde <-  paste( round(pde_claims/n_claims_historical,2)*100, "%")
                        }
                        valueBox(value = paste(format(pde_claims, big.mark = ","), "/", percent_pde  , sep = " "),
                                 "Hechos PDE (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                output$cad_historical <- renderValueBox({
                        
                        if (input$year_selection == "Todos los años") {
                                cad_claims <- sum(raw_historical_claims$CAD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_claims)
                                percent_cad <-  paste( round(cad_claims/n_claims_historical,2)*100, "%")
                        } else {
                                raw_historical_year <- subset(raw_historical_claims, year== input$year_selection)
                                
                                cad_claims <- sum(raw_historical_year$CAD_decision == "Yes")
                                
                                n_claims_historical <- nrow(raw_historical_year)
                                percent_cad <-  paste( round(cad_claims/n_claims_historical,2)*100, "%")
                        }
                        valueBox(value = paste(format(cad_claims, big.mark = ","), "/", percent_cad , sep = " "),
                                 "Hechos CAD (respecto a hechos totales)",
                                 icon = icon("fa-solid fa-folder-open"),            
                                 color = 'aqua')
                })
                
                output$pde_status_historical <- renderPlotly({ 
                        #Donut chart: PDE status - categories
                        PDE_categories_df <- read.csv("out/PDE_categories_df_historical.csv")
                        
                        if (input$year_selection == "Todos los años") {
                                
                                pde_status_df <- as.data.frame(table(PDE_categories_df$PDE_decision_categories))
                                colnames(pde_status_df) <- c("pde_status", "n")
                        } else {
                                
                                PDE_categories_df <- subset(PDE_categories_df, year== input$year_selection)
                                pde_status_df <- as.data.frame(table(PDE_categories_df$PDE_decision_categories))
                                colnames(pde_status_df) <- c("pde_status", "n")
                        }
                                
                        pde_status_df %>% 
                                plot_ly(labels = ~pde_status, values = ~n, marker = list(colors = c('#00aa7f', '#ccccc7', '#ff0000'))) %>%
                                add_pie(hole = 0.55, textfont = list(size = 13)) %>% 
                                layout(
                                        #title = "Estado PDE",
                                        showlegend = T,
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                        
                })
                
                
                output$table_hechos_dep_historical <- renderReactable({
                        
                        hechos_dep_census <- read.csv('out/hechos_dep_census_claims_historical.csv')
                        
                        if (input$year_selection == "Todos los años") {
                                
                                hechos_dep_census <- hechos_dep_census %>% 
                                                        group_by(departamento) %>% 
                                                        mutate( n = sum(n_year),
                                                                pop17 = as.integer(min(pop17)) ) %>% 
                                                        filter(row_number()==1) %>% 
                                                        select(departamento, pop17, n) %>% 
                                                        mutate( N_dpt_claims100Kpop = round( n/(pop17/100000) ,1) ) %>% 
                                                                ungroup() %>% 
                                                        mutate(percentage = round( (n/sum(n) )*100, 1))  
                                
                        } else {
                                
                                hechos_dep_census <- hechos_dep_census %>%
                                                        filter(year== input$year_selection) %>%
                                                        select(departamento, n_year, pop17) %>% 
                                                        mutate( N_dpt_claims100Kpop = round( n_year/(pop17/100000) ,1),
                                                                percentage = round( (n_year/sum(n_year) )*100, 1))
                                                        
                                colnames(hechos_dep_census) <- c("departamento", "n", "pop17", "N_dpt_claims100Kpop", "percentage")
                        }
                        
                        
                        reactable(
                                hechos_dep_census[,c("departamento","n","N_dpt_claims100Kpop", "percentage")],
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        departamento = colDef(name = "departamento"
                                                              
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(hechos_dep_census$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        N_dpt_claims100Kpop = colDef(
                                                name = "N° de hechos por 100 mil habitantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(hechos_dep_census$N_dpt_claims100Kpop), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#00aa7f")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                #output$map_dep_historical <- renderPlot({
                        
                        
                    #    if (input$year_selection == "Todos los años") {
                                
                                #departamentos_fortified_historical <- departamentos_fortified_historical %>% 
                                  #      group_by(id,long,lat,order,hole,piece,group) %>% 
                                   #     mutate( n_claims = sum(n_claims_year) ) %>% 
                                    #    filter(row_number()==1)  %>% 
                                     #   mutate( N_dpt_claims100Kpop = round( n_claims/(pop17/100000) ,1) ) 
                                
                     #   } else {
                                
                               
                                
                      #  }
                        
                        
                #})
                
                output$table_tipologia_historical <- renderReactable({
                        
                        if (input$year_selection == "Todos los años") {
                                
                                primary_class_count <- read.csv('out/primary_class_count_historical.csv')
                                
                        } else {
                                
                                primary_class_count <- subset(raw_historical_claims, tipodedenuncia_primario!= ""  & year== input$year_selection ) %>% 
                                        count(tipodedenuncia_primario, sort = TRUE) %>% 
                                        mutate( percentage = round( (n/sum(n) )*100, 1))
                        }
                        
                        reactable(
                                primary_class_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        tipodedenuncia_primario = colDef(name = "Tipo de denuncia"
                                                                         
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(primary_class_count$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                output$table_entidad_historical <- renderReactable({
                        
                        if (input$year_selection == "Todos los años") {
                                
                                type_entity_count_historical <- subset(raw_historical_claims, tipo_de_entidad!= "" ) %>%
                                        group_by(tipo_de_entidad) %>% 
                                        summarise( n= n() ) %>% 
                                        mutate( percentage = round( (n/sum(n))*100, 1 ) )
                                
                                
                        } else {
                                
                                type_entity_count_historical <- subset(raw_historical_claims, tipo_de_entidad!= "" & year== input$year_selection  ) %>%
                                        group_by(tipo_de_entidad) %>% 
                                        summarise( n= n() ) %>% 
                                        mutate( percentage = round( (n/sum(n))*100, 1 ) )
                                
                        }
                        
                        reactable(
                                type_entity_count_historical,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        tipo_de_entidad = colDef(name = "Tipo de entidad", width = 180),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(type_entity_count_historical$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                }) 
                
                output$table_uo_ara_historical <- renderReactable({
                        
                        if (input$year_selection == "Todos los años") {
                                
                                uo_ara_count <- read.csv("out/uo_ara_count_historical.csv")
                                
                        } else {
                                
                                
                                uo_ara_count <- raw_historical_claims %>% subset(uo_ara!= "" & year== input$year_selection) %>%
                                        group_by(uo_ara) %>%
                                        summarise(
                                                n = n(),
                                                percentage = NaN
                                        )  %>%
                                        mutate( percentage = round( (n/sum(n) )*100, 1)
                                        )
                                
                        }
                        
                        reactable(
                                uo_ara_count,
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        uo_ara = colDef(name = "Unidad Orgánica", width = 180),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(uo_ara_count$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                
                output$corruption_count_historical <- flexdashboard::renderGauge({
                        
                        
                        if (input$year_selection == "Todos los años") {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        group_by(corruption_denunc) %>% 
                                        summarise( n = sum(n) )
                                
                                max = sum(corruption_claims_count$n)
                                
                        } else {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        filter(year== input$year_selection) 
                                
                                max = sum(corruption_claims_count$n)
                                        
                        }
                        
                        
                        flexdashboard::gauge(corruption_claims_count$n[corruption_claims_count$corruption_denunc == 1], min = 0, max = max , gaugeSectors(
                                success = c(round((2/3)*max) + 1 , max ), warning = c(round(max/3) + 1, round((2/3)*max) ), danger = c(0, round(max/3) ))
                                , abbreviate = FALSE, abbreviateDecimals = 1)
                })
                
                output$corruption_perc_historical <- flexdashboard::renderGauge({
                        
                        if (input$year_selection == "Todos los años") {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        group_by(corruption_denunc) %>% 
                                        summarise( n = sum(n) )
                                
                                corruption_claims_count$percentage <- round( (corruption_claims_count$n/sum(corruption_claims_count$n) )*100, 1)
                                
                        } else {
                                
                                corruption_claims_count <- corruption_claims_count_historical %>% 
                                        filter(year== input$year_selection) 
                                
                                corruption_claims_count$percentage <- round( (corruption_claims_count$n/sum(corruption_claims_count$n) )*100, 1)
                                
                        }
                        
                        
                        flexdashboard::gauge(corruption_claims_count$percentage[corruption_claims_count$corruption_denunc == 1], min = 0, max = 100, symbol = '%', gaugeSectors(
                                success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                        ))
                }) 
                
                output$table_corruption_historical <- renderReactable({
                        
                        corruption_dep_census <- read.csv('out/corruption_dep_census_claims_historical.csv')
                        
                        
                        if (input$year_selection == "Todos los años") {
                                
                                corruption_dep_census <- corruption_dep_census %>% 
                                        group_by(departamento) %>% 
                                        mutate( n = sum(n_year),
                                                pop17 = as.integer(min(pop17)) ) %>% 
                                        filter(row_number()==1) %>% 
                                        select(departamento, pop17, n) %>% 
                                        mutate( N_dpt_claims100Kpop_corruption = round( n/(pop17/100000) ,1) ) %>% 
                                        ungroup() %>% 
                                        mutate(percentage = round( (n/sum(n) )*100, 1))  
                                
                        } else {
                                
                                corruption_dep_census <- corruption_dep_census %>%
                                        filter(year== input$year_selection) %>%
                                        select(departamento, n_year, pop17) %>% 
                                        mutate( N_dpt_claims100Kpop_corruption = round( n_year/(pop17/100000) ,1),
                                                percentage = round( (n_year/sum(n_year) )*100, 1))
                                
                                colnames(corruption_dep_census) <- c("departamento", "n", "pop17", "N_dpt_claims100Kpop_corruption", "percentage")
                        }
                        
                        
                        
                        
                        reactable(
                                corruption_dep_census[,c("departamento", "n", "percentage", "N_dpt_claims100Kpop_corruption")],
                                pagination = TRUE,
                                defaultSorted = "n",
                                defaultColDef = colDef(headerClass = "header", align = "left"),
                                columns = list(
                                        departamento = colDef(name = "departamento"
                                                              
                                        ),
                                        n = colDef(
                                                name = "N° de hechos",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(corruption_dep_census$n), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#3fc1c9")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        percentage = colDef(
                                                name = "Porcentaje",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        # Format as percentages with 1 decimal place
                                                        value <- paste0(format(value , nsmall = 1), "%")
                                                        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        ),
                                        N_dpt_claims100Kpop_corruption = colDef(
                                                name = "N° de hechos por 100 mil habitantes",
                                                defaultSortOrder = "desc",
                                                # Render the bar charts using a custom cell render function
                                                cell = function(value) {
                                                        width <- paste0(value * 100 / max(corruption_dep_census$N_dpt_claims100Kpop_corruption), "%")
                                                        # Add thousands separators
                                                        value <- format(value, big.mark = ",")
                                                        bar_chart(value, width = width, fill = "#00aa7f")
                                                },
                                                # And left-align the columns
                                                align = "left"
                                        )
                                )
                        )
                        
                })
                        

                        

        }
)

#runApp("C:/Users/Yoseph/Documents/GitHub/CGR_dashboard/CGR_Dashboard_shinydashboard.R")


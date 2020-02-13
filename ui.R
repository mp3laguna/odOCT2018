#########################################################################
###                                                                   ###
###     VISOR DE LA MATRIZ DE ORIGEN DESTINO DE LA ISLA DE TENERIFE   ###
###                                                                   ###
###     Datos procedentes de los resultados del trabajo de:           ###
###       Análisis de la movilidad y modelización del                 ###
###       sistema de transporte de la isla de Tenerife                ###
###                                                                   ###
###     Desarrollo de Miguel Pintor  mpsepulveda@ciccp.es             ###
###                                                                   ###
#########################################################################



#carga librerias
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyhelper)
library(leaflet)
library(chorddiag)
library(reshape2)
library(rgdal)
library(rgeos)
library(ggplot2)




choiceDia <- list("Laborable" = 1, "Sábado" = 2, "Festivo" = 3)
choiceMotivo <- list("Trabajo" = 1, "Casa" = 2, "Otra actividad frecuente" = 3, "Actividad no frecuente" = 4, "Extranjeros" = 5)
choiceResidencia <- list("Alemania" = 1, "Bélgica" = 2, "Francia" = 3, "Irlanda" = 4, "Italia" = 5,
                         "Países Bajos" = 6, "Países Nórdicos" = 7, "Portugal" = 8, "Reino Unido" = 9, "Resto de España" = 10,
                         "Resto de Europa" = 11, "Resto del mundo" = 12, "Rusia" = 13, "Tenerife" = 14)
choiceDistancia <- list("1000 metros o menos" = 1, "Más de 1000 metros" = 2)

tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))

dashboardPage(skin = "blue", title = "Movilidad Tenerife",              
              
              dashboardHeader(titleWidth = 500, title = div(img(src="images/escudoCabildoMIN.png"), "Matriz OD terminales móviles (0ctubre de 2018)"), 
                              tags$li(class = "dropdown",tags$a(actionBttn(inputId = "email", label = "", icon = icon("envelope", lib = "font-awesome"), 
                                                                           color = "primary", size = "sm"), href="mailto:movilidad@tenerife.es"))), 
              
              dashboardSidebar(width = 280,  
                               
                               selectInput("analisis", h4("Zonificación"), 
                                           choices = list("Macrozonas de Transporte (11)" = 1, "Municipios (31)" = 2, "Zonas de Transporte (150)" = 3), selected = 2) %>%
                                 helper(icon= "question", colour = "white", type = "markdown", content = "zonificacion"),
                               
                               selectInput("od", h4("Generación - Atracción de Viajes"), choices = list("Generados" = 1, "Atraídos" = 2), selected = 1) %>%
                                 helper(icon= "question", colour = "white", type = "markdown", content = "genera_atrae"),
                               
                               pickerInput(inputId = "distancia",
                                           label = h4("Distancia del viaje"),
                                           choices = choiceDistancia,
                                           multiple = TRUE,
                                           selected = choiceDistancia) %>%
                                 helper(icon= "question", colour = "white", type = "markdown", content = "distancia"),
                               
                               pickerInput(inputId = "motivoOrigen", 
                                           label = h4("Actividad en Origen"), 
                                           choices = choiceMotivo,
                                           multiple = TRUE,
                                           selected = choiceMotivo) %>%
                                 helper(icon= "question", colour = "white", type = "markdown", content = "actividad_origen"),
                               
                               pickerInput(inputId = "motivoDestino", 
                                           label = h4("Actividad en Destino"), 
                                           choices = choiceMotivo,
                                           multiple = TRUE,
                                           selected = choiceMotivo) %>%
                                 helper(icon= "question", colour = "white", type = "markdown", content = "actividad_destino"),
                               
                               pickerInput(inputId = "residencia", 
                                           label = h4("Residencia Habitual"), 
                                           choices = choiceResidencia,
                                           multiple = TRUE,
                                           selected = choiceResidencia)%>%
                                 helper(icon= "question", colour = "white", type = "markdown", content = "residencia"),
                               
                               sliderInput("horas", h4("Horas"), min = 0, max = 23, value = c(0, 23)) %>%
                                 helper(icon= "question", colour = "white", type = "markdown", content = "hora"),
                               
                               div(style="text-align: center;",  downloadLink("descargaMatriz", "descargar Matriz"), br(), downloadLink("descargaViajes", "descargar Viajes"), br(), downloadLink("descargaZonas", "descargar zonas"))),
              
              dashboardBody(tags$head(tags$link(rel="shortcut icon", href="favicon.png"), includeScript("mod_google-analytics.js")),
                            fluidRow(tabBox(id = "mainTabs", width = NULL,
                                            tabPanel(title="Mapa", id="mapa", value = "mapa",
                                                     htmlOutput("textTipoGrafico") ,
                                                     leafletOutput("mymap", width = "100%", height = 700) %>% withSpinner() %>% helper(icon= "question", colour = "white", type = "markdown", content = "vista_plano")),
                                            tabPanel("Gráfica por Horas", value = "grafica",
                                                     sidebarLayout(
                                                       sidebarPanel(
                                                         radioButtons("tipoGrafico", h3("Tipo de Gr?fico"), 
                                                                      choices = list("Por Actividad en Origen" = 1, "Por Actividad en Destino" = 2, "Por Residencia" = 3), selected = 1),
                                                         width = 2
                                                       ),
                                                       mainPanel(
                                                         plotOutput("grafica", width = "100%", height = 700) %>% withSpinner()
                                                       )
                                                     )),
                                            tabPanel("Diagrama de Cuerdas", value = "diagramaCuerdas", chorddiagOutput("diagCuerdas", width = "100%", height = 700))
                            )),
                            htmlOutput("auxiliar"))
)


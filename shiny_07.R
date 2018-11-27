# trace(utils:::unpackPkgZip, edit=TRUE)

library(parallel)
library(png)
library(bnlearn)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(zoo)
library(ggplot2)
library(DT)
library("plotly")
library(shinyBS)
library(devtools)
library("forecast")
library(networkD3)
library("shinycssloaders")
library("heatmaply")
library("rintrojs")
library("shinyAce")
library("shinydashboard")
library("shinyWidgets")
library(d3heatmap)
library(lattice)
library(networkD3)
library(rhandsontable)
library(viridisLite)
library(viridis)
library("kernlab")
library("gtools")
library(rintrojs)

setwd("C:/Users/user/Desktop/TFM1")
source("Analisis_descriptivo_menos_shiny_Nutria.R")

options(shiny.maxRequestSize = 50 * 1024 ^ 2)
dat <-
  read.csv("C:/Users/user/Desktop/Forecast M4/final_net.csv", sep = ";")
dat <- dat[,-c(1)]
box_customBackground <- function(box_object, color = '#00868B') {
  new_color_class <-
    paste0('<div class="box box-solid ', color , '">')
  box_modify <-
    gsub('<div class="box">', new_color_class, box_object)
  box_html <- HTML(box_modify)
  
  return(box_html)
}

ui <- shinyUI(dashboardPage(
  dashboardHeader(title = "Time series"),
  
  dashboardSidebar(
    sidebarMenu(
      introjsUI(), 
      tags$style(HTML(
        ".newClass {
      min-width: 800px;
      max-width: 1000px;
      }"
      )),
      menuItem(
        "Introduccion",
        tabName = "practicaA",
        icon = shiny::icon("globe"),
        badgeColor = "orange"
      ),
      menuItem(
        div(
          div(style = "width:80%; display:inline-block; vertical-align: right;",
              "Settings", tabName = "settings"),
          
          div(
            style = "display:inline-block; vertical-align: middle;",
            bsButton(
              "q1",
              label = "",
              style = "info",
              size = "extra-small"
            ),
            bsPopover(
              id = "q1",
              title = "Info",
              content = "Seleccionar el tipo, periodo y longitud de las series temporales a analizar",
              placement = "right",
              trigger = "click",
              options = list(container = "body")
            )
          )
        ),
        
        selectInput(
          "datasetperiod",
          "Periodo",
          choices = c("YEARLY", "QUARTERLY", "MONTHLY", "WEEKLY", "DAILY")
        ),
        
        selectInput(
          "datasetipe",
          "Tipo",
          choices = c(
            "DEMOGRAPHIC",
            "FINANCE",
            "INDUSTRIAL",
            "MACRO",
            "MICRO",
            "OTHER"
          )
        ),
        checkboxInput("checkbox", "Filtro por longitud", 0),
        conditionalPanel(condition = "input.checkbox==1",
                         uiOutput("time_series_longitud")),
        uiOutput("time_series"),
        actionButton("Go", "Ir")
      ),
      menuItem(
        "Description",
        tabName = "practicaB",
        icon = shiny::icon("table"),
        menuSubItem("Caso general", tabName = "subitem5"),
        menuSubItem("Caso especifico", tabName = "subitem6"),
        startExpanded = FALSE
        
      ),
      menuItem(
        "Ajuste y prediccion",
        tabName = "practicaC",
        icon = shiny::icon("bar-chart"),
        menuSubItem("ARIMA", tabName = "subitem1"),
        menuSubItem("ETS", tabName = "subitem2"),
        menuSubItem("Prophet", tabName = "subitem3"),
        menuSubItem("BSTS", tabName = "subitem4"),
        startExpanded = FALSE
      ),
      menuItem(
        "Bayesian Network",
        icon = shiny::icon("arrow-right"),
        tabName = "Bayesian_network",
        menuSubItem("Estructura y probabilidades", tabName = "subitem7"),
        menuSubItem("Inferencia", tabName = "subitem8"),
        menuSubItem("Conclusiones",tabName = "subitem9"),
        startExpanded = FALSE
      ),
      menuItem("Source Code",
               icon = icon("github"),
               href = "https://github.com/AnaBotey/Bayesian-network-to-compare-time-series-models")
    ),
    shiny::bookmarkButton(id = "Link"),
    tags$style(type='text/css', "#Link { display: block; margin: 0 auto; }")  
  ),
  dashboardBody(
    tags$head(tags$style(
      HTML(
        '
        /* logo */
        .skin-blue .main-header .logo {
        background-color: #006b6f;
        font-family: Calibri;
        }
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
        background-color: #00868B;
        }'
      )
    )),
    tags$style(
      type = "text/css",
      ".box-header{background-color: #E7EFF3; text-decoration: overline; color: Black; size: 14; font-family:Impact };
        .box-body {
        border-top-left-radius: 50%;
        border-top-right-radius: 50%;
        border-bottom-right-radius: 50%;
        border-bottom-left-radius: 50%;
        padding: 10px;}
        .shiny-spinner-output-container {
        position: relative;
        box-sizing: 100%;
        }}"
    ),
    
    tabItems(
      tabItem(tabName = "practicaA",
              fluidRow(
                column(
                  width = 12,
                  box(
                    title="",
                    width = NULL,
                    style = "text-align: justify;padding-left: 3%;padding-right: 3%;font-size: 12",
                    div(img(src="Aaaa.png", width = "65%", height = "40%", align = "center"), align = "center"),
                    fluidRow(style = "text-align: center", h2("Pasos a seguir")),
                    fluidRow(
                      style = "text-align: center",
                      box(
                        
                        div(img(src='tablaEventos.png', align = "center", width = "80%" , height = "500px"), align="center"),
                        collapsible = TRUE,
                        collapsed = T,
                        class = "not_others",
                        style = "text-align: justify;padding-left: 10%;padding-right: 10%;font-size: 12",
                        title = "1. Tabla de eventos",
                        width = "100%"
                      )
                    ),
                    
                    fluidRow(
                      style = "text-align: center",
                      box(
                        img(src='Fit&forecast.png', align = "center", width = "100%" , height = "350px"),
                        collapsible = TRUE,
                        collapsed = T,
                        class = "not_others",
                        style = "text-align: justify;padding-left: 10%;padding-right: 10%;font-size: 12",
                        title = "2. Descrip,ajust,predic ",
                        width = "100%"
                      )
                    ),
                    fluidRow(
                      style = "text-align: center",
                      box(
                        img(src='red.png', align = "center", width = "100%" , height = "250px"),
                        collapsible = TRUE,
                        collapsed = T,
                        class = "not_others",
                        title = "3.Comparacion resultados de los modelos",
                        style = "text-align: justify;padding-left: 10%;padding-right: 10%;font-size: 12",
                        width = "100%"
                      )
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "subitem5",
              
              tabsetPanel(
                tabPanel("Caso General",
                         fluidRow(
                           style = "margin-left:1vw;",
                           box(
                             width = NULL,
                             column(
                               width = 12,
                               style = "text-align: justify;padding-left: 8%;padding-right: 8%;font-size: 12",
                               h2("Analisis descriptivo del dataset M4", style = "text-align: center;"),
                               h4("El dataset empleado tiene un total de 100.000 series temporales de diferentes tipos 
                           (demograficas, financieras, industriales, micro, macro y otras) y distintas granularidades 
                                  (anuales, trimestrales, mensuales, semanales, diarias, horarias)."),
                               h4("La muestra de test empleada es 6 periodos para las series anuales, 8 para las trimestrales,	18 para las mensuales,
                          13 para las semanales,	14 para las diarias y	48 para las horarias."),
                               h4("La muestra para predecir ha sido del total de series para los modelos ARIMA y ETS 
                              y la mitad de series temporales para los
                            modelos BSTS y Prophet debido a su elevado tiempo de ejecucion."),
                               p("Descargar dataset:"),
                               shiny::actionButton(
                                 inputId = 'ab1',
                                 label = "M4 dataset",
                                 icon = icon("th"),
                                 onclick =
                                   "window.open('https://www.m4.unic.ac.cy/')"
                               )
                             ),
                             fluidRow(
                               column(
                                 width=6,
                                 box(style = "text-align: center;",
                                     title = "Tipo",
                                     collapsible = TRUE,
                                     width = NULL,
                                     style = "justify;padding-left: 3%;padding-right: 3%;font-size: 12",
                                     plotlyOutput("dounut_type"))
                               ),
                               column(style = "text-align: center;",
                                      width=6,           
                                      box(title = "Periodo",
                                          collapsible = TRUE,
                                          width = NULL,
                                          plotlyOutput("dounut_granularity"))
                               )
                             ),
                             fluidRow(
                               style = "margin-left:3vw;",
                               box(
                                 DT::dataTableOutput("summary_total_time_series"),
                                 collapsible = TRUE,
                                 class = "not_others",
                                 style = "text-align: justify;padding-left: 10%;padding-right: 10%;font-size: 12",
                                 title = "Frecuency table",
                                 collapsed = TRUE,
                                 width = "100%",
                                 height = "100%"
                               )
                             ),
                             fluidRow(
                               style = "margin-left:3vw;",
                               sankeyNetworkOutput(
                                 "sankey", width = "100%", height = "500px")
                             )
                           )
                         )
                ),
                tabPanel(
                  "Particular case",
                  fluidPage(
                    style = "text-align: justify;",
                    box(
                      class = "not_others",
                      style = "text-align: justify;padding-left: 3%;padding-right: 3%;font-size: 12",
                      width = 12,
                      h2(style = "text-align: center;", "Analisis descriptivo de las series temporales elegidas"),
                      fluidRow(
                        column(4,
                               box(
                                 width = NULL,
                                 numericInput("polinomial_trend", "Coeficiente de la tendencia polinomial", 
                                              value = 1),
                                 numericInput("number_diff", "numero de diferencias", 0),
                                 uiOutput("time_series_ACF"),
                                 title="Parametros"
                               )
                        ),
                        column(8,
                               box(
                                 width = NULL,
                                 style = "margin-left:3vw;",
                                 withSpinner(plotlyOutput("grafica1")),
                                 collapsible = T,
                                 title = "Series temporales seleccionadas"
                               )
                        )
                      ),
                      fluidRow(style = "margin-left:1vw;",
                               column(
                                 12,
                                 box(
                                   withSpinner(plotlyOutput("ACF_initial")),
                                   collapsible = T,
                                   title = "ACF"
                                 ),
                                 box(
                                   title = "PACF",
                                   withSpinner(plotlyOutput("PACF_initial"))
                                 )
                               )
                      )
                    )
                  )
                )
              )
      ),
      
      tabItem(
        tabName = "subitem1",
        tabsetPanel(
          tabPanel("ARIMA",
                   fluidPage(
                     style = "text-align: center;",
                     box(
                       class = "not_others",
                       style = "text-align: justify;padding-left: 1%;padding-right: 1%;font-size: 12",
                       width = 12,
                       h2(style = "text-align: center;",
                          "Ajuste y prediccion ARIMA"),
                       
                       # Add introjs btn
                       shiny::actionButton("homeIntro", "Explicacion"),
                       tags$style(type='text/css', "#homeIntro { display: block; margin: 0 auto; }"
                       ),
                       fluidRow(
                         style = "margin-left:3vw;",
                         box(
                           withSpinner(plotlyOutput("grafica2")),
                           width = 12,
                           collapsible = T,
                           title = "Model ARIMA"
                         )
                       ),
                       fluidRow(style = "margin-left:1vw;",
                                column(
                                  12,
                                  box(
                                    withSpinner(tableOutput("accuracy_model_test_ARIMA")),
                                    collapsible = T,
                                    title = "Accuracy test"
                                  ),
                                  box(
                                    uiOutput("time_series_residuals"),
                                    withSpinner(plotOutput("grafica_residuos"))
                                  )
                                )
                       )
                       
                     )
                   )
          ),
          tabPanel("ETS",
                   fluidPage(
                     style = "text-align: center;",
                     box(
                       class = "not_others",
                       style = "text-align: justify;padding-left: 1%;padding-right: 1%;font-size: 12",
                       width = 12,
                       h2(style = "text-align: center;",
                          "Ajuste y prediccion ETS"),
                       # Add introjs btn
                       shiny::actionButton("modeloETS", "Explicacion"),
                       tags$style(type='text/css', "#modeloETS { display: block; margin: 0 auto; }"
                       ),
                       fluidPage(
                         style = "text-align: center;",
                         fluidRow(
                           style = "margin-left:3vw;",
                           box(
                             withSpinner(plotlyOutput("grafica3")
                             ),
                             width = 12,
                             collapsible = T,
                             title = "Model ETS"
                           )
                         ),
                         
                         fluidRow(style = "margin-left:1vw;",
                                  column(
                                    12,
                                    box(
                                      withSpinner(tableOutput("accuracy_model_test_ETS")),
                                      collapsible = T,
                                      title = "Accuracy test"
                                    ),
                                    box(
                                      uiOutput("time_series_descompose"),
                                      withSpinner(plotOutput("model_decompose"))
                                    )
                                  )
                         )
                       )
                     )
                   )
          ),
          tabPanel("PROPHET" ,
                   fluidPage(
                     style = "text-align: center;",
                     box(
                       class = "not_others",
                       style = "text-align: justify;padding-left: 1%;padding-right: 1%;font-size: 12",
                       width = 12,
                       h2(style = "text-align: center;",
                          "Ajuste y prediccion modelo Prophet"),
                       # Add introjs btn
                       shiny::actionButton("modeloPROPHET", "Explicacion"),
                       tags$style(type='text/css', "#modeloPROPHET { display: block; margin: 0 auto; }"
                       ),
                       fluidPage(
                         style = "text-align: center;",
                         fluidRow(
                           style = "margin-left:3vw;",
                           box(
                             withSpinner(plotlyOutput("grafica4")),
                             width = 12,
                             collapsible = T,
                             title = "Model PROPHET"
                           )
                         ),
                         
                         fluidRow(style = "margin-left:1vw;",
                                  column(
                                    12,
                                    box(
                                      withSpinner(tableOutput("accuracy_model_test_Prophet")),
                                      collapsible = T,
                                      title = "Accuracy test"
                                    ),
                                    box(
                                      uiOutput("time_series_descompose_Prophet"),
                                      withSpinner(plotOutput("model_decompose_Prophet"))
                                    )
                                  )
                         )
                       )
                     )
                   )
          ),
          tabPanel("BSTS",
                   fluidPage(
                     style = "text-align: center;",
                     box(
                       class = "not_others",
                       width = 12,
                       h2(style = "text-align: center;", "Ajuste y prediccion BSTS"),
                       # Add introjs btn
                       shiny::actionButton("modeloBSTS", "Explicacion"),
                       tags$style(type='text/css', "#modeloBSTS { display: block; margin: 0 auto; }"
                       ),
                       fluidPage(
                         style = "text-align: center;",
                         
                         fluidRow(
                           style = "margin-left:1vw;",
                           #intro
                           box(
                             withSpinner(plotlyOutput("grafica5")),
                             width = 12,
                             # data.step = 2,
                             # data.intro = img(src='organigramaBSTS.png', align = "center", width = "60%" , height = "80%"),
                             # data.position =c("auto"),
                             collapsible = T,
                             title = "Modelo BSTS"
                           )
                         ),
                         
                         fluidRow(style = "margin-left:1vw;",
                                  column(
                                    12,
                                    box(
                                      withSpinner(tableOutput("accuracy_model_test_Bsts")),
                                      collapsible = T,
                                      title = "Accuracy test"
                                    ),
                                    box(
                                      uiOutput("time_series_descompose_bsts"),
                                      withSpinner(plotOutput("model_decompose_bsts"))
                                    )
                                  )
                         )
                       )
                     )
                   )
          )
        )
      ),
      tabItem(tabName = "subitem7",
              tabsetPanel(
                tabPanel(
                  "Estructura y parametros",
                  
                  fluidRow(
                    column(
                      width = 4,
                      box(
                        title = "Score de la red",
                        collapsible = TRUE,
                        width = NULL,
                        selectInput(
                          "type",
                          h5("Score de la red:"),
                          c(
                            "Log-Likelihood" = "loglik",
                            "Akaike Information Criterion" = "aic",
                            "Bayesian Information Criterion" = "bic",
                            "Bayesian Equivalent" = "be"
                          ),
                          'loglik-g'
                        ),
                        verbatimTextOutput("score")
                      ),
                      shiny::actionButton("modeloRED", "Explicacion"),
                      tags$style(type='text/css', "#modeloBSTS { display: block; margin: 0 auto; }"
                      )
                      # box(
                      #   title = "Probabilidades condicionadas",
                      #   collapsible = TRUE,
                      #   width = NULL,
                      #   shiny::selectInput(
                      #     "Node",
                      #     h5("Node"),
                      #     choices = c("nombre", "clase_mape", "longitud", "tipo", "periodo")
                      #   )
                      # )
                    ),
                    column(
                      width = 8,
                      shinydashboard::box(
                        title = "Red Bayesiana",
                        collapsible = TRUE,
                        width = NULL,
                        networkD3::simpleNetworkOutput("netPlot")
                      )
                    ),
                    fluidPage(style = "text-align: center;",
                              fluidRow(
                                style = "margin-left:2vw;",
                                column(
                                  width = 12,
                                  height = 600,
                                  style = "text-align: justify;padding-left: 2%;padding-right: 2%;font-size: 12",
                                  shinydashboard::box(
                                    style = "text-align: justify;padding-left: 2%;padding-right: 2%;font-size: 12",
                                    width = 12,
                                    height = 800,
                                    title = "Probabilidades condicionadas",
                                    collapsible = TRUE,
                                    #shiny::imageOutput("condPlot", width = "80%", height = "500px")
                                    #img(scr="clase_mape_tipo.png")
                                    div(img(src="clase_mape_tipo.png", width = "100%", height = "90%", align = "center"), align = "center")
                                  )
                                )
                              )
                    )
                  )
                ),
                tabPanel("Inferencia",
                         fluidRow(
                           column(
                             width = 4,
                             box(
                               title = "Evidence",
                               collapsible = TRUE,
                               width = NULL,
                               helpText("Selecciona las caracteristicas conocidas (evidencias):"),
                               
                               selectInput(
                                 "evidence2", label = h5("Evidencia nod0 longitud:"), 
                                 choices=c( "Dlongitud>234", "Alongitud<=56","B56<longitud<=102", "C102<longitud<234")
                               ),
                               selectInput(
                                 "evidence3", label = h5("Evidencia nodo periodo:"), 
                                 choices=c( "MONTHLY", "YEARLY", "QUARTERLY", "WEEKLY", "DAILY")
                               ),
                               selectInput(
                                 "evidence4", label = h5("Evidencia nodo tipo:"), 
                                 choices=c("DEMOGRAPHIC", "MICRO", "MACRO", "INDUSTRIAL", "FINANCE", "OTHER")
                               )
                             )
                           ),
                           column(
                             width = 8,
                             box(
                               title = "Evento nodo: clase_mape y modelo empleado",
                               collapsible = TRUE,
                               width = NULL,
                               plotlyOutput("distPlot")
                               
                             )
                           )
                         )
                ),
                tabPanel("conclusiones",
                         fluidRow(style = "text-align: center;",
                                  column(
                                    width = 12,
                                    box(
                                      title="Resultados obtenidos",
                                      width = NULL,
                                      style = "text-align: justify;padding-left: 3%;padding-right: 3%;font-size: 12",
                                      div(img(src="resultados+git.png", width = "70%", height = "70%", align = "center"), align = "center")
                                    )
                                    
                                  )
                         )
                )
              )
      )
      
    )
  )
)
)






server <- shinyServer(function(input, output, session) {
  
  hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"='alert("Wasn\'t that hint helpful")'))
  
  
  shiny::observeEvent(input$homeIntro,{
    
    homeHelp <- data.frame(
      element=c("#homeIntro", "#grafica2", "#accuracy_model_test_ARIMA","#grafica_residuos" ),
      intro = c(" Modelo ARIMA con la libreria forecast y la funcion auto.arima",
                HTML('<img src="organigramaARIMA.png", align = "center", width = "300px" , height = "400px">'),
                HTML('<img src="MedidasBondadAjuste.png", align = "center", width = "80%" , height = "150px">'),
                "ACF y PACF de los residuos del modelo estimado para cada serie temporal seleccionada"),
      position=c("auto","bottom-middle-aligned","bottom-right-aligned", "bottom-right-aligned")
    )
    rintrojs::introjs(session, options = list(steps = homeHelp,
                                              events = list("oncomplete"='alert("Glad that is over")',
                                                            "onbeforechange"='
                                    if (targetElement.getAttribute("data-step")==="1") {
                                    $(".newClass").css("max-width", "400px").css("min-width","400px");  
                                    } else {
                                    $(".newClass").css("max-width", "3000px").css("min-width","2000px");
                                    }'))
    )
  })
  
  shiny::observeEvent(input$modeloETS,{
    
    ETSHelp <- data.frame(
      element=c("#modeloETS", "#grafica3", "#accuracy_model_test_ETS","#model_decompose" ),
      intro = c(" Modelo ETS con la libreria forecast y la funcion ets",
                HTML('<img src="organigramaETS.png", align = "center", width = "300px" , height = "400px">'),
                HTML('<img src="MedidasBondadAjuste.png", align = "center", width = "80%" , height = "150px">'),
                "Descomposicion de la serie temporal en las componentes de estado del modelo ETS"),
      position=c("auto","bottom-middle-aligned","bottom-right-aligned", "bottom-right-aligned")
    )
    rintrojs::introjs(session, options = list(steps = ETSHelp,
                                              events = list("oncomplete"='alert("Glad that is over")',
                                                            "onbeforechange"='
                                                            if (targetElement.getAttribute("data-step")==="1") {
                                                            $(".newClass").css("max-width", "400px").css("min-width","400px");  
                                                            } else {
                                                            $(".newClass").css("max-width", "3000px").css("min-width","2000px");
                                                            }'))
    )
  })
  
  shiny::observeEvent(input$modeloPROPHET,{
    
    PROPHETHelp <- data.frame(
      element=c("#modeloPROPHET", "#grafica4", "#accuracy_model_test_Prophet","#model_decompose_Prophet" ),
      intro = c(" Modelo PROPHET con la libreria prophet",
                HTML('<img src="organigramaPROPHET.png", align = "center", width = "300px" , height = "400px">'),
                HTML('<img src="MedidasBondadAjuste.png", align = "center", width = "80%" , height = "150px">'),
                "Descomposicion de la serie temporal en las componentes de estado estimadas por el modelo Prophet"),
      position=c("auto","bottom-middle-aligned","bottom-right-aligned", "bottom-right-aligned")
    )
    rintrojs::introjs(session, options = list(steps = PROPHETHelp,
                                              events = list("oncomplete"='alert("Glad that is over")',
                                                            "onbeforechange"='
                                    if (targetElement.getAttribute("data-step")==="1") {
                                    $(".newClass").css("max-width", "400px").css("min-width","400px");  
                                    } else {
                                    $(".newClass").css("max-width", "3000px").css("min-width","2000px");
                                    }'))
    )
  })
  
  shiny::observeEvent(input$modeloBSTS,{
    
    BSTSHelp <- data.frame(
      element=c("#modeloBSTS", "#grafica5", "#accuracy_model_test_Bsts","#model_decompose_bsts" ),
      intro = c(" Modelo BSTS con la libreria bsts",
                HTML('<img src="organigramaBSTS.png", align = "center", width = "250px" , height = "450px">'),
                HTML('<img src="MedidasBondadAjuste.png", align = "center", width = "80%" , height = "150px">'),
                " Descomposicon de la serie temporal en componentes las componentes de estado estimadas por el modelo bsts"),
      position=c("auto","auto","bottom-right-aligned", "bottom-right-aligned")
    )
    rintrojs::introjs(session, options = list(steps = BSTSHelp,
                                              events = list("oncomplete"='alert("Glad that is over")',
                                                            "onbeforechange"='
                                    if (targetElement.getAttribute("data-step")==="1") {
                                    $(".newClass").css("max-width", "400px").css("min-width","400px");  
                                    } else {
                                    $(".newClass").css("max-width", "3000px").css("min-width","2000px");
                                    }'))
    )
  })
  
  
  shiny::observeEvent(input$modeloRED,{
    
    homeHelp <- data.frame(
      element=c("#modeloRED", "#netPlot"),
      intro = c(" Red bayesiana discreta: libreria bnlearn ",
                HTML('<img src="organigramaREDBAYES.png", align = "center", width = "400px" , height = "500px">')
      ),
      position=c("auto","bottom-middle-aligned")
    )
    rintrojs::introjs(session, options = list(steps = homeHelp,
                                              events = list("oncomplete"='alert("Glad that is over")',
                                                            "onbeforechange"='
                                                            if (targetElement.getAttribute("data-step")==="1") {
                                                            $(".newClass").css("max-width", "400px").css("min-width","400px");  
                                                            } else {
                                                            $(".newClass").css("max-width", "3000px").css("min-width","2000px");
                                                            }'))
    )
  })
  
  type <- reactive(input$datasetipe)
  period <- reactive(input$datasetperiod)
  
  datos <- eventReactive(input$Go, {
    generate_data_series(
      M4_final,
      input$datasetperiod,
      input$datasetipe,
      input$time_series,
      input$checkbox,
      input$longitud
    )
  })
  datos_arima <- eventReactive(input$Go, {
    generate_data_series(
      arima_timeseries,
      input$datasetperiod,
      input$datasetipe,
      input$time_series,
      input$checkbox,
      input$longitud
    )
  })
  datos_ets <- eventReactive(input$Go, {
    generate_data_series(
      ets_timeseries,
      input$datasetperiod,
      input$datasetipe,
      input$time_series,
      input$checkbox,
      input$longitud
    )
  })
  datos_prophet <- eventReactive(input$Go, {
    generate_data_series(
      prophet_timeseries,
      input$datasetperiod,
      input$datasetipe,
      input$time_series,
      input$checkbox,
      input$longitud
    )
  })
  datos_bsts <- eventReactive(input$Go, {
    generate_data_series(
      bsts_timeseries,
      input$datasetperiod,
      input$datasetipe,
      input$time_series,
      input$checkbox,
      input$longitud
    )
  })
  output$sankey <- renderSankeyNetwork({
    plot_sankey_diagram()
  })
  
  
  
  output$vbox1 <-
    renderInfoBox({
      valueBox(input$datasetipe,
               "tipo",
               width = 1,
               icon = icon("table"))
    })
  output$vbox2 <-
    renderValueBox({
      valueBox(
        input$datasetperiod,
        "granularidad",
        width = 2,
        icon = icon("bar-chart-o")
      )
    })
  
  output$dounut_type <- renderPlotly({
    count <- c(8708, 24534, 18798, 19402, 25121, 3437)
    manuf <-
      c("Demographic",
        "Finance",
        "Industry",
        "Macro",
        "Micro",
        "Other")
    plot_ly(
      labels = ~ manuf,
      values = ~ count,
      names =  ~ names
    ) %>%
      add_pie(hole = 0.6) %>%
      layout(
        title = "Type",
        showlegend = T,
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    
    
  })
  output$dounut_granularity <- renderPlotly({
    manuf <-
      c("Yearly",
        "Quarterly",
        "Monthly",
        "Weekly",
        "Daily",
        "Hourly")
    count <- c(23000, 24000, 48000, 359, 4227, 414)
    
    plot_ly(
      labels = ~ manuf,
      values = ~ count,
      names =  ~ names
    ) %>%
      add_pie(hole = 0.6) %>%
      layout(
        title = "Granularity",
        showlegend = T,
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    
  })
  
  output$texto3 <- renderText({
    "Fit and Forecast models"
  })
  output$texto10 <- renderText({
    "Visualizacion M4 competition"
  })
  
  output$summary_total_time_series <- DT::renderDataTable({
    make_table_timeseries()
  })
  
  output$box_plot <- renderPlotly({
    plot_data_a(datos())
  })
  
  
  output$ACF_initial <- renderPlotly({
    autocorrelation_plot(input$Time_series_ACF,"correlation", input$number_diff)
  })
  
  
  
  output$PACF_initial <- renderPlotly({
    autocorrelation_plot(input$Time_series_ACF, "partial", input$number_diff)
    
  })
  
  output$simple <- renderDiagonalNetwork({
    plot_description_paso1()
  })
  
  output$simple2 <- renderDiagonalNetwork({
    plot_description_paso3()
  })
  output$simple3 <- renderDiagonalNetwork({
    plot_description_paso4()
  })
  output$model_decompose <-
    renderPlot(plot_decompose(input$Time_series_descompose, ets_all(datos())))
  output$grafica20 <- renderPlot(pieChart_period_total(M4_final))
  
  output$grafica1 <- renderPlotly({
    plot_data(datos(), input$polinomial_trend, input$number_diff)
    
  })
  
  output$grafica2 <- renderPlotly({
    plot_model("ARIMA", datos_arima(), input$datasetperiod)
    
  })
  output$accuracy_model_test_ARIMA <- renderTable(accuracy <-
                                                    t(accuracy_model_arima(datos_arima())),
                                                  rownames = T,
                                                  colnames = T)
  output$grafica3 <- renderPlotly({
    plot_model("ETS", datos_ets())
    
  })
  output$accuracy_model_test_ETS <- renderTable(accuracy <-
                                                  t(accuracy_model_ets(datos_ets())),
                                                rownames = T,
                                                colnames = T)
  
  output$grafica4 <- renderPlotly({
    plot_model_Prophet("PROPHET", datos_prophet())
    
  })
  output$accuracy_model_test_Prophet <- renderTable(accuracy <-
                                                      t(accuracy_model_PROPHET(datos_prophet())),
                                                    rownames = T,
                                                    colnames = T)
  
  output$grafica10 <- renderPlot({
    pieChart_type_total(M4_final)
    
    
  })
  
  output$time_series <- renderUI({
    names_series <-
      as.data.table(unlist(lapply(generate_time_series_name(
        M4_final,
        input$datasetperiod,
        input$datasetipe,
        input$checkbox,
        input$longitud
      ),
      function(x)
        (x$st))))
    selectizeInput(
      'time_series',
      'Select time series',
      choices = names_series,
      multiple = TRUE,
      options = list(maxItems = 20)
    )
  })
  
  output$summary_time_series <-
    renderTable(expr = t(summary_data(datos())),
                rownames = T)
  output$time_series_longitud <- renderUI({
    max_period <-
      max_min_period(M4_final, input$datasetperiod, input$datasetipe)
    sliderInput(
      "longitud",
      "Size interval",
      min = max_period[2],
      max = max_period[1],
      value = c(max_period[2],
                as.integer((
                  max_period[1] + max_period[2]
                ) / 2)),
      step = 1
    )
    
  })
  output$accuracy_model_test_Bsts <- renderTable(accuracy <-
                                                   t(accuracy_model_bsts(datos_bsts())),
                                                 rownames = T,
                                                 colnames = T)
  
  output$grafica5 <- renderPlotly(plot_model_bsts(datos_bsts()))
  output$grafica_residuos <- renderPlot({
    plot_residuals(input$Time_series_residuals, datos_arima())
  })
  
  output$time_series_residuals <- renderUI({
    selectInput('Time_series_residuals',
                'Time series residuals',
                choices = input$time_series)
    
  })
  output$time_series_ACF <- renderUI({
    selectInput('Time_series_ACF',
                'ACF y PACF: Selecciona una serie',
                choices = input$time_series)
    
  })
  output$time_series_descompose <- renderUI({
    selectInput('Time_series_descompose',
                'Time series decompose',
                choices = input$time_series)
    
  })
  
  output$time_series_descompose_Prophet <- renderUI({
    selectInput(
      'Time_series_descompose_Prophet',
      'Time series decompose',
      choices = input$time_series
    )
    
  })
  
  output$time_series_descompose_bsts <- renderUI({
    selectInput('Time_series_descompose_bsts',
                'Time series decompose',
                choices = input$time_series)
    
  })
  
  output$model_decompose_Prophet <- renderPlot({
    plot_component_prophet(input$Time_series_descompose_Prophet, datos_prophet())
  })
  output$model_decompose_bsts <- renderPlot({
    plot_aditional_bsts(datos_bsts(), input$Time_series_descompose_bsts)
  })
  
  output$name_arima_models <- renderText(
    name_fitted_arima_models(
      input$Time_series_residuals,
      datos_arima()$forecast_arima$model
    )
    
  )
  output$grafica99 <- renderPlotly({
    plot_arima_demographic(arima_timeseries, input$datasetipe_conclutions, "ARIMA")
    
  })
  
  output$grafica100 <- renderPlotly({
    plot_arima_demographic(ets_timeseries, input$datasetipe_conclutions, "ETS")
    
  })
  
  
  output$net <- renderForceNetwork(plotD3bn(res))
  output$probability_categories <- renderPlot({
    plot_probability_node(res, arima_ets_tree, input$node)
    
  })
  
  
  dag <- shiny::reactive({
    white_list <-
      matrix(
        c ("longitud", "clase_mape", "periodo", "clase_mape"),
        ncol = 2 ,
        byrow = TRUE
      )
    black_list <-
      matrix(
        c (
          "nombre" ,
          "tipo",
          "tipo",
          "nombre",
          "longitud",
          "nombre",
          "nombre",
          "longitud",
          "periodo",
          "nombre",
          "nombre",
          "periodo",
          "tipo",
          "clase_mape",
          "clase_mape",
          "nombre"
        ),
        ncol = 2 ,
        byrow = TRUE
      )
    
    red_hc <-
      cextend(
        hc(
          x = dat,
          debug = FALSE,
          whitelist = white_list,
          blacklist = black_list,
          score = NULL,
          restart = 0,
          perturb = 1,
          max.iter = Inf,
          maxp = Inf,
          optimized = TRUE
        )
      )
    red_hc
    
  })
  
  output$netPlot <- networkD3::renderSimpleNetwork({
    networkData <- data.frame(bnlearn::arcs(dag()))
    varNames <- nodes(dag())
    links <- data.frame(arcs(dag())) %>%
      mutate(
        from = match(from, varNames) - 1,
        to = match(to, varNames) - 1,
        value = 1
      )
    nodes <-
      data.frame(name = varNames) %>% mutate(group = 1, size = 30)
    networkD3::forceNetwork(
      Links = links,
      Nodes = nodes,
      Source = "from",
      Target = "to",
      Value = "value",
      NodeID = "name",
      Group = "group",
      zoom = TRUE,
      arrows = TRUE,
      opacityNoHover = 1,
      opacity = 0.75
    )
    
  })
  
  output$score <- shiny::renderText({
    if (input$type == "loglik") {
      bnlearn::score(dag(), dat, type = "loglik")
    } else if (input$type == "aic") {
      bnlearn::score(dag(), dat, type = "aic")
    } else if (input$type == "bic") {
      bnlearn::score(dag(), dat, type = "bic")
    } else {
      bnlearn::score(dag(), dat, type = "bde")
    }
  })
  
  fit <- shiny::reactive({
    
    fit <- bnlearn::bn.fit(dag(), dat, method = "bayes", iss = input$iss)
    
  })
  
  
  #output$condPlot <- shiny::renderImage({
  # if (input$Node=="nombre"){
  #   div(img(src='nombre.png', align = "center", 
  #           width = "80%" , height = "500px"), align="center")
  # }else if (input$Node=="tipo"){
  #   div(img(src='tipo.png', align = "center", 
  #           width = "80%" , height = "500px"), align="center")
  #   
  # }else if (input$Node=="clase_mape"){
  #   div(img(src='clase_mape.png', align = "center", 
  #           width = "80%" , height = "500px"), align="center")
  #   
  # }else if (input$Node=="longitud"){
  #   div(img(src='longitud.png', align = "center", 
  #           width = "80%" , height = "500px"), align="center")
  #   
  # }else{
  #   div(img(src='periodo.png', align = "center", 
  #           width = "80%" , height = "500px"), align="center")
  #   
  # }
  #    outfile <- tempfile(fileext='periodo.png')
  #      png(outfile)
  # chart <- fit()[[input$Node]]
  # # dev.off()
  # plots <- bnlearn::bn.fit.barchart(chart)
  # plots
  
  
  #  })
  
  
  
  output$distPlot <- renderPlotly({
    list <- c("ARIMA", "ETS", "BSTS", "Prophet")
    prob <- NULL
    for (i in (1:length(list))){
      evidence= paste0("(tipo == '",input$evidence4,"') & ( periodo == '",
                       input$evidence3, "') & (longitud=='", input$evidence2,"')")
      event <- paste0("(clase_mape=='AMAPE<=3') & (nombre  =='", list[i], "')")
      nodeProbs = paste("cpquery(fit(), ", evidence, ", ", event, ")", sep = "")
      eval(parse(text = nodeProbs))
      prob[i] <- eval(parse(text = nodeProbs))
    }
    data <- data.frame(list, prob)
    # p <- plot_ly(
    #   x = list,
    #   y = prob,
    #   name = "SF Zoo",
    #   type = "bar"
    # )
    p <- plot_ly(data, x = ~list, y = ~prob, type = 'bar', text = text,
                 marker = list(color = 'rgb(158,202,225)',
                               line = list(color = 'rgb(8,48,107)',
                                           width = 1.5)))
    p
    # barplot(
    #   prob,
    #   col = "lightblue",
    #   main = "Conditional Probabilities AMPAE<=3",
    #   border = NA,
    #   xlab = "Levels models",
    #   ylab = "Probabilities",
    #   ylim = c(0, 1)
    # )
    
  })
  # bookmarking
  observeEvent(input$Link, {
    session$doBookmark()
  })
  
  enableBookmarking(store = "url")
  
  
})


# Shiny App
#=============


shinyApp(ui = ui, server = server)
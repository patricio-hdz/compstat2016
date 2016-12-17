library(shiny)
library(shinydashboard)
library(reshape2)
library(plyr)
library(ggplot2)
library(MASS)
library(DT)
options(shiny.maxRequestSize = 9*1024^2)
Rcpp::sourceCpp("myMCMC.cpp")


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Final"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Funcion Inversa", tabName = "funinv", icon = icon("bar-chart")),
      menuItem("Integral", tabName = "integral", icon = icon("bar-chart")),
      menuItem("Analisis de Regresion Bayesiano", tabName = "carga", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "funinv",
              fluidRow(titlePanel("Simulando Exponenciales"),
                       sidebarLayout(          
                         sidebarPanel(
                           numericInput(inputId = "lambda", 
                                        label = "Parametro de la Distribucion",
                                        10),
                           
                           numericInput(inputId = "sim", 
                                        label = "Numero de simulaciones",
                                        1000),
                           
                           numericInput(inputId = "yaxis", 
                                        label = "Escala para Y",
                                        10),
                           
                           numericInput(inputId = "seed", 
                                        label = "Semilla",
                                        160829)),
                         box(
                           plotOutput("hist"),
                           textOutput("summary"))
                       )
                       
              )
      ),
      
      # Second tab content
      tabItem(tabName = "integral",
              fluidRow(titlePanel("Integracion Monte Carlo"),
                       sidebarLayout(          
                         sidebarPanel(
                           textInput(inputId = "fun", 
                                     label = "Funcion a integrar",
                                     value = "x**2"),
                           
                           numericInput(inputId = "inf", 
                                        label = "Limite Inferior",
                                        0),
                           
                           numericInput(inputId = "sup", 
                                        label = "Limite Superior",
                                        1),
                           
                           numericInput(inputId = "sig", 
                                        label = "Significancia",
                                        .05)
                         ),box(
                           h2("Grafica Simulacion Monte Carlo con I.C."),
                           plotOutput("integral"),
                           textOutput("resintegral"))
                       )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "carga",
              titlePanel("Carga de archivo"),
              tabsetPanel(
                tabPanel("Cargar el Dataset",
                         titlePanel("Cargando Datos"),
                         sidebarLayout(
                           sidebarPanel(
                             fileInput('file1', 'Choose CSV File',
                                       accept=c('text/csv', 
                                                'text/comma-separated-values,text/plain', 
                                                '.csv')),
                             
                             # added interface for uploading data from
                             # http://shiny.rstudio.com/gallery/file-upload.html
                             tags$br(),
                             checkboxInput('header', 'Header', TRUE),
                             radioButtons('sep', 'Separator',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t'),
                                          ','),
                             radioButtons('quote', 'Quote',
                                          c(None='',
                                            'Double Quote'='"',
                                            'Single Quote'="'"),
                                          '"')
                             
                           ),
                           mainPanel(
                             tableOutput('contents')
                           )
                         )
                ),
                tabPanel("Ver Distribuciones para Variables",
                         pageWithSidebar(
                           headerPanel('Plot de X vs Y'),
                           sidebarPanel(
                             
                             # "Empty inputs" - they will be updated after the data is uploaded
                             selectInput('xcol', 'X Variable', ""),
                             selectInput('ycol', 'Y Variable', "", selected = ""),
                             numericInput("number", "Numero de cadenas", value=1, min=1, max=4, step=1),
                             sliderInput("length", "Tamaño de la cadena", min=10000, max=100000, value=50000),
                             sliderInput("sBurnin", "Burnin", min=100, max=5000, value=1000),
                             actionButton("button", "Comenzar!!")  
                             
                           ),
                           mainPanel(
                             tabsetPanel(type="tabs",
                                         tabPanel("Data", 
                                                  fluidRow(
                                                    plotOutput("MyPlot1"),
                                                    h2("A PRIORIS"),
                                                    h4("Alfa con a priori Normal"),
                                                    plotOutput("distribalfa"),
                                                    h4("Beta con a priori Normal"),
                                                    plotOutput("distribeta"),
                                                    h4("Sigma con a priori Gamma"),
                                                    plotOutput("distribsigma")
                                                  )),
                                         tabPanel("Cadenas",
                                                  fluidRow(
                                                    column(8,DT::dataTableOutput("cadenasMC"))
                                                  )),
                                         tabPanel("Histogramas",
                                                  fluidRow(
                                                    column(2, plotOutput("h_alfa")),
                                                    column(2, plotOutput("h_beta")),
                                                    column(2, plotOutput("h_sigma")),
                                                    h1("A priori en rosa y a posteriori en azul"),
                                                    column(2, plotOutput("dens_alfa")),
                                                    column(2, plotOutput("dens_beta")),
                                                    column(2, plotOutput("dens_sigma")),
                                                    h1("Series"),
                                                    column(2, plotOutput("c_alpha")),
                                                    column(2, plotOutput("c_beta")),
                                                    column(2, plotOutput("c_sigma"))
                                                  )),
                                         tabPanel("Valores",
                                                  fluidRow(
                                                    column(8,DT::dataTableOutput("vals")),
                                                    plotOutput("checking")
                                                  ))
                             )
                           )
                         )
                )
                
              )
      )
    )
  ))
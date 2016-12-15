library(shiny)
library(shinydashboard)
library(reshape2)
library(plyr)
library(ggplot2)
options(shiny.maxRequestSize = 9*1024^2)

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Final"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Funcion Inversa", tabName = "funinv", icon = icon("bar-chart")),
      menuItem("Integral", tabName = "integral", icon = icon("bar-chart")),
      menuItem("Cargar Archivo", tabName = "carga", icon = icon("bar-chart")),
      menuItem("Tabla", tabName = "tabla", icon = icon("bar-chart"))
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
              fluidRow(titlePanel("Carga de Archivos"),
                       sidebarLayout(          
                         sidebarPanel(
                           fileInput('file1', 'Choose file to upload',
                                     accept = c(
                                       'text/csv',
                                       'text/comma-separated-values',
                                       'text/tab-separated-values',
                                       'text/plain',
                                       '.csv',
                                       '.tsv'
                                     )
                           ),
                           tags$hr(),
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
                                        '"'),
                           tags$hr()
                         ),box(
                           h2("Datos Cargados"),
                           tableOutput('contents'))
                       )
              )
      )
    )
  ))
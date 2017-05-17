library(shiny)
library(shinydashboard)

data <- read.csv("/home/patricio/Documents/Shiny/wordprob.csv", header = TRUE)


ui <- dashboardPage(
  dashboardHeader(title = "Extropy Chat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interests", tabName = "interests", icon = icon("id-badge")),
      menuItem("Recommended Threads", tabName = "threads", icon = icon("connectdevelop"))
    )),
  dashboardBody(tabItems(
                        tabItem(tabName = "interests",
                                fluidRow(titlePanel("Please select all that apply to your research"),
                                          tags$head(tags$style(HTML('
                                          .main-header .logo {
                                            font-family: "Georgia", Times, "Times New Roman", serif;
                                            font-weight: bold;
                                            font-size: 24px;
                                          }'))),
                                          tags$head(
                                            tags$style(HTML("
                                      
                                           .multicol {
                                      
                                             -webkit-column-count: 3; /* Chrome, Safari, Opera */
                                      
                                             -moz-column-count: 3; /* Firefox */
                                      
                                             column-count: 3;
                                      
                                           }"))
      
    ),
    
    wellPanel( tags$div(class = "multicol", checkboxGroupInput("choices", label = NULL, choices = levels(data$Vocabulary), selected = NULL))
    )
                                        
                                      )
                              ),
                      tabItem(tabName = "threads",
                              fluidRow(titlePanel("YOU should take a look at these threads, they match you intrests!!!"),
                                       tableOutput("content")))
                        )
                )
)
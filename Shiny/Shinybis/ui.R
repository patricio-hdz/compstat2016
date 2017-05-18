library(shiny)
library(dplyr)
library(shinydashboard)
library(shinythemes)

data <- read.csv("/home/patricio/Documents/compstat2016/Shiny/Shinybis/wordprob.csv", header = TRUE)
threads <- read.csv("/home/patricio/Documents/compstat2016/Shiny/Shinybis/TopicsSubjects.csv", header = TRUE)
authors <- read.csv("/home/patricio/Documents/compstat2016/Shiny/Shinybis/TopicsAuthors.csv", header = TRUE)

fluidPage(theme = shinytheme("cyborg"),
  
  titlePanel("Looking for something?!!!"),

  sidebarLayout(
    sidebarPanel(
      #textInput("caption", "Caption:", "Data Summary"),
      selectInput("category", "Choose a category:", 
                  choices = c("Threads", "Authors")),
      
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
    ),
    
    mainPanel(tags$head(
      tags$style(
        HTML(
          '
          #view{
            text-align:center;
          }
          '
        )
      )
    ),

      h3(textOutput("category", container = span)),
      tableOutput("view")
    )
  )
)



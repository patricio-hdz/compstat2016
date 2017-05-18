library(shiny)
library(dplyr)
library(shinydashboard)

data <- read.csv("C:/Users/patricio.hernandez/Downloads/compstat2016/Shiny/wordprob.csv", header = TRUE)
topicos <- read.csv("C:/Users/patricio.hernandez/Downloads/compstat2016/Shiny/TopicsSubjects.csv", header = TRUE)
frame <- data.frame(c("10543","ability"))
colnames(frame) <- c("Vocabulary")
expose <- inner_join(data, frame, by='Vocabulary')
formax <- as.matrix(expose[,-1])
rownames(formax) <- expose$Vocabulary
max <- colSums(formax)
bla <- data.frame(which.max(max))

extra <- row.names(bla)
expose2 <- filter(topicos, Topic == "Topic1")
expose2
extra




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
library(dplyr)
#library(igraph)

data <- read.csv("/home/patricio/Documents/compstat2016/Shiny/Shinybis/wordprob.csv", header = TRUE)
threads <- read.csv("/home/patricio/Documents/compstat2016/Shiny/Shinybis/TopicsSubjects.csv", header = TRUE)
authors <- read.csv("/home/patricio/Documents/compstat2016/Shiny/Shinybis/TopicsAuthors.csv", header = TRUE)

library(shiny)
#library(datasets)

# Define server logic required to summarize and view the selected
# dataset
function(input, output) {
  
  # By declaring datasetInput as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #	  (it only executes a single time)
  #
  datasetInput <- reactive({
    switch(input$category,
           "Threads" = threads,
           "Authors" = authors)
  })

  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated
  # (i.e. whenever the input$dataset changes)
  output$category <- renderText({
    paste("We recommend these ",input$category," to you, they seem to fit your interests!", sep="")
  })
  

  output$view <- renderTable({

    frame <- data.frame(input$choices)
    colnames(frame) <- c("Vocabulary")
    expose <- inner_join(data, frame, by='Vocabulary')
    formax <- as.matrix(expose[,-1])
    rownames(formax) <- expose$Vocabulary
    max <- colSums(formax)
    topic <- data.frame(which.max(max))
    extra <- row.names(topic)
    expose2 <- filter(datasetInput(), Topic == extra)
    expose2
  })
}










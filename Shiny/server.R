library(dplyr)

data <- read.csv("C:/Users/patricio.hernandez/Downloads/compstat2016/Shiny/wordprob.csv", header = TRUE)
topicos <- read.csv("C:/Users/patricio.hernandez/Downloads/compstat2016/Shiny/TopicsSubjects.csv", header = TRUE)

server <- function(input, output) { 
  output$content <- renderTable({
    frame <- data.frame(input$choices)
    colnames(frame) <- c("Vocabulary")
    expose <- inner_join(data, frame, by='Vocabulary')
    formax <- as.matrix(expose[,-1])
    rownames(formax) <- expose$Vocabulary
    max <- colSums(formax)
    #max
    topic <- data.frame(which.max(max))
    extra <- row.names(topic)
    expose2 <- filter(topicos, Topic == extra)
    expose2
    #expose["Suma",] <- colSums(expose)
    #expose
  })
  
  
  }
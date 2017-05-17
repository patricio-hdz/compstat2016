server <- function(input, output) { 
  output$content <- renderTable({
    input$choices
  })
  
  
  }
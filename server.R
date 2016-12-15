library(shiny)
library(shinydashboard)
library(reshape2)
library(plyr)
library(ggplot2)

server <- function(input, output) {
  output$hist <- renderPlot({
    title<-"Histograma de las Simulaciones"
    set.seed(input$seed)
    y <- runif(input$sim)
    transy <- -log(1-y)/input$lambda
    x <- rexp(input$sim,rate=input$lambda)
    hist(transy,breaks=100,main=title,xlab="",ylim=c(0,input$yaxis),col="green",border="blue",prob=T)
    curve(dexp(x,rate=input$lambda), add = TRUE,col="red")
    
  })
  
  output$summary <- renderPrint({
    set.seed(input$seed)
    y <- runif(input$sim)
    Transformed <- -log(1-y)/input$lambda
    ks.test(Transformed,"pexp",rate=input$lambda)
  })
  
  Integralmc <- function(n,inf,sup,func,alfa){
    x <- runif(n,inf,sup)
    aux <- paste("func <- function(x) ",func)
    eval(parse(text = aux))
    y <- func(x)/(1/(sup-inf))
    int <- sum(y)/n
    x <- runif(n,inf,sup)
    yy <- (func(x)/(1/(sup-inf)))^2
    se <- sqrt(sum(yy)/n-int^2/n)
    ci <- c(int-qnorm((1-(alfa/2)))*se,int+qnorm((1-(alfa/2)))*se)
    list("Estimacion" = int, "Intervalo de Confianza" = ci)}
  
  output$integral <- renderPlot({
    resultados <- lapply(c(1:10,100:1000,10000,100000), function(x) Integralmc(x,input$inf,input$sup,input$fun,input$sig))
    resultados_df <- melt(ldply(resultados, data.frame)) # Convierte todo a dataframe y despu??s lo junta
    resultados_finales <- ddply(resultados_df, .(variable), function(x) { x$index <- 1:nrow(x); x})
    
    ggplot(resultados_finales, aes(index, value, color = variable)) + 
      labs(x="Simulaciones en escala log10",y="Valores")+
      geom_point(size=.5) + scale_x_log10(breaks = 1:100000)+
      scale_size(range=c(5,20))+
      theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),plot.title = element_text(hjust = 0.5),legend.justification="center", legend.position="bottom", legend.box = "horizontal",legend.title = element_blank())
  })
  
  output$resintegral <- renderPrint({
    salida <-  Integralmc(100000,input$inf,input$sup,input$fun,input$sig)$Estimacion
    cat("La integral es: ",salida )
    
  })
}

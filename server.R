library(shiny)
library(shinydashboard)
library(reshape2)
library(plyr)
library(ggplot2)
library(MASS)
library(DT)
options(shiny.maxRequestSize = 9*1024^2)
Rcpp::sourceCpp("myMCMC.cpp")


':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}


server <- function(input, output,session) {
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
  
  
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    dfS <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(dfS), selected = names(dfS))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(dfS), selected = names(dfS)[2])
    
    return(dfS)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  
  
  Asignando <- reactive({
    nombs <- c(names(data))
    for(i in 1:length(nombs)){
      if(input$xcol == nombs[i]) {
        indep <- i
        iesimo <- nombs[i]
      }
      if(input$ycol == nombs[i]) {
        depen <- i
        named <- nombs[i]
      }
    }
    return(list(indep,depen,iesimo,named))
  })
  
  
  output$MyPlot1 <- renderPlot({
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x, main = "Scatterplot")
    
  })
  
  output$distribalfa <- renderPlot({
    alfa <- c(seq(-5, 5, length=1000))
    dalfa <- c(dnorm(alfa,1))
    dfal <- data.frame(alfa,dalfa)
    colnames(dfal) <- c("alpha","distr")
    p1 <- ggplot(dfal,aes(x=alpha,y=distr))+
      geom_point(size = .5, color = "red")+ 
      ylab("Pr(Alfa)") + 
      xlab("Alfa")+
      ggtitle("Densidad de Alfa")
    p1
  })
  
  output$distribeta <- renderPlot({
    bet <- c(seq(-5, 5, length=1000))
    dbet <- c(dnorm(bet,1))
    dfbe <- data.frame(bet,dbet)
    colnames(dfbe) <- c("beta", "distr")
    p2 <- ggplot(dfbe,aes(x=beta,y=distr))+
      geom_point(size = .5, color = "red")+ 
      ylab("Pr(Beta)") + 
      xlab("Beta")+
      ggtitle("Densidad de Beta")
    p2
  })
  
  output$distribsigma <- renderPlot({
    sigma <- c(seq(0, 4, length=100))
    dsigma <- c(dgamma(sigma,0.001))
    dfta <- data.frame(sigma,dsigma)
    colnames(dfta) <- c("sigma", "distr")
    p2 <- ggplot(dfta,aes(x=sigma,y=distr))+
      geom_point(size = .5, color = "red")+ 
      ylab("Pr(Sigma)") + 
      xlab("Sigma")+
      ggtitle("Densidad de Sigma")
    p2
  })

  
  
  ############################################################################################################################
  cadena <- reactive({
    c(indep,depen,iesimo,named) := Asignando()
    theta0 <- c(1,1,1)
    cadena <- myMCMC(x = data[,indep], y = data[,depen], startValue=theta0, iterations=input$length)
    return(data.frame(alpha=chain[,1], beta=chain[,2], tau=chain[,3]))
  })
  
  
  df <- eventReactive(input$button, {
    c(indep,depen,iesimo,named) := Asignando()
    theta0 <- c(1,1,1)
    cadena <- myMCMC(x = data[,indep], y = data[,depen], startValue=theta0, iterations=input$length)
    cadena <- data.frame(alfa=cadena[,1], beta=cadena[,2], sigma=cadena[,3])
    for (i in 1:input$number-1){
      aux <- theta0 + round(10*runif(1))
      aux2 <- myMCMC(x = data[,indep], y = data[,depen], startValue=aux, iterations=input$length)
      aux2 <- data.frame(alfa=aux2[,1], beta=aux2[,2], sigma=aux2[,3])
      cadena <- cbind(cadena, aux2)
      return(chain)
    }
    
  })
  
  output$cadenasMC <- DT::renderDataTable(DT::datatable({
    if(is.null(df()))
      return()
    else 
      return(df())
  }))
  
  output$h_alfa <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({alfa_sb = df()[-(1:input$sBurnin),1]
      qplot(alfa_sb, geom = "histogram",binwidth = 0.2,
            main = "Alfa",
            xlab = "alpha",
            fill=I("blue"),
            col=I("black"),
            alpha=I(.75))
      })
    }
  })
  
  output$h_beta <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({beta_sb = df()[-(1:input$sBurnin),2]
      qplot(beta_sb, geom = "histogram",binwidth = 0.001,
            main = "Beta",
            xlab = "beta",
            fill=I("red"),
            col=I("black"),
            alpha=I(.75))
      })
    }
  })
  
  output$h_sigma <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({sigma_sb = df()[-(1:input$sBurnin),3]
      qplot(sigma_sb, geom = "histogram",binwidth = 0.05,
            main = "Sigma",
            xlab = "sigma",
            fill=I("green"),
            col=I("black"),
            alpha=I(.75))
      })
    }
  })
  
  output$dens_alfa <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({alfa_sb = df()[-(1:input$sBurnin),1]
      media <- mean(alfa_sb)
      desv <- sd(alfa_sb)
      alph <- c(seq(0, 1000, length=100))
      dalph <- c(dnorm(alph,1))
      xfit<-seq(min(alph),max(alfa_sb),length=100)
      yfit<-dnorm(xfit,mean=media,sd=desv)
      dfal <- data.frame(alph,dalph)
      colnames(dfal) <- c("alpha","distr")
      fit <- data.frame(xfit,yfit)
      ggplot()+
        geom_line(data=fit,aes(x=xfit,y=yfit),colour="#990000",size=1.5)+
        geom_area(data=fit,aes(x=xfit,y=yfit),fill="#06b9C7",alpha=0.3)+
        geom_line(data=dfal,aes(x=alph,y=dalph),colour="#990000",size=1.5)+
        geom_area(data=dfal,aes(x=alph,y=dalph),fill="#ff00ff",alpha=0.3)+ 
        ylab('Pr(Alpha)') + 
        xlab("Alpha")+ 
        ggtitle("A priori y a posteriori de Alfa")
      })
    }
  })
  
  output$dens_beta <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({beta_sb = df()[-(1:input$sBurnin),2]
      media <- mean(beta_sb)
      desv <- sd(beta_sb)
      bet <- c(seq(-4, 15, length=100))
      dbet <- c(dnorm(bet,1))
      xfit<-seq(min(bet),max(beta_sb),length=100)
      yfit<-dnorm(xfit,mean=media,sd=desv)
      dfbe <- data.frame(bet,dbet)
      colnames(dfbe) <- c("beta","distr")
      fit <- data.frame(xfit,yfit)
      ggplot()+
        geom_line(data=fit,aes(x=xfit,y=yfit),colour="#990000",size=1.5)+
        geom_area(data=fit,aes(x=xfit,y=yfit),fill="#06b9C7",alpha=0.3)+
        geom_line(data=dfbe,aes(x=beta,y=distr),colour="#990000",size=1.5)+
        geom_area(data=dfbe,aes(x=beta,y=distr),fill="#ff00ff",alpha=0.3)+ 
        ylab('Pr(beta)') + 
        xlab("beta")
      })
    }
  })
  
  output$dens_sigma <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({sigma_sb = df()[-(1:input$sBurnin),3]
      media <- mean(sigma_sb)
      desv <- sd(sigma_sb)
      sigm <- c(seq(0, 4, length=1000))
      dsigm <- c(dgamma(sigm,0.01))
      xfit<-seq(min(sigm),max(sigma_sb),length=100)
      yfit<-dnorm(xfit,mean=media,sd=desv)
      dfsi <- data.frame(sigm,dsigm)
      colnames(dfsi) <- c("sigma","distr")
      fit <- data.frame(xfit,yfit)
      ggplot()+
        geom_line(data=fit,aes(x=xfit,y=yfit),colour="#990000",size=1.5)+
        geom_area(data=fit,aes(x=xfit,y=yfit),fill="#06b9C7",alpha=0.3)+
        geom_line(data=dfsi,aes(x=sigma,y=distr),colour="#990000",size=1.5)+
        geom_area(data=dfsi,aes(x=sigma,y=distr),fill="#ff00ff",alpha=0.3)+ 
        ylab('Pr(sigma)') + 
        xlab("sigma")+ 
        ggtitle("A priori y a posteriori de Sigma")
      })
    }
  })
  
  output$c_alpha <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({alpha_sb = df()[,1]
      iteracion <- seq(1,length(alpha_sb),length=length(alpha_sb))
      df <- data.frame(alpha_sb,iteracion)
      ggplot(df,aes(x=iteracion,y=alpha_sb))+
        geom_line()+
        ylab('alphas') + 
        xlab("Iteracion")+ 
        ggtitle("Serie de alpha")
      })
    }
  })
  
  output$c_beta <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({beta_sb = df()[,2]
      iteracion <- seq(1,length(beta_sb),length=length(beta_sb))
      df <- data.frame(beta_sb,iteracion)
      ggplot(df,aes(x=iteracion,y=beta_sb))+
        geom_line()+
        ylab('Beta') + 
        xlab("Iteracion")+ 
        ggtitle("Serie de Beta")
      })
    }
  })
  
  output$c_sigma <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({sigma_sb = df()[,3]
      iteracion <- seq(1,length(sigma_sb),length=length(sigma_sb))
      df <- data.frame(sigma_sb,iteracion)
      ggplot(df,aes(x=iteracion,y=sigma_sb))+
        geom_line()+
        ylab('Sigma') + 
        xlab("Iteracion")+ 
        ggtitle("Serie de Sigma")
      })
    }
  })
  
  
  output$vals <- DT::renderDataTable(DT::datatable({
    if(is.null(df()))
      return()
    else 
      vals <- df()[-(1:input$sBurnin),]
    alfa <- mean(vals[,1])
    beta <- mean(vals[,2])
    sigma <- mean(vals[,3])
    Medias <- data.frame(alfa,beta,sigma)
    return(Medias)
  }))
  
  output$checking <- renderPlot({
    c(indep,depen,iesimo,named) := Asignando()
    vals <- df()[-(1:input$sBurnin),]
    alfa <- mean(valores[,1])
    beta <- mean(valores[,2])
    data$Sesiones <- data[,indep]*beta + alpha
    plot(data[,indep],data[,depen],main="Regresion",
         xlab=iesimo, ylab=named)
    lines(data[,indep],data$Sesiones)
  })


    ###########################################################################################################################
  
}

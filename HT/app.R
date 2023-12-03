library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Estimation of maximum likelihood"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #Radio buttons to choose the distribution
      radioButtons(inputId = "model", h3("Distribution"), 
        choices = list("Poisson" = 1, "Normal" = 2, "Exponent"=3, "Geometric"=4), selected = 1
      ),
      
      #Slider input to choose the max sample size
      sliderInput(
        inputId = "n", h3("Maximum sample size"), value = 100, min=1, max=10000
      ),
      
      uiOutput("thetaSlider")
    ),
      
      mainPanel(
      p("Plot shows how maximum likelihood estimates get closer to the theoretical value of the model parameter (red line)
        as the sample size rises."),
        
      plotOutput("plot")
    )
  )
)


server <- function(input, output) {
  
  #Theta-slider's min and max change depending on the chosen distribution.
  output$thetaSlider <- renderUI({
    minval <- 0.1
    maxval <- 10
    param <- "\u03bb"
    if (input$model == 1) 
    {
      minval <- 0.1
      maxval <- 10
      param <- "\u03bb"
    }
    else if (input$model == 2) 
    {
      minval <- -10
      maxval <- 10
      param <- "\u03bc"
    }
    else if (input$model == 3) 
    {
      minval <- 0.1
      maxval <- 10
      param <- "\u03bb"
    }
    else if (input$model == 4) 
    {
      minval <- 0.01
      maxval <- 0.99
      param <- "p"
    }
    
    sliderInput("theta", param, min=minval, max=maxval, value=minval)
  })
  
  
  output$plot <- renderPlot({
    
    theta <- input$theta
    
    #Generate random numbers from chosen distribution with chosen parameter (mu for normal) and
    #evaluate mle's from all different sample sizes from 1 to chosen max sample
    #size.
    if (input$model == 1 ) 
    {
      x <- rpois(input$n, lambda=theta)
      mle <- cumsum(x)/1:input$n
      
    }
    else if (input$model == 2 ) 
    {
      x <- rnorm(input$n, mean=theta, sd=1)
      mle <- cumsum(x)/1:input$n
    }
    else if (input$model == 3 ) 
    {
      x <- rexp(input$n, rate=theta)
      mle <- 1:input$n/cumsum(x)
    }
    else if (input$model == 4 ) 
    {
      x <- rgeom(input$n, prob=theta)
      mle <- 1/(1+cumsum(x)/(1:input$n))
    }
    
    estimates <- data.frame(n = 1:input$n, MLE = mle)
    
    
    #Plot the mle's against sample sizes and also the real value of
    #model parameter.
    ggplot(data=estimates, aes(x=n,y=MLE))+
    geom_point(color="blue")+
    geom_line(aes(y=theta), col="red")+
    xlab("Sample size")+
    ylab("Maximum likelihood estimate")
    
    #plot(1:input$n, mle)
    #abline(a=theta, b=0, col=2)
  
  })
  
}

shinyApp(ui = ui, server = server)

library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Estimation of maximum likelihood"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #Radio buttons to choose the model
      radioButtons(inputId = "model", h3("Choose the distribution"), 
        choices = list("Poisson" = 1, "Normal" = 2, "Exponent"=3, "Geometric"=4), selected = 1
      ),
      
      #Slider input to choose the max sample size
      sliderInput(
        inputId = "n", h3("Choose sample size"), value = 100, min=1, max=10000
      ),
      
      #Slider to choose the value of the model parameter. 
      #sliderInput(
      #  inputId = "theta", h3("Choose the value for estimated parameter"), value = 1, min = 1, max=10
      #),
      
      uiOutput("thetaSlider")
    ),
      
      mainPanel(
      plotOutput("plot")
    )
  )
)


server <- function(input, output) {
  
  output$thetaSlider <- renderUI({
    minval <- 0
    maxval <- 0
    if (input$model == 1 || input$model == 3) 
    {
      minval <- 0.1
      maxval <- 10
    }
    else if (input$model == 2 ) 
    {
      minval <- -10
      maxval <- 10
    }
    else if (input$model == 4 ) 
    {
      minval <- 0.01
      maxval <- 0.99
    }
    
    sliderInput("theta", "Theta", min=minval, max=maxval, value=minval)
  })
  
  output$plot <- renderPlot({
    
    theta <- input$theta
    
    #Generate random numbers from chosen distribution with chosen parameter and
    #evaluate mle's from all different sample sizesfrom 1 to chosen max sample
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
    
    estimates <- data.frame(sampleSize = 1:input$n, MLE = mle)
    
    #plot(1:input$n, mle)
    #Plot the mle's against sample sizes and also the real value of
    #model parameter.
    ggplot(data=estimates, aes(x=sampleSize,y=MLE))+
    geom_point(color="blue")+
    geom_line(aes(y=theta), col="red")
    #abline(a=theta, b=0, col=2)
  
  })
  
   
  
  
  
}

shinyApp(ui = ui, server = server)

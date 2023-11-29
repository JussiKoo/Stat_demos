ui <- fluidPage(
  
  titlePanel("Suurimman uskottavuuden estimointi"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons(inputId = "model", h3("Choose the model"), 
                   choices = list("Poisson" = 1, "Normal" = 2, "Exponential"=3), selected = 1),
      
      actionButton(inputId = "eval", label="Plot"),
      
      numericInput(
        "num", h3("Maximum size"), value = 100
      ),
      
      numericInput(
        "theta", h3("Theta"), value = 1, min = 1
      )
    ),
      
      
      
      
    
    mainPanel(
      plotOutput("plot")
    )
  )
)


server <- function(input, output) {
  
  output$plot <- renderPlot({
    #simulate random numbers from chosen distribution
    
    theta <- input$theta
    
    if (input$model == 1 ) 
    {
      x <- rpois(input$num, lambda=theta)
      mle <- cumsum(x)/1:input$num
    }
    else if (input$model == 2 ) 
    {
      x <- rnorm(input$num, mean=theta, sd=1)
      mle <- cumsum(x)/1:input$num
    }
    else if (input$model == 3 ) 
    {
      x <- rexp(input$num, rate=theta)
      mle <- 1:input$num/cumsum(x)
    }
    
    
    plot(1:input$num, mle)
    abline(a=theta, b=0)
  
  })
  
   
  
  
  
}

shinyApp(ui = ui, server = server)

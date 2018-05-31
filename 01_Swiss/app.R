library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Exercise 1 - Dataset: Swiss"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Bar" = "bar", "Scatterplot" = "scatter")),
      
      selectInput("outcome", label = h3("Select variable"),
                  choices = list("Fertility" = "Fertility",
                                 "Agriculture" = "Agriculture",
                                 "Education" = "Education",
                                 "Catholic" = "Catholic",
                                 "Infant.Mortality" = "Infant.Mortality"), selected = 1),
      
      selectInput("indepvar", label = h3("Select second variable"),
                  choices = list("Fertility" = "Fertility",
                                 "Agriculture" = "Agriculture",
                                 "Education" = "Education",
                                 "Catholic" = "Catholic",
                                 "Infant.Mortality" = "Infant.Mortality"), selected = 1),
      
      checkboxGroupInput("location", label = h3("Select location"), 
                         choices = list("Mean" = "Mean",
                                        "Median" = "Median"), selected = 1)
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", plotOutput("plot"), verbatimTextOutput("correlation"), renderPlot("lm")),
                  tabPanel("Distribution", plotOutput("distribution"), plotOutput("boxplot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))



# SERVER
server <- function(input, output) {
  
  # Regression output
  output$summary <- renderPrint({
    fit <- swiss[,input$outcome]
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(swiss, options = list(lengthChange = FALSE))
  })
  
  # Plot output
  output$plot <- renderPlot({
    if(input$plot == "scatter" && swiss[,input$outcome] != swiss[,input$indepvar]) {
      plot(swiss[,input$indepvar], swiss[,input$outcome], main="Scatterplot",
           xlab=input$indepvar, ylab=input$outcome, pch=19)
      abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
      lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
      
      # show legend
      legend(x = "topright",
             c("Variable", "Second variable"),
             col = c("blue", "red"),
             lwd = c(2, 2))
    }
    else if (input$plot == "scatter" && swiss[,input$outcome] == swiss[,input$indepvar]) {
      qqnorm(swiss[,input$outcome])
      qqline(swiss[,input$outcome], col="blue")
    }
    else if (input$plot == "bar") {
      barplot(swiss[,input$outcome], xlab=input$outcome)
    }
  })
  
  output$lm <- renderPlot({
    simple.fit <- lm(Education~Fertility, data = swiss)
  })
  
  # Plot boxplot
  output$boxplot <- renderPlot({
    # without frame: frame = F
    # without axes: axes = FALSE
    # TODO: Align boxplot with histogram axis
    boxplot(swiss[,input$outcome], horizontal = TRUE, staplewex = 1)
  })
  
  # Histogram output for distribution
  output$distribution <- renderPlot({
    h <- hist(swiss[,input$outcome], main="", xlab=input$outcome)
    
    # set density line
    xfit<-seq(min(swiss[,input$outcome]),max(swiss[,input$outcome]),length=40) 
    yfit<-dnorm(xfit,mean=mean(swiss[,input$outcome]),sd=sd(swiss[,input$outcome])) 
    yfit <- yfit*diff(h$mids[1:2])*length(swiss[,input$outcome]) 
    lines(xfit, yfit, col="blue", lwd=2)
    
    for(i in input$location){
        switch(i, 
               "Mean"={
                 abline(v = mean(swiss[,input$outcome]),
                        col = "green",
                        lwd = 2)
               },
               "Median"={
                 abline(v = median(swiss[,input$outcome]),
                        col = "red",
                        lwd = 2)   
               }
        )
    }
    
    # show legend
    legend(x = "topright",
           c("Density plot", "Mean", "Median"),
           col = c("blue", "green", "red"),
           lwd = c(2, 2, 2))
  })
  
  output$correlation <- renderText({
    if(input$plot == "scatter") {
      cor(swiss[,input$outcome], swiss[,input$indepvar], method = c("pearson"));
    }
  }) 
}

shinyApp(ui = ui, server = server)
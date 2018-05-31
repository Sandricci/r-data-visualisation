library(shiny)
library(DT)
dat <- as.data.frame(state.x77)
head(dat)

ui <- fluidPage(
  titlePanel("Exercise 2 - Dataset: State"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Bar" = "bar", "Scatterplot" = "scatter")),
      
      selectInput("outcome", label = h3("Select variable"),
                  choices = list("Population" = "Population",
                                 "Income" = "Income",
                                 "Illiteracy" = "Illiteracy",
                                 "Life expectancy" = "Life Exp",
                                 "Murder" = "Murder",
                                 "HS Grade" = "HS Grad",
                                 "Frost" = "Frost"), selected = 1),
      
      selectInput("indepvar", label = h3("Select second variable"),
                  choices = list("Population" = "Population",
                                 "Income" = "Income",
                                 "Illiteracy" = "Illiteracy",
                                 "Life expectancy" = "Life Exp",
                                 "Murder" = "Murder",
                                 "HS Grade" = "HS Grad",
                                 "Frost" = "Frost"), selected = 1),
      
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
    fit <- dat[,input$outcome]
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(dat, options = list(lengthChange = FALSE))
  })
  
  # Plot output
  output$plot <- renderPlot({
    if(input$plot == "scatter" && dat[,input$outcome] != dat[,input$indepvar]) {
      plot(dat[,input$indepvar], dat[,input$outcome], main="Scatterplot",
           xlab=input$indepvar, ylab=input$outcome, pch=19)
      abline(lm(dat[,input$outcome] ~ dat[,input$indepvar]), col="red")
      lines(lowess(dat[,input$indepvar],dat[,input$outcome]), col="blue")
      
      # show legend
      legend(x = "topright",
             c("Variable", "Second variable"),
             col = c("blue", "red"),
             lwd = c(2, 2))
    }
    else if (input$plot == "scatter" && dat[,input$outcome] == dat[,input$indepvar]) {
      qqnorm(dat[,input$outcome])
      qqline(dat[,input$outcome], col="blue")
    }
    else if (input$plot == "bar") {
      barplot(dat[,input$outcome], xlab=input$outcome)
    }
  })
  
  output$lm <- renderPlot({
    simple.fit <- lm(Education~Fertility, data = dat)
  })
  
  # Plot boxplot
  output$boxplot <- renderPlot({
    # without frame: frame = F
    # without axes: axes = FALSE
    # TODO: Align boxplot with histogram axis
    boxplot(dat[,input$outcome], horizontal = TRUE, staplewex = 1)
  })
  
  # Histogram output for distribution
  output$distribution <- renderPlot({
    h <- hist(dat[,input$outcome], main="", xlab=input$outcome)
    
    # set density line
    xfit<-seq(min(dat[,input$outcome]),max(dat[,input$outcome]),length=40) 
    yfit<-dnorm(xfit,mean=mean(dat[,input$outcome]),sd=sd(dat[,input$outcome])) 
    yfit <- yfit*diff(h$mids[1:2])*length(dat[,input$outcome]) 
    lines(xfit, yfit, col="blue", lwd=2)
    
    for(i in input$location){
        switch(i, 
               "Mean"={
                 abline(v = mean(dat[,input$outcome]),
                        col = "green",
                        lwd = 2)
               },
               "Median"={
                 abline(v = median(dat[,input$outcome]),
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
      cor(dat[,input$outcome], dat[,input$indepvar], method = c("pearson"));
    }
  }) 
}

shinyApp(ui = ui, server = server)
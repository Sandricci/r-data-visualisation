library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Exercise 1 - Dataset: Swiss"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Bar" = "bar", "Scatterplot" = "scatter")),
      
      selectInput("outcome", label = h3("Select variable:"),
                  choices = list("Fertility" = "Fertility",
                                 "Agriculture" = "Agriculture",
                                 "Examination" = "Examination",
                                 "Education" = "Education",
                                 "Catholic" = "Catholic",
                                 "Infant.Mortality" = "Infant.Mortality"), selected = 1),
      
      selectInput("indepvar", label = h3("Select second variable"),
                  choices = list("Fertility" = "Fertility",
                                 "Agriculture" = "Agriculture",
                                 "Examination" = "Examination",
                                 "Education" = "Education",
                                 "Catholic" = "Catholic",
                                 "Infant.Mortality" = "Infant.Mortality"), selected = 1)
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", plotOutput("plot")), # Plot
                  tabPanel("Distribution", plotOutput("distribution")),
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
    # TODO: Check if scatterplot makes sense -> variables are different
    if(input$plot == "scatter") {
      plot(swiss[,input$indepvar], swiss[,input$outcome], main="Scatterplot",
           xlab=input$indepvar, ylab=input$outcome, pch=19)
      abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
      lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
    }
    else if (input$plot == "bar") {
      barplot(swiss[,input$outcome])
    }
  })
  
  
  # Histogram output for distribution
  output$distribution <- renderPlot({
    hist(swiss[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
}

shinyApp(ui = ui, server = server)
library(shiny)
library(DT)
library(moments)
original <- as.data.frame(state.x77) # convert to data frame
colnames(original)[4] = "Life.Exp" # rename due to paste as formula issues
colnames(original)[6] = "HS.Grade" # rename due to paste as formula issues
original[8] = NULL # remove Area column

ui <- fluidPage(
  titlePanel("Exercise 2 - Dataset: State"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Bar" = "bar", "Scatterplot" = "scatter", "Q-Q-Plot" = "qqplot")),
      
      selectInput("outcome", label = h3("Select variable"), names(original), selected = "Murder"),
      
      selectInput("indepvar", label = h3("Select second variable"),names(original),multiple = T, selected = "Population"),
      
      selectInput("outliers", label = h3("Remove Outlier(s)"), 
                         choices = rownames(original), multiple=T, selected = 1),
      checkboxGroupInput("location", label = h3("Select location"), 
                         choices = list("Mean" = "Mean",
                                        "Median" = "Median"), selected = 1)
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", plotOutput("plot"), verbatimTextOutput("correlation")),
                  tabPanel("Moments, Location, Variation", 
                          column(htmlOutput("measures"), htmlOutput("locations"),htmlOutput("variations"), width = 6)),
                  tabPanel("Distribution", plotOutput("distplot"), plotOutput("distboxplot")),
                  tabPanel("Correlations", plotOutput("corplot"), verbatimTextOutput("corsummary")),
                  tabPanel("Regression Analysis", plotOutput("lmplot", height = 800, width = 1000)),
                  tabPanel("Summary", verbatimTextOutput("summary"), verbatimTextOutput("lmsummary")),
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))


# SERVER
server <- function(input, output) {
  
  # Regression output
  output$summary <- renderPrint({
    fit <- filtered()[,input$outcome]
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(original, options = list(lengthChange = FALSE))
  })
  
  # Plot output
  output$plot <- renderPlot({
    subset <- filtered()
    if(input$plot == "scatter") {
      plot(subset[,input$indepvar], subset[,input$outcome], main="Scatterplot",
           xlab=input$indepvar, ylab=input$outcome, pch=19)
      abline(lm(subset[,input$outcome] ~ subset[,input$indepvar]), col="red")
      lines(lowess(subset[,input$indepvar],subset[,input$outcome]), col="blue")
      
      # show legend
      legend(x = "topright",
             c("Variable", "Second variable"),
             col = c("blue", "red"),
             lwd = c(2, 2))
    }
    else if (input$plot == "qqplot") {
      qqnorm(subset[,input$outcome])
      qqline(subset[,input$outcome], col="red")
    }
    else if (input$plot == "bar") {
      barplot(subset[,input$outcome], xlab=input$outcome)
    }
  })
  
  # Plot boxplot
  output$distboxplot <- renderPlot({
    subset <- filtered()
    boxplot(subset[,input$outcome], horizontal = TRUE, staplewex = 1)
  })
  
  # Histogram output for distribution
  output$distplot <- renderPlot({
    subset <- filtered()
    h <- hist(original[,input$outcome], main="Histogram", xlab=input$outcome)
    
    # set density line
    xfit<-seq(min(subset[,input$outcome]),max(subset[,input$outcome]),length=40) 
    yfit<-dnorm(xfit,mean=mean(subset[,input$outcome]),sd=sd(original[,input$outcome])) 
    yfit <- yfit*diff(h$mids[1:2])*length(subset[,input$outcome]) 
    lines(xfit, yfit, col="blue", lwd=2)
    
    for(i in input$location){
      switch(i, 
             "Mean"={
               abline(v = mean(original[,input$outcome]),
                      col = "green",
                      lwd = 2)
             },
             "Median"={
               abline(v = median(original[,input$outcome]),
                      col = "red",
                      lwd = 2)   
             }
      )
    }
    
    # show legend
    legend(x = "topright",
           c("Density plot", "Mean", "Median"),
           col = c("blue", "green", "red", "orange"),
           lwd = c(2, 2, 2, 2))
  })
  
  output$correlation <- renderText({
    subset <- filtered()
    if(input$plot == "scatter") {
      cor(subset[,input$outcome], subset[,input$indepvar], method = c("pearson"));
    }
  })
  
  output$measures <- renderUI({
    subset <- filtered()
    tagList(
      tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Measure"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Skewness"), tags$td(skewness(subset[,input$outcome]))),
                 tags$tr(tags$th("Kurtosis / Excess"), tags$td(kurtosis(subset[,input$outcome]))),
                 tags$tr(tags$th("Variance"), tags$td(var(subset[,input$outcome])))
               )
    )
  )
  })
  output$locations <- renderUI({
    subset <- filtered()
    tagList(
      tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Location"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Mean"), tags$td(mean(subset[,input$outcome]))),
                 tags$tr(tags$th("Median"), tags$td(median(subset[,input$outcome]))),
                 tags$tr(tags$th("Weighted Mean"), tags$td(weighted.mean(subset[,input$outcome]))),
                 tags$tr(tags$th("Trimmed Mean (10%)"), tags$td(mean(subset[,input$outcome], trim = 0.10)))
               )
    )
  )
  })
  output$variations <- renderUI({
    subset <- filtered()
    tagList(
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Variation"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Variance"), tags$td(var(subset[,input$outcome]))),
                 tags$tr(tags$th("Standard Deviation"), tags$td(sd(subset[,input$outcome]))),
                 tags$tr(tags$th("MAD (median)"), tags$td(mad(subset[,input$outcome])))
               )
    )
  )
  })
  
  # Plot Scatterplot Matrix
  output$corplot <- renderPlot({
    ## put (absolute) correlations on the upper panels,
    ## with size proportional to the correlations.
    panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- abs(cor(x, y))
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste0(prefix, txt)
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor * r)
    }
    pairs(filtered(), lower.panel = panel.smooth, upper.panel = panel.cor, main="Scatterplot Matrix")
  })
  
  output$corsummary <- renderPrint({
    fit <- lm(Murder ~ ., filtered())
    summary(fit)
  })
  
  output$lmplot <- renderPlot({
    if(length(input$indepvar) == 0) return;
    fit <- lm(as.formula(paste(input$outcome," ~ ",paste0(input$indepvar,collapse="+"))), data=filtered())
    par(mfrow=c(2,2))
    plot(fit)
  })
  
  output$lmsummary <- renderPrint({
    
    if(length(input$indepvar) == 0) return;
    fit <- lm(as.formula(paste(input$outcome," ~ ",paste(input$indepvar,collapse="+"))), data=filtered())
    summary(fit)
  })
  
  filtered <- function() {
    subset <- original
    for(i in input$outliers) {
      subset <- subset[!(row.names(subset) %in% input$outliers),]
    }
    return(subset)
  }
  
}

shinyApp(ui = ui, server = server)
library(shiny)
library(DT)
library(moments)

ui <- fluidPage(
  titlePanel("Exercise 1 - Dataset: Swiss"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Bar" = "bar", "Scatterplot" = "scatter", "Q-Q-Plot" = "qqplot")),
      
      selectInput("outcome", label = h3("Select variable"), names(swiss), selected = "Education"),
      
      selectInput("indepvar", label = h3("Select second variable"),names(swiss),multiple = T, selected = "Fertility"),
      selectInput("outliers", label = h3("Remove Outlier(s)"), 
                  choices = rownames(swiss), multiple=T, selected = 1),
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
    DT::datatable(filtered(), options = list(lengthChange = FALSE))
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
  
  output$lm <- renderPlot({
    simple.fit <- lm(Education~Fertility, data = swiss)
  })
  
  # Plot boxplot
  output$distboxplot <- renderPlot({
    # without frame: frame = F
    # without axes: axes = FALSE
    # TODO: Align boxplot with histogram axis
    boxplot(filtered()[,input$outcome], horizontal = TRUE, staplewex = 1)
  })
  
  # Histogram output for distribution
  output$distplot <- renderPlot({
    subset <- filtered()
    h <- hist(subset[,input$outcome], main="Histogram", xlab=input$outcome)
    
    # set density line
    xfit<-seq(min(subset[,input$outcome]),max(subset[,input$outcome]),length=40) 
    yfit<-dnorm(xfit,mean=mean(subset[,input$outcome]),sd=sd(subset[,input$outcome])) 
    yfit <- yfit*diff(h$mids[1:2])*length(subset[,input$outcome]) 
    lines(xfit, yfit, col="blue", lwd=2)
    
    for(i in input$location){
      switch(i, 
             "Mean"={
               abline(v = mean(subset[,input$outcome]),
                      col = "green",
                      lwd = 2)
             },
             "Median"={
               abline(v = median(subset[,input$outcome]),
                      col = "red",
                      lwd = 2)   
             }
      )
    }
    
    # show legend
    legend(x = "topright",
           c("Density plot", "Mean", "Median", "Weighted Mean"),
           col = c("blue", "green", "red", "orange"),
           lwd = c(2, 2, 2, 2))
  })
  
  output$correlation <- renderText({
    if(input$plot == "scatter") {
      cor(subset[,input$outcome], subset[,input$indepvar], method = c("pearson"));
    }
  })
  
  output$measures <- renderUI({tagList(
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Measure"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Skewness"), tags$td(skewness(filtered()[,input$outcome]))),
                 tags$tr(tags$th("Kurtosis / Excess"), tags$td(kurtosis(filtered()[,input$outcome]))),
                 tags$tr(tags$th("Variance"), tags$td(var(filtered()[,input$outcome])))
               )
    )
  )
  })
  output$locations <- renderUI({tagList(
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Location"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Mean"), tags$td(mean(filtered()[,input$outcome]))),
                 tags$tr(tags$th("Median"), tags$td(median(filtered()[,input$outcome]))),
                 tags$tr(tags$th("Weighted Mean"), tags$td(weighted.mean(filtered()[,input$outcome]))),
                 tags$tr(tags$th("Trimmed Mean (10%)"), tags$td(mean(filtered()[,input$outcome], trim = 0.10)))
               )
    )
  )
  })
  output$variations <- renderUI({tagList(
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Variation"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Variance"), tags$td(var(filtered()[,input$outcome]))),
                 tags$tr(tags$th("Standard Deviation"), tags$td(sd(filtered()[,input$outcome]))),
                 # tags$tr(tags$th("Range"), tags$td(range(filtered()[,input$outcome]))), # error here
                 tags$tr(tags$th("MAD (median)"), tags$td(mad(filtered()[,input$outcome])))
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
    fit <- lm(Education ~ ., filtered())
    summary(fit)
  })
  
  output$lmplot <- renderPlot({
    if(length(input$indepvar) == 0) return;
    fit <- lm(as.formula(paste(input$outcome," ~ ",paste(input$indepvar,collapse="+"))), data=filtered())
    par(mfrow=c(2,2))
    plot(fit)
  })
  
  output$lmsummary <- renderPrint({
    if(length(input$indepvar) == 0) return;
    fit <- lm(as.formula(paste(input$outcome," ~ ",paste(input$indepvar,collapse="+"))), data=filtered())
    summary(fit)
  })
  
  filtered <- function() {
    subset <- swiss
    for(i in input$outliers) {
      subset <- swiss[!(row.names(swiss) %in% input$outliers),]
    }
    return(subset)
  }
}

shinyApp(ui = ui, server = server)
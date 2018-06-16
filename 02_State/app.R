library(shiny)
library(DT)
library(moments)
dat <- as.data.frame(state.x77)
colnames(dat)[4] = "Life.Exp"    
colnames(dat)[6] = "HS.Grad"

ui <- fluidPage(
  titlePanel("Exercise 2 - Dataset: State"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Bar" = "bar", "Scatterplot" = "scatter", "Q-Q-Plot" = "qqplot")),
      
      selectInput("outcome", label = h3("Select variable"), names(dat), selected = "Murder"),
      
      selectInput("indepvar", label = h3("Select second variable"),names(dat),multiple = T, selected = "Population"),
      
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
    fit <- dat[,input$outcome]
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(dat, options = list(lengthChange = FALSE))
  })
  
  # Plot output
  output$plot <- renderPlot({
    if(input$plot == "scatter") {
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
    else if (input$plot == "qqplot") {
      qqnorm(dat[,input$outcome])
      qqline(dat[,input$outcome], col="red")
    }
    else if (input$plot == "bar") {
      barplot(dat[,input$outcome], xlab=input$outcome)
    }
  })
  
  # Plot boxplot
  output$distboxplot <- renderPlot({
    # without frame: frame = F
    # without axes: axes = FALSE
    # TODO: Align boxplot with histogram axis
    boxplot(dat[,input$outcome], horizontal = TRUE, staplewex = 1)
  })
  
  # Histogram output for distribution
  output$distplot <- renderPlot({
    h <- hist(dat[,input$outcome], main="Histogram", xlab=input$outcome)
    
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
           col = c("blue", "green", "red", "orange"),
           lwd = c(2, 2, 2, 2))
  })
  
  output$correlation <- renderText({
    if(input$plot == "scatter") {
      cor(dat[,input$outcome], dat[,input$indepvar], method = c("pearson"));
    }
  })
  
  output$measures <- renderUI({tagList(
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Measure"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Skewness"), tags$td(skewness(dat[,input$outcome]))),
                 tags$tr(tags$th("Kurtosis / Excess"), tags$td(kurtosis(dat[,input$outcome]))),
                 tags$tr(tags$th("Variance"), tags$td(var(dat[,input$outcome])))
               )
    )
  )
  })
  output$locations <- renderUI({tagList(
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Location"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Mean"), tags$td(mean(dat[,input$outcome]))),
                 tags$tr(tags$th("Median"), tags$td(median(dat[,input$outcome]))),
                 tags$tr(tags$th("Weighted Mean"), tags$td(weighted.mean(dat[,input$outcome]))),
                 tags$tr(tags$th("Trimmed Mean (10%)"), tags$td(mean(dat[,input$outcome], trim = 0.10)))
               )
    )
  )
  })
  output$variations <- renderUI({tagList(
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Variation"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Variance"), tags$td(var(dat[,input$outcome]))),
                 tags$tr(tags$th("Standard Deviation"), tags$td(sd(dat[,input$outcome]))),
                 # tags$tr(tags$th("Range"), tags$td(range(dat[,input$outcome]))), # error here
                 tags$tr(tags$th("MAD (median)"), tags$td(mad(dat[,input$outcome])))
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
    pairs(dat, lower.panel = panel.smooth, upper.panel = panel.cor, main="Scatterplot Matrix")
  })
  
  output$corsummary <- renderPrint({
    fit <- lm(Murder ~ ., dat)
    summary(fit)
  })
  
  output$lmplot <- renderPlot({
    if(length(input$indepvar) == 0) return;
    fit <- lm(as.formula(paste(input$outcome," ~ ",paste0(input$indepvar,collapse="+"))), data=dat)
    par(mfrow=c(2,2))
    plot(fit)
  })
  
  output$lmsummary <- renderPrint({
    if(length(input$indepvar) == 0) return;
    fit <- lm(as.formula(paste(input$outcome," ~ ",paste(input$indepvar,collapse="+"))), data=dat)
    summary(fit)
  })
  
}

shinyApp(ui = ui, server = server)
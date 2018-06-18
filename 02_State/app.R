library(shiny)
library(DT)
library(moments)
library(vioplot)
original <- as.data.frame(state.x77) # convert to data frame
colnames(original)[4] = "Life.Exp" # rename due to paste as formula issues
colnames(original)[6] = "HS.Grade" # rename due to paste as formula issues
original[8] = NULL # remove Area column

ui <- fluidPage(
  titlePanel("Exercise 2 - Dataset: State"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Bar" = "bar", "Scatterplot" = "scatter", "Q-Q-Plot" = "qqplot", "Vioplot" = "vioplot")),
      checkboxInput("density", label = "Show Density Plot", value = FALSE),
      sliderInput("bins", label = "Number of bins (Histogram)",
                  min = 1, max = 20, value = 5, step = 1),
      selectInput("outcome", label = h3("Select variable"), names(original), selected = "Murder"),
      
      selectInput("indepvar", label = h3("Select second variable"),names(original),multiple = T, selected = "Population"),
      
      selectInput("outliers", label = h3("Remove Outlier(s)"), 
                         choices = rownames(original), multiple=T, selected = 1),
      checkboxGroupInput("location", label = h3("Select location"), 
                         choices = list("Mean" = "Mean",
                                        "Median" = "Median", "Modus" = "Modus", "Midrange" = "Midrange"), selected = 1),
      h3("Transform Axis"),
      checkboxInput("logx", label = "log(x)", value = FALSE),
      checkboxInput("logy", label = "log(y)", value = FALSE)
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", plotOutput("plot"), htmlOutput("correlation")),
                  tabPanel("Moments, Location, Variation", 
                          column(htmlOutput("measures"), htmlOutput("locations"),htmlOutput("variations"), width = 6)),
                  tabPanel("Distribution", plotOutput("distplot"), plotOutput("distboxplot")),
                  tabPanel("Correlations", plotOutput("corplot", height = 800), verbatimTextOutput("corsummary")),
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
    summary(filtered()[,input$outcome])
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
           xlab=input$indepvar, ylab=input$outcome, pch=19, log = logAxis())
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
      for(i in input$location){
        switch(i, 
               "Mean"={
                 abline(h = mean(subset[,input$outcome]),
                        col = "green",
                        lwd = 2)
               },
               "Median"={
                 abline(h = median(subset[,input$outcome]),
                        col = "red",
                        lwd = 2)   
               },
               "Modus"={
                 abline(h = Mode(subset[,input$outcome]),
                        col = "blue",
                        lwd = 2)   
               },
               "Midrange"={
                 abline(h = Midrange(subset[,input$outcome]),
                        col = "orange",
                        lwd = 2)   
               }
        )
      }
      legend(x = "topright",
             c("Mean", "Median", "Modus", "Midrange"),
             col = c("green", "red", "blue", "orange"),
             lwd = c(2, 2))
    }
    else if (input$plot == "vioplot") {
      vioplot(subset[, input$outcome],horizontal = T,col = "darkseagreen1")
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
    var <- subset[, input$outcome]
    if(input$density) {
      hist(var, main="Histogram with Density Plot", xlab = input$outcome, freq = F, breaks = input$bins)
      d <- density(var)
      lines(d, col = "darkred", lwd = 2)
      lines(d, adjust=2, lty="dotted", lwd = 2)
      polygon(d, col = rgb(1,0.5,0.2 ,0.5))
      # show legend
      legend(x = "topright",
             c("Density"),
             col = c("darkred"), lty = c("solid"),
             lwd = c(2, 2)) 
    }
    else {
      h <- hist(subset[,input$outcome], main="Histogram", xlab=input$outcome)
      # set normal curve
      xfit<-seq(min(var),max(var),length=40) 
      yfit<-dnorm(xfit,mean=mean(var),sd=sd(var)) 
      yfit <- yfit*diff(h$mids[1:2])*length(var) 
      lines(xfit, yfit, col="darkred", lwd=2)
      
      for(i in input$location){
        switch(i, 
               "Mean"={
                 abline(v = mean(var),
                        col = "lightblue",
                        lwd = 2)
               },
               "Median"={
                 abline(v = median(var),
                        col = "red",
                        lwd = 2)   
               },
               "Modus"={
                 abline(v = Mode(var),
                        col = "blue",
                        lwd = 2)   
               },
               "Midrange"={
                 abline(v = Midrange(var),
                        col = "orange",
                        lwd = 2)   
               }
        )
      }
      
      # show legend
      legend(x = "topright",
             c("Normal Curve", "Mean", "Median", "Mode", "Midrange"),
             col = c("darkred", "lightblue", "red", "blue",  "orange"),
             lwd = c(2, 2, 2, 2)) 
    }
  })
  
  output$correlation <- renderUI({
    if(input$plot == "scatter") {
      subset <- filtered()
      pearson <- cor(subset[,input$outcome], subset[,input$indepvar], method = c("pearson"))
      spearman <- cor(subset[,input$outcome], subset[,input$indepvar], method = c("spearman"))
      tagList(
        tags$table(class="table table-condensed table-bordered table-striped table-hover", 
                 tags$thead(tags$tr(tags$th("Method"), tags$th("Correlation"))),
                 tags$tbody(tags$tr(tags$td("Pearson (not robust)"), tags$td(pearson)), 
                            tags$tr(tags$td("Spearman (robust)"), tags$td(spearman)))
        ))
    }
  })
  
  output$measures <- renderUI({
    subset <- filtered()
    tagList(
      tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Measure", width=200), tags$th("Value", width=200))),
               tags$tbody(
                 tags$tr(tags$th("Skewness (not robust)"), tags$td(skewness(subset[,input$outcome]))),
                 tags$tr(tags$th("Kurtosis  / Excess (not robust)"), tags$td(kurtosis(subset[,input$outcome]))),
                 tags$tr(tags$th("Variance"), tags$td(var(subset[,input$outcome])))
               )
    )
  )
  })
  output$locations <- renderUI({
    subset <- filtered()
    tagList(
      tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Location", width=200), tags$th("Value", width=200))),
               tags$tbody(
                 tags$tr(tags$th("Mean"), tags$td(mean(subset[,input$outcome]))),
                 tags$tr(tags$th("Median"), tags$td(median(subset[,input$outcome]))),
                 tags$tr(tags$th("Modus"), tags$td(Mode(subset[,input$outcome]))),
                 tags$tr(tags$th("Midrange"), tags$td(Midrange(subset[,input$outcome]))),
                 tags$tr(tags$th("Trimmed Mean (10%)"), tags$td(mean(subset[,input$outcome], trim = 0.10)))
               )
    )
  )
  })
  output$variations <- renderUI({
    subset <- filtered()
    tagList(
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Variation", width=200), tags$th("Value", width=200))),
               tags$tbody(
                 tags$tr(tags$th("Variance"), tags$td(var(filtered()[,input$outcome]))),
                 tags$tr(tags$th("Standard Deviation"), tags$td(sd(filtered()[,input$outcome]))),
                 tags$tr(tags$th("MAD (median)"), tags$td(mad(filtered()[,input$outcome], center = median(filtered()[,input$outcome])))),
                 tags$tr(tags$th("MAD (mean)"), tags$td(mad(filtered()[,input$outcome], center = mean(filtered()[,input$outcome])))),
                 tags$tr(tags$th("Medmed*)"), tags$td(Medmed(filtered()[,input$outcome]))),
                 tags$tr(tags$th("Coefficient of Variation)"), tags$td(CoefficientOfVariation(filtered()[,input$outcome])))
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
    summary(lm(Murder ~ ., filtered()))
  })
  
  output$lmplot <- renderPlot({
    if(length(input$indepvar) == 0) return;
    par(mfrow=c(2,2))
    plot(linearModel())
  })
  
  output$lmsummary <- renderPrint({
    if(length(input$indepvar) == 0) return;
    summary(linearModel())
  })
  
  filtered <- function() {
    subset <- original
    for(i in input$outliers) {
      subset <- subset[!(row.names(subset) %in% input$outliers),]
    }
    return(subset)
  }
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  Midrange <- function(x) {
    (max(x) + min(x)) / 2
  }
  
  Medmed <- function(x) {
    median(abs(x - median(x)))
  }
  
  CoefficientOfVariation <- function(x){
    sd(x) / mean(x)
  }
  
  linearModel <- function() {
    y <- paste(input$outcome, " ~ ")
    if(input$logy)
      y <- paste(sprintf("log(%s)", input$outcome), " ~ ")
    x <- paste(input$indepvar, collapse = " + ")
    if(input$logx) {
      xaxis <- list()
      for(i in input$indepvar) {
        xaxis <- append(xaxis, paste(sprintf("log(%s)", i), ""))
      }
      x <- xaxis
    }
    formula <- as.formula(paste(y, paste(x,collapse="+")))
    model <- lm(formula, data = filtered())
  }
  
  linearFormula <- function() {
    formula <- as.formula(paste(input$outcome," ~ ", paste(input$indepvar,collapse="+")))
  }
  
  logAxis <- function() {
    logdir <- ""
    if(input$logx)
      logdir <- paste(logdir, "x", sep = "")
    if(input$logy)
      logdir <- paste(logdir, "y", sep = "")
    print(logdir)
    return(logdir)
  }
}

shinyApp(ui = ui, server = server)
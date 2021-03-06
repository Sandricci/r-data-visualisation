library(shiny)
library(DT)
library(moments)
library(vioplot)

ui <- fluidPage(
  titlePanel("Exercise 3 - Dataset: LakeHuron"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Plot" = "plot", "Time Series" = "series", "QQ-Plot" = "qqplot","Scatterplot" = "scatter", "ACF" = "acf", "Vioplot" = "vioplot"), selected = "plot"),
      checkboxInput("density", label = "Show Density Plot", value = FALSE),
      sliderInput("bins", label = "Number of bins (Histogram)",
                  min = 1, max = 20, value = 5, step = 1),
      selectInput("lags", label = h3("Number of lags:"),
                  choices = c(1:9), selected = 1),
      
      checkboxGroupInput("location", label = h3("Select location"), 
                         choices = list("Mean" = "Mean",
                                        "Median" = "Median"))
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", plotOutput("plot", height = 800), htmlOutput("correlation")),
                  tabPanel("Moments, Location, Variation", 
                           column(htmlOutput("measures"), htmlOutput("locations"),htmlOutput("variations"), width = 6)),
                  tabPanel("Distribution", plotOutput("distribution"),plotOutput("boxplot")),
                  tabPanel("Regression Analysis", plotOutput("lmplot", height = 800, width = 1000)),
                  tabPanel("Summary", verbatimTextOutput("summary"), verbatimTextOutput("corsummary")),
                  tabPanel("Data", fluidRow(column(DT::dataTableOutput('tbl'), width = 6))) # Data as datatable
                  
      )
    )
  ))


# SERVER
server <- function(input, output) {
  
  # Plot output
  output$plot <- renderPlot({
    if(input$plot == "series") {
      lag.plot(LakeHuron,as.numeric(input$lags),do.lines=F)
    }
    else if(input$plot == "plot") {
      plot(LakeHuron, ylab="depth (in feet)", xlab = "Time (in years)") 
      for(i in input$location){
        switch(i, 
               "Mean"={
                 abline(h = mean(LakeHuron),
                        col = "forest green",
                        lwd = 2)
               },
               "Median"={
                 abline(h = median(LakeHuron),
                        col = "tomato",
                        lwd = 2)   
               }
        )
      }
      legend(x = "topright",
             c("Mean", "Median"),
             col = c("forest green", "tomato"),
             lwd = c(2,2)) 
    }
    else if(input$plot == "scatter") {
      plot(LakeHuron, time(LakeHuron), ylab="depth (in feet)", xlab = "Time (in years)") 
    }
    else if (input$plot == "qqplot") {
      qqnorm(LakeHuron)
      qqline(LakeHuron, col="tomato")
    }
    else if(input$plot == "acf") {
      acf(LakeHuron, main ="Estimation of Auto-Correlation Function (ACF)") 
    } 
    else if (input$plot == "vioplot") {
      vioplot(LakeHuron, horizontal = T, col = "darkseagreen1")
    }
  })
  
  # TODO
  output$correlation <- renderUI({
    if(input$plot == "scatter") {
      pearson <- cor(LakeHuron, time(LakeHuron), method = c("pearson"))
      spearman <- cor(LakeHuron, time(LakeHuron), method = c("spearman"))
      tagList(
        tags$table(class="table table-condensed table-bordered table-striped table-hover", 
                   tags$thead(tags$tr(tags$th("Method"), tags$th("Correlation"))),
                   tags$tbody(tags$tr(tags$td("Pearson"), tags$td(pearson)), 
                              tags$tr(tags$td("Spearman*"), tags$td(spearman)))
        ))
    }
  })
  
  output$boxplot <- renderPlot({
    boxplot(LakeHuron, horizontal = TRUE, staplewex = 1)
  })
  
  output$distribution <- renderPlot({
    if(input$density) {
      hist(LakeHuron, main = "Histogram with Density Plot", yaxt = "n", ylab = NULL, xaxs ="i", breaks = input$bins, freq = FALSE, xlab = "Water levels")
      d <- density(LakeHuron)
      lines(d, col = "tomato", lwd = 2)
      polygon(d, col = rgb(1,0.5,0.2 ,0.5))
      legend(x = "topright",
             c("Density"),
             col = c("tomato"), lty = c("solid"),
             lwd = c(2)) 
    }
    else {
      h <- hist(LakeHuron, main="Histogram (Counts)", xlab= "Water levels", yaxt = "n", ylab = NULL, xaxs ="i")
      
      # set normal curve
      xfit<-seq(min(LakeHuron),max(LakeHuron),length=40) 
      yfit<-dnorm(xfit,mean=mean(LakeHuron),sd=sd(LakeHuron)) 
      yfit <- yfit*diff(h$mids[1:2])*length(LakeHuron) 
      lines(xfit, yfit, col="navy", lwd=2)
      
      for(i in input$location){
        switch(i, 
               "Mean"={
                 abline(v = mean(LakeHuron),
                        col = "forest green",
                        lwd = 2)
               },
               "Median"={
                 abline(v = median(LakeHuron),
                        col = "tomato",
                        lwd = 2)   
               }
        )
      }
      
      # show legend
      legend(x = "topright",
             c("Normal Curve", "Mean", "Median"),
             col = c("navy", "forest green", "tomato"),
             lwd = c(2, 2, 2)) 
    }
  })
  
  output$lmplot <- renderPlot({
    par(mfrow=c(2,2))
    fit <- lm(LakeHuron ~ time(LakeHuron))
    plot(fit)
  })
  
  # 5 Number summary
  output$summary <- renderPrint({
    summary(LakeHuron)
  })
  
  output$corsummary <- renderPrint({
    fit <- lm(LakeHuron ~ time(LakeHuron))
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    t <- cbind(LakeHuron, time(LakeHuron))
    colnames(t) <- c("Level", "Year")
    DT::datatable(as.data.frame(t), options = list(lengthChange = FALSE, paging = TRUE, info = FALSE))
  })
  
  output$measures <- renderUI({tagList(
    tags$h4("Measures"),
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Measure"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Skewness"), tags$td(skewness(LakeHuron))),
                 tags$tr(tags$th("Kurtosis / Excess"), tags$td(kurtosis(LakeHuron))),
                 tags$tr(tags$th("Variance"), tags$td(var(LakeHuron)))
               )
    )
  )
  })
  output$locations <- renderUI({tagList(
    tags$h4("Locations"),
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Location"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Mean"), tags$td(mean(LakeHuron))),
                 tags$tr(tags$th("Median"), tags$td(median(LakeHuron))),
                 tags$tr(tags$th("Weighted Mean"), tags$td(weighted.mean(LakeHuron))),
                 tags$tr(tags$th("Trimmed Mean (10%)"), tags$td(mean(LakeHuron, trim = 0.10)))
               )
    )
  )
  })
  
  output$variations <- renderUI({tagList(
    tags$h4("Variations"),
    tags$table(class="table table-condensed table-bordered table-striped table-hover",
               tags$thead(tags$tr(tags$th("Variation"), tags$th("Value"))),
               tags$tbody(
                 tags$tr(tags$th("Variance"), tags$td(var(LakeHuron))),
                 tags$tr(tags$th("Standard Deviation"), tags$td(sd(LakeHuron))),
                 tags$tr(tags$th("Range"), tags$td(range(LakeHuron))),
                 tags$tr(tags$th("MAD (median)"), tags$td(mad(LakeHuron, center = median(LakeHuron)))),
                 tags$tr(tags$th("MAD (mean)"), tags$td(mad(LakeHuron, center = mean(LakeHuron)))),
                 tags$tr(tags$th("Medmed)"), tags$td(Medmed(LakeHuron))),
                 tags$tr(tags$th("Coefficient of Variation)"), tags$td(CoefficientOfVariation(LakeHuron)))
               )
    )
  )
    
  })
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
}

shinyApp(ui = ui, server = server)
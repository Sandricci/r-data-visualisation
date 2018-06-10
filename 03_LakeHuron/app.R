library(shiny)
library(DT)
library(moments)
dat <- as.data.frame(LakeHuron)
head(dat)

# rename var x in dataset
names(dat)[names(dat)=="x"] <- "Level"
# set years per level
dat["Year"] <- c(1875:1972)

ui <- fluidPage(
  titlePanel("Exercise 3 - Dataset: LakeHuron"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot", label = h3("Select visualisation:"),
                  choices = list("Bar" = "bar", "Scatterplot" = "scatter")),
      selectInput("lags", label = h3("Number of lags:"),
                  choices = c(1:24), selected = 1),
      
      checkboxGroupInput("time", label = h3("Select time"), 
                         choices = list("Time" = "time"),selected = 1),
      
      checkboxGroupInput("location", label = h3("Select location"), 
                         choices = list("Mean" = "Mean",
                                        "Median" = "Median"), selected = 1)
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", plotOutput("plot", height = 800), verbatimTextOutput("correlation")),
                  tabPanel("Distribution", plotOutput("distribution"), plotOutput("boxplot")),
                  tabPanel("Regression Analysis", plotOutput("lmplot", height = 800, width = 1000)),
                  tabPanel("Summary", verbatimTextOutput("summary"), verbatimTextOutput("corsummary")),
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))



# SERVER
server <- function(input, output) {
  
  # Plot output
  output$plot <- renderPlot({
      if(!is.null(input$time)){
        plot(LakeHuron, ylab="depth (in feet)", xlab = "Time (in years)")
      }
      else {
        lag.plot(LakeHuron,as.numeric(input$lags),do.lines=F)
      }
  })
  
  # TODO
  output$correlation <- renderText({
    if(input$plot == "scatter") {
      cor(dat[,"Level"], dat[,"Year"], method = c("pearson"));
    }
  })
  
  # TODO
  # Plot boxplot
  output$boxplot <- renderPlot({
    # without frame: frame = F
    # without axes: axes = FALSE
    # TODO: Align boxplot with histogram axis
    boxplot(dat[,"Level"], horizontal = TRUE, staplewex = 1)
  })
  
  # TODO
  # Histogram output for distribution
  output$distribution <- renderPlot({
    h <- hist(dat[,"Level"], main="", xlab=dat[,"Year"])
    
    # set density line
    xfit<-seq(min(dat[,"Level"]),max(dat[,"Level"]),length=40) 
    yfit<-dnorm(xfit,mean=mean(dat[,"Level"]),sd=sd(dat[,"Level"])) 
    yfit <- yfit*diff(h$mids[1:2])*length(dat[,"Level"]) 
    lines(xfit, yfit, col="blue", lwd=2)
    
    for(i in input$location){
        switch(i, 
               "Mean"={
                 abline(v = mean(dat[,"Level"]),
                        col = "green",
                        lwd = 2)
               },
               "Median"={
                 abline(v = median(dat[,"Level"]),
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
  
  output$lmplot <- renderPlot({
    if(length(input$indepvar) == 0) return;
    lm <- lm(dat[,"Level"] ~ dat[, "Year"])
    par(mfrow=c(2,2))
    plot(lm, which=1)
    plot(lm, which=2)
    plot(lm, which=3)
    plot(lm, which=4)
  })
  
  # 5 Number summary
  output$summary <- renderPrint({
    fit <- dat[,"Level"]
    summary(fit)
  })
  
  output$corsummary <- renderPrint({
    # TODO - HOW TO generate a Model vs. Time Series
    lm <- lm(dat[,"Level"] ~ dat[, "Year"])
    summary(lm)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(dat, options = list(lengthChange = FALSE))
  })
}

shinyApp(ui = ui, server = server)
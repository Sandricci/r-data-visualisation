library(shiny)
library(DT)

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
      
      checkboxGroupInput("time", label = h3("Select time"), 
                         choices = list("Time" = "time"),selected = 1),
      
      checkboxGroupInput("location", label = h3("Select location"), 
                         choices = list("Mean" = "Mean",
                                        "Median" = "Median"), selected = 1)
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Plot", plotOutput("plot"), verbatimTextOutput("correlation")),
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
    fit <- dat[,"Level"]
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(dat, options = list(lengthChange = FALSE))
  })
  
  # Plot output
  output$plot <- renderPlot({
      if(!is.null(input$time)){
        plot(LakeHuron)
      }
      else {
        lag.plot(LakeHuron,do.lines=F)
      }
  })
  
  # TODO
  output$lm <- renderPlot({
    simple.fit <- lm(Education~Fertility, data = dat)
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
  
  # TODO
  output$correlation <- renderText({
    if(input$plot == "scatter") {
      cor(dat[,input$outcome], dat[,input$indepvar], method = c("pearson"));
    }
  }) 
}

shinyApp(ui = ui, server = server)
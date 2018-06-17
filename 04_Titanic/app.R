library(shiny)
library(ggplot2)
library(moments)
t <- as.data.frame(Titanic)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Titanic Tragedy and its Simpson Paradox"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("plot", label = h3("Select visualisation:"),
                    choices = list("Barplot" = "bar", "Mosaicplot" = "mosaic"), selected = "mosaic"),
        selectInput("variables", label = h3("Select variables"),names(t),multiple = T, selected = "Population"),
        h3("Simpsons Paradox 1"),
        tags$ul(
          tags$li("~25 % survivors 3rd and crew class"),
          tags$li("Class, Survived"), 
          tags$li("Class, Survived, Sex -> F: 3rd: 46%,Crew: 87%"),
          tags$li("-> better surival rates for women")),
      h3("Simpsons Paradox 2"),
      tags$ul(
        tags$li("Young Survivors 3rd Class > 1st Class"),
        tags$li("Class, Survived, Age"),
        tags$li("-> 1st class 100%, 3rd class 34%" )),
      h3("Simpsons Paradox 3"),
      tags$ul(
        tags$li("male survivors 2nd = 2* 1st class children survivors"),
        tags$li("Class, Survived, Age, Sex"),
        tags$li("true, but 100% Children, 20% 2nd Class Men survived")
      )
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("plot", height = 800)),
                    tabPanel("Proportions", 
                      fluidRow(
                        column(DT::dataTableOutput("propTable1"),width = 6),
                        column(DT::dataTableOutput("propTable2"),width = 6))),
                    tabPanel("Data", DT::dataTableOutput('tbl'))) # Data as datatable)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
     if(input$plot == "bar") {
       prop <- prop.table(apply(Titanic, c(1, 4), sum), margin = 1) * 100
       prop <- round(prop, digits = 2)
       barplot(t(prop), beside = FALSE)
     }
     else if (input$plot == "mosaic") {
        #str(Titanic)
       if(length(input$variables) == 0 || length(input$variables) == length(names(t))) {
         mosaicplot(Titanic, main = "Survival on Titanic", color = TRUE)
       }
       else {
        mosaicplot(as.formula(paste("~", paste(input$variables, collapse ="+"))), main = "Survival on the Titanic", data = Titanic, color = TRUE)
       }
     }
   })
   
   # Data output
   output$tbl = DT::renderDataTable({
     DT::datatable(t, options = list(lengthChange = FALSE))
   })
   
   output$propTable1 = DT::renderDataTable({
      prop <- prop.table(apply(Titanic, c(1, 4), sum), margin = 1) * 100
      prop <- round(prop, digits = 2)
      DT::datatable(prop, options = list(lengthChange = FALSE))
   })
   
   output$propTable2 = DT::renderDataTable({
     prop <- round(prop.table(ftable(Titanic, row.vars = 1:2, col.vars = 3:4)) * 100, digits = 2)
     DT::datatable(prop, options = list(lengthChange = FALSE, paging = FALSE))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(ggplot2)
library(moments)
t <- as.data.frame(Titanic)
adult <- subset(t, Age == 'Adult')
child <- subset(t, Age == 'Child')
died <- subset(t, Survived == 'No')
survived <- subset(t, Survived == 'Yes')
adult_died <- subset(adult, Survived == 'No')
adult_survived <- subset(adult, Survived == 'Yes')
child_died <- subset(child, Survived == 'No')
child_survived <- subset(child, Survived == 'Yes')

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
        h3("Simpsons Paradox"),
        tags$ul(
          tags$li("Class, Survived"), 
          tags$li("Class, Survived, Sex"))
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
     prop <- round(prop.table(ftable(Titanic, row.vars = 1:2, col.vars = "Survived"),1) * 100, digits = 2)
     DT::datatable(prop, options = list(lengthChange = FALSE, paging = FALSE))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

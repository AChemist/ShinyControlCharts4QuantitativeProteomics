#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plyr)



# Import Data -------------------------------------------------------------

dat <- data.frame(
  proteinAccesion = c(rep("A",60),rep("B",60)), 
  peptideSequence = rep(c(rep("ABC",30), rep("CDE",30)),2),
  Transition = rep(c("A","B","C","D"),30),
  time = rep(1:30,4),
  value = runif(120)
)

dat$Transition <- paste0(dat$proteinAccesion,"_",dat$peptideSequence," -> ",dat$Transition )
dat$ProtPep <- paste(dat$proteinAccesion, dat$peptideSequence)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    # sliderInput(
    #   "start",
    #   "Startdate:",
    #   min = min(airquality$Day),
    #   max = max(airquality$Day),
    #   value = min(airquality$Day)
    # ),
    #   
    #   # Input: Simple integer interval ----
    #   sliderInput(
    #     "integer",
    #     "Integer:",
    #     min = 0,
    #     max = 1000,
    #     value = 500
    #   ),
    #   
    #   # Input: Decimal interval with step value ----
    #   sliderInput(
    #     "decimal",
    #     "Decimal:",
    #     min = 0,
    #     max = 1,
    #     value = 0.5,
    #     step = 0.1
    #   ),
      
    selectInput(
      "ProtPep", 
      "Protein",
      choices = unique(dat$ProtPep)
    ),
    
    # selectInput(
    #   "peptideSequence", 
    #   "Peptid",
    #   choices = unique(dat$peptideSequence[ dat$proteinAccesion == proteinAccesion,])
    # ),
    
    
      # Input: Specification of range within an interval ----
      sliderInput(
        "range",
        "Range:",
        min = min(dat$time),
        max = max(dat$time),
        value = c(min(dat$time), max(dat$time))
      )
      
      
    ),

  
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("A"),
    plotOutput("B"),
    plotOutput("C"),
    plotOutput("D"),
    plotOutput("E"),
    plotOutput("F"),
    plotOutput("G"),
    plotOutput("H"),
    plotOutput("I"),
    plotOutput("J")
  )
))

# This is really a bottleneck !?!?!?
# How is it possbile to create multiple plots according to the data without naming them before ? 

# Define server logic required to draw a histogram
server <- function(input, output){
  
    # # Reactive expression to create data frame of all input values ----
    # sliderValues <- reactive({
    # 
    #   dat <- dat[dat$proteinAccesion == proteinAccession,]
    # 
    # })
   
  output$A <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  
  output$B <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  
  output$C <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  
  output$D <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  
  output$E <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  
  output$F <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  output$G <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  output$H <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  output$I <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  output$J <- renderPlot(
    
    ggplot(dat[dat$ProtPep == input$ProtPep,] ,aes(time, value)) + geom_point() + coord_cartesian(xlim = c(input$range[1], input$range[2]))
  )
  
     
  }


# Run the application 
shinyApp(ui = ui, server = server)

# 
# 
# library(shiny)
# 
# # Define UI for slider demo app ----
# ui <- fluidPage(
#   
#   # App title ----
#   titlePanel("Sliders"),
#   
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar to demonstrate various slider options ----
#     sidebarPanel(
#       
#       # Input: Simple integer interval ----
#       sliderInput("integer", "Integer:",
#                   min = 0, max = 1000,
#                   value = 500),
#       
#       # Input: Decimal interval with step value ----
#       sliderInput("decimal", "Decimal:",
#                   min = 0, max = 1,
#                   value = 0.5, step = 0.1),
#       
#       # Input: Specification of range within an interval ----
#       sliderInput("range", "Range:",
#                   min = 1, max = 1000,
#                   value = c(200,500)),
#       
#       # Input: Custom currency format for with basic animation ----
#       sliderInput("format", "Custom Format:",
#                   min = 0, max = 10000,
#                   value = 0, step = 2500,
#                   pre = "$", sep = ",",
#                   animate = TRUE),
#       
#       # Input: Animation with custom interval (in ms) ----
#       # to control speed, plus looping
#       sliderInput("animation", "Looping Animation:",
#                   min = 1, max = 2000,
#                   value = 1, step = 10,
#                   animate =
#                     animationOptions(interval = 300, loop = TRUE))
#       
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(
#       
#       # Output: Table summarizing the values entered ----
#       tableOutput("values")
#       
#     )
#   )
# )
# 
# # Define server logic for slider examples ----
# server <- function(input, output) {
#   
#   # Reactive expression to create data frame of all input values ----
#   sliderValues <- reactive({
#     
#     data.frame(
#       Name = c("Integer",
#                "Decimal",
#                "Range",
#                "Custom Format",
#                "Animation"),
#       Value = as.character(c(input$integer,
#                              input$decimal,
#                              paste(input$range, collapse = " "),
#                              input$format,
#                              input$animation)),
#       stringsAsFactors = FALSE)
#     
#   })
#   
#   # Show the values in an HTML table ----
#   output$plot1 <- renderPlot({
#     p <- ggplot(airquality,aes(Day, Wind)) + geom_point()
#     p <- p + coord_cartesian(xlim = c(input$range[1], input$range[]))
#     p
#   })
#   
# }
# 
# # Create Shiny app ----
# shinyApp(ui, server)
# 
# 

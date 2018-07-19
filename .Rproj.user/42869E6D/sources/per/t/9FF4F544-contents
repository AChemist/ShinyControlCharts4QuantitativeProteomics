library(shiny)
library(DT)

# data("diamonds") don't know where I can find this dataset, hence I'll use
# diamond dataset 

library(ggplot2) # for diamonds dataset


cut <- unique(as.character(diamonds$cut)) # or just levels(diamonds$cut)
color <- unique(as.character(diamonds$color))
clarity <- unique(as.character(diamonds$clarity))

runApp(list(ui = fluidPage(
  titlePanel("Summary"),
  
  sidebarLayout(
    sidebarPanel(
      # changed names of inputs
      selectInput("cut", label = "Cut", choices = cut, selected = NULL, multiple = T),
      selectInput("filter_join1", label = "", choices = c("OR","AND")),
      selectInput("color", label = "Color", choices = color, selected = NULL, multiple = T),
      selectInput("filter_join2", label = "", choices = c("OR","AND")),
      selectInput("clarity", label = "Clarity", choices = clarity, selected = NULL, multiple = T)
    ),
    
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
),

server = function(input, output, session) {
  
  WorkingDataset <- reactive({ 
    req(input$cut, input$color, input$clarity)
    # show table only if all three inputs are available
    
    # depending on filter_join inputs return "OR" or "AND"
    join1 <- ifelse(test = input$filter_join1 == "OR", yes = "| ", no = "& ")
    join2 <- ifelse(test = input$filter_join2 == "OR", yes = "| ", no = "& ")
    
    # You could do this differently: just set choices = c("OR" = "|", "AND" = "&"))
    # in the selectInput widget.
    
    # Similar as in the example above with the iris dataset. 
    
    cond_str <- paste0(
      "with(diamonds, ", 
      paste0("cut %in% ", "c(", paste0("'", input$cut, collapse = "',"), "')", colapse = " "),
      join1, 
      paste0("color %in% ", "c(", paste0("'", input$color, collapse = "',"), "')", colapse = " "),
      join2,
      paste0("clarity %in% ", "c(", paste0("'", input$clarity, collapse = "',"), "')", colapse = " "),
      ")")
    
    print(cond_str) # print the result to the console
    cond <- parse(text = cond_str)
    df <- as.data.frame(diamonds)[eval(cond), ]
    df
  }) 
  
  output$table <- DT::renderDataTable({ WorkingDataset() })
  
})
)
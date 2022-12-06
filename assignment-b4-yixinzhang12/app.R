library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      sliderInput("alcoholInput", "Alcohol Content", 0, 100, c(10, 30), pre = "%"),
      ## changed radiobutton to checkbox
      checkboxGroupInput("typeInput", "Product type",
                         choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                         selected = "WINE"),
      uiOutput("countryOutput")
      # ,
      # ## create checkboxinput for whether arrange in ascending order
      # checkboxInput("arrPrice", "Arrange Price from Low to High", FALSE)
    ),
    mainPanel(
      ## add output text showing how many results have been found
      textOutput("text"),
      ## separated table and plot into different panels
      tabsetPanel(
        tabPanel("Plot", plotOutput("coolplot")), 
        tabPanel("Table", DT::dataTableOutput("results"))
      )
      # plotOutput("coolplot"),
      # br(), br(),
      # tableOutput("results")
    )
  )
)

server <- function(input, output) {
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  }) 
  
  filtered <- reactive({
    ## prevent red error message
    if (is.null(input$typeInput)) {
      return(NULL)
    }  
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Alcohol_Content >= input$alcoholInput[1],
             Alcohol_Content <= input$alcoholInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ## added color based on type
    # ggplot(filtered(), aes(Alcohol_Content, fill = Type)) +
    #   geom_histogram()
    ggplot(filtered(), aes(Price, fill = Type)) +
      geom_histogram()
  })
  
  ## used DT package to displays fewer rows at a time and make it possible to arrange
  output$results <- DT::renderDataTable({
    DT::datatable(filtered(), options = list(orderClasses = TRUE))
  })
  
  # output$results <- renderTable({
  #   ## arrange in ascending order based on checkboxinput value
  #   if (input$arrPrice == TRUE && !is.null(input$typeInput)){
  #     filtered() %>% arrange(Price)
  #   } else{filtered()}
  # })
  
  ## add output text showing number of results
  output$text <- renderText(paste("We found ", nrow(filtered()), "options for you. "))
}

shinyApp(ui = ui, server = server)

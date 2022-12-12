library(shiny)
library(dplyr)
library(readr)
library(purrr) # just for `%||%`

categories <- unique(albums$Category)

ui <- fluidPage(plotlyOutput("pie"), uiOutput("back"))

server <- function(input, output, session) {
  # for maintaining the current category (i.e. selection)
  current_category <- reactiveVal()
  
  # report sales by category, unless a category is chosen
  albums_data <- reactive({
    if (!length(current_category())) {
      return(count(albums, Category, wt = Freq))
    }
    albums %>%
      filter(Category %in% current_category()) %>%
      count(Var1, wt = Freq)
  })
  
  # Note that pie charts don't currently attach the label/value 
  # with the click data, but we can include as `customdata`
  output$pie <- renderPlotly({
    d <- setNames(albums_data(), c("labels", "values"))
    plot_ly(d) %>%
      add_pie(
        labels = ~labels, 
        values = ~values, 
        customdata = ~labels, textposition = 'inside',
        textinfo = 'label+percent'
      ) %>%
      layout(title = current_category() %||% "Albums realesed by genres from 1985 to 2015")
  })
  
  # update the current category when appropriate
  observe({
    cd <- event_data("plotly_click")$customdata[[1]]
    if (isTRUE(cd %in% categories)) current_category(cd)
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(current_category())) 
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, current_category(NULL))
}

shinyApp(ui, server)


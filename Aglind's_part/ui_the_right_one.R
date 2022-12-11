library(dplyr)
library(ggplot2)
library(shiny)




Album_HipHop <- read.csv("Album_HipHop_Only.csv")
Album_hh <- Album_HipHop %>% select(name, genre, publicationDate, deezerFans, country)
Album_hh[Album_hh == 'NULL'] <- NA
Album_hh[Album_hh == ''] <- NA
Album_hh <- filter(Album_hh, !is.na(publicationDate))
Album_hh$publicationDate <- strtoi(Album_hh$publicationDate)
Album_hh$deezerFans <- strtoi(Album_hh$deezerFans)



ui <- fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  tableOutput("data")
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(Album_hh, aes(x = publicationDate, y = country, size = genre)) + geom_point(aes(color = genre), size=5)
      
  })
  output$data <- renderTable({
    brushedPoints(Album_hh, input$plot_brush)
  })
}

shinyApp(ui=ui, server = server)
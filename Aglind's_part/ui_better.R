library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)



#Album_HipHop <- read.csv("Album_HipHop_Only.csv")
Album_hh <- Album_HipHop %>% select(name, genre, publicationDate, deezerFans, country)
Album_hh[Album_hh == 'NULL'] <- NA
Album_hh[Album_hh == ''] <- NA
Album_hh <- filter(Album_hh, !is.na(publicationDate))
Album_hh$publicationDate <- strtoi(Album_hh$publicationDate)
Album_hh$deezerFans <- strtoi(Album_hh$deezerFans)


ui <- fluidPage(
  sidebarLayout(sidebarPanel( h4("Test Plot")),
                  mainPanel(plotlyOutput("plot1"))
  )
)
  
server <- function(input, output) {
    output$plot1 <- renderPlotly({
      p <- ggplot(Album_hh, aes(x = publicationDate, y = country, size = genre)) + geom_point(aes(color = genre))
      
      
  })
}


shinyApp(ui=ui, server = server)

library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(markdown)
library(ggrepel)
library(shinyjs)  
library(shinyBS)
library(plotly)
artistKnown$testDate=as.integer(substr(artistKnown$lifeSpan.begin,1,4))
artistKnown$testYear=as.integer(artistKnown$deezerFans)


ui <- fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  tableOutput("data")
  
  
  
)
server <- function(input, output, session) {
  output$plot <- renderPlot({
    
    artistKnown%>%ggplot(aes(x=testDate,y=testYear,color=gender))+
      geom_point()+
      facet_wrap(facets=~ gender)+
      labs(title="Comparison of popularity in the Hip Hop categorie of Deezer (in terms of DeezerFans)",x="Begining of Career",y="Deezer Fans")+
      geom_text_repel(aes(label = name)) 
  })
  
  output$data <- renderTable({
    #head(artistKnown[artistKnown$gender=="Male",],3)
    brushedPoints(artistKnown, input$plot_brush)
    #brushedPoints(artistKnown, input$plot_brush)
  })
}
shinyApp(ui=ui, server = server)
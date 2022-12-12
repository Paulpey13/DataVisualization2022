library(tidyverse)
library(dbplyr)
library(DataExplorer)
library(skimr)
library(ggplot2)
library(hrbrthemes)
library(forcats)
library(shiny)
library(ggiraph)





#publication date 
publicationDate=table(albumsProject$publicationDate)
publicationDate=as.data.frame(publicationDate)   #year of publication Date of albums


#year of publication Date Graph
x <- publicationDate$Var1
y <- publicationDate$Freq
data <- data.frame(x, y)



ui <- fluidPage(
  
  
  sliderInput(inputId = "animation", "Click to start the barchart",
              min=1986, max=2016, value=1986, step=1,
              animate=animationOptions()
  ),
  
  
  plotlyOutput(outputId = "distPlot"),
  textOutput(outputId="message")
)





# Server logic
server <- function(input, output) {
  
  
  output$distPlot <- renderPlotly({
    
    
    #
    
    data$x=as.integer(as.character(data$x))
    #data2=data[data$x>input$range[1],]
    data2=data[data$x<input$animation,]
    
    fig1 <- plot_ly(data2, x = ~x, y = ~y, type = 'bar',
                    text = ~paste( y, ' albums released'),
                    textposition = 'auto',
                    marker = list(color = 'rgb(158,202,225)',
                                  line = list(color = 'rgb(8,48,107)', width = 2)))
    fig1 <- fig1 %>% layout(title = "Released Hip Hop albums in the last 30 years",
                            yaxis = list(title = "Number of albums per year"),
                            xaxis = list(title = "Year of release"))
    fig1
    
    
    
  })
  
}

# Complete app with UI and server components
shinyApp(ui, server)




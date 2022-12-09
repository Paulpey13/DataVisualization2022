#Load libraries ----------------------------------------------------
library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(markdown)
library(ggrepel)
library(shinyjs)  
library(shinyBS)
library(plotly)



ui2<-shinyUI(fluidPage(
  
  "Data Visualization",
  shinyjs::useShinyjs(),
  tabPanel("Home",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "var",
                 label = "Select the Plot",
                 choices = c(
                   "song" = 1,
                   "album" = 2,
                   "artist" = 3,
                   "Men Vs Women"=4
                 ),
                 
                 selected = 4
               ),
               sliderInput(
                 "year",
                 "Select the year",
                 min = 1990,
                 max = 2016,
                 value = 2005,
                 step=1,animate=TRUE
               ),bsTooltip("year", "Move the slider in order to change the year",
                           "bottom"),
               radioButtons(
                 "radio",
                 label = "Set the plot color",
                 choices = c("Blue", "Orange", "Green"),
                 select = "Orange"
               ),
               radioButtons(
                 "gender",
                 label = "Select the gender",
                 choices = c("Male", "Female","All"),
                 select = "All"
               ),
               radioButtons(
                 "gender2",
                 label = "Select the gender",
                 choices = c("Male", "Female","unknown","All","Male Vs Female"),
                 select = "All"
               ),
               #tipify(el, title, placement = "bottom", trigger = "hover", options = NULL)
               radioButtons(
                 "award",
                 label = "Select the award",
                 choices = c("Silver","Gold","Platinum","Million"),
                 select = "Silver"
               ),
             ),
             mainPanel(
               #tipify(plotOutput("distPlot",click = "plot_click")#si tu veux remettre ici cest , et parenthese apres brush
               #dblclick = "plot_dblclick",
               # hover = "plot_hover",
               # brush = "plot_brush")
               #      ,title="METTRE DESCRIPTION OU AUTRE ICI",placement="bottom",trigger="click"),
               plotOutput("distPlot"),
               textOutput("Description"),
               plotlyOutput("distPlot2"),
               tableOutput("data"),
               
               verbatimTextOutput("info")
               
             )
           ),
  )
  
  
  #tabPanel("About",
  #         mainPanel(includeMarkdown("about.md"))),
)

)

shinyServer(
  server2<-function(input, output,session) {
    cond <- reactive(input$var)
    output$valueText <- renderText(paste(value()))
    
    observeEvent(cond(), { #In order to disable year and colors where it has no use
      if(cond()==3 |cond()==4 |cond()==5 ){
        shinyjs::disable('radio')
        shinyjs::disable('year')
        hide('radio')
        hide('year')
        
      }
      if(cond()==1 |cond()==2){#In order to enable after it has been disabled 
        shinyjs::enable('radio')
        shinyjs::enable('year')
        show('radio')
        show('year')
        
      } 
      if(cond()==3){
        shinyjs::enable('gender')
        show('gender')
      }else{
        shinyjs::disable('gender')
        hide('gender')
      }
      
      if(cond()==4){
        shinyjs::enable('gender2')
        show('gender2')
      }else{
        shinyjs::disable('gender2')
        hide('gender2')
      }
      
      if(cond()==5){
        shinyjs::enable('award')
        show('award')
      }else{
        shinyjs::disable('award')
        hide('award')
      }
    })
    
    
    output$distPlot <- renderPlot({
      
      if(input$var==1){ #Song, need to change rank viz
        songByYear=songMoreArtist2[songMoreArtist2$year_publication==input$year,]
        songByYear2=head(songByYear,10)
        #songByYear2$rankHere2=c(length(songByYear2$rankHere):1)#to have rank reversed
        songByYear2$rankHere=c(1:length(songByYear2$rankHere))#to have rank reversed
        
        #songByYear2=head(songByYear,10)
        #songByYear2=songByYear2%>% mutate(songByYear2,rankplot1=c(1:length(songByYear2)-1))
        
        plot1=songByYear2 %>% ggplot(aes(x=reorder(title,rankHere),y=as.numeric(rankHere),fill=rankHere))+
          geom_bar(stat='identity',position='stack')+labs(title=paste("Top ranked songs in the Hip Hop categorie of Deezer in",input$year)
                                                          ,x="Song Titles",y="Song ranks",fill="Song's rank")+
          #coord_flip()+
          geom_text(aes(label = c(songByYear2$Artist_name)),vjust = 1.5, colour = "black")+
          
          #geom_text(aes(label = c(head(songByYear,10)$Artist_name)),vjust = 1.5, colour = "black")+
          #geom_text(aes(label = c(0:length(head(songByYear,10))),vjust = 1.5, colour = "black"))+
          theme_classic()+
          scale_x_discrete(guide = guide_axis(n.dodge=3))
        
          #scale_y_reverse()
        #theme(legend.position = "none")  
        if(input$radio=="Blue"){
          plot1=plot1+scale_fill_gradient(low="cyan", high="#4553ff")}
        else if(input$radio=="Orange"){
          plot1=plot1+scale_fill_gradient(low="yellow", high="red")}
        else if(input$radio=="Green"){
          plot1=plot1+scale_fill_gradient(low="white", high="green")}
        
        #plot1=ggplotly(plot1)
        plot1
        
      }
      
      
      else if(input$var==2){ #Album DONE
        albumByYear=album[album$publicationDate==input$year,]
        albumByYear=albumByYear[!albumByYear$deezerFans=="unknown",]
        albumByYear$deezerFans= as.numeric(albumByYear$deezerFans)
        plot2=head(albumByYear,10) %>% ggplot(aes(x=reorder(title,as.integer(deezerFans)),y=log2(as.integer(deezerFans)),fill=log2(as.integer(deezerFans))))+
          geom_bar(stat='identity',position='dodge',width = 0.5)+
          scale_x_discrete(guide = guide_axis(n.dodge=3))+labs(title=paste("Most followed albums in the Hip Hop categorie of Deezer in",input$year),x="Album Titles",y="Number of Deezer fans (log2 Scale)",fill="Deezer Fans (log2 scale)")+
          geom_text(aes(label = c(head(albumByYear,10)$Artist_name)),vjust = 1.5, colour = "black")
        
        if(input$radio=="Blue"){
          plot2+scale_fill_gradient(low="cyan", high="#4553ff")}
        else if(input$radio=="Orange"){
          plot2+scale_fill_gradient(low="yellow", high="red")}
        else if(input$radio=="Green"){
          plot2+scale_fill_gradient(low="white", high="green")}
        
      }
      
      
      
      
      else if(input$var==4){ #DONE but we can maybe add a filter
        
          
          
          if(input$gender2=="Male"|input$gender2=="Female"|input$gender2=="unknown"){
            dfbySex2=artistKnown[artistKnown$gender==input$gender2,]
          }
          if (input$gender2=="All"){
            dfbySex2=artistKnown
          }
          if(input$gender2=="Male Vs Female"){
            dfbySex2=artistKnown[artistKnown$gender!="unknown",]
          }
          
        
          
          
        
        
        dfbySex2%>%ggplot(aes(x=as.integer(substr(lifeSpan.begin,1,4)),y=as.integer(deezerFans),color=gender))+
          geom_point()+
          facet_wrap(facets=~ gender)+
          labs(title="Comparison of popularity in the Hip Hop categorie of Deezer (in terms of DeezerFans)",x="Begining of Career",y="Deezer Fans")+
          geom_text_repel(aes(label = name)) 
        
        
      }
    })
    
    output$distPlot2 <- renderPlotly({
      if(input$var==3){
        artistKnown=artist[!artist$lifeSpan.begin=="unknown",]
        dfbySex=artistKnown #in order to save in case
        
        if(input$gender=="Male"|input$gender=="Female"){
          dfbySex=artistKnown[artistKnown$gender==input$gender,]
        }
        if (input$gender=="All"){
          dfbySex=artistKnown
        }
        #in order to save in case
        
        
        plot3=head(dfbySex,100)%>%ggplot(aes(y=log2(as.numeric(deezerFans)),x=as.integer(substr(lifeSpan.begin,1,4)),color=gender,text=paste("Name : ",name,"<br>","Deezers Fans :",deezerFans,"<br>","Gender :",gender)))+
          geom_point()+
          labs(title="Popularity of Hip Hop artist according to their Deezer Fans ",x="Begining of Career",y="Fans (log2 scale)")+
          scale_fill_manual(values = c("#f9ebaf", "#1f2c33","#1f2c33"))+
          theme_classic()+
          theme(plot.title = element_text(hjust = 0.5))
        # scale_x_discrete(breaks = seq(1924, 2010, 2))
        
        
        
        # geom_text(aes(label = name))
        #rajouter un meilleur size scale et les noms du top 10 ou 20
        #mettre le temps dans laxe x cest plus conventionnel
        
        #ggplotly(plot3)
        
        ggplotly(plot3,tooltip = c("text"))      }
    })
    
    
    
    output$Description <- renderText({
      
      if(input$var==1){#DONE
        paste("This plot shows the 10 most popular song of the year selected.
                  The Y axis shows the rank of the song inside the hip hop genre while the X axis shows the title of the song.
                  On top of the bars, you can see the name of the artist who composed the song.
                  ")
      }
      else if(input$var==2){ #DONE
        paste("This plot shows the 10 most popular Hip Hop album genre of the year selected
                  The X axis shows the album title while the Y axis and the color shows the number of Deezers fans in a log2 Scale
                  On top of the bars, you can see the name of the artist who composed the album.
                  ")
        
      }
      else if(input$var==3){ #DONE, 
        paste("This plot shows the the comparaision of artist with the number of their deezer Fans and the start year of their career.
                  The X axis is a time axis, it shows the year of begining of their career.
                  The Y axis represent their deezer fans with a log2 scale.
                  The color represent the gender of the artist and you can see the name of the artist with the dot that represent him.
                  You can also chose if you want to display only men,women or unknown by clicking on the legend.
                  ")
        
        
      }else if(input$var==4){ #DONE, ecrire description
        paste("This plot shows the comparaision of the popularity in hip hop between men and women.
                  The X axis is a time axis, it shows the year of begining of their career.
                  The Y axis represent the number of deezer Fans
                  The color represent the gender of the artist and you can see the name of the artist with the dot that represent him.")
        
      }
      
    })
    
    
    
  })

shinyApp(ui2,server2)






shinyUI(fluidPage( 
  titlePanel("Analisis Sentimental"), 
  textOutput("TiempoReal"),   
  h4("tweets@:"),   
  sidebarLayout(
    sidebarPanel(
      dataTableOutput('tweets_table')
    ),
      
      mainPanel(
        plotOutput("distPlot"), 
        sidebarPanel(
          plotOutput("positive_wordcloud") 
        ),
        sidebarPanel(
          plotOutput("negative_wordcloud") 
        ),
        sidebarPanel(
          plotOutput("neutral_wordcloud")
        )))))

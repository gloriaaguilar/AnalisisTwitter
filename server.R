library(shiny) 
library(tm)
library(wordcloud)
library(twitteR)
shinyServer(function(input, output, session) {
  setup_twitter_oauth(consumer_key = "hWI7IzxIQibeVEzU9dHMErbpT", consumer_secret = "uNJdWaxMpVSH76RpbCQwPOXZUFSWkANWUj3kuQvftDwEVZm0uJ",access_token =  "238339004-8SeWy66JSTE9wUR0pbugdRRpjPyt8zAkr6sraJrl", access_secret= "SCfU3vRFNXGth8YGXz1L8vzZtxUGbBhYjH1Szfh0aN4cW") 
  output$currentTime <- renderText({invalidateLater(100000, session) 
    paste("TiempoReal:",Sys.time())})
  observe({
    invalidateLater(6000000,session)
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    positive_text <- vector()
    positive_split<-list()
    negative_text <- vector()
    negative_split <- list()
    neutral_text <- vector()
    neutral_split <- list()
    vector_users <- vector()
    vector_sentiments <- vector()
    tweets_result = ""
    tweets_result = searchTwitter("#jueves", n=100) 
    for (tweet in tweets_result){
      print(paste(tweet$screenName, ":", tweet$text))
      vector_users <- c(vector_users, as.character(tweet$screenName)); 
      if (grepl("días", tweet$text, ignore.case = FALSE) | grepl("buen", tweet$text, ignore.case = FALSE) | grepl("abrazo", tweet$text, ignore.case = FALSE)){ 
        count_positive = count_positive + 1 
        vector_sentiments <- c(vector_sentiments, "Positivo") 
        positive_text <- c(positive_text, as.character(tweet$text)) 
        positive_split<-strsplit(positive_text, split=" ")
        positive_columnas <- data.frame(unlist(positive_split))
        tbl_positive <- table(positive_columnas)
      } else if (grepl("aburrido", tweet$text, ignore.case = FALSE) | grepl("serio", tweet$text, ignore.case = FALSE) | grepl("no", tweet$text, ignore.case = FALSE)) { 
        count_negative = count_negative + 1
        vector_sentiments <- c(vector_sentiments, "Negativo")
        negative_text <- c(negative_text, as.character(tweet$text))
        negative_split=strsplit(negative_text, split=" ")
        negative_columnas = data.frame(unlist(negative_split))
        tbl_negative <- table(negative_columnas)
      } else { 
        count_neutral = count_neutral + 1
        print("neutral")
        vector_sentiments <- c(vector_sentiments, "Neutral")
        neutral_text <- c(neutral_text, as.character(tweet$text))
        neutral_split<-strsplit(neutral_text, split=" ")
        neutral_columnas<-data.frame(unlist(neutral_split))
        tbl_neutral <- table(neutral_columnas)
      }
    }
    df_users_sentiment <- data.frame(vector_users, vector_sentiments) 
    output$tweets_table = renderDataTable({ df_users_sentiment})
    
    output$distPlot <- renderPlot({
      results = data.frame(tweets = c("Positivo", "Negativo", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
      barplot(results$numbers, names = results$tweets, xlab = "Sentimiento", ylab = "Cantidad", col = c("Orange","Gray","Blue"))
      
      
      
    
      
      
      
      if (length(positive_text) > 0){
     
        output$positive_wordcloud <- renderPlot({ wordcloud(names(tbl_positive), tbl_positive, min.freq = 1, scale=c(8,.5),random.color=TRUE)  })
      }
      if (length(negative_text) > 0) {
       
        output$negative_wordcloud <- renderPlot({ wordcloud(names(tbl_negative), tbl_negative, min.freq = 1, scale=c(8,.5),random.color=TRUE)  })
      }
      if (length(neutral_text) > 0){
       
        output$neutral_wordcloud <- renderPlot({ wordcloud(names(tbl_neutral), tbl_neutral, min.freq = 1, scale=c(8,.5),random.color=TRUE)  })
      }
    })

  })
})

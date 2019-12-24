#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


if(!require(textdata)) {install.packages("textdata")}

library(dplyr)
library(sentimentr)
library(tidytext)
library(shiny)
library(wordcloud)

source('https://raw.githubusercontent.com/deepanshu-goyal/Text-Analytics/master/Common_Functions.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  inp_data <- reactive({
    
    if(is.null(input$file1)) {return(NULL)} else
    {
      data = read.csv(input$file1$datapath, header = TRUE, sep = ",")
      cols = colnames(data)
      
      if ('Review' %in% cols) return(data) else
      {
        return(NULL)
      }
    }
    
  })

  
  key_words <- reactive({
    
    if(is.null(input$keyword)) {return(NULL)} else
    {
      keywords <- tolower((as.character(strsplit(input$keyword,',')[[1]])))
      return(keywords)
    }
  })
  
  output$output1 <- renderTable({
    
    data_df = data.frame(Review = tolower(as.character(inp_data()$Review)))
    
    withProgress(message = "Making Table", value = 0, {
    
    sent_df = get_sentences(data_df)
    sentiment_score = sentiment(sent_df)
    sentiment_score$keyword = NA
    final_df = sentiment_score %>% select(Review,sentiment,keyword)
    
    keywords = as.character(key_words())
    
    for (feature in keywords){
      final_df$keyword = ifelse(grepl(feature,final_df$Review),feature,final_df$keyword)
    }
    
    })

    return(final_df[!is.na(final_df$keyword),])
    
  })
  
  output$output2 <-renderPlot({
    
    data_df = data.frame(Review = tolower(as.character(inp_data()$Review)))
    
    withProgress(message = "Making WordCloud", value = 0, {
    sent_df = get_sentences(data_df)
    keywords = as.character(key_words())
    
    df = data.frame(element_id = sent_df$element_id, Review = sent_df$Review, keyword = NA )
    
    for (feature in keywords){
      df$keyword = ifelse(grepl(feature,df$Review),feature,df$keyword)
    }
    
    final_df = df[!is.na(df$keyword),]
    
    tokens = word_tokens(final_df)
    })
    
    return(wordcloud(tokens$word,tokens$n,colors = brewer.pal(8, "Dark2")))
    
  })
  
  output$output3 <-renderPlot({
    
    data_df = data.frame(Review = tolower(as.character(inp_data()$Review)))
    
    withProgress(message = "Making BarChart", value = 0, {
    sent_df = get_sentences(data_df)
    keywords = as.character(key_words())
    
    df = data.frame(element_id = sent_df$element_id, Review = sent_df$Review, keyword = NA )
    
    for (feature in keywords){
      df$keyword = ifelse(grepl(feature,df$Review),feature,df$keyword)
    }
    
    final_df = df[!is.na(df$keyword),]
    
    tokens = word_tokens(final_df)
    })
    
    return(bar_chart(tokens,"Top 20 words in this dataset"))
    
  },height = 800, width = 1000)
})

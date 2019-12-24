#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sentiment Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # user input file into input box here:
      fileInput("file1", 
                "Customer Review (csv file)",placeholder = "CSV file with header"),
      
      textInput("keyword","Keywords (comma seperated)",placeholder = "Input comma seperated keywords")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overview", 
                           br(),
                           h4(p("About this app")), 
                           p("This app perform following task:"),
                           p("1. Sentence tokenize input customer reviews & calculate sentiment score"),
                           p("2. Filter sentences based on the input keyword"),
                           p("3. Create a word cloud on the filtered dataset"),
                           p("4. Create a bar chart of the frequent word in the filtered dataset"),
                           br(),
                           h4(p("About Input Data")), 
                           p("This app works only for csv files with header. Comma seperated file (csv) file should have atleast two columns with one column header as 'Review'"),
                           p("Please refere to  below link for sample csv file"),
                           a(href="https://github.com/deepanshu-goyal/Text-Analytics/blob/master/seltos_hector_compass_reviews.csv", "Sample Input File"),
                           br(),
                           br(),
                           h4(p("How to use this app?")), 
                           p("Please follow below two steps to successfully run this app:"),
                           p("1. Upload csv file containing customer reviews.Make sure your csv should have atleast two column with one column header as 'Review'. Please refer sample csv file in case of any confusion."),
                           p("2. Provide comma seperated keyword to find out customer sentiment score based on these keywords e.g. 'engine, sound, Price Range'")
                  ),
                  
                  tabPanel("Filtered Dataset",
                           br(),
                           h4("Sentences filtered on the input keyword"),
                           tableOutput('output1')),
                  
                  tabPanel("WordCloud",
                           br(),
                           h4("Wordcloud on the filtered dataset"),
                           plotOutput('output2')), 
                  
                  tabPanel("Bar-Chart",
                           br(),
                           h4("Bar-chart of frequent used words in the filtered dataset"),
                           plotOutput('output3'))
                  
      ) # end of tabsetPanel
    )
  )
))
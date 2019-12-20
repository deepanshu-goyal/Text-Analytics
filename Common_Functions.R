## Install Packages
if(!require(dplyr)) {install.packages("dplyr")}
if(!require(tidytext)) {install.packages("tidytext")}
if(!require(ggplot2)) {install.packages("ggplot2")}
if(!require(wordcloud)) {install.packages("wordcloud")}
if(!require(igraph)) {install.packages("igraph")}
if(!require(tidyr)) {install.packages("tidyr")}
if(!require(topicmodels)) {install.packages("topicmodels")}
if(!require(ggraph)) {install.packages("ggraph")}

library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(igraph)
library(tidyr)
library(topicmodels)


## Function 1: Create DTM from a csv file containing reviews in 'Reviews' column

create_dtm <- function(raw_data)
{
  values = c('car','kia','seltos','cars','jeep','compass','MG','hector','mg')
  reviews_df = data_frame(brand = tolower(as.character(raw_data$Brand)), review = tolower(as.character(raw_data$Review)))
  dtm_df = reviews_df %>% mutate(doc=row_number()) %>% unnest_tokens(word,review) %>% anti_join(stop_words) %>% 
           filter(!word %in% values) %>% count(word)
  
  final_dtm = cast_sparse(dtm_df,doc,word,n);
  return(final_dtm)
}



## Function 2: Create word tokens and calcuate top 20 words with maximum frequency. 

word_tokens <- function(raw_data)
{
  values = c('car','kia','seltos','cars','jeep','compass','MG','hector','mg')
  review = as.character(raw_data$Review)
  review_df = tibble(review=review)
  token = review_df %>% unnest_tokens(word,review) %>% anti_join(stopwords) %>% count(word,sort = TRUE) %>% 
    filter(!word %in% values) %>% top_n(25) %>% mutate(word =reorder(word,n))
  return(token)
}


## Function 3: Create bar chart for dataframe containing word tokens and its frequency

bar_chart <- function(tokens,title)
{
  tokens %>% ggplot(aes(word,n))+
    geom_bar(stat = "identity", col = "blue", fill = "blue") +
    geom_text(aes(label=n), hjust=-0.2) +
    ggtitle(label = title)+
    coord_flip()
  
}

## Function 4: Create bigrams and its frequency

create_bigram <- function(raw_data)
{
  values = c('kia','seltos','jeep','compass','MG','hector','mg')
  reviews = tolower(as.character(raw_data$Review))
  reviews_df = data.frame(review=reviews)
  dtm_df = reviews_df %>% unnest_tokens(bigram,review,token = 'ngrams',n=2) %>% 
    separate(bigram, c('word1','word2'),sep =" ") %>% 
    filter(!word1 %in% stopwords$word) %>% filter(!word2 %in% stop_words$word) %>%
    filter(!word1 %in% values) %>% filter(!word2 %in% values) %>% count(word1,word2,sort = TRUE)
  return(dtm_df)
}



# Function 5: Perfrom LDA 

find_topics = function(dtm, k=10)
{
  car_lta = LDA(dtm,k)
  car_topic = tidy(car_lta, matrix="beta")
  car_top_terms = car_topic %>% group_by(topic) %>% top_n(10,beta) %>% ungroup() %>% arrange(topic,-beta)
  print(car_top_terms)
  
  # Plat top 10 words in each topic as bar chart and identify topics
  car_graph = car_top_terms %>% mutate(term = reorder_within(term,beta,topic)) %>%
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic, scales = 'free')+
  coord_flip()+
  scale_x_reordered()
  
  plot(car_graph)
  
  # Map topics with each documents having highest value of gamma
  car_document = tidy(car_lta, matrix="gamma")
  car_document_top_topics = car_document %>% group_by(document) %>% top_n(1,gamma) %>% ungroup()
  print(car_document_top_topics)
}

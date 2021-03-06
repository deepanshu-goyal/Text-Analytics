## Install Packages
if(!require(dplyr)) {install.packages("dplyr")}
if(!require(tidytext)) {install.packages("tidytext")}
if(!require(ggplot2)) {install.packages("ggplot2")}
if(!require(wordcloud)) {install.packages("wordcloud")}
if(!require(igraph)) {install.packages("igraph")}
if(!require(tidyr)) {install.packages("tidyr")}
if(!require(topicmodels)) {install.packages("topicmodels")}
if(!require(ggraph)) {install.packages("ggraph")}
if(!require(stopwords)) {install.packages("stopwords")}

library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(igraph)
library(tidyr)
library(topicmodels)
library(stopwords)


## Function 1: Create DTM from a csv file containing reviews in 'Reviews' column

create_dtm <- function(raw_data)
{
  values = c('car','kia','seltos','cars','jeep','compass','MG','hector','mg')
  reviews_df = data_frame(brand = tolower(as.character(raw_data$Brand)),review = tolower(as.character(raw_data$Review)))
  reviews_df = data_frame(brand = tolower(as.character(raw_data$Brand)),review = tolower(as.character(raw_data$Review)))
  dtm_df = reviews_df %>% group_by(brand) %>% mutate(doc=row_number()) %>% unite(document,brand,doc) %>% 
    unnest_tokens(word,review) %>% anti_join(stop_words) %>% 
    filter(!word %in% values) %>% group_by(document) %>% count(word)
  
  final_dtm = cast_sparse(dtm_df,document, word,n);
  return(final_dtm)
}



## Function 2: Create word tokens and calcuate top 20 words with maximum frequency. 

word_tokens <- function(raw_data)
{
  values = c('car','kia','seltos','cars','jeep','compass','MG','hector','mg')
  review = as.character(raw_data$Review)
  review_df = tibble(review=review)
  token = review_df %>% unnest_tokens(word,review) %>% anti_join(stop_words) %>% count(word,sort = TRUE) %>% 
    filter(!word %in% values) %>% top_n(25) %>% mutate(word =reorder(word,n))
  return(token)
}


## Function 3: Create bar chart for dataframe containing word tokens and its frequency

bar_chart <- function(tokens,title)
{
  tokens %>% ggplot(aes(word,n))+
    geom_bar(stat = "identity", col = "cadetblue", fill = "cadetblue", width=.7, position = position_dodge(width = 1)) +
    geom_text(aes(label=n), hjust=-0.2,size=4) +
    ggtitle(label = title)+
    theme(text = element_text(size=15))+
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
    filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>%
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

# Function 6: Create COG graph

distill.cog = function(dtm, # input dtm
                       title="COG", # title for the graph
                       central.nodes=4,    # no. of central nodes
                       max.connexns = 5){  # max no. of connections  
  
  # first convert dtm to an adjacency matrix
  dtm1 = as.matrix(dtm)   # need it as a regular matrix for matrix ops like %*% to apply
  adj.mat = t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
  diag(adj.mat) = 0     # no self-references. So diag is 0.
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  mat1 = as.matrix(adj.mat[a0[1:50], a0[1:50]])
  
  # now invoke network plotting lib igraph
  library(igraph)
  
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:central.nodes){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[max.connexns]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc, word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  
  # building and plotting a network object
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:central.nodes] = "green"
  V(graph)$color[(central.nodes+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
} # distill.cog func ends


# Function 7: Create graph from bigram

create.graph = function(bigram,frequency=5)
  {
  bigram_graph = bigram %>% filter(n>frequency) %>% graph_from_data_frame()
  
  ggraph(bigram_graph, layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
}

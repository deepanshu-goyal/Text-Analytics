create_dtm <- function(raw_data)
{
  data_df = data_frame(text = tolower(as.character(raw_data$text)))
  
  dtm_df = data_df%>% mutate(docid=row_number()) %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>%   group_by(docid) %>% count(word)
  
  final_dtm = cast_sparse(dtm_df,docid, word,n)
  return(final_dtm)
  
}

create_bigram <- function(data)
{
  text = tolower(as.character(data$text))
  text_df = data.frame(text=text)
  bigram_df = text_df %>% unnest_tokens(bigram,text,token = 'ngrams',n=2) %>% 
    separate(bigram, c('word1','word2'),sep =" ") %>% 
    filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>%
    count(word1,word2,sort = TRUE)
  return(bigram_df)
}

find_topics = function(dtm, k=5)
{
  data_lta = LDA(dtm,k)
  data_topic = tidy(data_lta, matrix="beta")
  data_top_terms = data_topic %>% group_by(topic) %>% top_n(10,beta) %>% ungroup() %>% arrange(topic,-beta)
  
  # Plat top 10 words in each topic as bar chart and identify topics
  data_graph = data_top_terms %>% mutate(term = reorder_within(term,beta,topic)) %>%
    ggplot(aes(term,beta,fill=factor(topic)))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~topic, scales = 'free')+
    theme_bw()+
    coord_flip()+
    scale_x_reordered()
  
  plot(data_graph)
  
  # Map topics with each documents having highest value of gamma
  data_document = tidy(data_lta, matrix="gamma")
  document_top_topics = data_document %>% group_by(document) %>% top_n(1,gamma) %>% ungroup()
  return(data_graph)
}

# function to create word tokens
create_word_tokens <- function(data)
{
  text = as.character(data$text)
  text_df = tibble(text=text)
  token = text_df %>% unnest_tokens(word,text) %>% anti_join(stop_words) %>% count(word,sort = TRUE) %>% top_n(25) %>% mutate(word =reorder(word,n))
  return(token)
}


#function to plot bar charts
bar_chart <- function(tokens,title)
{
  head(tokens,10) %>% ggplot(aes(word,n))+
    geom_bar(stat = "identity", col = "cadetblue", fill = "cadetblue", width=.7, position = position_dodge(width = 1)) +
    geom_text(aes(label=n), hjust=-0.2,size=4) +
    ggtitle(label = title)+
    labs(title = "", x = "Word", y = "Frequency") +
    theme_bw()+
    coord_flip()
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
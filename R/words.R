# Script cria o acompanhamento geral das publicações ao longo do tempo


# Library

require(rjson)
require(tidyverse)
require(lubridate)
require(tm)
require(formattable)
require(quanteda)
require(tidytext)
require(stringi)
require(tidyr)
library(igraph)
library(ggraph)
library(animation)


# loading e colocando o nome bolsonaro em todos as publicacões deste (para criar o grafo)



graph_function <- function(year){ 
  
  tbl_ajuste <- read.csv("bolsonaro.csv") %>% 
    filter(year(date) == year)
  
# Palavras mais citadas
  
  text <- tbl_ajuste$tweet %>% 
    tolower() %>% 
    as.tibble() %>% 
    mutate(value = stri_trans_general(value, "Latin-ASCII"))
  
 nowords <- c("sei", "to", "sobre", "ja", "ta", "vai", "nao", "📈", "🤦♀", 
               "tendo", "poucos", "obg", "desses", "tudo", 'vdd', "
             mil", "so", "pra", "tudo", "n", "q", "vou", "ter", "pq", "tds", 
               "+", "la", "fez", "vcs", "pro", "etc", "nada", "oq", 
               "voce", "ver", "vc", 'ficar', "toda", "c", "d", "todos", 
              "p", "dia", "31", "hj", "vídeo", "noite", "papo", "anos", 
              "agradeco", "discurso", "ainda", "paises", "unico", 
              "qualquer", "pode", "desde", "problemas", "boa", 
              "pic.twitter.com", "sao", "cada", "assim", "alem", 
              "2018", "grande", "abraco", "parte", "assista", "ate", 
              "ha", "tao", "enquanto", "nunca", "favor", "agora", 
              "fazer", "1", "2", "3", "5", "15","2019", "partir", 
              "serao", "menor")
  
    toks_news <- tokens(text$value, remove_punct = TRUE)
    
    toks_news <- tokens_select(toks_news, pattern = stopwords('pt'), 
                               selection = 'remove')
    
    toks_news <- tokens_select(toks_news, pattern = nowords, 
                               selection = 'remove')
    
    dfmat_keep <- dfm(toks_news)
    
    
    word_fcm <- fcm(dfmat_keep)
    
    topword <- names(topfeatures(word_fcm, 50))
  
  
    ### Dataframe
  
  word_fcm <- fcm_select(word_fcm, pattern = topword)
  
  p <- textplot_network(word_fcm, min_freq = 0.7, 
                        edge_color = "orange", edge_size = 5)
  
  p + labs(title = paste0("Redes de palavras citadas por Bolsonaro no ano de", " ", 
                          year))
  
}


all_plots <- 2014:2020 %>% 
  map(graph_function)


for(i in 1:7){ 
  
  
  ggsave(all_plots[[i]], filename = paste0("Grafico de palavras do ano de", " ", 
                                           i+2013, ".jpg"))
  
  
}





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
  mutate(tweet = paste0(tweet, " ", "#jairbolsonaro")) %>% 
  filter(year(date) == year)


# Os mais citados

# Retorna os 50 usuarios mais citados na internet depois da limpeza
# feita em redes_usuarios.R

# Quanteda


opt <- theme_bw()+
  theme(axis.title = element_text(face = "bold", color = "black", size = 20),
        axis.text.x = element_text(face = "plain", color = "black", 
                                   size = 18, angle = 90),
        axis.text.y = element_text(face = "plain", color = "black", size = 18),
        legend.text=element_text(size=20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))


# ajstando


text <- tbl_ajuste$tweet %>% 
  tolower() %>% 
  as.tibble() %>% 
  mutate(value = stri_trans_general(value, "Latin-ASCII"))

# tipos 

text1 <- tbl_ajuste$tweet %>% 
  tolower() %>% 
  as.tibble() %>% 
  mutate(value = stri_trans_general(value, "Latin-ASCII"))


all1 <- quanteda::corpus(text1$value)

all_dfm1 <- dfm(all1) 

user_dfm1 <- dfm_select(all_dfm1, ('#*'))

user_fcm1 <- fcm(user_dfm1)

topuser1 <- names(topfeatures(user_dfm1, 50))

### Dataframe

user_fcm1 <- fcm_select(user_fcm1, pattern = topuser1)

p <- textplot_network(user_fcm1, min_freq = 0.5, 
                 edge_color = "orange", edge_size = 5)

p + labs(title = paste0("Redes de # citando Bolsonaro no ano de", " ", 
                        year))

}


all_plots <- 2014:2019 %>% 
  map(graph_function)



for(i in 1:6){ 
  
  
  ggsave(all_plots[[i]], filename = paste0("Grafico # do ano de", " ", 
                                           i+2013, ".jpg"))
  
  
  }
  




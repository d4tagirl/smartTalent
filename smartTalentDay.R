# Cargo las librerías que uso:
  
library(dplyr)
library(rtweet)
library(wordcloud)
library(igraph)
library(viridis)
library(tm)
library(stringr)
library(widyr)
library(ggraph)
library(tidytext)
library(rcorpora)
library(purrr)

# Bajo tweets que hablen de SmartTalentUY y miro los primeros

tweets <- readRDS("tweets_hasta_4oct.rds")

tweets_nuevos <- rtweet::search_tweets(q = "@SmartTalentUY OR to:SmartTalentUY OR SmartTalentUY", 
                                include_rts = FALSE) 

tweets <- tweets %>% 
  bind_rows(tweets_nuevos) %>% 
  distinct(status_id, .keep_all= TRUE)

head(tweets)
# Extraigo la info de los usuarios que hablaron de SmartTalentUY:
  
users <- toString(tweets$screen_name) %>%
  str_remove_all(pattern = "SmartTalentUY,") %>% 
  str_split(pattern = " ", simplify = TRUE)
  
# Hago una nube de palabras con los usuarios que hablaron de la SmartTalentUY:
  
set.seed(16995145)
wordcloud(users, colors = viridis::viridis_pal(end = 0.8)(10),
          min.freq = 1,
          random.color = TRUE, max.words = 100,
          scale = c(2.5,.2), rot.per=.3)


# Selecciono las palabras comúnes como "el", "es", "la" para que no las tome en cuenta:

stopwords <- corpora("words/stopwords/en")$stopWords

stopwords <- c(stopwords, corpora("words/stopwords/es")$stopWords, "t.co", "https", "ésto")

# Veo qué palabras son las más usadas en los Tweets que mencionan a la SmartTalentUY, sacando las menciones y las urls:
  
words <- data_frame(text = tweets$text) %>% 
  mutate(text = str_replace_all(text, '@([a-zA-Z0-9\\_\\.]+)', ''),
         text = str_replace_all(text, 'https:([a-zA-Z0-9\\_\\.\\/]+)', '')) %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% stopwords)

words_used <- toString(words$word) %>%
  str_remove_all(pattern = "smarttalentday,") %>% 
  str_split(pattern = " ", simplify = TRUE)

# Hago una nube de palabras con las palabras usadas en los Tweets que mencionaron a la SmartTalentUY:
  
set.seed(16993512)
wordcloud(words_used, colors = viridis::viridis_pal(end = 0.8)(10),
          min.freq = 2, random.color = TRUE, 
          # max.words = 40,
          scale = c(3.5,.4),
          rot.per=.2)

# Preparo los datos para armar la red de los mencionados en los Tweets:

mentioned_users <- tweets %>% 
  mutate(mentions = map_chr(.$mentions_screen_name, paste0, collapse = " ")) %>% 
  select(status_id, mentions, screen_name, reply_to_screen_name)

# Construyo la red de los usuarios que hablaron de la SmartTalentUY y cómo se vincularon entre ellos:
  
set.seed(365464)
data_frame(users = mentioned_users$mentions, status_id = mentioned_users$status_id)  %>% 
  unnest_tokens(user, users) %>% 
  pairwise_count(user, status_id, sort = TRUE, upper = FALSE) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,edge_colour = "red", edge_width = 1,
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "red", size = 5, alpha = .5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines"), vjust = 1, hjust = 1) +
  theme_void()

# Y ésto es lo que se ve 🎉

set.seed(16995145)
wordcloud(mentioned_users$mentions, colors = viridis::viridis_pal(end = 0.8)(10),
          min.freq = 700, random.color = TRUE, max.words = 60,
          scale = c(3.5,0.5),
          rot.per=.3)


# saveRDS(tweets, "tweets_hasta_11oct.rds")


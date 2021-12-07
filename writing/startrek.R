# Get the Data
library(tidyverse)
library(tidytext)
library(textdata)

startrek <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv")

#tidy the data
startrek_tidy <- startrek %>% 
  unnest_tokens(words, line) %>% 
  unlist(words)
  #anti_join(stop_words)

######Sentiment Analysis######
#sent_afinn <- get_sentiments("afinn")
#sent_nrc <- get_sentiments("nrc")
#sent_loughran <- get_sentiments("loughran")

#get sentiments from bing dataset (pos/neg)
sentiment <- unlist(get_sentiments("bing")) 
 
star_sent <- startrek_tidy %>% 
  inner_join(sentiment, startrek_tidy, by = c("words" = "words"), copy = TRUE) 
    count(word, sort = TRUE) %>% 
    pivot_wider(names_from = , values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative)


ggplot(startrek_sent, aes(index, sentiment, fill = line)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~line, ncol = 2, scales = "free_x")





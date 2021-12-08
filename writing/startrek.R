# Get the Data
library(tidyverse)
library(tidytext)
library(textdata)
library(magrittr)


startrek <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv")

data(stop_words)

#tidy the data
startrek_tidy <- tibble(startrek)  
startrek_tidy <- startrek_tidy %>% 
  unnest_tokens(words, line) %>% 
  rename("script_words" = words)
unlist(script_words)


#get sentiments from bing dataset (pos/neg)
sentiment <- get_sentiments("bing") 
sentiment <- sentiment %>% 
  filter(words %in% words)


######Sentiment Analysis######
#sent_afinn <- get_sentiments("afinn")
#sent_nrc <- get_sentiments("nrc")
#sent_loughran <- get_sentiments("loughran")


select(words, w1, new_word) %>% 
  filter(w1 %in% wordy$words) %>% 
  filter(new_word %in% wordy$words)


startrek_sent <- startrek_tidy
startrek_sent %>% 
  inner_join(sentiment, startrek_tidy, copy = TRUE, suffix = c(".sentiment", ".startrek_tidy")) 
    count(word, sort = TRUE) %>% 
    pivot_wider(names_from = , values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative)


ggplot(startrek_sent, aes(index, sentiment, fill = line)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~line, ncol = 2, scales = "free_x")





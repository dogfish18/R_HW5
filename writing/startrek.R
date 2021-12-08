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
  select(char, line) %>% 
  unnest_tokens(words, line) %>% 
  rename("script_words" = words) 
  
sent_afinn <- get_sentiments("afinn") 

sent_nrc <- get_sentiments("nrc") %>% 
  filter(word %in% startrek_tidy$script_words)

sent_loughran <- get_sentiments("loughran") %>% 
  filter(word %in% startrek_tidy$script_words)

sentiment_bing <- get_sentiments("bing") %>% 
  filter(word %in% startrek_tidy$script_words)

######Sentiment Analysis (pos/neg)######

startrek_sentiment <- sentiment_bing %>% 
  inner_join(sentiment_bing, copy = TRUE) %>% 
  add_column(obs = 1:244)

count_sent <- startrek_sentiment %>% 
  count(sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(difference = positive - negative) 
  
p <- startrek_sentiment %>% 
  ggplot(aes(sentiment, obs)) +
  geom_col(show.legend = FALSE) 
  #facet_wrap(~obs, nrow = 2, ncol = 244 , scales = "fixed")
print(p)


##Comparison cloud
# x <- austin_tidy %>%
#   anti_join(bind_rows(stop_words,
#                       tibble(word = c("miss"), lexicon = c("custom")))) %>%
#   inner_join(get_sentiments("bing")) %>% 
#   filter(book == "Pride & Prejudice") %>%
#   count(word, sentiment, sort = TRUE) %>%
#   pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
#   as.data.frame()
# ## Joining, by = "word"
# ## Joining, by = "word"
# rownames(x) <- x[,1]
# comparison.cloud(x[, 2:3])
# ## Warning in comparison.cloud(x[, 2:3]): agreeable could not be fit on page. It
# ## will not be plotted.

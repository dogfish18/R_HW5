# Get the Data
library(tidyverse)
library(tidytext)
library(textdata)
library(viridis)

startrek <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv")

#tidy the data
tibble(startrek)
startrek_tidy2 <- startrek %>% 
  select(char, line) %>%
  unnest_tokens(words, line) %>% #makes each word an observation
  rename("script_words" = words) %>% 
  group_by(char) %>% 
  mutate(char = recode(char, 'Geordi (V.O.)' = "Geordi",'Geordi (O.S.)' = "Geordi", 
                       'Computer (V.O.)' = "Computer", 'Computer Voice' = "Computer", 
                       'Computer Voice (V.O.)' = "Computer", 
                       'New Computer Voice' = "Computer",
                       'Riker (O.S.)' = "Riker",
                       'Picard (O.S.)' = "Picard", 'Picard (V.O.)' = "Picard", 'Young Picard' = "Picard",
                       'Jean-Luc' = "Picard")) 

select_char <- c("Picard", "Geordi", "Data", "Riker", "Computer", "Beverly")
startrek_tidy3 <- filter(startrek_tidy2, char %in% select_char)

######Sentiment Analysis######

##bing
bing_join <- get_sentiments("bing") %>% 
  inner_join(startrek_tidy3, by = c("word" = "script_words")) %>% 
  group_by(sentiment, char) %>% 
  count()

plot_bing <- bing_join %>% 
    ggplot(aes(char, sentiment, fill = sentiment, y = n)) +
    geom_bar(position = "stack", stat = "identity") +
    xlab("Star Trek Character") + ylab("Number of words") +
    labs(title = "TNG Character and Computer Interactions", 
         subtitle = "Word Sentiment Analysis of the 'Bing' Lexicon", 
         caption = "Data source: www.tidytuesday.com") +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 0.5),
      legend.title = element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5),
      ) 
print(plot_bing)

###nrc (feelings)
omit_sent <- c("positive", "negative") #take out data with pos/neg because already analyzed
sent_nrc <- get_sentiments("nrc") %>% 
  filter(!sentiment %in% omit_sent)
  
nrc_join <- sent_nrc %>% 
  inner_join(startrek_tidy3, by = c("word" = "script_words")) %>% 
  group_by(sentiment, char) %>% 
  count() 

plot_nrc <- nrc_join %>% 
  ggplot(aes(char, sentiment, fill = sentiment, y = n)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Star Trek Character") + ylab("Number of words") +
  labs(title = "TNG Character and Computer Interactions", 
       subtitle = "Word Sentiment Analysis of the 'NRC' Lexicon", 
       caption = "Data source: www.tidytuesday.com") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0.5),
    legend.title = element_blank(),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5),
  ) 
print(plot_nrc)

###afinn (score)
sent_afinn <- get_sentiments("afinn") #score
afinn_join <- sent_afinn %>% 
  inner_join(startrek_tidy3, by = c("word" = "script_words")) %>% 
  group_by(value, char) %>% 
  count() 

x_labels = c(-4:4)
plot_afinn <- afinn_join %>% 
  ggplot(aes(value, n, fill = value)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_viridis(option = "D") +
  scale_color_viridis(option = "D") +
  facet_wrap(vars(char)) +
  xlab("Sentiment Score") +
  ylab("Word Count") +
  labs(title = "TNG Character and Computer Interactions", 
       subtitle = "Word Sentiment Analysis of the 'Afinn' Lexicon", 
       caption = "Data source: www.tidytuesday.com", 
       color = "Word Count") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0.5),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_blank()) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_continuous(labels = x_labels, breaks = x_labels)

print(plot_afinn)


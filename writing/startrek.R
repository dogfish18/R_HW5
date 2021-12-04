
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-08-17')
tuesdata <- tidytuesdayR::tt_load(2021, week = 34)

computer <- tuesdata$computer

# Or read in the data manually

computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')


library(tidyverse)
library(jsonlite)

url <- "http://www.speechinteraction.org/TNG/teaearlgreyhotdataset.json"

raw_json <- parse_json(url(url))

raw_json %>% 
  listviewer::jsonedit()

clean_df <- raw_json %>% 
  enframe() %>% 
  unnest_longer(value) %>% 
  unnest_wider(value) %>% 
  unnest_longer(type) %>% 
  unnest_longer(domain) %>% 
  unnest_longer(`sub-domain`) %>% 
  janitor::clean_names()

clean_df %>% write_csv("2021/2021-08-17/computer.csv")
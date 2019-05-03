library(tidyverse)
setwd("~/Projects/groundswell-takehome")

tags_df <- read.csv('ml-latest-small/tags.csv', stringsAsFactors = F)
ratings_df <- read.csv('ml-latest-small/ratings.csv')
movies_df <- read.csv('ml-latest-small/movies.csv', stringsAsFactors = F)
links_df <- read.csv('ml-latest-small/links.csv')

movies_df <- movies_df %>% 
  mutate(releaseYr = str_match(movies_df$title, '(?<=\\()\\d{4}(?=\\))'),
         title = str_match(movies_df$title, '.+(?=\\s)'))

avg_rating_df <- movies_df %>% 
  filter(releaseYr>=1980) %>% 
  inner_join(ratings_df) %>% 
  group_by(movieId, title, genres) %>% 
  summarize(avg_rating = mean(rating), n_rating = n()) %>% 
  filter(n_rating>=10) %>% 
  arrange(desc(avg_rating)) %>% 
  top_n(10)

avg_rating_df %>% 
  ggplot(aes(x = title, y = avg_rating))+
  geom_bar()





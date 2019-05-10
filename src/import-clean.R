#Load Dependencies
library(tidyverse)
library(forcats)
library(stringr)

# Load Data
tags_df <- read.csv('ml-latest-small/tags.csv', stringsAsFactors = F)
ratings_df <- read.csv('ml-latest-small/ratings.csv')
movies_df <- read.csv('ml-latest-small/movies.csv', stringsAsFactors = F, encoding = 'UTF-8')
links_df <- read.csv('ml-latest-small/links.csv')

# Clean data
## Movie Titles
clean_titles <- function(raw_title){
  titles = str_replace(raw_title, '\\(\\d{4}\\)', "") # Remove release year
  titles = str_replace(titles, ', The.+$', "") # Remove irrelevant ends
  titles
}

## Apply clean movie title and extract release year
movies_tidy_df <- movies_df %>% 
  mutate(releaseYr = as.integer(str_match(movies_df$title, '(?<=\\()\\d{4}(?=\\))')),
         title = clean_titles(movies_df$title))

## Find number of genres
get_temp_genre <- function(movies_df){
  genre_list <- movies_df %>% select(genres) %>% pull()
  max_genre_n <- max(lengths(str_split(genre_list, "\\|")))
  temp_genre_col <- paste('genre', c(1:max_genre_n), sep = "")
  temp_genre_col
}

# Convert genres column into tidy format
movies_tidy_df <- movies_tidy_df %>% 
  separate(genres, get_temp_genre(movies_df), sep = "\\|", fill = 'right') %>% 
  gather(get_temp_genre(movies_df), key = 'temp', value = 'genres') %>% 
  select(-temp) %>% 
  filter(!is.na(genres)) %>% 
  arrange(movieId)

## List of genres
genres <- movies_tidy_df %>% select(genres) %>% arrange(genres) %>% unique() %>% pull()

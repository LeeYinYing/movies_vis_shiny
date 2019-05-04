#Load Dependencies
library(plotly)
library(tidyverse)
library(forcats)
library(shiny)
library(stringr)


# Load Data
tags_df <- read.csv('ml-latest-small/tags.csv', stringsAsFactors = F)
ratings_df <- read.csv('ml-latest-small/ratings.csv')
movies_df <- read.csv('ml-latest-small/movies.csv', stringsAsFactors = F)
links_df <- read.csv('ml-latest-small/links.csv')

# Clean data
## Movie Titles
titles = str_replace(movies_df$title, '\\(\\d{4}\\)', "") # Remove release year
titles = str_replace(titles, ', The.+', "") # Remove irrelevant ends

## Apply clean movie title and extract release year
movies_df <- movies_df %>% 
  mutate(releaseYr = as.integer(str_match(movies_df$title, '(?<=\\()\\d{4}(?=\\))')),
         title = titles)

## Find number of genres
genre_list <- movies_df %>% select(genres) %>% pull()
max_genre_n <- max(lengths(str_split(genre_list, "\\|")))
temp_genre_col <- paste('genre', c(1:max_genre_n), sep = "")

# Convert genres column into tidy format
movies_tidy_df <- movies_df %>% 
  separate(genres, temp_genre_col, sep = "\\|", fill = 'right') %>% 
  gather(temp_genre_col, key = 'temp', value = 'genres') %>% 
  select(-temp) %>% 
  filter(!is.na(genres)) %>% 
  arrange(movieId)
## List of genres
genres <- movies_tidy_df %>% select(genres) %>% arrange(genres) %>% unique() %>% pull()

# Shiny App
ui <- navbarPage("Movie Ratings Analysis",
  tabPanel("Top Ratings Barplot",
    sidebarLayout(
      sidebarPanel(width = 4,
        tags$b('Description'), br(),
        "Display the top rated movies. Alter the filters to select for movies released within a selected year range and within certain genres. To access its IMDB page, click on movie titles.",br(),
        tags$small("Note: Only movies with at least 10 ratings are considered"), br(),br(),
        
        sliderInput("year_input", "Select Year Range", 
                    min = min(movies_df$releaseYr, na.rm = T), 
                    max = max(movies_df$releaseYr, na.rm = T),
                    value = c(1980, max(movies_df$releaseYr, na.rm = T)),
                    sep = ""),
        numericInput("n_movie_input", "Select Number of Top Rated Movies (3-10)",
                     value = 10, min = 3, max = 10, step = 1),
        checkboxGroupInput("genre_input", "Select Genres",
                           choices = genres,
                           selected = genres,
                           inline = TRUE)
      ),
      mainPanel(
        plotlyOutput('ratings_barplot')
      )
    )
  ),
  tabPanel("Releases Timeplot",
    sidebarLayout(
      sidebarPanel(),
      mainPanel()
    )
  )
)

server <- function(input, output) {

    avg_rating_df <- reactive({
    rm_movies <- movies_tidy_df %>% 
      filter(!genres %in% input$genre_input) %>%
      distinct(movieId) %>% pull()
    
    movies_tidy_df %>% 
    filter(releaseYr >= input$year_input[1] & releaseYr <= input$year_input[2]) %>% 
    filter(!movieId %in% rm_movies) %>% 
    distinct(movieId, .keep_all = T) %>%
    inner_join(ratings_df) %>% 
    group_by(movieId, title) %>% 
    summarize(avg_rating = mean(rating), n_rating = n()) %>%
    ungroup() %>% 
    filter(n_rating >= 10) %>% 
    arrange(desc(avg_rating)) %>% 
    head(input$n_movie_input) %>% 
    left_join(movies_df) %>% 
    left_join(links_df) %>% 
    mutate(imdbLink = paste("http://www.imdb.com/title/tt", 
                            str_pad(imdbId, 7, side = "left", pad = "0"), 
                            sep = "")) %>% 
    mutate(title = fct_reorder(title, avg_rating)) %>% 
    arrange(title)
  })
  
  axis_range <- reactive(
    c(floor(min(avg_rating_df()$avg_rating)), ceiling(max(avg_rating_df()$avg_rating)))
  )
  
  ratings_barplot <- reactive(
    avg_rating_df() %>% 
      ggplot(aes(x = title, y = avg_rating,
                 text = paste("Avgerage Rating: ", round(avg_rating, 3),
                              "<BR>Number of Ratings: ", n_rating,
                              "<BR>Genres: ", str_replace_all(genres, '\\|', ', '),
                              sep = "")))+
      geom_col(fill = "#FFDEAD")+
      coord_flip(ylim = axis_range())+
      theme_bw()+
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major.y = element_blank())+
      labs(x = "Movie", y = "Average Rating",
           title = paste("Top", input$n_movie_input, "Movies with Highest Rates"))
  )
  
  ratings_barplotly <- reactive(
    ggplotly(ratings_barplot(), height = 530, tooltip = 'text') %>% 
      add_annotations(x = rep(axis_range()[1],input$n_movie_input),
                      y = c(1:input$n_movie_input),
                      text = paste('<a href="',avg_rating_df()$imdbLink,'">',
                                   avg_rating_df()$title, '</a>', sep = ""),
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE,
                      xanchor = 'left'
    )
  )
  
  output$ratings_barplot <- renderPlotly(ratings_barplotly())
  output$ratings_table <- renderDataTable(avg_rating_df())
}
shinyApp(ui = ui, server = server)

library(plotly)
library(tidyverse)
library(forcats)
library(shiny)
library(stringr)

source(file = '../src/import-clean.R')

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
                      xanchor = 'left')
  )
  
  top_genres_df <- reactive(
    movies_tidy_df %>% 
      filter(releaseYr >= input$year_input2[1] & releaseYr <= input$year_input2[2]) %>%
      semi_join(movies_tidy_df %>%
        filter(releaseYr >= input$year_input2[1] & releaseYr <= input$year_input2[2]) %>% 
        group_by(genres) %>% 
        summarize(n = n()) %>% 
        arrange(desc(n)) %>% 
        head(input$n_genre_input)) %>% 
      group_by(releaseYr, genres) %>% 
      summarize(n_movies = n()) %>% 
      mutate(genres = as.factor(genres))
  )
  
  genre_table <- reactive(top_genres_df() %>% spread(key = releaseYr, value = n_movies, fill = 0))
  
  genres_timeplot <- reactive(
    top_genres_df() %>% 
      ggplot(aes(x = releaseYr, y = n_movies, group = genres, color = genres,
                 text = paste("Genre: ", genres,
                              "<BR>Year: ", releaseYr,
                              "<BR>Number of movies: ", n_movies,
                              sep = "")))+
        geom_line(size = 0.3)+
        geom_point(size = 0.7)+
        labs(title = paste("Number of movies released in the", 
                           input$n_genre_input, "most prevelant genres"),
             x = "Year",
             y = "Number of Movies")+
        scale_color_discrete("Genres")+
        theme_bw()
  )
  
  genres_timeplotly <- reactive(
    ggplotly(genres_timeplot(), tooltip = 'text') %>% 
      add_annotations(text="Movie Genres",
                      x= 0.5, xref = 'paper', xanchor="center",
                      y= -0.45, yref = 'paper', yanchor = 'bottom',
                       legendtitle=TRUE, showarrow=FALSE) %>% 
      layout(legend = list(orientation = "h", x = 0.5,y = -0.44,
                           xanchor = "center", xref = 'paper', yanchor = 'top', yref = 'paper'))
  )
    
  output$ratings_barplot <- renderPlotly(ratings_barplotly())
  output$genre_table <- renderDataTable(genre_table())
  output$genre_timeplot <- renderPlotly(genres_timeplotly())
}

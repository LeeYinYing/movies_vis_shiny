# Dependencies
library(plotly)
library(tidyverse)
library(forcats)
library(shiny)
library(stringr)
library(treemapify)
library(scales)
library(colorspace)
library(DT)

# Import and clean datasets
source(file = 'src/import-clean.R')

###################
# Shiny App Server
##################

server <- function(input, output) {
  
  #######
  # Tab1
  #######
  
  # Generate a dataframe that contains only movies in the selected year range and selected
  # genres. 
  # Features(columns): movieId, title, genres, avg_rating, n_rating, imdbLink
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
      left_join(movies_df, by = c("movieId")) %>%
      left_join(links_df) %>%
      mutate(imdbLink = paste("http://www.imdb.com/title/tt",
                              str_pad(imdbId, 7, side = "left", pad = "0"),
                              sep = "")) %>%
      select(-'title.y') %>% 
      mutate(title = fct_reorder(title.x, avg_rating)) %>%
      arrange(title)
  })
  
  # Axis range based on selection 
  axis_range <- reactive(
    c(floor(min(avg_rating_df()$avg_rating)), ceiling(max(avg_rating_df()$avg_rating)))
  )
  
  # Generate barplot
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
  
  # Connfigure barplot interactivity
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
  
  #########
  # Tab 2
  #########
  
  # Create dataframe that groups movies based on their average ratings and assign 
  # a color that is indicative of each movie's rating
  # Features(columns): movieId, title, genres, avg_rating, n_reviews, rating_grp, palette, size, color
  treemap_df <- reactive(
    movies_tidy_df %>%
      filter(genres == input$genre_input2) %>%
      left_join(ratings_df) %>%
      group_by(movieId, title, genres) %>%
      summarize(avg_rating = mean(rating, na.rm = T),
                 n_reviews = n()) %>%
      ungroup() %>% 
      mutate(rating_grp = case_when(avg_rating < 1 ~ 0,
                                    avg_rating >= 1 & avg_rating < 2 ~ 1,
                                    avg_rating >= 2 & avg_rating < 3 ~ 2,
                                    avg_rating >= 3 & avg_rating < 4 ~ 3,
                                    avg_rating >= 4 ~ 4)) %>% 
      mutate(palette = case_when(rating_grp == 0 ~ "Purples",
                                 rating_grp == 1 ~ "Red-Yellow",
                                 rating_grp == 2 ~ "Green-Yellow",
                                 rating_grp == 3 ~ "Teal",
                                 rating_grp == 4 ~ "Reds"),
             size = 1/n()) %>% 
      group_by(palette) %>% 
      filter(palette %in% c("Green-Yellow", 'Red-Yellow', 'Purples', 'Teal', 'Reds')) %>% 
      mutate(color = gradient_n_pal(sequential_hcl(6, palette = palette,rev = T)[2:6])
             (avg_rating - rating_grp))
  )

  # Generate treemap based on rating_grp, colors, palette, size assigned to each movie in
  # treemap_df
  treemap_plot <- reactive(
    treemap_df() %>% 
      ggplot(aes(area = size, fill = color,
                 subgroup = paste0(rating_grp,".0 - ", rating_grp+1,".0")))+
        geom_treemap()+
        geom_treemap_subgroup_border(color = 'white')+
        geom_treemap_subgroup_text(place = 'center')+
        scale_fill_identity()
  )
  
  # Generate a treemap that gives an overview of all genres in respect to the number of
  # movies in each genre and the average rating among all movies in their respective genres
  treemap_overview <- movies_tidy_df %>% 
    filter(genres != '(no genres listed)') %>%
    left_join(ratings_df) %>%
    group_by(movieId, title, genres) %>%
    summarize(avg_rating = mean(rating, na.rm = T), n_reviews = n()) %>% 
    group_by(genres) %>% 
    summarize(genre_avg_rating = mean(avg_rating, na.rm = T), 
              genre_n_reviews = sum(n_reviews, na.rm = T),
              n_movies = n()) %>% 
    ggplot(aes(area = n_movies, fill = genre_avg_rating, label = genres))+
      geom_treemap()+
      geom_treemap_text(place = 'center', alpha = 0.8, reflow = T, grow = F, )+
      scale_fill_continuous_sequential(rev = T, name = "Average\nRatings", palette = "PinkYl")
  
  # Create a dataframe that records basic stats for each ratings range groups
  # Feature(columns): n_movies, avg_range_rating (the averge rating in each range)
  stat_dt <- reactive(
    treemap_df() %>% 
      ungroup() %>% 
      group_by(rating_grp) %>% 
      summarize(n_movies = n(), avg_range_rating = round(mean(avg_rating, na.rm = T),2)) %>% 
      `rownames<-`(c('0 - 1', '1 - 2', '2 - 3', '3 - 4', '4 - 5')) %>% 
      select(-rating_grp) %>% 
      rename('#movies' = n_movies, 'Average\nrating' = avg_range_rating)
  )
  
  ########
  # Tab 3
  ########
  
  # Create dataframe contaning the top most prevalent genres based on user selection of 
  # year range and number of genres
  # Feature(columns): releaseYr, movieId, title, genres, n(number of movies) 
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
  
  # Generate timeplot based on top_genres_df
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
  
  # Configure interactivity for the timeplot
  genres_timeplotly <- reactive(
    ggplotly(genres_timeplot(), tooltip = 'text') %>% 
      add_annotations(text="Movie Genres",
                      x= 0.5, xref = 'paper', xanchor="center",
                      y= -0.45, yref = 'paper', yanchor = 'bottom',
                       legendtitle=TRUE, showarrow=FALSE) %>% 
      layout(legend = list(orientation = "h", x = 0.5,y = -0.44,
                           xanchor = "center", xref = 'paper', yanchor = 'top', yref = 'paper'))
  )
  
  #######################
  # LIST OF OUTPUTS TO UI
  #######################
  output$ratings_barplot <- renderPlotly(ratings_barplotly())
  output$genre_timeplot <- renderPlotly(genres_timeplotly())
  output$treemap_plot <- renderPlot(treemap_plot())
  output$treemap_overview <- renderPlot(treemap_overview)
  output$genres_dt <- renderDataTable(treemap_df() %>% ungroup() %>% 
                                        select(title, avg_rating, n_reviews) %>% 
                                        mutate(avg_rating = round(avg_rating, 2)),
                                      rownames = F,
                                      colnames = c('Title', 'Average Rating', '# Ratings'),
                                      filter = 'top',
                                      options = list(order = list(list(2,'desc'),list(1,'desc')),
                                                     pageLength = 10,
                                                     lengthMenu = c(5,10,15,20),
                                                     sDom  = '<"top">lrt<"bottom">ip'),
                                      caption = "Instructions: Default table is ordered by 
                                      # Ratings per movie. Use boxes at the top of each column 
                                      to search for specific movie titles, or select for a range
                                      of average ratings or # ratings per movie")
  output$stat_dt <- renderDataTable(stat_dt(),
                                    options = list(dom = 't', ordering = F,
                                                   autoWidth = T,
                                                   columnDefs = list(list(width = '300px',
                                                                          targets = c(0))
                                                   )))
}

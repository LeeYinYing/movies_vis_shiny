library(shiny)

source(file = '../src/import-clean.R')

# Shiny App
ui <- navbarPage("Movie Ratings Analysis",
  tabPanel("Top Ratings Barplot",
    sidebarLayout(
      sidebarPanel(width = 4,
         tags$b('Description'), br(),
         "Display the top rated movies. Alter the filters to select for movies released within a selected year range and within certain genres. To access its IMDB page, click on movie titles.",br(),
         tags$small("Note: Only movies with at least 10 ratings are considered"), br(),br(),
         
         sliderInput("year_input", "Select Year Range", 
                     min = min(movies_tidy_df$releaseYr, na.rm = T), 
                     max = max(movies_tidy_df$releaseYr, na.rm = T),
                     value = c(1980, max(movies_tidy_df$releaseYr, na.rm = T)),
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
  tabPanel("Movie Releases Timeplot",
    plotlyOutput('genre_timeplot'),br(),
    
    fluidRow(
      column(width = 3,
             tags$b('Description'), br(),
             "Number of movies released each year in the top most prevelant genres. Alter the 
             filters to select for movies released within a year range of interest and number of
             genres to display",br()),
      column(width = 4, offset = 1,
        sliderInput("year_input2", "Select Year Range", 
                    min = min(movies_tidy_df$releaseYr, na.rm = T), 
                    max = max(movies_tidy_df$releaseYr, na.rm = T),
                    value = c(2000, max(movies_tidy_df$releaseYr, na.rm = T)),
                    sep = "")),
      column(width = 4,
        numericInput("n_genre_input", "Select Number of Most Prevalent Genre (3-8)",
                     value = 8, min = 3, max = 8, step = 1))
    )
    #,
    #dataTableOutput('genre_table')
  )
)
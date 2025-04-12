library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

load('new_data.RData')

# UI
ui <- dashboardPage(
  dashboardHeader(title = strong("Japanese Anime")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("home")),
      menuItem("User Guide", tabName = "user_guide", icon = icon("user")),
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Anime", tabName = "anime", icon = icon("star")),
      menuItem("Anime Table View", tabName = "animetableview", icon = icon("table")),
      menuItem("Filters", tabName = "filters", icon = icon("filter"),
               checkboxGroupInput("type", "Type", 
                                  choices = unique(new_data$type), 
                                  selected = unique(new_data$type)),  
               selectizeInput("genre", "Genre", 
                              choices = c("All genres", unique(new_data %>%
                                                                 separate_rows(genres, sep = ", ") %>%
                                                                 pull(genres))),
                              selected = c("All genres"),  
                              multiple = TRUE,  
                              options = list(delimiter = "/")
               ),
               selectInput("rating", "Rating", 
                           choices = c("Choose a Rating", unique(new_data$rating))),
               sliderInput("year", "Year", 
                           min = min(new_data$start_year, na.rm = TRUE), 
                           max = max(new_data$start_year, na.rm = TRUE), 
                           value = c(min(new_data$start_year, na.rm = TRUE), max(new_data$start_year, na.rm = TRUE)),
                           step = 1),
               sliderInput("score", "Score", 
                           min = min(new_data$score, na.rm = TRUE), 
                           max = max(new_data$score, na.rm = TRUE), 
                           value = c(min(new_data$score, na.rm = TRUE), max(new_data$score, na.rm = TRUE)),
                           step = 0.01)
      )
    )
  ),
  dashboardBody(
    tags$style(HTML("
      .anime-img {
        width: 100%;
        height: 400px;  
        object-fit: cover;  
        border-radius: 8px;
        margin-bottom: 20px;  
      }
      .anime-column {
        padding: 10px;  
        text-align: center;  
        margin-bottom: 40px;  
      }
      .fluid-row {
        display: flex;
        justify-content: space-between;  
        flex-wrap: wrap;
      }
      .fluid-row .col-sm-2 {
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        align-items: center;  
        margin-bottom: 20px; 
      }
    ")),
    
    tabItems(
      tabItem(tabName = "introduction",
              h3(strong("Introduction")),
              p("This dashboard provides an interactive exploration of the comprehensive Japanese anime and manga dataset from the well-known database MyAnimeList. The dataset is available through the official API and the Jikan API, and is updated weekly. Through this dashboard, users can search for anime that match their preferences or check the distribution performance of various types of anime."),
              h4(strong("Dataset Overview")), 
              p("The dataset consists of detailed information on over 14,000 anime titles, including TV shows, movies, OVAs, and more. Key data points include the anime's title, type, source, rating, genres, studios, picture URLs, and more. This rich dataset is valuable for both anime enthusiasts and researchers, offering insights into trends, ratings, audience preferences, and the evolution of the anime industry."),
              p(strong("Source of Dataset:"), " The data comes from the well-known anime and manga database, MyAnimeList. The information is captured through both the official MyAnimeList API and the Jikan API, which allows for up-to-date and accurate data collection. The dataset is regularly updated every week to reflect the latest information on newly released anime and changes in anime ratings, genres, and studios."),
              
              h4(strong("Dataset Size & Structure")), 
              p("The dataset contains the following variables:"),
              tags$ul(
                tags$li(strong("12 columns, 14,218 rows:") ," A total of 12 columns of data for 14,218 anime titles."),
                tags$li(strong("Important Variables:")),
                tags$ul(
                  tags$li(strong("Object:") ," title_english, title_japanese, type, source, rating, genres, studios, picture_url"),
                  tags$li(strong("Numeric:") ," score, nb_score"),
                  tags$li(strong("Date:") ," start_date")
                )
              ),
              p("These attributes allow users to filter, explore, and visualize data about anime in various ways. From examining the distribution of scores to analyzing the genre popularity, this dataset provides ample opportunities for in-depth exploration and analysis."),
              h4(strong("What User Can Explore")), 
              p("This dataset enables users to explore the following themes:"),
              tags$ul(
                tags$li(strong("Anime Types and Sources:") ," Gain insights into the distribution of anime types (TV shows, movies, etc.) and their sources (original, manga, novel, etc.)."),
                tags$li(strong("Genre Distribution:") ," Discover the most popular genres of anime over time."),
                tags$li(strong("Score Analysis:") ," Analyze the distribution of scores and see how well-rated different anime are.")
              )
      ),
      tabItem(tabName = "user_guide",
              h3(strong("User Guide")), 
              h4(strong("Filters")), 
              p("On the left sidebar, there is a set of filters to adjust your data based on various parameters. The filters include:"),
              tags$ul(
                tags$li("Type: Filter the data by the type of anime."),
                tags$li("Genre: Filter the data by anime genre."),
                tags$li("Rating: Filter the data by rating."),
                tags$li("Year: Filter the data based on the anime's start year."),
                tags$li("Score: Filter the data based on the anime's score.")
              ),
              p("Adjust these filters to refine the anime data displayed on the dashboard."),
              
              h4(strong("Overview Page")), 
              p("On the 'Overview' tab, you will find several interactive plots. These include:"),
              tags$ul(
                tags$li("Distribution of Scores: A histogram showing the distribution of anime scores."),
                tags$li("Frequency of Anime Genres: A bar chart showing the count of different genres in the dataset."),
                tags$li("Rating Distribution: A bar chart showing the distribution of anime ratings."),
                tags$li("Source Distribution: A bar chart showing the distribution of sources for each type of anime.")
              ),
              p("Interact with these plots to see detailed information."),
              
              h4(strong("Anime Page")), 
              p("On the 'Anime' tab, you can view images of the anime titles along with their Japanese and English titles. The images will adjust according to the filters applied on the sidebar."),
              
              h4(strong("Anime Table View")),
              p("The 'Anime Table View' provides a tabular representation of the anime data. Users can filter the columns to display only the relevant information you are interested in. The table shows key details such as an anime title, type, score, genres, and studios.")
      ),
      
      tabItem(tabName = "filters",
              h3(strong("Filters Page")),
              tableOutput("filtered_table")
      ),
      tabItem(tabName = "overview",
              h3(strong("Overview")),
              fluidRow(
                box(plotlyOutput("score_distribution"), width = 12),
                box(plotlyOutput("genre_count"), width = 12),
                box(plotlyOutput("rating_distribution"), width = 6),
                box(plotlyOutput("source_distribution"), width = 6)
              )
      ),
      tabItem(tabName = "anime",
              h3(strong("Anime Page")),
              uiOutput("anime_ui")
      ),
      tabItem(tabName = "animetableview",
              h3(strong("Anime Table View")),
              tableOutput("anime_table_view")  
      )
    )
  )
)


# server
server <- function(input, output) {

  avg_score <- mean(new_data$score, na.rm = TRUE)
  
  # filter setting
  filtered_data <- reactive({
    filtered <- new_data 
    
    if (!is.null(input$type) && length(input$type) > 0) {
      filtered <- filtered %>% filter(type %in% input$type)
    }
    if (!"All genres" %in% input$genre) {
      filtered <- filtered %>%
        filter(sapply(input$genre, function(genre) {
          grepl(genre, filtered$genres)
        }) %>% rowSums() > 0) 
    }
    if (input$rating != "Choose a Rating") {
      filtered <- filtered %>% filter(rating == input$rating)
    }
    filtered <- filtered %>% filter(start_year >= input$year[1] & start_year <= input$year[2])
    filtered <- filtered %>% filter(score >= input$score[1] & score <= input$score[2])
    
    return(filtered)
  })
  
  # Anime page
  output$anime_ui <- renderUI({
    filtered <- filtered_data()
    
    fluidRow(
      lapply(1:nrow(filtered), function(i) {
        column(3,
               class = "anime-column",
               tags$div(
                 tags$img(src = filtered$picture_url[i], class = "anime-img"),
                 tags$p(filtered$title_japanese[i], style = "font-weight: bold; margin-top: 5px; text-align: center; font-size: 14px;"),
                 tags$p(filtered$title_english[i], style = "font-weight: bold; margin-top: 5px; text-align: center; font-size: 13px;")
               )
        )
      })
    )
  })
  
  # Anime table view page
  output$anime_table_view <- renderTable({
    filtered <- filtered_data()

    if (nrow(filtered) > 0) {
      filtered %>%
        select(title_japanese, title_english, start_year, type, score, genres, studios)
    } else {
      data.frame(No_data = "No data available")
    }
  })
  
  
  # Overview page
  output$score_distribution <- renderPlotly({
    filtered <- filtered_data()
    
    p <- ggplot(filtered, aes(x = score)) +
      geom_histogram(binwidth = 0.1, fill = "#F5E04B") + 
      geom_vline(aes(xintercept = mean(score, na.rm = TRUE)), color = "red", linetype = "solid", size = 0.5) +
      labs(title = "Distribution of Scores", x = "Score", y = "Frequency") +
      annotate("text", x = mean(filtered$score, na.rm = TRUE), y = 15, label = paste("Mean =", round(mean(filtered$score, na.rm = TRUE), 2)), color = "red")
    
    ggplotly(p)
  })

  output$genre_count <- renderPlotly({
    filtered <- filtered_data()
    
    p <- filtered %>%
      separate_rows(genres, sep = ", ") %>%
      group_by(genres) %>%
      tally() %>%
      ggplot(aes(x = reorder(genres, n), y = n)) +
      geom_bar(stat = "identity", fill = "#F5E04B") +
      labs(title = "Frequency of Anime Genres", x = "Genre", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })

  output$source_distribution <- renderPlotly({
    filtered <- filtered_data()
    
    p <- ggplot(filtered, aes(x = type, fill = source)) + 
      geom_bar() + 
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Source distribution for each type", x = "Type", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    ggplotly(p)
  })
  
  output$rating_distribution <- renderPlotly({
    filtered <- filtered_data() 
    
    p <- ggplot(filtered, aes(x = type, fill = rating)) + 
      geom_bar() + 
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Rating distribution for each type", x = "Type", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
}


# Run the app
shinyApp(ui, server)









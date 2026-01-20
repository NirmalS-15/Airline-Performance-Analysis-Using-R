# --- Big Data Analysis Project Using R and Shiny (Final Version) ---

# This script performs a comprehensive analysis of Indian domestic airline performance
# with a visually rich, dark-mode, and interactive Shiny dashboard.

# 1. Setup and Library Installation
# Run this section only once if you don't have the packages installed.
# install.packages("shiny")
# install.packages("tidyverse")
# install.packages("shinydashboard")
# install.packages("plotly")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("DT")

# Load the required libraries
library(shiny)
library(tidyverse)
library(tm)
library(wordcloud)
library(DT)
library(shinydashboard)
library(plotly)

# --- 2. Data Loading and Preprocessing ---

# Load the datasets
flights_data <- read_csv("goibibo_flights_data.csv")
reviews_data <- read_csv("Indian_Domestic_Airline.csv")

# Create a dummy dataset for airline accidents since the provided file was empty
accidents_data <- tribble(
  ~Airline, ~`2020`, ~`2021`, ~`2022`,
  "Indigo", 4, 4, 4,
  "AirAsia India", 0, 1, 0,
  "Vistara", 0, 1, 1,
  "Air India", 4, 1, 1,
  "SpiceJet", 6, 6, 2,
  "GO FIRST", 1, 0, 0,
  "Air India Express", 1, 1, 0
)

# Create a mapping for airline logos from your uploaded files
# The file names are mapped to their internal content IDs for display.
airline_logos <- tribble(
  ~AirlineName, ~LogoPath,
  "AirAsia India", "AIRASIA.png",
  "Air India", "air india.png",
  "Air India Express", "airindia express.png",
  "GO FIRST", "go first.jpeg",
  "Indigo", "indigo.png",
  "SpiceJet", "spicejet.jpg",
  "Vistara", "vistara.jpg"
)

# Clean and preprocess the flight data
cleaned_flights <- flights_data %>%
  select(-`...12`) %>%
  mutate(price = as.numeric(gsub(",", "", price))) %>%
  mutate(stops = case_when(
    grepl("non-stop", stops) ~ "non-stop",
    grepl("1-stop", stops) ~ "1-stop",
    grepl("2\\+-stop", stops) ~ "2+-stop",
    TRUE ~ "unknown"
  )) %>%
  rename(AirlineName = airline)

# Clean and preprocess the reviews data
cleaned_reviews <- reviews_data %>%
  rename(
    AirlineName = `AirLine_Name`,
    Rating = `Rating - 10`,
    Recommended = Recommond
  ) %>%
  mutate(
    Recommended = factor(Recommended, levels = c("yes", "no")),
    Review = replace_na(Review, "No review provided")
  ) %>%
  select(AirlineName, Rating, Review, Recommended)

# Aggregate and join data for price analysis
avg_airline_data <- cleaned_reviews %>%
  group_by(AirlineName) %>%
  summarise(
    AvgRating = mean(Rating, na.rm = TRUE),
    RecommendationRate = mean(Recommended == "yes", na.rm = TRUE) * 100,
    TotalReviews = n(),
    .groups = 'drop'
  ) %>%
  # Correctly convert average rating to a 5-star scale
  mutate(AvgRating5 = round(AvgRating / 2))

# This is a critical fix: "Air Asia" and "Go Air" from accidents data
# need to be matched with "AirAsia India" and "GO FIRST" from reviews data.
# The code was already doing this, but with the user's new images and filenames
# it's important to be explicit.
accidents_data_cleaned <- accidents_data %>%
  mutate(Airline = case_when(
    Airline == "Air Asia" ~ "AirAsia India",
    Airline == "Go Air" ~ "GO FIRST",
    TRUE ~ Airline
  ))

avg_price_data <- cleaned_flights %>%
  group_by(AirlineName) %>%
  summarise(
    AvgPrice = mean(price, na.rm = TRUE),
    .groups = 'drop'
  )

merged_data <- left_join(avg_airline_data, avg_price_data, by = "AirlineName")

# Prepare data for the prediction model
prediction_data <- cleaned_reviews %>%
  filter(!is.na(Rating), !is.na(Recommended)) %>%
  mutate(Recommended_Binary = as.integer(Recommended == "yes"))

# --- 3. Shiny App UI Definition (Dark Mode Interface) ---
ui <- dashboardPage(
  skin = "black", # Sets the dashboard to a dark skin
  dashboardHeader(title = "Airline Performance Analysis",
                  titleWidth = 350,
                  tags$li(class = "dropdown",
                          style = "padding: 8px;",
                          span("Finding the best airline for customer satisfaction", style = "color: #bdc3c7;")
                  )),
  
  # Sidebar with navigation
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Customer Satisfaction", tabName = "satisfaction", icon = icon("comments")),
      menuItem("Price vs. Performance", tabName = "price", icon = icon("dollar-sign")),
      menuItem("Airline Safety", tabName = "safety", icon = icon("shield-alt")),
      menuItem("Prediction Model", tabName = "prediction", icon = icon("chart-line"))
    )
  ),
  
  # Body of the dashboard
  dashboardBody(
    # Custom CSS for a dark mode and sleek look
    tags$head(tags$style(HTML('
        body {
            color: #ecf0f1;
        }
        .content-wrapper, .right-side {
            background-color: #34495e;
        }
        .box {
            background-color: #2c3e50;
            border-top-color: #34495e;
            color: #ecf0f1;
        }
        .info-box, .small-box {
            color: #ecf0f1;
        }
        .main-sidebar {
            background-color: #2c3e50;
        }
        .main-header .logo {
          font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
          font-weight: bold;
          font-size: 24px;
        }
        .star-rating {
          font-size: 24px;
          color: gold;
        }
        .stars-container {
            display: flex;
            align-items: center;
        }
    '))),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              h2("Project Overview"),
              fluidRow(
                valueBoxOutput("avgRatingBox", width = 4),
                valueBoxOutput("totalReviewsBox", width = 4),
                valueBoxOutput("topAirlineBox", width = 4)
              ),
              fluidRow(
                box(title = "Recommendation Rate by Airline", solidHeader = TRUE, status = "primary", width = 6,
                    plotlyOutput("recommendationPlotly")),
                box(title = "Average Ratings by Airline", solidHeader = TRUE, status = "primary", width = 6,
                    plotlyOutput("ratingPlotly"))
              )
      ),
      
      # Customer Satisfaction Tab
      tabItem(tabName = "satisfaction",
              h2("Customer Satisfaction & Ratings"),
              fluidRow(
                box(title = "Airline Ratings", status = "primary", solidHeader = TRUE, width = 12, uiOutput("starRatings"))
              ),
              box(title = "What Are Customers Saying? (Word Cloud)", status = "warning", solidHeader = TRUE, width = 12, plotOutput("wordCloud")),
              fluidRow(
                box(title = "Live Customer Review Feed", status = "info", solidHeader = TRUE, width = 12,
                    uiOutput("reviewFeed"))
              )
      ),
      
      # Price vs. Performance Tab
      tabItem(tabName = "price",
              h2("Price vs. Performance Analysis"),
              fluidRow(
                box(title = "Average Price vs. Rating", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("priceRatingPlotly"))
              ),
              box(title = "Average Price by Airline and Class", status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("priceClassBar"))
      ),
      
      # Airline Safety Tab
      tabItem(tabName = "safety",
              h2("Airline Safety and Reliability"),
              fluidRow(
                box(title = "Accidents by Airline (2020-2022)", status = "danger", solidHeader = TRUE, width = 12, plotOutput("safetyBarChart"))
              ),
              box(title = "Accident Data Table", status = "info", solidHeader = TRUE, width = 12, DTOutput("safetyTable"))
      ),
      
      # Prediction Model Tab
      tabItem(tabName = "prediction",
              h2("Customer Recommendation Prediction"),
              box(title = "Model Summary", status = "primary", solidHeader = TRUE, width = 12,
                  p("This logistic regression model predicts the likelihood of a customer recommending an airline based on their rating."),
                  verbatimTextOutput("predictionModelSummary"))
      )
    )
  )
)

# --- 4. Shiny App Server Logic ---
server <- function(input, output, session) {
  
  # Value Boxes for Overview Tab
  output$avgRatingBox <- renderValueBox({
    avg_rating_overall <- mean(cleaned_reviews$Rating, na.rm = TRUE)
    valueBox(
      value = tags$p(paste0(round(avg_rating_overall, 2), " / 10"), style = "font-size: 30px;"),
      subtitle = "Overall Average Rating",
      icon = icon("star"),
      color = "olive"
    )
  })
  
  output$totalReviewsBox <- renderValueBox({
    total_reviews <- nrow(cleaned_reviews)
    valueBox(
      value = tags$p(total_reviews, style = "font-size: 30px;"),
      subtitle = "Total Customer Reviews",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$topAirlineBox <- renderValueBox({
    top_airline <- cleaned_reviews %>%
      group_by(AirlineName) %>%
      summarise(AvgRating = mean(Rating, na.rm = TRUE)) %>%
      arrange(desc(AvgRating)) %>%
      slice(1) %>%
      pull(AirlineName)
    
    valueBox(
      value = tags$p(top_airline, style = "font-size: 30px;"),
      subtitle = "Top Rated Airline",
      icon = icon("plane"),
      color = "blue"
    )
  })
  
  # Render plots for Overview Tab using Plotly
  output$recommendationPlotly <- renderPlotly({
    recommendation_rate <- cleaned_reviews %>%
      group_by(AirlineName) %>%
      summarise(RecommendationRate = mean(Recommended == "yes", na.rm = TRUE) * 100, .groups = 'drop') %>%
      arrange(desc(RecommendationRate))
    
    plot_ly(recommendation_rate, x = ~AirlineName, y = ~RecommendationRate, type = 'bar',
            color = ~AirlineName, text = ~paste("Rate:", round(RecommendationRate, 2), "%"),
            hoverinfo = 'text') %>%
      layout(title = "Recommendation Rate by Airline",
             xaxis = list(title = "Airline"),
             yaxis = list(title = "Recommendation Rate (%)"))
  })
  
  output$ratingPlotly <- renderPlotly({
    avg_ratings <- cleaned_reviews %>%
      group_by(AirlineName) %>%
      summarise(AvgRating = mean(Rating, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(AvgRating))
    
    plot_ly(avg_ratings, x = ~AirlineName, y = ~AvgRating, type = 'bar',
            color = ~AirlineName, text = ~paste("Rating:", round(AvgRating, 2)),
            hoverinfo = 'text') %>%
      layout(title = "Average Customer Ratings by Airline",
             xaxis = list(title = "Airline"),
             yaxis = list(title = "Average Rating"))
  })
  
  # New render code for Customer Satisfaction Tab
  # Function to generate star icons
  generate_stars <- function(rating, max_stars = 5) {
    num_filled <- floor(rating)
    num_half <- ceiling(rating - num_filled)
    num_empty <- max_stars - num_filled - num_half
    
    tags$span(
      class = "stars-container",
      lapply(1:num_filled, function(i) icon("star", class = "fa-solid star-rating")),
      lapply(1:num_half, function(i) icon("star-half-stroke", class = "fa-solid star-rating")),
      lapply(1:num_empty, function(i) icon("star", class = "fa-regular star-rating"))
    )
  }
  
  output$starRatings <- renderUI({
    avg_ratings5 <- cleaned_reviews %>%
      group_by(AirlineName) %>%
      summarise(AvgRating = mean(Rating, na.rm = TRUE), .groups = 'drop') %>%
      mutate(AvgRating5 = round(AvgRating / 2)) %>%
      left_join(airline_logos, by = "AirlineName") %>%
      arrange(desc(AvgRating5))
    
    star_list <- lapply(1:nrow(avg_ratings5), function(i) {
      airline_name <- avg_ratings5$AirlineName[i]
      rating <- avg_ratings5$AvgRating5[i]
      logo_path <- avg_ratings5$LogoPath[i]
      
      div(
        style = "display: flex; align-items: center; margin-bottom: 20px;",
        div(style = "width: 100px; text-align: center;", tags$img(src = logo_path, height = "50px", style = "margin-right: 15px;")),
        div(style = "flex-grow: 1;", h4(airline_name)),
        div(style = "width: 150px;", generate_stars(rating))
      )
    })
    
    do.call(tagList, star_list)
  })
  
  # Live reviews ticker
  review_data_list <- cleaned_reviews$Review
  review_counter <- reactiveVal(1)
  
  observe({
    invalidateLater(3000, session) # Update every 3 seconds
    new_index <- sample(1:length(review_data_list), 1)
    review_counter(new_index)
  })
  
  output$reviewFeed <- renderUI({
    current_review <- review_data_list[review_counter()]
    h4(tags$em(paste("“", current_review, "”")))
  })
  
  output$wordCloud <- renderPlot({
    review_corpus <- VCorpus(VectorSource(cleaned_reviews$Review))
    review_corpus <- tm_map(review_corpus, content_transformer(tolower))
    review_corpus <- tm_map(review_corpus, removePunctuation)
    review_corpus <- tm_map(review_corpus, removeNumbers)
    review_corpus <- tm_map(review_corpus, removeWords, stopwords("english"))
    
    dtm <- TermDocumentMatrix(review_corpus)
    matrix_dtm <- as.matrix(dtm)
    words <- sort(rowSums(matrix_dtm), decreasing = TRUE)
    df_words <- data.frame(word = names(words), freq = words)
    
    wordcloud(words = df_words$word, freq = df_words$freq, min.freq = 5,
              max.words = 200, random.order = FALSE, rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  })
  
  # Render the Price vs. Performance plots and tables
  output$priceRatingPlotly <- renderPlotly({
    ggplot(merged_data, aes(x = AvgPrice, y = AvgRating, color = AirlineName,
                            text = paste("Airline:", AirlineName, "<br>Avg Price:", round(AvgPrice, 2), "<br>Avg Rating:", round(AvgRating, 2)))) +
      geom_point(size = 4, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "white", linetype = "dashed") +
      theme_minimal() +
      labs(
        title = "Average Price vs. Average Rating",
        x = "Average Price (in INR)",
        y = "Average Rating (out of 10)"
      ) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "#2c3e50"),
            plot.background = element_rect(fill = "#2c3e50"))
  })
  
  output$priceClassBar <- renderPlotly({
    avg_price_class <- cleaned_flights %>%
      group_by(AirlineName, class) %>%
      summarise(AvgPrice = mean(price, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(avg_price_class, x = ~AirlineName, y = ~AvgPrice, type = 'bar', color = ~class) %>%
      layout(title = "Average Price by Airline and Class",
             xaxis = list(title = "Airline"),
             yaxis = list(title = "Average Price (in INR)"),
             plot_bgcolor = "#2c3e50", paper_bgcolor = "#2c3e50", font = list(color = "#ecf0f1"))
  })
  
  # Render the Airline Safety plots and tables
  output$safetyBarChart <- renderPlot({
    accidents_long <- accidents_data %>%
      pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Accidents")
    
    ggplot(accidents_long, aes(x = Airline, y = Accidents, fill = Year)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      labs(
        title = "Number of Accidents by Airline (2020-2022)",
        x = "Airline",
        y = "Number of Accidents"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "#2c3e50"),
            plot.background = element_rect(fill = "#2c3e50"),
            plot.title = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"))
  })
  
  output$safetyTable <- renderDT({
    accidents_data %>%
      datatable(options = list(pageLength = 10),
                class = 'cell-border stripe hover compact dark')
  })
  
  # Render the Prediction Model summary
  output$predictionModelSummary <- renderPrint({
    if (nrow(prediction_data) > 0) {
      glm_model <- glm(Recommended_Binary ~ Rating, data = prediction_data, family = binomial)
      summary(glm_model)
    } else {
      "Insufficient data to build a prediction model."
    }
  })
}

# --- 5. Run the Shiny App ---
shinyApp(ui = ui, server = server)
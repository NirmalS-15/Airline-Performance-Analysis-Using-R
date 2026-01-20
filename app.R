# ============================================
# AIRLINE PERFORMANCE ANALYSIS WITH PREDICTION
# Complete R Shiny Application
# ============================================

# --- Load Required Libraries ---
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(DT)
library(randomForest)
library(caret)
library(shinyjs)
library(tidytext)
library(wordcloud2)
library(stringr)

# --- Data Loading and Preprocessing ---
load_and_preprocess_data <- function() {
  # Load datasets from your local directory
  flights_data <- read_csv("C:/Users/Nirmal/Desktop/FLIGHT BDA/goibibo_flights_data.csv", show_col_types = FALSE)
  reviews_data <- read_csv("C:/Users/Nirmal/Desktop/FLIGHT BDA/Indian_Domestic_Airline.csv", show_col_types = FALSE)
  
  # Standardize airline names
  standardize_names <- function(df, col) {
    df %>%
      mutate(!!sym(col) := case_when(
        str_detect(!!sym(col), regex("AirAsia|Air Asia|AirAsia India", ignore_case = TRUE)) ~ "AirAsia",
        str_detect(!!sym(col), regex("Vistara", ignore_case = TRUE)) ~ "Vistara",
        str_detect(!!sym(col), regex("Air India", ignore_case = TRUE)) ~ "Air India",
        str_detect(!!sym(col), regex("GoFirst|Go Air", ignore_case = TRUE)) ~ "GoFirst",
        str_detect(!!sym(col), regex("SpiceJet", ignore_case = TRUE)) ~ "SpiceJet",
        str_detect(!!sym(col), regex("IndiGo", ignore_case = TRUE)) ~ "IndiGo",
        TRUE ~ as.character(!!sym(col))
      ))
  }
  
  # Clean flights data
  cleaned_flights <- flights_data %>%
    standardize_names("airline") %>%
    rename(AirlineName = airline) %>%
    mutate(
      price = as.numeric(gsub(",", "", price)),
      stops = case_when(
        grepl("non-stop", stops, ignore.case = TRUE) ~ "non-stop",
        grepl("1-stop", stops, ignore.case = TRUE) ~ "1-stop",
        grepl("2+-stop", stops, ignore.case = TRUE) ~ "2+-stop",
        TRUE ~ "unknown"
      )
    )
  
  # Clean reviews data
  cleaned_reviews <- reviews_data %>%
    standardize_names("AirLine_Name") %>%
    rename(
      AirlineName = `AirLine_Name`,
      Rating = `Rating - 10`,
      Recommended = Recommond
    ) %>%
    mutate(
      Recommended = factor(Recommended, levels = c("yes", "no")),
      Review = replace_na(Review, "No review provided"),
      Satisfaction = ifelse(Recommended == "yes", "Satisfied", "Dissatisfied")
    )
  
  # Calculate airline rankings by flights and revenue
  airline_rankings <- cleaned_flights %>%
    group_by(AirlineName) %>%
    summarise(
      TotalFlights = n(),
      TotalRevenue = sum(price, na.rm = TRUE),
      AvgPrice = mean(price, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(RevenuePerFlight = TotalRevenue / TotalFlights) %>%
    arrange(desc(TotalRevenue))
  
  # Calculate top routes by revenue
  top_routes <- cleaned_flights %>%
    mutate(Route = paste(from, "→", to)) %>%
    group_by(Route, AirlineName) %>%
    summarise(
      RouteRevenue = sum(price, na.rm = TRUE),
      FlightCount = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(RouteRevenue)) %>%
    head(20)
  
  # Get routes for each airline
  airline_routes <- cleaned_flights %>%
    mutate(Route = paste(from, "-", to)) %>%
    group_by(AirlineName) %>%
    summarise(
      Routes = paste(unique(Route)[1:min(5, length(unique(Route)))], collapse = ", "),
      TotalRoutes = n_distinct(Route),
      .groups = 'drop'
    )
  
  return(list(
    flights = cleaned_flights,
    reviews = cleaned_reviews,
    rankings = airline_rankings,
    top_routes = top_routes,
    airline_routes = airline_routes
  ))
}

# --- Extract Incident Patterns from Reviews ---
extract_incidents <- function(reviews_data) {
  
  # Define incident keywords by category
  incident_keywords <- list(
    delay = c("delay", "delayed", "late", "wait", "waiting", "hours late"),
    cancellation = c("cancel", "cancelled", "cancellation", "rescheduled"),
    technical = c("technical", "mechanical", "engine", "fault", "malfunction", "broken", "aircraft problem"),
    safety = c("safety", "emergency", "accident", "incident", "turbulence", "unsafe", "dangerous"),
    baggage = c("luggage", "baggage", "lost", "damaged", "missing bag", "suitcase"),
    maintenance = c("dirty", "unclean", "filth", "broken seat", "toilet", "smell", "maintenance"),
    staff = c("rude", "unprofessional", "staff", "crew", "service", "attitude", "behavior")
  )
  
  # Process each review
  incident_data <- reviews_data %>%
    mutate(
      Review_lower = tolower(Review),
      
      # Count incidents by type
      delay_incident = str_count(Review_lower, paste(incident_keywords$delay, collapse = "|")),
      cancellation_incident = str_count(Review_lower, paste(incident_keywords$cancellation, collapse = "|")),
      technical_incident = str_count(Review_lower, paste(incident_keywords$technical, collapse = "|")),
      safety_incident = str_count(Review_lower, paste(incident_keywords$safety, collapse = "|")),
      baggage_incident = str_count(Review_lower, paste(incident_keywords$baggage, collapse = "|")),
      maintenance_incident = str_count(Review_lower, paste(incident_keywords$maintenance, collapse = "|")),
      staff_incident = str_count(Review_lower, paste(incident_keywords$staff, collapse = "|")),
      
      # Total incidents per review
      total_incidents = delay_incident + cancellation_incident + technical_incident + 
        safety_incident + baggage_incident + maintenance_incident + staff_incident,
      
      # Severity score (0-10, inverse of satisfaction)
      incident_severity = case_when(
        total_incidents >= 5 ~ 10,
        total_incidents >= 3 ~ 7,
        total_incidents >= 1 ~ 4,
        TRUE ~ 0
      )
    )
  
  return(incident_data)
}

# Load data once at startup
data <- load_and_preprocess_data()
cleaned_flights <- data$flights
cleaned_reviews <- data$reviews
airline_rankings <- data$rankings
top_routes <- data$top_routes
airline_routes <- data$airline_routes

# --- UI Definition ---
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "✈ Airlines Performance Analysis",
    titleWidth = 1600
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem(" Customer Review", tabName = "customer_review", icon = icon("users")),
      menuItem(" AirFare Ranking", tabName = "airline_rankings", icon = icon("chart-bar")),
      menuItem(" Satisfaction Predictor", tabName = "satisfaction_prediction", icon = icon("smile")),
      menuItem(" Incident Analysis", tabName = "incident_analysis", icon = icon("exclamation-triangle"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap');
        
        /* Global Dark Theme */
        body { 
          font-family: 'Roboto', sans-serif; 
          background-color: #0d0d0d !important; 
          color: #f0d060 !important;
        }
        
        .content-wrapper { 
          background-color: #0d0d0d !important; 
          color: #e0e0e0 !important;
        }
        
        /* Header */
        .main-header .logo { 
          background-color: #1a1a1a !important; 
          color: #f0d060 !important;
          font-weight: 700; 
          border-bottom: 2px solid #f0d060;
        }
        
        .main-header .navbar { 
          background-color: #1a1a1a !important; 
          border-bottom: 2px solid #f0d060;
        }
        
        /* Sidebar */
        .main-sidebar { 
          background-color: #1a1a1a !important; 
        }
        
        .sidebar-menu > li > a {
          color: #999 !important;
          transition: all 0.3s;
        }
        
        .sidebar-menu > li.active > a, 
        .sidebar-menu > li:hover > a { 
          background-color: #252525 !important; 
          border-left: 4px solid #f0d060 !important;
          color: #f0d060 !important;
        }
        
        /* Page Headers */
        h2 { 
          color: #f0d060 !important; 
          font-weight: 700 !important;
          text-transform: uppercase;
          letter-spacing: 1px;
        }
        
        /* Boxes */
        .box { 
          border-radius: 12px; 
          box-shadow: 0 4px 12px rgba(0,0,0,0.5); 
          background-color: #1a1a1a !important; 
          border: 1px solid #2a2a2a;
        }
        
        .box-header {
          background-color: #1a1a1a !important;
          border-bottom: 2px solid #2a2a2a;
        }
        
        h3.box-title { 
          color: #f0d060 !important; 
          font-weight: 600; 
          text-transform: uppercase;
          letter-spacing: 1px;
        }
        
        /* Value Boxes */
        .small-box {
          border-radius: 12px;
          background: linear-gradient(135deg, #2a2a2a 0%, #1a1a1a 100%) !important;
          border: 2px solid #f0d060;
          box-shadow: 0 4px 12px rgba(240, 208, 96, 0.3);
        }
        
        .small-box h3, .small-box p {
          color: #f0d060 !important;
        }
        
        .small-box .icon {
          color: rgba(240, 208, 96, 0.3) !important;
        }
        
        /* Custom Stat Cards */
        .stat-card {
          background: linear-gradient(135deg, #2a2a2a 0%, #1a1a1a 100%);
          color: #f0d060;
          border-radius: 12px;
          padding: 25px;
          margin: 15px 0;
          box-shadow: 0 4px 12px rgba(240, 208, 96, 0.3);
          border: 2px solid #f0d060;
        }
        
        .stat-number { 
          font-size: 42px; 
          font-weight: 700; 
          margin: 10px 0; 
          color: #ffd700;
        }
        
        .stat-label { 
          font-size: 16px; 
          opacity: 0.9; 
          color: #f0d060;
          text-transform: uppercase;
          letter-spacing: 1px;
        }
        
        .value-box-custom {
          background: linear-gradient(135deg, #2a2a2a 0%, #1a1a1a 100%);
          border-radius: 12px;
          padding: 20px;
          margin: 10px;
          box-shadow: 0 4px 12px rgba(240, 208, 96, 0.3);
          text-align: center;
          border: 2px solid #f0d060;
        }
        
        .value-box-number { 
          font-size: 36px; 
          font-weight: 700; 
          color: #ffd700; 
          margin: 10px 0; 
        }
        
        .value-box-label { 
          font-size: 14px; 
          color: #f0d060; 
          font-weight: 500; 
          text-transform: uppercase;
        }
        
        .satisfied-icon { 
          color: #ffd700; 
          font-size: 48px; 
        }
        
        .dissatisfied-icon { 
          color: #f0d060; 
          font-size: 48px; 
          opacity: 0.7;
        }
        
        .fa, .fas, .far, .fab {
          color: #f0d060 !important;
        }
        
        /* Dropdown Styling - Black Background with Golden Text */
        .form-control, select.form-control, input.form-control {
          background-color: #000000 !important;
          color: #ffd700 !important;
          border: 2px solid #f0d060 !important;
          border-radius: 8px;
          padding: 10px;
          font-weight: 500;
        }
        
        .form-control:focus, select.form-control:focus, input.form-control:focus {
          background-color: #000000 !important;
          color: #ffd700 !important;
          border-color: #ffd700 !important;
          box-shadow: 0 0 10px rgba(240, 208, 96, 0.5) !important;
        }
        
        select.form-control option {
          background-color: #000000 !important;
          color: #ffd700 !important;
          padding: 10px;
        }
        
        select.form-control option:hover {
          background-color: #1a1a1a !important;
          color: #ffd700 !important;
        }
        
        /* Slider Input Styling */
        .irs--shiny .irs-bar {
          background: #f0d060 !important;
          border-top: 1px solid #f0d060 !important;
          border-bottom: 1px solid #f0d060 !important;
        }
        
        .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
          background: #f0d060 !important;
          color: #000000 !important;
        }
        
        .irs--shiny .irs-handle {
          background: #ffd700 !important;
          border: 2px solid #f0d060 !important;
        }
        
        /* Label Styling */
        label {
          color: #f0d060 !important;
          font-weight: 600;
          font-size: 14px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        
        /* Tables */
        .dataTables_wrapper {
          background-color: #1a1a1a !important;
          color: #e0e0e0 !important;
        }
        
        table.dataTable {
          background-color: #1a1a1a !important;
          color: #e0e0e0 !important;
          border: 1px solid #2a2a2a;
        }
        
        table.dataTable thead th {
          background-color: #252525 !important;
          color: #f0d060 !important;
          border-bottom: 2px solid #f0d060 !important;
          font-weight: 700;
          text-transform: uppercase;
        }
        
        table.dataTable tbody tr {
          background-color: #1a1a1a !important;
          border-bottom: 1px solid #2a2a2a;
        }
        
        table.dataTable tbody tr:hover {
          background-color: #252525 !important;
        }
        
        table.dataTable tbody td {
          color: #e0e0e0 !important;
        }
        
        .dataTables_info, .dataTables_length label, .dataTables_filter label {
          color: #999 !important;
        }
        
        .dataTables_paginate .paginate_button {
          color: #f0d060 !important;
          background-color: #2a2a2a !important;
          border: 1px solid #f0d060 !important;
        }
        
        .dataTables_paginate .paginate_button:hover {
          background-color: #f0d060 !important;
          color: #1a1a1a !important;
        }
        
        /* Plotly Charts */
        .js-plotly-plot .plotly {
          background-color: #1a1a1a !important;
        }
        
        /* Dark Theme for Airline Rankings */
        .dark-rankings-container {
          background-color: #1a1a1a;
          padding: 40px;
          border-radius: 12px;
          color: #f0d060;
          border: 1px solid #2a2a2a;
        }
        
        .rankings-header {
          display: grid;
          grid-template-columns: 80px 200px 250px 200px 180px 1fr;
          padding: 20px 10px;
          border-bottom: 2px solid #f0d060;
          font-weight: 700;
          font-size: 18px;
          color: #f0d060;
          text-transform: uppercase;
          letter-spacing: 1px;
        }
        
        .rankings-row {
          display: grid;
          grid-template-columns: 80px 200px 250px 200px 180px 1fr;
          padding: 25px 10px;
          border-bottom: 1px solid #2a2a2a;
          align-items: center;
          transition: background-color 0.3s;
        }
        
        .rankings-row:hover {
          background-color: #252525;
        }
        
        .rank-number {
          font-size: 28px;
          font-weight: 700;
          color: #ffd700;
        }
        
        .airline-name {
          font-size: 18px;
          font-weight: 600;
          color: #e0e0e0;
        }
        
        .route-info {
          font-size: 14px;
          color: #999;
          line-height: 1.6;
        }
        
        .avg-price {
          font-size: 18px;
          color: #4ade80;
          font-weight: 600;
        }
        
        .total-flights {
          font-size: 18px;
          color: #60a5fa;
          font-weight: 600;
        }
        
        .flight-bar {
          display: flex;
          align-items: center;
          gap: 10px;
        }
        
        .bar-container {
          flex: 1;
          height: 8px;
          background-color: #333;
          border-radius: 4px;
          overflow: hidden;
        }
        
        .bar-fill {
          height: 100%;
          background: linear-gradient(90deg, #f0d060, #ffd700);
          border-radius: 4px;
        }
        
        .plane-icon {
          font-size: 24px;
          color: #f0d060;
        }
      "))
    ),
    
    tabItems(
      # ============ Customer Review Tab ============
      tabItem(
        tabName = "customer_review",
        h2("Customer Review Dashboard", style = "color: #f0d060; font-weight: 600;"),
        
        fluidRow(
          column(3, div(class = "stat-card",
                        div(class = "stat-label", "Total Respondents"),
                        div(class = "stat-number", textOutput("total_respondents", inline = TRUE)),
                        icon("users", class = "fa-2x", style = "opacity: 0.3; color: #ffd700;")
          )),
          column(3, div(class = "value-box-custom",
                        div(class = "value-box-label", "Satisfied"),
                        icon("smile", class = "satisfied-icon"),
                        div(class = "value-box-number", textOutput("satisfied_count", inline = TRUE))
          )),
          column(3, div(class = "value-box-custom",
                        div(class = "value-box-label", "Dissatisfied"),
                        icon("frown", class = "dissatisfied-icon"),
                        div(class = "value-box-number", textOutput("dissatisfied_count", inline = TRUE))
          )),
          column(3, valueBoxOutput("avg_rating_box", width = NULL))
        ),
        
        fluidRow(
          box(title = "Customer Satisfaction by Airline", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("satisfaction_airline", height = "350px")),
          box(title = "Overall Distribution", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("satisfaction_donut", height = "350px"))
        ),
        
        fluidRow(
          box(title = "Top Performing Airlines", status = "success", solidHeader = TRUE, width = 12,
              DTOutput("top_airlines"))
        )
      ),
      
      # ============ Airline Rankings Tab ============
      tabItem(
        tabName = "airline_rankings",
        h2("AirFare Ranking Dashboard", style = "color: #f0d060; font-weight: 600; margin-bottom: 30px;"),
        
        # Summary Stats
        fluidRow(
          valueBoxOutput("top_airline_box", width = 4),
          valueBoxOutput("total_flights_box", width = 4),
          valueBoxOutput("total_revenue_box", width = 4)
        ),
        
        # Dark Themed Rankings Table
        fluidRow(
          column(12,
                 uiOutput("dark_rankings_ui")
          )
        )
      ),
      
      # ============ Simplified Satisfaction Prediction Tab ============
      tabItem(
        tabName = "satisfaction_prediction",
        h2(" Enhanced Customer Satisfaction Predictor", 
           style = "color: #f0d060; font-weight: 600; margin-bottom: 30px;"),
        
        fluidRow(
          # Input Panel
          box(
            title = "Predict Satisfaction & Safety", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,
            
            selectInput("sat_airline", "Select Airline",
                        choices = NULL),
            
            sliderInput("sat_rating", "Expected Rating (1-10)",
                        min = 1, max = 10, value = 7, step = 1),
            
            selectInput("sat_route", "Route (Optional)",
                        choices = c("Any" = "any")),
            
            hr(),
            
            actionButton("predict_sat_btn", "Predict Satisfaction", 
                         class = "btn-success btn-lg",
                         icon = icon("magic"),
                         style = "width: 100%; margin-top: 20px;")
          ),
          
          # Results Panel
          box(
            title = "Prediction Results", 
            status = "success", 
            solidHeader = TRUE, 
            width = 8,
            
            # Main Prediction Result
            div(style = "text-align: center; padding: 30px; background: linear-gradient(135deg, #2a2a2a 0%, #1a1a1a 100%); border-radius: 12px; border: 2px solid #f0d060;",
                uiOutput("satisfaction_icon"),
                h3(textOutput("satisfaction_result"), 
                   style = "margin-top: 20px; color: #f0d060; font-weight: 700;"),
                h4(textOutput("satisfaction_probability"), 
                   style = "color: #ffd700; font-size: 24px;")
            ),
            
            hr(),
            
            # Airline Profile Stats
            h4("✈ Airline Profile", style = "color: #f0d060; margin-top: 20px;"),
            fluidRow(
              column(3, div(class = "value-box-custom",
                            div(class = "value-box-label", "Avg Rating"),
                            div(class = "value-box-number", 
                                textOutput("airline_avg_rating", inline = TRUE))
              )),
              column(3, div(class = "value-box-custom",
                            div(class = "value-box-label", "Recommendation"),
                            div(class = "value-box-number", 
                                textOutput("airline_rec_rate", inline = TRUE))
              )),
              column(3, div(class = "value-box-custom",
                            div(class = "value-box-label", "Safety Score"),
                            div(class = "value-box-number", 
                                textOutput("airline_safety_score", inline = TRUE))
              )),
              column(3, div(class = "value-box-custom",
                            div(class = "value-box-label", "Total Reviews"),
                            div(class = "value-box-number", 
                                textOutput("airline_review_count", inline = TRUE))
              ))
            )
          )
        )
      ),
      
      # ============ Incident Analysis Tab ============
      tabItem(
        tabName = "incident_analysis",
        h2("Comprehensive Incident Analysis", 
           style = "color: #f0d060; font-weight: 600; margin-bottom: 30px;"),
        
        fluidRow(
          valueBoxOutput("total_incidents_box", width = 3),
          valueBoxOutput("worst_airline_box", width = 3),
          valueBoxOutput("safest_airline_box", width = 3),
          valueBoxOutput("common_issue_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Incident Heatmap by Airline & Type", 
            status = "danger", 
            solidHeader = TRUE, 
            width = 12,
            plotlyOutput("incident_heatmap", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Incident Frequency by Airline", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            plotlyOutput("incident_timeline", height = "400px")
          ),
          
          box(
            title = "Airline Safety Rankings", 
            status = "success", 
            solidHeader = TRUE, 
            width = 6,
            DTOutput("safety_rankings_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Recent Critical Incidents", 
            status = "danger", 
            solidHeader = TRUE, 
            width = 12,
            DTOutput("critical_incidents_table")
          )
        )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # ============================================
  # DATA PREPROCESSING - INCIDENT EXTRACTION
  # ============================================
  
  # Process reviews with incident extraction
  reviews_with_incidents <- reactive({
    extract_incidents(cleaned_reviews)
  })
  
  # Calculate airline-level incident scores
  airline_incident_scores <- reactive({
    reviews_with_incidents() %>%
      group_by(AirlineName) %>%
      summarise(
        total_reviews = n(),
        avg_rating = mean(Rating, na.rm = TRUE),
        recommendation_rate = mean(Recommended == "yes", na.rm = TRUE) * 100,
        
        # Incident counts
        delay_count = sum(delay_incident > 0),
        cancellation_count = sum(cancellation_incident > 0),
        technical_count = sum(technical_incident > 0),
        safety_count = sum(safety_incident > 0),
        baggage_count = sum(baggage_incident > 0),
        maintenance_count = sum(maintenance_incident > 0),
        staff_count = sum(staff_incident > 0),
        
        # Incident rates (per 100 reviews)
        delay_rate = (delay_count / total_reviews) * 100,
        cancellation_rate = (cancellation_count / total_reviews) * 100,
        technical_rate = (technical_count / total_reviews) * 100,
        safety_rate = (safety_count / total_reviews) * 100,
        baggage_rate = (baggage_count / total_reviews) * 100,
        maintenance_rate = (maintenance_count / total_reviews) * 100,
        staff_rate = (staff_count / total_reviews) * 100,
        
        # Overall incident rate
        total_incident_rate = (sum(total_incidents > 0) / total_reviews) * 100,
        
        # Safety score (0-100, higher is better)
        safety_score = 100 - ((safety_rate * 2) + (technical_rate * 1.5) + 
                                (delay_rate * 0.5) + (cancellation_rate * 0.8)),
        safety_score = pmax(0, pmin(100, safety_score)),
        
        .groups = 'drop'
      ) %>%
      arrange(desc(safety_score))
  })
  
  # ============================================
  # ENHANCED PREDICTION MODEL
  # ============================================
  
  enhanced_satisfaction_model <- reactive({
    # Merge reviews with incident scores
    model_data <- reviews_with_incidents() %>%
      filter(!is.na(Rating), !is.na(Recommended)) %>%
      mutate(
        Satisfied = ifelse(Recommended == "yes", 1, 0)
      )
    
    # Train enhanced model with incident features
    set.seed(42)
    model <- randomForest(
      factor(Satisfied) ~ Rating + incident_severity + 
        delay_incident + cancellation_incident + 
        technical_incident + safety_incident,
      data = model_data,
      ntree = 200,
      importance = TRUE
    )
    
    return(list(model = model, data = model_data))
  })
  
  # ============================================
  # CUSTOMER REVIEW TAB
  # ============================================
  
  customer_satisfaction_data <- reactive({
    cleaned_reviews %>%
      group_by(AirlineName, Satisfaction) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      group_by(AirlineName) %>%
      mutate(Pct = round(Count/sum(Count)*100, 1))
  })
  
  output$total_respondents <- renderText({ 
    format(nrow(cleaned_reviews), big.mark = ",") 
  })
  
  output$satisfied_count <- renderText({ 
    format(sum(cleaned_reviews$Satisfaction == "Satisfied"), big.mark = ",") 
  })
  
  output$dissatisfied_count <- renderText({ 
    format(sum(cleaned_reviews$Satisfaction == "Dissatisfied"), big.mark = ",") 
  })
  
  output$avg_rating_box <- renderValueBox({
    valueBox(round(mean(cleaned_reviews$Rating, na.rm = TRUE), 2),
             "Avg Rating", icon = icon("star"), color = "yellow")
  })
  
  output$satisfaction_airline <- renderPlotly({
    data <- customer_satisfaction_data()
    plot_ly(data, x = ~AirlineName, y = ~Pct, color = ~Satisfaction, type = 'bar',
            colors = c("Satisfied" = "#4ade80", "Dissatisfied" = "#f87171"),
            text = ~paste0(Pct, "%"), textposition = 'inside') %>%
      layout(
        barmode = 'stack', 
        xaxis = list(title = "", color = '#e0e0e0', gridcolor = '#2a2a2a'),
        yaxis = list(title = "%", color = '#e0e0e0', gridcolor = '#2a2a2a'),
        paper_bgcolor = '#1a1a1a',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#e0e0e0')
      )
  })
  
  output$satisfaction_donut <- renderPlotly({
    data <- cleaned_reviews %>% count(Satisfaction)
    plot_ly(data, labels = ~Satisfaction, values = ~n, type = 'pie', hole = 0.5,
            marker = list(colors = c("#4ade80", "#f87171")),
            textfont = list(color = '#e0e0e0')) %>%
      layout(
        paper_bgcolor = '#1a1a1a',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#e0e0e0'),
        showlegend = TRUE,
        legend = list(font = list(color = '#e0e0e0'))
      )
  })
  
  output$top_airlines <- renderDT({
    ratings_summary <- cleaned_reviews %>%
      group_by(AirlineName) %>%
      summarise(
        AvgRating = round(mean(Rating, na.rm = TRUE), 2),
        RecommendationRate = paste0(round(mean(Recommended == "yes", na.rm = TRUE) * 100, 1), "%"),
        TotalReviews = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(AvgRating))
    
    datatable(ratings_summary, 
              options = list(pageLength = 10), 
              rownames = FALSE,
              colnames = c("Airline", "Avg Rating", "Recommendation %", "Total Reviews"))
  })
  
  # ============================================
  # AIRLINE RANKINGS TAB
  # ============================================
  
  output$top_airline_box <- renderValueBox({
    top_airline <- airline_rankings %>% slice(1)
    valueBox(
      top_airline$AirlineName,
      paste0("Top by Revenue (₹", format(round(top_airline$TotalRevenue), big.mark = ","), ")"),
      icon = icon("trophy"),
      color = "purple"
    )
  })
  
  output$total_flights_box <- renderValueBox({
    total <- sum(airline_rankings$TotalFlights)
    valueBox(
      format(total, big.mark = ","),
      "Total Flights",
      icon = icon("plane"),
      color = "green"
    )
  })
  
  output$total_revenue_box <- renderValueBox({
    total <- sum(airline_rankings$TotalRevenue)
    valueBox(
      paste0("₹", format(round(total / 1000000), big.mark = ","), "M"),
      "Total Revenue",
      icon = icon("rupee-sign"),
      color = "orange"
    )
  })
  
  # Dark-themed rankings UI
  output$dark_rankings_ui <- renderUI({
    rankings_with_routes <- airline_rankings %>%
      left_join(airline_routes, by = "AirlineName")
    
    max_flights <- max(rankings_with_routes$TotalFlights)
    
    rows_html <- lapply(1:nrow(rankings_with_routes), function(i) {
      row <- rankings_with_routes[i, ]
      bar_width <- (row$TotalFlights / max_flights) * 100
      
      num_planes <- ceiling((row$TotalFlights / max_flights) * 7)
      plane_icons <- paste(rep("✈", num_planes), collapse = "")
      
      route_text <- if(!is.na(row$Routes)) {
        paste0(row$Routes, " <span style='color:#666'>(+", row$TotalRoutes - 5, " more)</span>")
      } else {
        "Various routes"
      }
      
      HTML(paste0(
        '<div class="rankings-row">',
        '<div class="rank-number">', i, '</div>',
        '<div class="airline-name">', row$AirlineName, '</div>',
        '<div class="route-info">', route_text, '</div>',
        '<div class="avg-price">₹', format(round(row$AvgPrice), big.mark = ","), '</div>',
        '<div class="total-flights">', format(row$TotalFlights, big.mark = ","), '</div>',
        '<div class="flight-bar">',
        '<div class="bar-container"><div class="bar-fill" style="width:', bar_width, '%"></div></div>',
        '<span class="plane-icon">', plane_icons, '</span>',
        '</div>',
        '</div>'
      ))
    })
    
    HTML(paste0(
      '<div class="dark-rankings-container">',
      '<div class="rankings-header">',
      '<div>RANK</div>',
      '<div>AIRLINE</div>',
      '<div>TOP ROUTES</div>',
      '<div>AVG PRICE</div>',
      '<div>FLIGHTS</div>',
      '<div></div>',
      '</div>',
      paste(rows_html, collapse = ""),
      '</div>'
    ))
  })
  
  # ============================================
  # SATISFACTION PREDICTION TAB
  # ============================================
  
  # UI UPDATES & DROPDOWNS
  observe({
    airlines <- sort(unique(cleaned_reviews$AirlineName))
    updateSelectInput(session, "sat_airline", choices = airlines)
    
    routes <- cleaned_flights %>%
      mutate(route = paste(from, "→", to)) %>%
      pull(route) %>%
      unique() %>%
      sort()
    updateSelectInput(session, "sat_route", 
                      choices = c("Any" = "any", routes))
  })
  
  # PREDICTION LOGIC
  observeEvent(input$predict_sat_btn, {
    req(input$sat_airline, input$sat_rating)
    
    airline_data <- airline_incident_scores() %>%
      filter(AirlineName == input$sat_airline)
    
    estimated_severity <- case_when(
      input$sat_rating <= 3 ~ 8,
      input$sat_rating <= 5 ~ 5,
      input$sat_rating <= 7 ~ 3,
      TRUE ~ 1
    )
    
    new_data <- data.frame(
      Rating = input$sat_rating,
      incident_severity = estimated_severity,
      delay_incident = ifelse(airline_data$delay_rate > 30, 1, 0),
      cancellation_incident = ifelse(airline_data$cancellation_rate > 20, 1, 0),
      technical_incident = ifelse(airline_data$technical_rate > 15, 1, 0),
      safety_incident = ifelse(airline_data$safety_rate > 10, 1, 0)
    )
    
    prob <- predict(enhanced_satisfaction_model()$model, new_data, type = "prob")[1, "1"]
    adjusted_prob <- prob * (airline_data$safety_score / 100)
    
    result_text <- ifelse(adjusted_prob > 0.5, 
                          "LIKELY SATISFIED ✅", 
                          "LIKELY DISSATISFIED ❌")
    prob_text <- paste0(round(adjusted_prob * 100, 1), "% Probability")
    
    output$satisfaction_result <- renderText({ result_text })
    output$satisfaction_probability <- renderText({ prob_text })
    
    output$satisfaction_icon <- renderUI({
      if(adjusted_prob > 0.5) {
        icon("smile", class = "fa-5x", style = "color: #4ade80;")
      } else {
        icon("frown", class = "fa-5x", style = "color: #f87171;")
      }
    })
  })
  
  # AIRLINE STATISTICS
  output$airline_avg_rating <- renderText({
    req(input$sat_airline)
    airline_incident_scores() %>%
      filter(AirlineName == input$sat_airline) %>%
      pull(avg_rating) %>%
      round(1)
  })
  
  output$airline_rec_rate <- renderText({
    req(input$sat_airline)
    airline_incident_scores() %>%
      filter(AirlineName == input$sat_airline) %>%
      pull(recommendation_rate) %>%
      round(1) %>%
      paste0("%")
  })
  
  output$airline_safety_score <- renderText({
    req(input$sat_airline)
    score <- airline_incident_scores() %>%
      filter(AirlineName == input$sat_airline) %>%
      pull(safety_score) %>%
      round(0)
    
    paste0(score, "/100")
  })
  
  output$airline_review_count <- renderText({
    req(input$sat_airline)
    airline_incident_scores() %>%
      filter(AirlineName == input$sat_airline) %>%
      pull(total_reviews) %>%
      format(big.mark = ",")
  })
  
  # ============================================
  # INCIDENT ANALYSIS TAB
  # ============================================
  
  # Value Boxes
  output$total_incidents_box <- renderValueBox({
    total <- reviews_with_incidents() %>%
      filter(total_incidents > 0) %>%
      nrow()
    
    valueBox(
      format(total, big.mark = ","),
      "Total Incidents Reported",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$worst_airline_box <- renderValueBox({
    worst <- airline_incident_scores() %>%
      arrange(safety_score) %>%
      slice(1)
    
    valueBox(
      worst$AirlineName,
      paste0("Lowest Safety (", round(worst$safety_score), "/100)"),
      icon = icon("thumbs-down"),
      color = "red"
    )
  })
  
  output$safest_airline_box <- renderValueBox({
    safest <- airline_incident_scores() %>%
      arrange(desc(safety_score)) %>%
      slice(1)
    
    valueBox(
      safest$AirlineName,
      paste0("Highest Safety (", round(safest$safety_score), "/100)"),
      icon = icon("shield-alt"),
      color = "green"
    )
  })
  
  output$common_issue_box <- renderValueBox({
    incident_totals <- airline_incident_scores() %>%
      summarise(
        Delay = sum(delay_count),
        Cancellation = sum(cancellation_count),
        Technical = sum(technical_count),
        Safety = sum(safety_count),
        Baggage = sum(baggage_count),
        Maintenance = sum(maintenance_count),
        Staff = sum(staff_count)
      ) %>%
      pivot_longer(everything(), names_to = "type", values_to = "count") %>%
      arrange(desc(count)) %>%
      slice(1)
    
    valueBox(
      incident_totals$type,
      paste0("Most Common (", incident_totals$count, " reports)"),
      icon = icon("chart-pie"),
      color = "orange"
    )
  })
  
  # Incident Heatmap
  output$incident_heatmap <- renderPlotly({
    heatmap_data <- airline_incident_scores() %>%
      select(AirlineName, delay_rate, cancellation_rate, technical_rate, 
             safety_rate, baggage_rate, maintenance_rate, staff_rate) %>%
      pivot_longer(-AirlineName, names_to = "incident_type", values_to = "rate") %>%
      mutate(
        incident_type = str_remove(incident_type, "_rate"),
        incident_type = str_to_title(incident_type)
      )
    
    plot_ly(heatmap_data, x = ~incident_type, y = ~AirlineName, z = ~rate,
            type = "heatmap", 
            colorscale = list(c(0, "#4ade80"), c(0.5, "#fbbf24"), c(1, "#f87171")),
            text = ~paste0(round(rate, 1), "%"),
            texttemplate = "%{text}",
            textfont = list(color = '#1a1a1a')) %>%
      layout(
        xaxis = list(title = "", color = '#e0e0e0'),
        yaxis = list(title = "", color = '#e0e0e0'),
        paper_bgcolor = '#1a1a1a',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#e0e0e0')
      )
  })
  
  # Incident Timeline
  output$incident_timeline <- renderPlotly({
    timeline_data <- airline_incident_scores() %>%
      select(AirlineName, total_incident_rate) %>%
      arrange(desc(total_incident_rate))
    
    plot_ly(timeline_data, x = ~AirlineName, y = ~total_incident_rate,
            type = 'bar',
            marker = list(color = ~total_incident_rate,
                          colorscale = list(c(0, "#4ade80"), c(1, "#f87171")))) %>%
      layout(
        xaxis = list(title = "", color = '#e0e0e0', gridcolor = '#2a2a2a'),
        yaxis = list(title = "Incident Rate (%)", color = '#e0e0e0', gridcolor = '#2a2a2a'),
        paper_bgcolor = '#1a1a1a',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#e0e0e0')
      )
  })
  
  # Safety Rankings Table
  output$safety_rankings_table <- renderDT({
    rankings <- airline_incident_scores() %>%
      select(AirlineName, safety_score, total_incident_rate, avg_rating) %>%
      arrange(desc(safety_score)) %>%
      mutate(
        Rank = row_number(),
        safety_score = round(safety_score, 1),
        total_incident_rate = round(total_incident_rate, 1),
        avg_rating = round(avg_rating, 1)
      ) %>%
      select(Rank, AirlineName, safety_score, total_incident_rate, avg_rating)
    
    datatable(rankings, 
              options = list(pageLength = 10, dom = 't'), 
              rownames = FALSE,
              colnames = c("Rank", "Airline", "Safety Score", "Incident Rate %", "Avg Rating"))
  })
  
  # Critical Incidents Table
  output$critical_incidents_table <- renderDT({
    critical <- reviews_with_incidents() %>%
      filter(incident_severity >= 7) %>%
      select(AirlineName, Date, Rating, total_incidents, Review) %>%
      arrange(desc(incident_severity), desc(total_incidents)) %>%
      mutate(
        Review = str_trunc(Review, 150)
      ) %>%
      head(20)
    
    datatable(critical, 
              options = list(pageLength = 10), 
              rownames = FALSE,
              colnames = c("Airline", "Date", "Rating", "Incidents", "Review Excerpt"))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
# Install and load required packages
if (!require(readr)) install.packages("readr")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(leaflet)) install.packages("leaflet")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(leaflet)
library(RColorBrewer)

# Function to read data from CSV file
read_csv_data <- function(file_path) {
  tryCatch({
    data <- read_csv(file_path)
    # Rename 'ride_name' or 'attraction_name' column to 'name' if present
    if ("ride_name" %in% colnames(data)) {
      data <- data %>% rename(name = ride_name)
    } else if ("attraction_name" %in% colnames(data)) {
      data <- data %>% rename(name = attraction_name)
    }
    data
  }, error = function(e) {
    message("Failed to read CSV file. Error: ", e$message)
    NULL
  })
}

# CSV file paths
ride_file_path <- "disney-ride-data.csv"
attraction_file_path <- "disney-attractions.csv"
show_file_path <- "disney-show-data.csv"

ui <- dashboardPage(
  dashboardHeader(title = "Disney World Experience"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table"))
    ),
    actionButton("load_data", "Load Data", icon = icon("sync"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "visualizations",
              fluidRow(
                box(
                  title = "Filters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("data_type", "Data Type:", 
                              choices = c("Rides", "Attractions", "Shows")),
                  selectInput("theme_park", "Theme Park:", choices = NULL),
                  selectInput("sub_land", "Sub-land:", choices = NULL),
                  checkboxGroupInput("height_requirement", "Height Requirements (inches):",
                                     choices = NULL)
                ),
                box(
                  title = "Wait Times / Duration",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  plotOutput("barplot")
                )
              ),
              fluidRow(
                box(
                  title = "Disney World Map",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  leafletOutput("disney_map")
                )
              )
      ),
      tabItem(tabName = "data_table",
              fluidRow(
                box(
                  title = "Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("data_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  all_data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    withProgress(message = 'Loading data...', value = 0.1, {
      ride_data <- read_csv_data(ride_file_path)
      incProgress(0.3)
      attraction_data <- read_csv_data(attraction_file_path)
      incProgress(0.3)
      show_data <- read_csv_data(show_file_path)
      incProgress(0.3)
      
      if (!is.null(ride_data) && !is.null(attraction_data) && !is.null(show_data)) {
        ride_data$data_type <- "Rides"
        attraction_data$data_type <- "Attractions"
        show_data$data_type <- "Shows"
        
        combined_data <- bind_rows(ride_data, attraction_data, show_data)
        all_data(combined_data)
        
        updateSelectInput(session, "theme_park", choices = unique(combined_data$theme_park))
        updateSelectInput(session, "sub_land", choices = c("All", unique(combined_data$sub_land)))
        
        height_choices <- sort(unique(combined_data$height_requirement[!is.na(combined_data$height_requirement)]))
        updateCheckboxGroupInput(session, "height_requirement", 
                                 choices = height_choices,
                                 selected = height_choices)
      } else {
        showNotification("Failed to load one or more data files. Please check the CSV files.", type = "error")
      }
    })
  })
  
  filtered_data <- reactive({
    req(all_data())
    data <- all_data() %>%
      filter(data_type == input$data_type,
             theme_park == input$theme_park)
    
    if (length(input$height_requirement) > 0) {
      data <- data %>% filter(height_requirement %in% input$height_requirement | is.na(height_requirement))
    }
    
    if (input$sub_land != "All") {
      data <- data %>% filter(sub_land == input$sub_land)
    }
    
    data
  })
  
  observe({
    req(all_data())
    park_sublands <- unique(all_data()$sub_land[all_data()$theme_park == input$theme_park & all_data()$data_type == input$data_type])
    updateSelectInput(session, "sub_land", choices = c("All", park_sublands))
  })
  
  output$barplot <- renderPlot({
    req(filtered_data())
    # Use Dark2 color palette
    dark_colors <- brewer.pal(n = 8, name = "Dark2")
    
    y_var <- ifelse(input$data_type == "Shows", "duration", "avg_wait_time")
    y_label <- ifelse(input$data_type == "Shows", "Duration (minutes)", "Average Wait Time (minutes)")
    
    ggplot(filtered_data(), aes(x = reorder(name, .data[[y_var]]), y = .data[[y_var]], fill = sub_land)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Name", y = y_label, fill = "Sub-land") +
      scale_y_continuous(breaks = seq(0, max(filtered_data()[[y_var]], na.rm = TRUE), by = 10)) +
      scale_fill_manual(values = dark_colors) +
      theme_minimal() +
      theme(axis.text.y = element_text(angle = 0, hjust = 1))
  })
  
  output$data_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$disney_map <- renderLeaflet({
    req(filtered_data())
    
    # Create a color palette for sub-lands using Dark2
    sub_lands <- unique(filtered_data()$sub_land)
    dark_colors <- brewer.pal(n = max(8, length(sub_lands)), name = "Dark2")
    pal <- colorFactor(palette = dark_colors, domain = sub_lands)
    
    popup_content
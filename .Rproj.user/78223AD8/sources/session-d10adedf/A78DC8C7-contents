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
    read_csv(file_path)
  }, error = function(e) {
    message("Failed to read CSV file. Error: ", e$message)
    NULL
  })
}

# CSV file path - Update this to your local file path
csv_file_path <- "disney-all-data.csv"  # Modify this path

ui <- dashboardPage(
  dashboardHeader(title = "Disney World Wait Times"),
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
                  selectInput("theme_park", "Theme Park:", choices = NULL),
                  selectInput("sub_land", "Sub-land:", choices = NULL),
                  checkboxGroupInput("height_requirement", "Height Requirements (inches):",
                                     choices = NULL)
                ),
                box(
                  title = "Wait Times (All Attractions)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 9,
                  plotOutput("barplot")
                )
              ),
              fluidRow(
                box(
                  title = "Wait Times (Rides Only)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("rides_barplot")
                )
              ),
              fluidRow(
                box(
                  title = "Wait Times (Attractions Only)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("attractions_barplot")
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
                  title = "Ride Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("ride_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  rides_data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    withProgress(message = 'Loading data...', value = 0.1, {
      data <- read_csv_data(csv_file_path)
      incProgress(0.9)
      if (!is.null(data)) {
        # Use actual latitude and longitude from the CSV file
        rides_data(data)
        updateSelectInput(session, "theme_park", choices = unique(data$theme_park))
        updateSelectInput(session, "sub_land", choices = c("All", unique(data$sub_land)))
        
        # Update height requirement checkboxes with only supplied values
        height_choices <- sort(unique(data$height_requirement[!is.na(data$height_requirement)]))
        updateCheckboxGroupInput(session, "height_requirement", 
                                 choices = height_choices,
                                 selected = height_choices)
      } else {
        showNotification("Failed to load data. Please check the CSV file.", type = "error")
      }
    })
  })
  
  filtered_data <- reactive({
    req(rides_data())
    data <- rides_data() %>%
      filter(theme_park == input$theme_park)
    
    if (length(input$height_requirement) > 0) {
      data <- data %>% filter(height_requirement %in% input$height_requirement | is.na(height_requirement))
    }
    
    if (input$sub_land != "All") {
      data <- data %>% filter(sub_land == input$sub_land)
    }
    
    data
  })
  
  observe({
    req(rides_data())
    park_sublands <- unique(rides_data()$sub_land[rides_data()$theme_park == input$theme_park])
    updateSelectInput(session, "sub_land", choices = c("All", park_sublands))
  })
  
  output$barplot <- renderPlot({
    req(filtered_data())
    # Use Dark2 color palette
    dark_colors <- brewer.pal(n = 8, name = "Dark2")
    ggplot(filtered_data(), aes(x = reorder(name, avg_wait_time), y = avg_wait_time, fill = sub_land)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Attraction Name", y = "Average Wait Time (minutes)", fill = "Sub-land") +
      scale_y_continuous(breaks = seq(0, max(filtered_data()$avg_wait_time, na.rm = TRUE), by = 10)) +
      scale_fill_manual(values = dark_colors) +
      theme_minimal() +
      theme(axis.text.y = element_text(angle = 0, hjust = 1))
  })
  
  output$rides_barplot <- renderPlot({
    req(filtered_data())
    # Filter data for rides only
    rides_only <- filtered_data() %>% filter(type == "ride")
    
    # Use Dark2 color palette
    dark_colors <- brewer.pal(n = 8, name = "Dark2")
    ggplot(rides_only, aes(x = reorder(name, avg_wait_time), y = avg_wait_time, fill = sub_land)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Ride Name", y = "Average Wait Time (minutes)", fill = "Sub-land") +
      scale_y_continuous(breaks = seq(0, max(rides_only$avg_wait_time, na.rm = TRUE), by = 10)) +
      scale_fill_manual(values = dark_colors) +
      theme_minimal() +
      theme(axis.text.y = element_text(angle = 0, hjust = 1))
  })
  
  output$attractions_barplot <- renderPlot({
    req(rides_data())
    # Filter data for attractions only, without applying height requirement filter
    attractions_only <- rides_data() %>% 
      filter(theme_park == input$theme_park, type == "attraction") %>%
      {if(input$sub_land != "All") filter(., sub_land == input$sub_land) else .}
    
    # Use Dark2 color palette
    dark_colors <- brewer.pal(n = 8, name = "Dark2")
    ggplot(attractions_only, aes(x = reorder(name, avg_wait_time), y = avg_wait_time, fill = sub_land)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Attraction Name", y = "Average Wait Time (minutes)", fill = "Sub-land") +
      scale_y_continuous(breaks = seq(0, max(attractions_only$avg_wait_time, na.rm = TRUE), by = 10)) +
      scale_fill_manual(values = dark_colors) +
      theme_minimal() +
      theme(axis.text.y = element_text(angle = 0, hjust = 1))
  })
  output$ride_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$disney_map <- renderLeaflet({
    req(filtered_data())
    
    # Create a color palette for sub-lands using Dark2
    sub_lands <- unique(filtered_data()$sub_land)
    dark_colors <- brewer.pal(n = max(8, length(sub_lands)), name = "Dark2")
    pal <- colorFactor(palette = dark_colors, domain = sub_lands)
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~pal(sub_land),
        radius = 8,
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste0("<strong>", name, "</strong><br>",
                        "Wait Time: ", avg_wait_time, " min<br>",
                        "Height Requirement: ", ifelse(is.na(height_requirement), "None", paste(height_requirement, "inches")),
                        "<br>Sub-land: ", sub_land),
        label = ~name
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~sub_land,
        title = "Sub-lands",
        opacity = 1
      ) %>%
      fitBounds(
        ~min(longitude), ~min(latitude),
        ~max(longitude), ~max(latitude)
      )
  })
}

shinyApp(ui, server)

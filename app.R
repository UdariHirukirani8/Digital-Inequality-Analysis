# ===============================
# LOAD LIBRARIES
# ===============================
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# ===============================
# LOAD DATA
# ===============================
data <- read_csv("combined_data.csv")

# ===============================
# UI
# ===============================
ui <- dashboardPage(
  
  dashboardHeader(title = "🌍 Digital Divide Explorer"),
  
  dashboardSidebar(
    
    selectInput("countries",
                "Select Countries:",
                choices = sort(unique(data$`Country Name`)),
                selected = "Sri Lanka",
                multiple = TRUE),
    
    selectInput("region",
                "Select Region:",
                choices = c("All", "Africa", "Asia", "Europe", "Americas"),
                selected = "All"),
    
    sliderInput("year",
                "Select Year Range:",
                min = 2000,
                max = 2022,
                value = c(2000, 2022),
                sep = ""),
    
    selectInput("metric",
                "Select Metric:",
                choices = c("Internet_Users_Pct",
                            "Mobile_Subscriptions",
                            "GDP_Per_Capita"),
                selected = "Internet_Users_Pct")
  ),
  
  dashboardBody(
    
    fluidRow(
      valueBoxOutput("rank_box"),
      valueBoxOutput("growth_box"),
      valueBoxOutput("region_box")
    ),
    
    tabBox(
      width = 12,
      
      tabPanel("📈 Trend", plotlyOutput("line_plot")),
      tabPanel("📊 Comparison", plotlyOutput("bar_plot")),
      tabPanel("🌐 Scatter", plotlyOutput("scatter_plot")),
      tabPanel("📋 Data", DTOutput("table"))
    )
  )
)

# ===============================
# SERVER
# ===============================
server <- function(input, output) {
  
  # Filtered data
  filtered_data <- reactive({
    
    df <- data %>%
      filter(Year >= input$year[1],
             Year <= input$year[2])
    
    if (input$region != "All") {
      df <- df %>% filter(Continent == input$region)
    }
    
    df %>% filter(`Country Name` %in% input$countries)
  })
  
  # ===============================
  # 📈 LINE CHART
  # ===============================
  output$line_plot <- renderPlotly({
    
    p <- ggplot(filtered_data(),
                aes(x = Year,
                    y = .data[[input$metric]],
                    color = `Country Name`)) +
      geom_line(size = 1.2) +
      theme_minimal() +
      labs(title = "Trend Over Time",
           y = input$metric)
    
    ggplotly(p)
  })
  
  # ===============================
  # 📊 BAR CHART
  # ===============================
  output$bar_plot <- renderPlotly({
    
    latest <- filtered_data() %>%
      filter(Year == max(Year))
    
    p <- ggplot(latest,
                aes(x = reorder(`Country Name`, .data[[input$metric]]),
                    y = .data[[input$metric]],
                    fill = `Country Name`)) +
      geom_col() +
      coord_flip() +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ===============================
  # 🌐 SCATTER PLOT
  # ===============================
  output$scatter_plot <- renderPlotly({
    
    df <- data %>%
      filter(Year == max(input$year)) %>%
      filter(!is.na(GDP_Per_Capita),
             !is.na(Internet_Users_Pct))
    
    p <- ggplot(df,
                aes(x = GDP_Per_Capita,
                    y = Internet_Users_Pct,
                    color = Continent,
                    text = `Country Name`)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_x_log10() +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # ===============================
  # 📋 TABLE
  # ===============================
  output$table <- renderDT({
    datatable(filtered_data())
  })
  
  # ===============================
  # 📦 INFO BOX 1 — RANK
  # ===============================
  output$rank_box <- renderValueBox({
    
    req(input$countries)
    
    df <- data %>%
      filter(Year == max(Year)) %>%
      arrange(desc(Internet_Users_Pct))
    
    rank <- which(df$`Country Name` == input$countries[1])
    
    shinydashboard::valueBox(
      value = paste("Rank:", rank),
      subtitle = input$countries[1],
      color = "blue"
    )
  })
  
  # ===============================
  # 📦 INFO BOX 2 — GROWTH
  # ===============================
  output$growth_box <- renderValueBox({
    
    req(input$countries)
    
    df <- data %>%
      filter(`Country Name` == input$countries[1])
    
    start <- df %>%
      filter(Year == 2000) %>%
      pull(Internet_Users_Pct)
    
    end <- df %>%
      filter(Year == max(Year)) %>%
      pull(Internet_Users_Pct)
    
    growth <- round(end - start, 2)
    
    shinydashboard::valueBox(
      value = paste(growth, "%"),
      subtitle = "Growth since 2000",
      color = "green"
    )
  })
  
  # ===============================
  # 📦 INFO BOX 3 — REGION COMPARE
  # ===============================
  output$region_box <- renderValueBox({
    
    req(input$countries)
    
    df <- data %>%
      filter(Year == max(Year))
    
    country_val <- df %>%
      filter(`Country Name` == input$countries[1]) %>%
      pull(Internet_Users_Pct)
    
    region_avg <- df %>%
      summarise(avg = mean(Internet_Users_Pct, na.rm = TRUE)) %>%
      pull(avg)
    
    diff <- round(country_val - region_avg, 2)
    
    shinydashboard::valueBox(
      value = paste(diff, "%"),
      subtitle = "Vs Global Avg",
      color = "maroon"
    )
  })
  
}

# ===============================
# RUN APP
# ===============================
shinyApp(ui = ui, server = server)
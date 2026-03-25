#import libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

#load data
data <- read_csv("combined_data.csv")

#clean metric labels
metric_labels <- c(
  "Internet Users (%)"             = "Internet_Users_Pct",
  "Mobile Subscriptions (per 100)" = "Mobile_Subscriptions",
  "GDP Per Capita (USD)"           = "GDP_Per_Capita"
)

#continent list
continent_list <- c("All Regions",
                    sort(unique(na.omit(data$Continent))))

#chart colors
chart_colors <- c(
  "#3b82f6", "#10b981", "#f59e0b",
  "#ef4444", "#8b5cf6", "#06b6d4",
  "#f97316", "#84cc16"
)

#continent colors
continent_colors <- c(
  "Africa"   = "#f59e0b",
  "Americas" = "#3b82f6",
  "Asia"     = "#10b981",
  "Europe"   = "#8b5cf6",
  "Oceania"  = "#ef4444"
)



###UI###

ui <- dashboardPage(
  skin = "blue",
  
  #header
  dashboardHeader(
    titleWidth = 240,
    title = span(
      "🌍 Digital Divide Explorer",
      style = "
        font-weight: 800;
        font-size: 25px;
        color: white;
        letter-spacing: 0.8px;
      "
    )
  ),
  
  #sidebar
  dashboardSidebar(
    width = 240,
    
    div(
      style = "
        padding: 12px 15px;
        font-size: 14px;
        color: #94a3b8;
        line-height: 1.6;
        border-bottom: 1px solid #334155;
      ",
      "📊 Explore global internet access patterns
       across countries and regions."
    ),
    
    br(),
    
    div(
      style = "padding: 0 15px 15px;",
      selectInput(
        inputId  = "region",
        label    = tags$span(
          "📌 FILTER BY REGION",
          style  = "color:#93c5fd;
                    font-weight:700;
                    font-size:13px;
                    letter-spacing:0.5px;"
        ),
        choices  = continent_list,
        selected = "All Regions"
      )
    ),
    
    div(
      style = "padding: 0 15px 15px;",
      uiOutput("country_ui")
    ),
    
    hr(style = "border-color:#334155; margin: 10px 15px;"),
    
    div(
      style = "padding: 0 15px 15px;",
      sliderInput(
        inputId = "year",
        label   = tags$span(
          "📅 YEAR RANGE",
          style = "color:#93c5fd;
                   font-weight:700;
                   font-size:13px;
                   letter-spacing:0.5px;"
        ),
        min   = 2000,
        max   = 2022,
        value = c(2000, 2022),
        sep   = ""
      )
    ),
    
    div(
      style = "padding: 0 15px 15px;",
      selectInput(
        inputId  = "metric",
        label    = tags$span(
          "📊 SELECT METRIC",
          style  = "color:#93c5fd;
                    font-weight:700;
                    font-size:13px;
                    letter-spacing:0.5px;"
        ),
        choices  = metric_labels,
        selected = "Internet_Users_Pct"
      )
    ),
    
    hr(style = "border-color:#334155;"),
    
    div(
      style = "text-align: center; margin-bottom: 25px;",
      actionButton(
        inputId = "reset",
        label   = list(icon("sync-alt"), "Reset All Filters"),
        style   = "
          width: 210px;
          height: 50px;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          gap: 12px;
          margin: 0 auto !important;
          background: linear-gradient(
            135deg, #3b82f6, #1d4ed8
          ) !important;
          color: white !important;
          border: none !important;
          border-radius: 12px !important;
          font-weight: 700 !important;
          font-size: 16px !important;
          padding: 0 !important;
          cursor: pointer !important;
          box-shadow: 0 4px 15px rgba(59,130,246,0.3) !important;
          transition: all 0.2s ease !important;
        "
      )
    )
  ),
  

    ##body##
    dashboardBody(
    
    ##csss parts to body
    tags$head(
      tags$style(HTML("

        /* ── PAGE ── */
        body, .wrapper {
          background-color: #f0f4f8 !important;
          font-family: 'Segoe UI', Tahoma, sans-serif;
        }
        .content-wrapper, .right-side {
          background-color: #f0f4f8 !important;
        }

        /* ── HEADER ── */
        .main-header .logo,
        .main-header .navbar {
          background: linear-gradient(
            135deg, #1e3a5f 0%, #1d4ed8 100%
          ) !important;
          border-bottom: none !important;
        }
        .main-header .logo {
          font-weight: 800 !important;
          text-align: center !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
        }

        /* ── SIDEBAR ── */
        .main-sidebar, .left-side {
          background: linear-gradient(
            180deg, #0f172a 0%, #1e293b 100%
          ) !important;
        }
        .sidebar-menu > li > a {
          color: #cbd5e1 !important;
          font-size: 14px !important;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li > a:hover {
          background-color: #1e3a5f !important;
          color: white !important;
        }

        /* ── VALUE BOXES ── */
        .small-box {
          border-radius: 16px !important;
          box-shadow: 0 4px 20px
            rgba(0,0,0,0.15) !important;
          transition: transform 0.2s ease,
                      box-shadow 0.2s ease !important;
          padding: 18px !important;
        }
        .small-box:hover {
          transform: translateY(-4px) !important;
          box-shadow: 0 8px 30px
            rgba(0,0,0,0.2) !important;
        }
        .small-box .inner h3 {
          font-size: 2.4rem !important;
          font-weight: 900 !important;
          letter-spacing: -0.5px !important;
          margin-bottom: 6px !important;
        }
        .small-box .inner p {
          font-size: 1rem !important;
          font-weight: 600 !important;
          opacity: 0.95 !important;
          line-height: 1.4 !important;
        }
        .small-box .icon {
          font-size: 65px !important;
          top: 15px !important;
          opacity: 0.25 !important;
        }
        .small-box.bg-blue {
          background: linear-gradient(
            135deg, #1d4ed8, #3b82f6
          ) !important;
        }
        .small-box.bg-green {
          background: linear-gradient(
            135deg, #059669, #10b981
          ) !important;
        }
        .small-box.bg-orange {
          background: linear-gradient(
            135deg, #d97706, #f59e0b
          ) !important;
        }
        .small-box.bg-red {
          background: linear-gradient(
            135deg, #dc2626, #ef4444
          ) !important;
        }

        /* ── TAB BOX ── */
        .nav-tabs-custom {
          background: white !important;
          border-radius: 16px !important;
          box-shadow: 0 4px 20px
            rgba(0,0,0,0.08) !important;
          border: none !important;
          overflow: hidden !important;
        }
        .nav-tabs-custom > .nav-tabs {
          border-bottom: 2px solid #e2e8f0 !important;
          background: white !important;
          padding: 8px 12px 0 !important;
        }
        .nav-tabs-custom > .nav-tabs > li > a {
          color: #64748b !important;
          font-weight: 600 !important;
          font-size: 13px !important;
          border-radius: 8px 8px 0 0 !important;
          padding: 10px 14px !important;
          transition: all 0.2s !important;
          border: none !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active > a {
          color: #1d4ed8 !important;
          border-top: 3px solid #1d4ed8 !important;
          background: #eff6ff !important;
          font-weight: 700 !important;
        }
        .nav-tabs-custom > .nav-tabs > li > a:hover {
          background: #f8fafc !important;
          color: #1d4ed8 !important;
        }
        .nav-tabs-custom > .tab-content {
          background: white !important;
          padding: 20px !important;
        }

        /* ── INPUTS ── */
        .form-control,
        .selectize-input {
          border-radius: 8px !important;
          border: 1px solid #475569 !important;
          background-color: #1e293b !important;
          color: #e2e8f0 !important;
          font-size: 13px !important;
        }
        .selectize-dropdown {
          background-color: #1e293b !important;
          color: #e2e8f0 !important;
          border: 1px solid #475569 !important;
          border-radius: 8px !important;
        }
        .selectize-dropdown-content
        .option:hover {
          background-color: #1d4ed8 !important;
        }
         /* ── SELECTIZE MULTI-SELECT TAGS ── */
         .selectize-control.multi .selectize-input > div {
           background: #3b82f6 !important;
           color: white !important;
           border-radius: 4px !important;
           padding: 2px 8px !important;
           border: none !important;
           margin: 2px !important;
         }
         .selectize-control.multi .selectize-input > div.active {
           background: #1d4ed8 !important;
         }
         .selectize-control.multi .selectize-input > div > a {
           color: white !important;
         }

        /* ── SLIDER ── */
        .irs--shiny .irs-bar {
          background: linear-gradient(
            90deg, #1d4ed8, #3b82f6
          ) !important;
          border-top: none !important;
          border-bottom: none !important;
        }
        .irs--shiny .irs-handle {
          background: #1d4ed8 !important;
          border-color: #1d4ed8 !important;
        }
        .irs--shiny .irs-from,
        .irs--shiny .irs-to,
        .irs--shiny .irs-single {
          background: #1d4ed8 !important;
          border-radius: 6px !important;
        }
         .irs--shiny .irs-line {
           background: #334155 !important;
           border: none !important;
         }
         .irs-grid-text {
           opacity: 0 !important;
           pointer-events: none !important;
         }
         .irs-grid-pol {
           opacity: 0.3 !important;
         }

        /* ── INFO TEXT ── */
        .info-text {
          color: #64748b;
          font-size: 15px;
          font-style: italic;
          border-left: 4px solid #3b82f6;
          padding: 6px 12px;
          margin-bottom: 16px;
          background: #eff6ff;
          border-radius: 0 8px 8px 0;
        }

        /* ── SCROLLBAR ── */
        ::-webkit-scrollbar { width:6px; height:6px; }
        ::-webkit-scrollbar-track { background:#f1f5f9; }
        ::-webkit-scrollbar-thumb {
          background: #94a3b8;
          border-radius: 3px;
        }

        /* ── CONTROL LABELS ── */
        .control-label {
          color: #93c5fd !important;
          font-weight: 700 !important;
          font-size: 11px !important;
          text-transform: uppercase !important;
          letter-spacing: 0.5px !important;
        }

        /* ══════════════════════════════════
           MOBILE RESPONSIVE
           ══════════════════════════════════ */

        /* ── TABLET (≤992px) ── */
        @media (max-width: 992px) {

          /* sidebar works with hamburger menu */
          .content-wrapper,
          .main-footer,
          .right-side {
            margin-left: 0 !important;
            transition: margin-left 0.3s ease !important;
          }
          body:not(.sidebar-collapse) .content-wrapper,
          body:not(.sidebar-collapse) .main-footer,
          body:not(.sidebar-collapse) .right-side {
            margin-left: 240px !important;
          }
          .main-sidebar {
            transition: transform 0.3s ease !important;
          }
          body.sidebar-collapse .main-sidebar {
            transform: translateX(-240px) !important;
          }

          /* value boxes */
          .small-box .inner h3 {
            font-size: 2.2rem !important;
          }
          .small-box .inner p {
            font-size: 1rem !important;
          }

          /* tabs */
          .nav-tabs-custom > .nav-tabs {
            display: flex !important;
            flex-wrap: wrap !important;
            padding: 6px 8px 0 !important;
          }
          .nav-tabs-custom > .nav-tabs > li > a {
            font-size: 13px !important;
            padding: 10px 12px !important;
          }
          .nav-tabs-custom > .tab-content {
            padding: 14px !important;
          }
        }

        /* ── PHONE (≤576px) ── */
        @media (max-width: 576px) {

          /* sidebar auto-collapse on phone */
          body:not(.sidebar-collapse) .content-wrapper,
          body:not(.sidebar-collapse) .main-footer {
            margin-left: 0 !important;
          }
          .main-sidebar {
            z-index: 9999 !important;
          }
          body.sidebar-collapse .main-sidebar {
            transform: translateX(-240px) !important;
          }

          /* header */
          .main-header .logo {
            width: auto !important;
            font-size: 22px !important;
            padding: 0 10px !important;
          }
          .main-header .sidebar-toggle {
            font-size: 20px !important;
            padding: 12px 15px !important;
          }

          /* body padding */
          .content-wrapper {
            padding: 10px !important;
          }
          .content {
            padding: 0 !important;
          }

          /* value boxes full width */
          .col-lg-4, .col-md-4, .col-sm-6 {
            width: 100% !important;
            padding: 4px 8px !important;
          }
          .small-box {
            padding: 16px !important;
            border-radius: 12px !important;
            margin-bottom: 8px !important;
          }
          .small-box .inner h3 {
            font-size: 2rem !important;
          }
          .small-box .inner p {
            font-size: 1rem !important;
          }
          .small-box .icon {
            font-size: 45px !important;
            top: 10px !important;
          }

          /* tabs scroll horizontally */
          .nav-tabs-custom {
            border-radius: 10px !important;
          }
          .nav-tabs-custom > .nav-tabs {
            overflow-x: auto !important;
            flex-wrap: nowrap !important;
            white-space: nowrap !important;
            -webkit-overflow-scrolling: touch;
            padding: 4px 6px 0 !important;
          }
          .nav-tabs-custom > .nav-tabs > li {
            flex-shrink: 0 !important;
          }
          .nav-tabs-custom > .nav-tabs > li > a {
            font-size: 12px !important;
            padding: 9px 12px !important;
          }
          .nav-tabs-custom > .tab-content {
            padding: 10px !important;
            overflow: auto !important;
            -webkit-overflow-scrolling: touch;
          }

          /* chart scrollable containers */
          .plotly.html-widget {
            min-width: 600px !important;
          }

          /* table scrollable */
          .dataTables_wrapper {
            overflow-x: auto !important;
            -webkit-overflow-scrolling: touch;
          }

          /* info text */
          .info-text {
            font-size: 13px !important;
            padding: 6px 12px !important;
            margin-bottom: 12px !important;
          }

          /* scrollbar on mobile */
          ::-webkit-scrollbar {
            width: 3px !important;
            height: 3px !important;
          }
        }

        /* ── PORTRAIT ── */
        @media (max-width: 768px) and (orientation: portrait) {
          .nav-tabs-custom > .tab-content {
            overflow-x: auto !important;
            overflow-y: auto !important;
            -webkit-overflow-scrolling: touch;
          }
          .plotly.html-widget {
            min-width: 700px !important;
          }
        }

      ")),
      tags$meta(
        name    = "viewport",
        content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
      )
    ),
    
    #value boxes
    fluidRow(
      valueBoxOutput("rank_box",   width = 4),
      valueBoxOutput("growth_box", width = 4),
      valueBoxOutput("region_box", width = 4)
    ),
    
    br(),
    
    #main tabs
    fluidRow(
      tabBox(
        width = 12,
        id    = "main_tabs",
        
        tabPanel(
          title = "📈 Trend Over Time",
          div(
            "Track how internet access has changed
             over the years for your selected countries.",
            class = "info-text"
          ),
          plotlyOutput("line_plot", height = "420px")
        ),
        
        tabPanel(
          title = "📊 Country Comparison",
          div(
            "Compare selected countries side by side
             for the most recent available year.",
            class = "info-text"
          ),
          plotlyOutput("bar_plot", height = "420px")
        ),
        
        tabPanel(
          title = "💰 Wealth vs Access",
          div(
            "Does money determine internet access?
             Each dot = one country.
             ⭐ Highlighted = your selected countries.",
            class = "info-text"
          ),
          plotlyOutput("scatter_plot", height = "420px")
        ),
        
        tabPanel(
          title = "🗺️ World Map",
          div(
            "Global internet access heatmap.
             Use the year slider to see how
             the world changed over time.",
            class = "info-text"
          ),
          plotlyOutput("map_plot", height = "470px")
        ),
        
        tabPanel(
          title = "📋 Raw Data",
          div(
            "Browse, search and explore
             the complete underlying dataset.",
            class = "info-text"
          ),
          DTOutput("table")
        )
      )
    )
  )
)



##server
server <- function(input, output, session) {
  
  #country list
  output$country_ui <- renderUI({
    
    if (input$region == "All Regions") {
      choices <- sort(unique(data$`Country Name`))
    } else {
      choices <- data %>%
        filter(Continent == input$region) %>%
        pull(`Country Name`) %>%
        unique() %>%
        sort()
    }
    
    default <- intersect(
      c("Sri Lanka", "India", "United States",
        "Germany",   "Ethiopia", "China"),
      choices
    )
    
    selectInput(
      inputId  = "countries",
      label    = tags$span(
        "🌍 SELECT COUNTRIES",
        style = "color:#93c5fd;
                 font-weight:700;
                 font-size:11px;
                 letter-spacing:0.5px;"
      ),
      choices  = choices,
      selected = default,
      multiple = TRUE
    )
  })
  
  #reset part
  observeEvent(input$reset, {
    updateSelectInput(session, "region",
                      selected = "All Regions")
    updateSliderInput(session, "year",
                      value = c(2000, 2022))
    updateSelectInput(session, "metric",
                      selected = "Internet_Users_Pct")
  })
  
  #filter data
  filtered_data <- reactive({
    req(input$countries)
    data %>%
      filter(
        `Country Name` %in% input$countries,
        Year >= input$year[1],
        Year <= input$year[2]
      )
  })
  
  #clean label
  get_label <- reactive({
    names(metric_labels)[
      metric_labels == input$metric
    ]
  })
  
  #value box 1 - global data
  output$rank_box <- renderValueBox({
    req(input$countries)
    
    df_latest <- data %>%
      filter(Year == max(Year, na.rm = TRUE)) %>%
      arrange(desc(Internet_Users_Pct)) %>%
      mutate(Rank = row_number())
    
    selected <- input$countries[1]
    
    rank_val <- df_latest %>%
      filter(`Country Name` == selected) %>%
      pull(Rank)
    
    total <- nrow(df_latest)
    
    display <- if (length(rank_val) == 0 ||
                   is.na(rank_val[1])) {
      "N/A"
    } else {
      paste0("#", rank_val[1], " of ", total)
    }
    
    valueBox(
      value = tags$span(
        display,
        style = "font-size:2.2rem;
                 font-weight:900;"
      ),
      subtitle = tags$span(
        paste("🏆 Global Internet Rank —", selected),
        style = "font-size:1.3rem;
                 font-weight:600;
                 opacity:0.95;"
      ),
      icon  = icon("trophy"),
      color = "blue"
    )
  })
  
  #value box 2 - growth
  output$growth_box <- renderValueBox({
    req(input$countries)
    
    selected <- input$countries[1]
    
    df <- data %>%
      filter(`Country Name` == selected)
    
    start_val <- df %>%
      filter(Year == 2000) %>%
      pull(Internet_Users_Pct) %>%
      mean(na.rm = TRUE)
    
    end_val <- df %>%
      filter(Year == max(Year, na.rm = TRUE)) %>%
      pull(Internet_Users_Pct) %>%
      mean(na.rm = TRUE)
    
    if (is.na(start_val) || is.na(end_val)) {
      
      valueBox(
        value = tags$span(
          "No Data",
          style = "font-size:2.2rem;
                   font-weight:900;"
        ),
        subtitle = tags$span(
          paste("📈 Growth Since 2000 —", selected),
          style = "font-size:1.3rem;
                   font-weight:600;
                   opacity:0.95;"
        ),
        icon  = icon("chart-line"),
        color = "red"
      )
      
    } else {
      
      growth <- round(end_val - start_val, 1)
      
      valueBox(
        value = tags$span(
          paste0("+", growth, "%"),
          style = "font-size:2.2rem;
                   font-weight:900;"
        ),
        subtitle = tags$span(
          paste("📈 Internet Growth Since 2000 —",
                selected),
          style = "font-size:1.3rem;
                   font-weight:600;
                   opacity:0.95;"
        ),
        icon  = icon("chart-line"),
        color = "green"
      )
    }
  })
  
  #value box 3 - vs & glb av
  output$region_box <- renderValueBox({
    req(input$countries)
    
    selected <- input$countries[1]
    
    df_latest <- data %>%
      filter(Year == max(Year, na.rm = TRUE))
    
    global_avg <- mean(
      df_latest$Internet_Users_Pct,
      na.rm = TRUE
    )
    
    country_val <- df_latest %>%
      filter(`Country Name` == selected) %>%
      pull(Internet_Users_Pct) %>%
      mean(na.rm = TRUE)
    
    if (is.na(country_val)) {
      
      valueBox(
        value = tags$span(
          "No Data",
          style = "font-size:2.2rem;
                   font-weight:900;"
        ),
        subtitle = tags$span(
          "🌐 Vs Global Average",
          style = "font-size:1.3rem;
                   font-weight:600;
                   opacity:0.95;"
        ),
        icon  = icon("globe"),
        color = "red"
      )
      
    } else {
      
      diff <- round(country_val - global_avg, 1)
      
      diff_text <- ifelse(
        diff >= 0,
        paste0("+", diff, "%"),
        paste0(diff, "%")
      )
      
      col <- ifelse(diff >= 0, "green", "orange")
      
      valueBox(
        value = tags$span(
          diff_text,
          style = "font-size:2.2rem;
                   font-weight:900;"
        ),
        subtitle = tags$span(
          paste("🌐 Vs Global Average —", selected),
          style = "font-size:1.3rem;
                   font-weight:600;
                   opacity:0.95;"
        ),
        icon  = icon("globe"),
        color = col
      )
    }
  })
  
  #chart 1 - line chart
  output$line_plot <- renderPlotly({
    
    df <- filtered_data() %>%
      filter(!is.na(.data[[input$metric]]))
    
    req(nrow(df) > 0)
    
    label <- get_label()
    
    p <- ggplot(
      df,
      aes(
        x     = Year,
        y     = .data[[input$metric]],
        color = `Country Name`,
        group = `Country Name`,
        text  = paste0(
          "<b>", `Country Name`, "</b>",
          "<br>Year: ", Year,
          "<br>", label, ": ",
          round(.data[[input$metric]], 1)
        )
      )
    ) +
      geom_line(linewidth = 2) +
      geom_point(size = 3) +
      scale_color_manual(values = chart_colors) +
      scale_y_continuous(
        labels = if (input$metric == "GDP_Per_Capita")
          dollar_format()
        else
          function(x) paste0(x, "%")
      ) +
      labs(
        title = paste(label, "Trend (2000–2022)"),
        x     = "Year",
        y     = label,
        color = "Country"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background   = element_rect(
          fill = "white", color = NA),
        panel.background  = element_rect(
          fill = "#fafafa", color = NA),
        panel.grid.major  = element_line(
          color = "#e2e8f0", linewidth = 0.5),
        panel.grid.minor  = element_blank(),
        plot.title        = element_text(
          face = "bold", size = 15,
          color = "#1e3a5f"),
        axis.text         = element_text(
          color = "#64748b"),
        axis.title        = element_text(
          color = "#374151", face = "bold"),
        legend.title      = element_text(
          color = "#374151", face = "bold"),
        legend.text       = element_text(
          color = "#374151"),
        legend.background = element_rect(
          fill = "white", color = "#e2e8f0"),
        plot.margin       = margin(10, 20, 10, 10)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "#fafafa",
        font = list(
          color  = "#374151",
          family = "Segoe UI"
        ),
        legend = list(
          bgcolor     = "white",
          bordercolor = "#e2e8f0",
          borderwidth = 1
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #chart 2 - bar chart
  output$bar_plot <- renderPlotly({
    
    df <- filtered_data() %>%
      filter(!is.na(.data[[input$metric]])) %>%
      filter(Year == max(Year))
    
    req(nrow(df) > 0)
    
    label <- get_label()
    
    df <- df %>%
      arrange(.data[[input$metric]]) %>%
      mutate(
        `Country Name` = factor(
          `Country Name`,
          levels = `Country Name`
        )
      )
    
    p <- ggplot(
      df,
      aes(
        x    = `Country Name`,
        y    = .data[[input$metric]],
        fill = `Country Name`,
        text = paste0(
          "<b>", `Country Name`, "</b>",
          "<br>", label, ": ",
          round(.data[[input$metric]], 1)
        )
      )
    ) +
      geom_col(width = 0.65,
               show.legend = FALSE) +
      geom_text(
        aes(label = round(.data[[input$metric]], 1)),
        hjust    = -0.2,
        size     = 4,
        color    = "#1e3a5f",
        fontface = "bold"
      ) +
      coord_flip() +
      scale_fill_manual(values = chart_colors) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.18))
      ) +
      labs(
        title = paste(
          "Country Comparison —", label,
          "(", max(df$Year), ")"
        ),
        x = NULL,
        y = label
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background    = element_rect(
          fill = "white", color = NA),
        panel.background   = element_rect(
          fill = "#fafafa", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(
          color = "#e2e8f0"),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(
          face = "bold", size = 15,
          color = "#1e3a5f"),
        axis.text          = element_text(
          color = "#374151", face = "bold",
          size = 12),
        axis.title.x       = element_text(
          color = "#374151", face = "bold"),
        plot.margin        = margin(10, 20, 10, 10)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "#fafafa",
        font = list(
          color  = "#374151",
          family = "Segoe UI"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #chart 3 - plot
  output$scatter_plot <- renderPlotly({
    
    df <- data %>%
      filter(Year == max(input$year)) %>%
      filter(
        !is.na(GDP_Per_Capita),
        !is.na(Internet_Users_Pct),
        !is.na(Continent)
      ) %>%
      mutate(
        is_selected = `Country Name` %in%
          input$countries,
        dot_size    = ifelse(is_selected, 15, 7),
        dot_opacity = ifelse(is_selected, 1, 0.4),
        label_text  = paste0(
          "<b>", `Country Name`, "</b>",
          "<br>💰 GDP: $",
          format(round(GDP_Per_Capita),
                 big.mark = ","),
          "<br>🌐 Internet: ",
          round(Internet_Users_Pct, 1), "%",
          ifelse(is_selected,
                 "<br><b>⭐ Selected</b>", "")
        )
      )
    
    req(nrow(df) > 0)
    
    plot_ly(
      df,
      x             = ~GDP_Per_Capita,
      y             = ~Internet_Users_Pct,
      color         = ~Continent,
      colors        = continent_colors,
      size          = ~dot_size,
      text          = ~label_text,
      hovertemplate = "%{text}<extra></extra>",
      type          = "scatter",
      mode          = "markers",
      marker        = list(
        opacity = ~dot_opacity,
        line    = list(color = "white", width = 1)
      )
    ) %>%
      layout(
        title = list(
          text = paste0(
            "💰 Does Wealth Determine Internet Access? (",
            max(input$year), ")"
          ),
          font = list(
            color  = "#1e3a5f",
            size   = 15,
            family = "Segoe UI"
          )
        ),
        xaxis = list(
          title      = "GDP Per Capita (USD) — Log Scale",
          type       = "log",
          color      = "#374151",
          gridcolor  = "#e2e8f0",
          tickformat = "$,.0f"
        ),
        yaxis = list(
          title      = "Internet Users (%)",
          color      = "#374151",
          gridcolor  = "#e2e8f0",
          ticksuffix = "%"
        ),
        paper_bgcolor = "white",
        plot_bgcolor  = "#fafafa",
        font = list(
          color  = "#374151",
          family = "Segoe UI"
        ),
        legend = list(
          bgcolor     = "white",
          bordercolor = "#e2e8f0",
          borderwidth = 1,
          font        = list(color = "#374151")
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #chart 4 - world map
  output$map_plot <- renderPlotly({
    
    df <- data %>%
      filter(Year == max(input$year)) %>%
      filter(!is.na(Internet_Users_Pct))
    
    req(nrow(df) > 0)
    
    plot_ly(
      df,
      locations = ~`Country Code`,
      z         = ~Internet_Users_Pct,
      type      = "choropleth",
      colorscale = list(
        c(0,    "#fff7ed"),
        c(0.15, "#fed7aa"),
        c(0.35, "#fb923c"),
        c(0.60, "#2563eb"),
        c(0.80, "#1d4ed8"),
        c(1,    "#1e3a5f")
      ),
      text = ~paste0(
        "<b>", `Country Name`, "</b>",
        "<br>🌐 Internet: ",
        round(Internet_Users_Pct, 1), "%"
      ),
      hovertemplate = "%{text}<extra></extra>",
      colorbar = list(
        title = list(
          text = "Internet %",
          font = list(color = "#374151", size = 13)
        ),
        ticksuffix  = "%",
        tickfont    = list(color = "#374151"),
        bgcolor     = "white",
        bordercolor = "#e2e8f0",
        borderwidth = 1
      )
    ) %>%
      layout(
        title = list(
          text = paste0(
            "🌍 Global Internet Access (",
            max(input$year), ")"
          ),
          font = list(
            color  = "#1e3a5f",
            size   = 15,
            family = "Segoe UI"
          )
        ),
        geo = list(
          showframe      = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "#94a3b8",
          showland       = TRUE,
          landcolor      = "#f1f5f9",
          showocean      = TRUE,
          oceancolor     = "#dbeafe",
          showlakes      = TRUE,
          lakecolor      = "#dbeafe",
          bgcolor        = "#f0f4f8",
          projection     = list(type = "natural earth")
        ),
        paper_bgcolor = "#f0f4f8",
        font = list(
          color  = "#374151",
          family = "Segoe UI"
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  #data table
  output$table <- renderDT({
    
    df <- filtered_data() %>%
      select(
        Country            = `Country Name`,
        Region             = Continent,
        Year,
        `Internet (%)`     = Internet_Users_Pct,
        `GDP Per Capita`   = GDP_Per_Capita,
        `Mobile (per 100)` = Mobile_Subscriptions
      ) %>%
      mutate(across(where(is.numeric),
                    ~round(., 2)))
    
    datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        columnDefs = list(
          list(
            className = "dt-center",
            targets   = "_all"
          )
        )
      ),
      style    = "bootstrap4",
      rownames = FALSE
    ) %>%
      formatStyle(
        "Internet (%)",
        background = styleColorBar(
          c(0, 100), "#3b82f6"
        ),
        backgroundSize     = "100% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatCurrency(
        "GDP Per Capita",
        currency = "$",
        digits   = 0
      ) %>%
      formatStyle(
        columns    = names(df),
        color      = "#1e293b",
        fontWeight = "500",
        fontSize   = "13px"
      )
  })
}



##run app
shinyApp(ui = ui, server = server)
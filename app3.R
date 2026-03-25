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
    title = span(
      "🌍 Digital Divide Explorer",
      style = "
        font-weight: 800;
        font-size: 15px;
        color: white;
        letter-spacing: 0.5px;
      "
    )
  ),
  
  #sidebar
  dashboardSidebar(
    width = 240,
    
    div(
      style = "
        padding: 12px 15px;
        font-size: 12px;
        color: #94a3b8;
        line-height: 1.6;
        border-bottom: 1px solid #334155;
      ",
      "📊 Explore global internet access patterns
       across countries and regions."
    ),
    
    br(),
    
    #region filter
    selectInput(
      inputId  = "region",
      label    = tags$span(
        "📌 FILTER BY REGION",
        style  = "color:#93c5fd;
                  font-weight:700;
                  font-size:11px;
                  letter-spacing:0.5px;"
      ),
      choices  = continent_list,
      selected = "All Regions"
    ),
    
    uiOutput("country_ui"),
    
    hr(style = "border-color:#334155;"),
    
    #year range
    sliderInput(
      inputId = "year",
      label   = tags$span(
        "📅 YEAR RANGE",
        style = "color:#93c5fd;
                 font-weight:700;
                 font-size:11px;
                 letter-spacing:0.5px;"
      ),
      min   = 2000,
      max   = 2022,
      value = c(2000, 2022),
      sep   = ""
    ),
    
    #metric dropdown
    selectInput(
      inputId  = "metric",
      label    = tags$span(
        "📊 SELECT METRIC",
        style  = "color:#93c5fd;
                  font-weight:700;
                  font-size:11px;
                  letter-spacing:0.5px;"
      ),
      choices  = metric_labels,
      selected = "Internet_Users_Pct"
    ),
    
    hr(style = "border-color:#334155;"),
    
    #reset button
    div(
      style = "padding: 0 15px 20px;",
      actionButton(
        inputId = "reset",
        label   = "🔄 Reset All Filters",
        style   = "
          width: 100%;
          background: linear-gradient(
            135deg, #3b82f6, #1d4ed8
          );
          color: white;
          border: none;
          border-radius: 8px;
          padding: 9px;
          font-weight: 700;
          font-size: 13px;
          cursor: pointer;
          box-shadow: 0 2px 8px rgba(59,130,246,0.4);
        "
      )
    )
  ),
  

    ##body##
    dashboardBody(
    
    tags$head(
      includeCSS("style.css")
    ),
    
    # ── VALUE BOXES ─────────────────────
    fluidRow(
      valueBoxOutput("rank_box",   width = 4),
      valueBoxOutput("growth_box", width = 4),
      valueBoxOutput("region_box", width = 4)
    ),
    
    br(),
    
    # ── MAIN TABS ───────────────────────
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

# ===============================
# SERVER
# ===============================
server <- function(input, output, session) {
  
  # ── DYNAMIC COUNTRY LIST ─────────────
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
  
  # ── RESET ────────────────────────────
  observeEvent(input$reset, {
    updateSelectInput(session, "region",
                      selected = "All Regions")
    updateSliderInput(session, "year",
                      value = c(2000, 2022))
    updateSelectInput(session, "metric",
                      selected = "Internet_Users_Pct")
  })
  
  # ── FILTERED DATA ────────────────────
  filtered_data <- reactive({
    req(input$countries)
    data %>%
      filter(
        `Country Name` %in% input$countries,
        Year >= input$year[1],
        Year <= input$year[2]
      )
  })
  
  # ── GET CLEAN LABEL ──────────────────
  get_label <- reactive({
    names(metric_labels)[
      metric_labels == input$metric
    ]
  })
  
  # ── VALUE BOX 1: GLOBAL RANK ─────────
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
        style = "font-size:1rem;
                 font-weight:600;
                 opacity:0.95;"
      ),
      icon  = icon("trophy"),
      color = "blue"
    )
  })
  
  # ── VALUE BOX 2: GROWTH ──────────────
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
          style = "font-size:1rem;
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
          style = "font-size:1rem;
                   font-weight:600;
                   opacity:0.95;"
        ),
        icon  = icon("chart-line"),
        color = "green"
      )
    }
  })
  
  # ── VALUE BOX 3: VS GLOBAL AVG ───────
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
          style = "font-size:1rem;
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
          style = "font-size:1rem;
                   font-weight:600;
                   opacity:0.95;"
        ),
        icon  = icon("globe"),
        color = col
      )
    }
  })
  
  # ── CHART 1: LINE CHART ──────────────
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
  
  # ── CHART 2: BAR CHART ───────────────
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
  
  # ── CHART 3: SCATTER PLOT ────────────
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
  
  # ── CHART 4: WORLD MAP ───────────────
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
  
  # ── DATA TABLE ───────────────────────
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

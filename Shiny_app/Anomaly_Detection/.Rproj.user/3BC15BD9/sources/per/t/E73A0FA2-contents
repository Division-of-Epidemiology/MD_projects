
library(leaflet)
library(sf)
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(plotly)
library(forcats)

##OVERDOSE SURVIELLANCE## ##AT THE ZIP CODE LEVEL##
# ── 1. Load & Prepare All Three Datasets ─────────────────────
load_dataset <- function(path, date_col, zip_col, label) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$date_parsed <- as.Date(df[[date_col]], format = "%m/%d/%Y")
  df$zip_parsed  <- as.character(df[[zip_col]])
  
  daily <- df %>%
    filter(!is.na(date_parsed)) %>%
    group_by(date = date_parsed) %>%
    summarise(cases = n(), .groups = "drop") %>%
    mutate(
      source = label,
      year   = lubridate::year(date),
      month  = lubridate::month(date, label = TRUE, abbr = TRUE),
      dow    = lubridate::wday(date, label = TRUE, abbr = TRUE, week_start = 1),
      week   = lubridate::isoweek(date)
    )
  
  zip_daily <- df %>%
    filter(!is.na(date_parsed), !is.na(zip_parsed), zip_parsed != "") %>%
    mutate(
      date = date_parsed,
      zip  = zip_parsed,
      year = lubridate::year(date_parsed)
    ) %>%
    select(date, zip, year)
  
  list(daily = daily, zip = zip_daily)
}

ed_raw  <- load_dataset(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ESSENCE_2326.csv",
  "Date", "Zipcode", "ED"
)
ems_raw <- load_dataset(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/EMS_2326.csv",
  "Incident_Date", "Zip", "EMS"
)
me_raw  <- load_dataset(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ME_2326.csv",
  "Exam_Date", "Zip", "ME"
)

dataset_list <- list(
  "ED — Emergency Department"        = ed_raw$daily,
  "EMS — Emergency Medical Services" = ems_raw$daily,
  "ME — Medical Examiner"            = me_raw$daily
)

zip_list <- list(
  "ED — Emergency Department"        = ed_raw$zip,
  "EMS — Emergency Medical Services" = ems_raw$zip,
  "ME — Medical Examiner"            = me_raw$zip
)

# Davidson County zip codes
davidson_zips <- c(
  "37013","37027","37072","37076","37080","37115","37116",
  "37138","37143","37189","37201","37203","37204","37205",
  "37206","37207","37208","37209","37210","37211","37212",
  "37213","37214","37215","37216","37217","37218","37219",
  "37220","37221","37228","37229","37232","37234","37235",
  "37236","37238","37240","37241","37242","37243","37244",
  "37246","37250"
)

# Load shapefile (adjust path to where your .shp file is)
zip_shapes <- st_read("C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/Zip_Boundaries/Zip_Codes_Boundaries.shp") %>%
  st_transform(crs=4326) %>%
  mutate(zip = as.character(ZipCode)) 


# ── 2. Classify Days (SD-based) ──────────────────────────────
classify_days <- function(df, window = 30) {
  df <- df %>% arrange(date)
  df$roll_avg <- NA_real_
  df$roll_sd  <- NA_real_
  for (i in seq_len(nrow(df))) {
    start_i        <- max(1, i - window)
    df$roll_avg[i] <- mean(df$cases[start_i:(i-1)], na.rm = TRUE)
    df$roll_sd[i]  <- sd(df$cases[start_i:(i-1)],   na.rm = TRUE)
  }
  df$roll_avg[is.nan(df$roll_avg)] <- mean(df$cases)
  df$roll_sd[is.na(df$roll_sd)]    <- sd(df$cases)
  df %>% mutate(
    threshold_warn  = roll_avg + roll_sd,
    threshold_alert = roll_avg + 2 * roll_sd,
    status = case_when(
      cases >= threshold_alert ~ "Anomaly Detected",
      cases >= threshold_warn  ~ "Elevated",
      TRUE                     ~ "Normal"
    ),
    color_hex = case_when(
      status == "Anomaly Detected" ~ "#E63946",
      status == "Elevated"         ~ "#F4A261",
      TRUE                         ~ "#2A9D8F"
    )
  )
}

# ── 3. UI ─────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Merriweather:wght@700;900&family=Roboto:wght@400;500;600&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
      body { background:#f4f6f8; 
      color:#1a1a2e; 
      font-family:'Roboto',sans-serif; 
      font-size:25px; 
      min-height:100vh; }
      
      .dash-header { background:#ffffff; 
      border-bottom:2px solid #e0e4ea; 
      padding:24px 36px 20px; 
      display:flex; 
      align-items:flex-end; 
      gap:18px; }
      
      .dash-header h1 { font-family:'Merriweather',serif; 
      font-size:3rem; 
      font-weight:900; 
      color:#000000; 
      line-height:1; }
      
      .dash-header .subtitle { font-size:1.5rem; 
      color:#555e6d; 
      letter-spacing:0.12em; 
      text-transform:uppercase; 
      margin-bottom:4px; 
      font-weight:600; }
      
      .dataset-badge { display:inline-block; 
      background:#1d6ae5; 
      color:#ffffff; 
      border-radius:6px; 
      padding:5px 14px; 
      font-size:1.5rem; 
      font-weight:700; 
      letter-spacing:0.06em; 
      text-transform:uppercase; 
      align-self:flex-end;
      margin-bottom: 8px;
      margin-left: 12px; }
      
      
      .controls-bar { background:#ffffff; 
      border-bottom:1px solid #e0e4ea; 
      padding:14px 36px; 
      display:flex; 
      align-items:center; 
      gap:28px; 
      flex-wrap:wrap; }
      
      .controls-bar label { font-size:1.5rem; 
      letter-spacing:0.06rem; 
      text-transform:uppercase; 
      color:#333d4a; 
      margin-bottom:4px; 
      display:block; 
      font-weight:700; }
      
      .controls-bar .form-control, .controls-bar .selectize-input { background:#f4f6f8 !important; 
      border:1px solid #c8d0da !important; 
      color:#1a1a2e !important; 
      border-radius:6px !important; 
      font-family:'Roboto',sans-serif !important; 
      font-size:1.3rem !important; 
      padding:6px 10px !important; }
      
      
      .shiny-input-container { margin-bottom:0 !important; }
      
      .legend-row { display:flex; 
      align-items:center; 
      gap:20px; 
      margin-left:auto; }
      
      .legend-item { display:flex; 
      align-items:center; 
      gap:7px; 
      font-size:1.3rem; 
      color:#333d4a; 
      font-weight:600; }
      
      .legend-dot { width:12px; 
      height:12px; 
      border-radius:50%; 
      flex-shrink:0; }
      
      .kpi-row { display:flex; 
      gap:16px; 
      padding:20px 36px; 
      flex-wrap:wrap; }
      
      .kpi-card { background:#ffffff; 
      border:1px solid #e0e4ea; 
      border-radius:10px; 
      padding:16px 22px; 
      flex:1; 
      min-width:140px; 
      box-shadow:0 1px 4px rgba(0,0,0,0.06); }
      
      .kpi-card .kpi-label { font-size:1.3rem; 
      letter-spacing:0.08em; 
      text-transform:uppercase; 
      color:#555e6d; 
      margin-bottom:6px; 
      font-weight:700; }
      
      .kpi-card .kpi-value { font-family:'Merriweather',serif; 
      font-size:2.5rem; 
      font-weight:900; 
      line-height:1; 
      color:#000000; }
      
      .kpi-card .kpi-sub { font-size:1.3rem; 
      color:#555e6d; 
      margin-top:4px; 
      font-weight:500; }
      
      .kpi-card.red   { border-left:4px solid #E63946; }
      .kpi-card.amber { border-left:4px solid #F4A261; }
      .kpi-card.green { border-left:4px solid #2A9D8F; }
      .kpi-card.blue  { border-left:4px solid #1d6ae5; }
      
      
      .panel-grid { display:grid; 
      grid-template-columns:1fr 1fr; 
      gap:20px; 
      padding:0 36px 36px; }
      
      
      @media (max-width:900px) { .panel-grid { grid-template-columns:1fr; } }
      
      .panel { background:#ffffff; 
      border:1px solid #e0e4ea; 
      border-radius:10px; 
      overflow:hidden; 
      box-shadow:0 1px 4px rgba(0,0,0,0.06); }
      
      .panel-full { background:#ffffff; 
      border:1px solid #e0e4ea; 
      border-radius:10px; 
      overflow:hidden; 
      margin:0 36px 20px; 
      box-shadow:0 1px 4px rgba(0,0,0,0.06); }
      
      .panel-header { padding:14px 20px 12px; 
      border-bottom:1px solid #e0e4ea; 
      display:flex; 
      align-items:center; 
      gap:10px; 
      background:#f8f9fb; }
      
      .panel-header h2 { font-family:'Merriweather',serif; 
      font-size:3rem; 
      font-weight:700; 
      color:#000000; }
      
      .panel-header .panel-badge { font-size:1.1rem; 
      background:#e8edf3; 
      border:1px solid #c8d0da; 
      color:#444e5c; 
      border-radius:4px; 
      padding:4px 10px; 
      letter-spacing:0.06em; 
      text-transform:uppercase; 
      margin-left:auto; 
      font-weight:600; }
      
      .panel-body { padding:8px 4px; }
      
      .shiny-html-output table { width:100%; 
      border-collapse:collapse; 
      font-size:1.5rem; }
      
      .shiny-html-output table th { padding:9px 16px; 
      text-align:left; 
      font-size:1.1rem; 
      letter-spacing:0.08em; 
      text-transform:uppercase; 
      color:#333d4a; 
      border-bottom:2px solid #e0e4ea; 
      font-weight:700; 
      background:#f8f9fb; }
      
      .shiny-html-output table td { padding:9px 16px; 
      border-bottom:1px solid #f0f2f5; 
      color:#1a1a2e; 
      font-weight:500; }
      
      .shiny-html-output table tr:last-child td { border-bottom:none; }
      
      .shiny-html-output table tr:hover td { background:#f4f6f8; }
      
      ::-webkit-scrollbar { width:6px; }
      
      ::-webkit-scrollbar-track { background:#f4f6f8; }
      
      ::-webkit-scrollbar-thumb { background:#c8d0da; border-radius:3px; }
      
      .selectize-dropdown { background:#ffffff !important; border:1px solid #c8d0da !important; color:#1a1a2e !important; }
      .selectize-dropdown .option { color:#1a1a2e !important; font-size:0.85rem !important; }
      .selectize-dropdown .option:hover, .selectize-dropdown .option.active { background:#f4f6f8 !important; }
    "))
  ),
  
  div(class = "dash-header",
      div(
        div(class = "subtitle", "Overdose Surveillance System"),
        tags$h1("Daily Case Monitoring for Anomaly Detection")
      ),
      uiOutput("dataset_badge")
  ),
  
  div(class = "controls-bar",
      div(
        tags$label("Dataset"),
        selectInput("sel_dataset", NULL, choices = names(dataset_list), selected = names(dataset_list)[1], width = "280px")
      ),
      div(
        tags$label("Year"),
        selectInput("sel_year", NULL, choices = NULL, width = "110px")
      ),
      div(
        tags$label("Rolling window (days)"),
        sliderInput("window", NULL, min = 7, max = 60, value = 30, step = 1, width = "200px")
      ),
      div(class = "legend-row",
          div(class = "legend-item", div(class = "legend-dot", style = "background:#2A9D8F;"), "Normal  (< 1 SD)"),
          div(class = "legend-item", div(class = "legend-dot", style = "background:#F4A261;"), "Elevated  (≥ 1 SD)"),
          div(class = "legend-item", div(class = "legend-dot", style = "background:#E63946;"), "Anomaly Detected  (≥ 2 SD)")
      )
  ),
  
  div(class = "kpi-row",
      div(class = "kpi-card blue",
          div(class = "kpi-label", "Total Cases"),
          div(class = "kpi-value", textOutput("kpi_total", inline = TRUE)),
          div(class = "kpi-sub", "selected year")
      ),
      div(class = "kpi-card red",
          div(class = "kpi-label", "Anomaly Detected Days"),
          div(class = "kpi-value", textOutput("kpi_high", inline = TRUE)),
          div(class = "kpi-value", style="font-size:1rem; margin-top:2px;", textOutput("kpi_high_pct", inline=TRUE)),
          div(class = "kpi-sub", "≥ 2 SD above rolling avg")
      ),
      div(class = "kpi-card amber",
          div(class = "kpi-label", "Elevated Days"),
          div(class = "kpi-value", textOutput("kpi_elev", inline = TRUE)),
          div(class = "kpi-value", style="font-size:1rem; margin-top:2px;", textOutput("kpi_elev_pct", inline=TRUE)),
          div(class = "kpi-sub", "≥ 1 SD above rolling avg")
      ),
      div(class = "kpi-card green",
          div(class = "kpi-label", "Peak Day"),
          div(class = "kpi-value", textOutput("kpi_peak", inline = TRUE)),
          div(class = "kpi-sub", textOutput("kpi_peak_date", inline = TRUE))
      ),
      div(class = "kpi-card",
          div(class = "kpi-label", "Daily Average"),
          div(class = "kpi-value", textOutput("kpi_avg", inline = TRUE)),
          div(class = "kpi-sub", "cases / day")
      )
  ),
  
  div(class = "panel-full",
      div(class = "panel-header",
          tags$h2("📈 Daily Case Trend"),
          div(class = "panel-badge", "interactive")
      ),
      div(class = "panel-body", plotlyOutput("trend_plot", height = "280px"))
  ),
  
  div(class = "panel-grid",
      div(class = "panel",
          div(class = "panel-header",
              tags$h2("📅 Calendar Heatmap"),
              div(class = "panel-badge", "by week")
          ),
          div(class = "panel-body", plotOutput("calendar_plot", height = "520px"))
      ),
      div(class = "panel",
          div(class = "panel-header",
              tags$h2("🔴 Highest Case Days"),
              div(class = "panel-badge", "top 15")
          ),
          div(style = "overflow-x:auto;", tableOutput("top_table"))
      )
  ),


# ── Map + Zip table
div(class = "panel-grid", style = "padding-bottom:36px;",
    div(class = "panel",
        div(class = "panel-header",
            tags$h2("🗺️ Cases by Zip Code"),
            div(class = "panel-badge", "interactive map")
        ),
        div(class = "panel-body", leafletOutput("zip_map", height = "420px"))
    ),
    div(class = "panel",
        div(class = "panel-header",
            tags$h2("📍 Top 10 Zip Codes"),
            div(class = "panel-badge", "by total cases")
        ),
        div(style = "overflow-x:auto;", tableOutput("zip_table"))
    )
)
)


# ── 4. Server ─────────────────────────────────────────────────
server <- function(input, output, session) {
  
  observeEvent(input$sel_dataset, {
    ds  <- dataset_list[[input$sel_dataset]]
    yrs <- sort(unique(ds$year))
    updateSelectInput(session, "sel_year", choices = yrs, selected = max(yrs))
  })
  
  output$dataset_badge <- renderUI({
    lbl <- switch(input$sel_dataset,
                  "ED — Emergency Department"         = "ED",
                  "EMS — Emergency Medical Services"  = "EMS",
                  "ME — Medical Examiner"             = "ME", "ED")
    div(class = "dataset-badge", lbl)
  })
  
  yr_data <- reactive({
    req(input$sel_year)
    dataset_list[[input$sel_dataset]] %>%
      filter(year == as.integer(input$sel_year)) %>%
      classify_days(window = input$window)
  })
  
  # Zip reactive — filtered by year and Davidson County only
  zip_data <- reactive({
    req(input$sel_year)
    zip_list[[input$sel_dataset]] %>%
      filter(
        year == as.integer(input$sel_year),
        zip  %in% davidson_zips
      ) %>%
      group_by(zip) %>%
      summarise(cases = n(), .groups = "drop") %>%
      arrange(desc(cases))
  })
  
  output$kpi_total    <- renderText({ format(sum(yr_data()$cases), big.mark = ",") })
  output$kpi_high     <- renderText({ sum(yr_data()$status == "Anomaly Detected") })
  output$kpi_high_pct <- renderText({
    n <- nrow(yr_data()); h <- sum(yr_data()$status == "Anomaly Detected")
    paste0("(", round(100*h/n, 1), "%)")
  })
  output$kpi_elev     <- renderText({ sum(yr_data()$status == "Elevated") })
  output$kpi_elev_pct <- renderText({
    n <- nrow(yr_data()); e <- sum(yr_data()$status == "Elevated")
    paste0("(", round(100*e/n, 1), "%)")
  })
  output$kpi_peak      <- renderText({ max(yr_data()$cases) })
  output$kpi_peak_date <- renderText({
    d <- yr_data() %>% filter(cases == max(cases)) %>% slice(1)
    format(d$date, "%b %d")
  })
  output$kpi_avg <- renderText({ round(mean(yr_data()$cases), 1) })
  
  output$trend_plot <- renderPlotly({
    df <- yr_data()
    plot_ly(df, x = ~date) %>%
      add_bars(y = ~cases, marker = list(color = ~color_hex, opacity = 0.85),
               text = ~paste0(format(date, "%b %d, %Y"), "<br>Cases: ", cases,
                              "<br>Status: ", status, "<br>Roll. Avg: ", round(roll_avg, 1)),
               hoverinfo = "text", name = "Daily cases", showlegend = FALSE) %>%
      add_lines(y = ~roll_avg, line = list(color = "#1d6ae5", width = 2, dash = "dot"),
                name = "Rolling avg", hoverinfo = "skip") %>%
      add_lines(y = ~threshold_alert, line = list(color = "#E63946", width = 1.5, dash = "dash"),
                name = "Anomaly threshold (2 SD)", hoverinfo = "skip") %>%
      add_lines(y = ~threshold_warn, line = list(color = "#F4A261", width = 1, dash = "dash"),
                name = "Elevated threshold (1 SD)", hoverinfo = "skip") %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
        font  = list(color = "#333d4a", family = "'Roboto', sans-serif", size = 12),
        xaxis = list(gridcolor = "#f0f2f5", zerolinecolor = "#e0e4ea", tickfont = list(color = "#333d4a", size = 12)),
        yaxis = list(title = "Daily Cases", gridcolor = "#f0f2f5", zerolinecolor = "#e0e4ea",
                     tickfont = list(color = "#333d4a", size = 12), titlefont = list(color = "#333d4a", size = 12)),
        legend = list(orientation = "h", x = 0, y = 1.12, font = list(color = "#333d4a", size = 12)),
        bargap = 0.15, margin = list(l = 50, r = 20, t = 10, b = 40), hovermode = "closest"
      ) %>% config(displayModeBar = FALSE)
  })
  
  output$calendar_plot <- renderPlot({
    df <- yr_data() %>%
      mutate(
        week_of_year = isoweek(date),
        dow_label    = factor(wday(date, label=TRUE, abbr=TRUE, week_start=1),
                              levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
      ) %>%
      group_by(year) %>% mutate(week_num = dense_rank(week_of_year)) %>% ungroup()
    
    month_labels <- df %>% group_by(month) %>% summarise(week_num = min(week_num), .groups = "drop")
    
    df <- df %>%
      mutate(
        week_of_month = ceiling(day(date) / 7),
        week_label    = paste0("W", week_of_month)
      )
    
    ggplot(df, aes(x = as.factor(mday(date)), y = fct_rev(month))) +
      geom_point(
        aes(color = status, size = cases, alpha = cases),
        shape = 16
      ) +
      scale_color_manual(
        values = c("Normal" = "#2A9D8F", "Elevated" = "#F4A261", "Anomaly Detected" = "#E63946"),
        guide  = "none"
      ) +
      scale_size_continuous(range = c(3, 9), guide = "none") +
      scale_alpha_continuous(range = c(0.55, 1), guide = "none") +
      scale_x_discrete(
        breaks = as.character(c(1, 5, 10, 15, 20, 25, 30)),
        labels = c("1","5","10","15","20","25","30"),
        expand = expansion(add = c(0.5, 0.5))
      ) +
      scale_y_discrete(expand = expansion(add = c(0.8, 0.8))) +
      labs(x = "Day of Month", y = NULL) +
      theme_minimal(base_family = "roboto") +
      theme(
        plot.background  = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major = element_line(color = "#f0f2f5"),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_text(color = "#1a1a2e", size = 15, face = "bold"),
        axis.text.x      = element_text(color = "#1a1a2e", size = 15, face = "bold"),
        axis.title.x     = element_text(color = "#555e6d", size = 15, face="bold"),
        plot.margin      = margin(20, 24, 20, 24)
      )
  }, bg = "#ffffff")
  
  output$top_table <- renderTable({
    yr_data() %>% arrange(desc(cases)) %>% slice_head(n = 15) %>%
      mutate(Date = format(date, "%b %d, %Y"), Cases = cases,
             `Roll. Avg` = round(roll_avg, 1),
             `vs Avg` = paste0(ifelse(cases >= roll_avg, "+", ""), round(cases - roll_avg, 1)),
             Status = status) %>%
      select(Date, Cases, `Roll. Avg`, `vs Avg`, Status)
  }, striped = FALSE, hover = TRUE, bordered = FALSE, spacing = "s", align = "l", rownames = FALSE)


  output$zip_map <- renderLeaflet({
    zd <- zip_data()
    
    map_df <- zip_shapes %>%
      left_join(zd, by = "zip") %>%
      mutate(cases = ifelse(is.na(cases), 0, cases))
    
    pal <- colorNumeric(
      palette = c("#2A9D8F", "#F4A261", "#E63946"),
      domain  = map_df$cases
    )
    
    leaflet(map_df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -86.7816, lat = 36.1627, zoom = 11) %>%
      addPolygons(
        fillColor   = ~pal(cases),
        fillOpacity = 0.7,
        color       = "#ffffff",
        weight      = 1.5,
        popup       = ~paste0("<b>Zip: ", zip, "</b><br>Cases: <b>", cases, "</b>"),
        label       = ~paste0(zip, ": ", cases, " cases"),
        highlightOptions = highlightOptions(
          weight      = 3,
          color       = "#1d6ae5",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ~cases,
        title    = "Cases",
        opacity  = 0.8
      )
  })

# ── Top 10 zip code table
output$zip_table <- renderTable({
  zd <- zip_data() %>%
    slice_head(n = 10) %>%
    mutate(
      Rank      = row_number(),
      `Zip Code` = zip,
      Cases     = cases,
      `% of Total` = paste0(round(100 * cases / sum(zip_data()$cases), 1), "%")
    ) %>%
    select(Rank, `Zip Code`, Cases, `% of Total`)
  zd
}, striped = FALSE, hover = TRUE, bordered = FALSE, spacing = "s", align = "l", rownames = FALSE)
}

shinyApp(ui, server)
#| standalone: true
#| viewerHeight: 600
#| runtime: shiny

library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)

#df <- read_csv("HUC07030004_EQuIS_sample.csv") %>% mutate(SAMPLE_DATE = mdy_hm(SAMPLE_DATE))

df <- read_csv("https://raw.githubusercontent.com/dKvale/shinylive-test/refs/heads/main/simpler_df.csv")


# UI ----
ui <- fluidPage(
  fluidRow(
    column(4,
           selectInput("chemical", "Select a Parameter", choices = unique(df$CHEMICAL_NAME) %>% sort,
                       selected = "Alkalinity, total")),
    column(5, uiOutput("location_ui"))
  ),
  
  tabsetPanel(
    tabPanel("Parameter list",
             br(),
             DTOutput("paramTable")
    ),
    tabPanel("Data Explorer",
  br(),
  fluidRow(
    column(6,
           tags$h4("Time Series"),
           plotlyOutput("timePlot")
    ),
    column(6,
           tags$h4("Monthly Comparisons"),
           plotlyOutput("boxPlot")
    )
  ),
  br(),
  fluidRow(
    column(12,
           tags$h4("Summary Table"),
           DTOutput("summaryTable")
    )
  )
),
    tabPanel("All data",
             br(),
             DTOutput("fullTable")
    )
  )
)


# Server ----
server <- function(input, output, session) {
  
  # Filtered data
  filtered_df <- reactive({
    req(input$chemical)
    
    df %>%
      filter(CHEMICAL_NAME == input$chemical,
             is.null(input$location) | LOCATION == "All" | LOCATION == input$location)
  })
  
  # Dynamically update location choices based on chemical
  output$location_ui <- renderUI({
    
    locations <- if (is.null(input$chemical) || input$chemical == "All") {
      df$LOCATION
    } else {
      df[df$CHEMICAL_NAME == input$chemical, "LOCATION", drop = TRUE]
    }
    
    selectInput(
      "location",
      "Select a Station",
      choices = c("All", sort(unique(locations))),
      selected = "ANN Lake :: 33-0040-00-201"
    )
  })
  
  # Parameter table
  output$paramTable <- renderDT({
    
    df %>% 
    group_by(SYS_LOC_CODE) %>% 
    summarize(LOC_NAME = LOC_NAME[1],
              LOC_TYPE = LOC_TYPE[1],
              PARAMETERS = unique(CHEMICAL_NAME) %>% sort %>% paste(collapse = ", ")
              #YEARS = unique(year(SAMPLE_DATE)) %>% sort %>% paste(collapse = ", "),
              #STATIONS = unique(SYS_LOC_CODE) %>% sort %>% paste(collapse = ", ")
    ) %>% 
    arrange(LOC_NAME) %>%
    datatable(options = list(pageLength = 7, scrollX = TRUE), 
              filter = "top",
              rownames = FALSE)
  })
  
  # Time series line plot
  output$timePlot <- renderPlotly({
    plot_ly(filtered_df(), 
            x = ~SAMPLE_DATE, y = ~RESULT_NUMERIC,
            type = "scatter", 
            mode = "lines+markers",
            #color = ~location,
            line = list(shape = "spline"),
            connectgaps = TRUE) %>%
      layout(title = "Results Over Time")
  })
  
  # Monthly boxplot
  output$boxPlot <- renderPlotly({
    plot_ly(filtered_df(),
            x = ~MONTH, y = ~RESULT_NUMERIC,
            type = "box",
            boxpoints = "all",
            jitter = 0.1,
            pointpos = -1.5) %>%
      layout(title = "Results by Month")
  })
  
  # Summary table
  output$summaryTable <- renderDT({
    
    df <- filtered_df()
    
    summary <- df %>%
      summarise(
        N = n(),
        Min = min(RESULT_NUMERIC, na.rm = TRUE),
        Mean = signif(mean(RESULT_NUMERIC, na.rm = TRUE), 3),
        Median = median(RESULT_NUMERIC, na.rm = TRUE),
        Max = max(RESULT_NUMERIC, na.rm = TRUE),
        SD = signif(sd(RESULT_NUMERIC, na.rm = TRUE), 3),
        NA_Count = sum(is.na(RESULT_NUMERIC))
      )
    
    datatable(summary, options = list(dom = 't'))
  })
  
  # Full table
  output$fullTable <- renderDT({
    
  datatable(df %>% 
              select(CHEMICAL_NAME, LOC_NAME, everything()) %>%
              select(-FACILITY_CODE, -FACILITY_NAME, -FACILITY_ID) %>% 
              arrange(CHEMICAL_NAME), 
            options = list(pageLength = 5, scrollX = TRUE), 
            filter = "top",
            rownames = FALSE)
  })
}


shinyApp(ui, server)


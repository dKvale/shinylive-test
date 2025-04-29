#| standalone: true
#| viewerHeight: 600
#| runtime: shiny

library(shiny)
library(plotly)
library(DT)
library(dplyr)

#df <- read_csv("HUC07030004_EQuIS_sample.csv") %>% mutate(SAMPLE_DATE = mdy_hm(SAMPLE_DATE))

df <- tibble::tribble(~FACILITY_CODE,  ~FACILITY_TYPE, ~FACILITY_ID,                                               ~FACILITY_NAME,    ~SYS_LOC_CODE, ~STREAM_CODE, ~LOC_NAME, ~LOC_TYPE, ~LOC_DESC, ~LOC_COUNTY_CODE, ~LOC_MAJOR_BASIN,   ~X_COORD,  ~Y_COORD, ~COORD_TYPE_CODE,          ~SAMPLE_DATE, ~SAMPLE_TYPE_CODE,                 ~SYS_SAMPLE_CODE, ~START_DEPTH, ~DEPTH_UNIT, ~MEDIUM_CODE, ~MATRIX_CODE, ~REPORTABLE_YN, ~TASK_CODE, ~TASK_CODE_2,          ~TASK_DESC,         ~CONTACT, ~DTTASK_TC,      ~CAS_RN,                                ~CHEMICAL_NAME, ~ANALYTIC_METHOD, ~FRACTION, ~DETECT_FLAG,  ~RESULT_TEXT, ~RESULT_NUMERIC, ~RESULT_UNIT, ~REPORTABLE_RESULT, ~INTERPRETED_QUALIFIERS, ~REPORTING_DETECTION_LIMIT, ~METHOD_DETECTION_LIMIT, ~DETECTION_LIMIT_UNIT, ~APPROVAL_CODE, ~VALUE_TYPE, ~REMARK, ~DQM_REMARK, ~LAB_NAME_CODE, ~LAB_CODE, ~COMPANY_CODE,                     ~LOCATION, ~MONTH,
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-18 12:30:00",          "Sample", "01-0064-00-201.1809181230.000S",           NA,          NA,      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049", "14808-79-8",                                     "Sulfate",           "D516",   "Total",          "N",            NA,              NA,       "mg/L",                "Y",                     "<",                          3,                   0.751,                "mg/L",        "Final",    "Actual",      NA,          NA,      "MN00918",        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-18 12:30:00",          "Sample", "01-0064-00-201.1809181230.000S",           NA,          NA,      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",  "7723-14-0",                                  "Phosphorus",          "365.3",   "Total",          "Y",       "0.026",           0.026,       "mg/L",                "Y",                      NA,                      0.003,                   0.001,                "mg/L",        "Final",    "Actual",      NA,          NA,      "MN00918",        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-18 12:30:00",          "Sample", "01-0064-00-201.1809181230.000S",           NA,          NA,      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",  "CHLA-CORR",     "Chlorophyll a, corrected for pheophytin",        "10200-H",   "Total",          "Y",        "10.7",            10.7,       "ug/L",                "Y",                      NA,                          1,                      NA,                "ug/L",        "Final",    "Actual",    "nc",          NA,      "MN00918",        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-18 12:30:00",             "FMO", "01-0064-00-201.1809181230.000F",            0,         "m",      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",     "SECCHI",                    "Depth, Secchi disk depth",            "FLD",   "Total",          "Y",        "0.81",            0.81,          "m",                "Y",                      NA,                         NA,                      NA,                    NA,        "Final",    "Actual",      NA,          NA,             NA,        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-18 12:30:00",             "FMO", "01-0064-00-201.1809181230.000F",            0,         "m",      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",    "LAKE-RS", "Lake recreational suitability (choice list)",            "FLD",   "Total",          "Y", "1.VERY GOOD",              NA,           NA,                "Y",                      NA,                         NA,                      NA,                    NA,        "Final",    "Actual",      NA,          NA,             NA,        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-18 12:30:00",             "FMO", "01-0064-00-201.1809181230.000F",            0,         "m",      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",    "LAKE-PA",      "Lake physical appearance (choice list)",            "FLD",   "Total",          "Y", "3.MED ALGAE",              NA,           NA,                "Y",                      NA,                         NA,                      NA,                    NA,        "Final",    "Actual",      NA,          NA,             NA,        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-05 14:00:00",             "FMO", "01-0064-00-201.1809051400.000F",            0,         "m",      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",    "LAKE-PA",      "Lake physical appearance (choice list)",            "FLD",   "Total",          "Y", "3.MED ALGAE",              NA,           NA,                "Y",                      NA,                         NA,                      NA,                    NA,        "Final",    "Actual",      NA,          NA,             NA,        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-05 14:00:00",          "Sample", "01-0064-00-201.1809051400.000S",           NA,          NA,      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",  "CHLA-CORR",     "Chlorophyll a, corrected for pheophytin",        "10200-H",   "Total",          "Y",        "9.79",            9.79,       "ug/L",                "Y",                      NA,                          1,                      NA,                "ug/L",        "Final",    "Actual",    "nc",          NA,      "MN00918",        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-05 14:00:00",             "FMO", "01-0064-00-201.1809051400.000F",            0,         "m",      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",     "SECCHI",                    "Depth, Secchi disk depth",            "FLD",   "Total",          "Y",        "0.76",            0.76,          "m",                "Y",                      NA,                         NA,                      NA,                    NA,        "Final",    "Actual",      NA,          NA,             NA,        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep",
                                                "MNPCA", "Statewide-ASW",            1, "Minnesota Pollution Control Agency - Ambient Surface Water", "01-0064-00-201", "01-0064-00",    "BEAR",    "Lake",    "Bear",         "Aitkin",       "07030004", -93.370456, 46.239632,       "LAT-LONG", "2018-09-05 14:00:00",             "FMO", "01-0064-00-201.1809051400.000F",            0,         "m",      "Water",   "Wtr-Surf",             NA, "PRJ08049",   "PRJ08049", "Kanabec SWCD SWAG",  "O'Hara, Kelly", "PRJ08049",    "LAKE-RS", "Lake recreational suitability (choice list)",            "FLD",   "Total",          "Y", "1.VERY GOOD",              NA,           NA,                "Y",                      NA,                         NA,                      NA,                    NA,        "Final",    "Actual",      NA,          NA,             NA,        NA,            NA, "BEAR Lake :: 01-0064-00-201",  "Sep"
                                         )


# UI ----
ui <- fluidPage(
  fluidRow(
    column(4,
           selectInput("chemical", "Select a Parameter", choices = unique(df$CHEMICAL_NAME) %>% sort)),
    column(5, uiOutput("location_ui"))
  ),
  
  tabsetPanel(
    tabPanel("Parameter list",
             br(),
             DTOutput("paramTable")
    ),
    tabPanel("Time Series",
             br(),
             plotlyOutput("timePlot")
    ),
    tabPanel("Monthly Comparisons",
             br(),
             plotlyOutput("boxPlot")
    ),
    tabPanel("Summary Table",
             DTOutput("summaryTable")
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
      selected = isolate(input$location)  # preserve selection on re-render
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


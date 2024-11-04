
knitr::opts_chunk$set(echo = TRUE)



rm(list = ls())
# Install and load required packages
install_packages_if_not_present <- function(x) {
  if (sum(!x %in% installed.packages())) {
    install.packages(x[!x %in% installed.packages()])
  }
}
packages <- c("spData","leaflet",'leaflet.extras', "shiny","data.table",'DT',
              'readxl','readr','dplyr','sf','ggplot2','plotly'
)
install_packages_if_not_present(packages)
sapply(packages, require, character.only = TRUE)

# Data1: Hospital level data
data <- read_csv('https://raw.githubusercontent.com/wqian26/436-project/refs/heads/main/dataset.csv')
data <- data %>%
  # clean up the names
  rename(
    hrr = `HRR`,
    city = `City`,
    state = `State`,
    system = `System`,
    provider_id = `Provider ID`,
    hospital_name = `Hospital Name`,
    deaths_chronically_ill = `Number of deaths among chronically ill patients assigned to hospital`,
    inpatient_days = `Inpatient Days per Decedent during the Last Two Years of Life`,
    icu_deaths = `Percent of Deaths Associated With ICU Admission`,
    hospice_enrolled = `Percent of Decedents Enrolled In Hospice during the Last Six Months of Life`,
    pct_inpatient_days = `Percent of enrollees medical inpatient days at hospital to which they were assigned`,
    care_intensity_idx = `Hospital Care Intensity Index during the Last Two Years of Life`,
    
    # reimbursements (suffix '_reimburse')
    medicare_total_reimburse = `Total Medicare Reimbursements per Decedent during the Last Two Years of Life`,
    inpatient_reimburse = `Inpatient Sector Reimbursements per Decedent during the Last Two Years of Life`,
    outpatient_reimburse = `Outpatient Sector Reimbursements per Decedent during the Last Two Years of Life`,
    snf_reimburse = `SNF/Long-Term Care Sector Reimbursements per Decedent during the Last Two Years of Life`,
    home_health_reimburse = `Home Health Sector Reimbursements per Decedent during the Last Two Years of Life`,
    hospice_reimburse = `Hospice Sector Reimbursements per Decedent during the Last Two Years of Life`,
    dme_reimburse = `Reimbursements for Durable Medical Equipment per Decedent during the Last Two Years of Life`,
    hospital_reimburse = `Hospital reimbursements per Decedent during the last two years of life`,
    per_day_reimburse = `Reimbursements per patient day (calculated)`,
    hosp_reimburse_ratio = `Hospital Reimbursements: Ratio to US Average (calculated)`,
    reimburse_day_ratio = `Reimbursements per Day: Ratio to US Average (calculated)`,
    
    # spending (suffix '_spend')
    ambulance_spend = `Ambulance spending per Decedent during the last two years of life`,
    other_spend = `Other spending per Decedent during the last two years of life`,
    part_b_total_spend = `Part B Spending per Decedent during the Last Two Years of Life`,
    part_b_eval_manage_spend = `Part B Spending for Evaluation & Management per Decedent during the Last Two Years of Life`,
    part_b_procedures_spend = `Part B Spending for Procedures per Decedent during the Last Two Years of Life`,
    part_b_imaging_spend = `Part B Spending for Imaging per Decedent during the Last Two Years of Life`,
    part_b_tests_spend = `Part B Spending for Tests per Decedent during the Last Two Years of Life`,
    part_b_other_spend = `Other Part B spending per Decedent during the last two years of life`,
    
    # payments (suffix '_payment')
    physician_payment = `Payments for physician visits per Decedent during the last two years of life`,
    physician_payment_per_visit = `Payments per physician visit (calculated)`,
    physician_payment_ratio = `Physician visit payments: Ratio to US Average (calculated)`,
    
    # average co-payments
    avg_copay_total = `Average Co-Payments per Decedent during the Last Two Years of Life`,
    avg_copay_physician = `Average Co-Payments for Physician Services per Decedent during the Last Two Years of Life`,
    avg_copay_dme = `Average Co-Payments for Durable Medical Equipment per Decedent during the Last Two Years of Life`
  ) %>%
  # select the economic related variables
  select(
    hrr, city, state, system, provider_id, hospital_name,
    deaths_chronically_ill, pct_inpatient_days, care_intensity_idx,
    medicare_total_reimburse, inpatient_reimburse, outpatient_reimburse,
    snf_reimburse, home_health_reimburse, hospice_reimburse, dme_reimburse,
    hospital_reimburse, per_day_reimburse, hosp_reimburse_ratio, reimburse_day_ratio,
    ambulance_spend, other_spend, part_b_total_spend, part_b_eval_manage_spend,
    part_b_procedures_spend, part_b_imaging_spend, part_b_tests_spend, part_b_other_spend,
    physician_payment, physician_payment_per_visit, physician_payment_ratio,
    avg_copay_total, avg_copay_physician,avg_copay_dme,
    inpatient_days,icu_deaths,hospice_enrolled,pct_inpatient_days
  ) %>%
  # check the numericalbility
  mutate(across(c(
    deaths_chronically_ill, pct_inpatient_days, care_intensity_idx,
    medicare_total_reimburse, inpatient_reimburse, outpatient_reimburse,
    snf_reimburse, home_health_reimburse, hospice_reimburse, dme_reimburse,
    hospital_reimburse, per_day_reimburse, hosp_reimburse_ratio, reimburse_day_ratio,
    ambulance_spend, other_spend, part_b_total_spend, part_b_eval_manage_spend,
    part_b_procedures_spend, part_b_imaging_spend, part_b_tests_spend, part_b_other_spend,
    physician_payment, physician_payment_per_visit, physician_payment_ratio,
    avg_copay_total, avg_copay_physician, avg_copay_dme,
    inpatient_days,icu_deaths,hospice_enrolled,pct_inpatient_days
  ), ~as.numeric(gsub(",", "", .)))) %>%
  mutate(across(
    c(
      deaths_chronically_ill, pct_inpatient_days, care_intensity_idx,
      medicare_total_reimburse, inpatient_reimburse, outpatient_reimburse,
      snf_reimburse, home_health_reimburse, hospice_reimburse, dme_reimburse,
      hospital_reimburse, per_day_reimburse, hosp_reimburse_ratio, reimburse_day_ratio,
      ambulance_spend, other_spend, part_b_total_spend, part_b_eval_manage_spend,
      part_b_procedures_spend, part_b_imaging_spend, part_b_tests_spend, part_b_other_spend,
      physician_payment, physician_payment_per_visit, physician_payment_ratio,
      avg_copay_total, avg_copay_physician, avg_copay_dme,
      inpatient_days,icu_deaths,hospice_enrolled,pct_inpatient_days
    ), ~ ifelse(. < 0, NA, .)
  ))

# Supplement data: city data & us_state map data, merge to the hospital data for the further ploting the map
city <- read_csv('https://raw.githubusercontent.com/wqian26/436-project/refs/heads/main/uscities.csv')
city <- city %>%
  filter(!state_id %in% c("AK", "HI"))

merged_data <- data %>%
  left_join(city, by = c("city" = "city")) %>%
  select(-city_ascii, -county_fips, -county_name, -military, -incorporated, -timezone, -ranking, -id, -state_id, -zips)

us_states <- st_as_sf(spData::us_states)
us_mainland <- us_states %>%
  st_transform(crs = 4326) %>%
  filter(!NAME %in% c("Alaska", "Hawaii"))


# specify the allowed variables with user-friendly names
allowed_vars <- c(
  "Deaths Among Chronically Ill Patients" = "deaths_chronically_ill",
  "Percent Inpatient Days" = "pct_inpatient_days",
  "Care Intensity Index" = "care_intensity_idx",
  "Total Medicare Reimbursements" = "medicare_total_reimburse",
  "Inpatient Reimbursements" = "inpatient_reimburse",
  "Outpatient Reimbursements" = "outpatient_reimburse",
  "SNF Reimbursements" = "snf_reimburse",
  "Home Health Reimbursements" = "home_health_reimburse",
  "Hospice Reimbursements" = "hospice_reimburse",
  "DME Reimbursements" = "dme_reimburse",
  "Hospital Reimbursements" = "hospital_reimburse",
  "Reimbursements per Patient Day" = "per_day_reimburse",
  "Hospital Reimburse Ratio" = "hosp_reimburse_ratio",
  "Reimburse per Day Ratio" = "reimburse_day_ratio",
  "Ambulance Spend" = "ambulance_spend",
  "Other Spend" = "other_spend",
  "Part B Total Spend" = "part_b_total_spend"
)


# functions for plotting
stateMap <- function(merged_data, us_mainland, state_var, title = "US Map", palette = "viridis") {
  state_data <- merged_data %>%
    group_by(state_name) %>%
    summarise(selected_value = sum(.data[[state_var]], na.rm = TRUE))
  
  merged_state_data <- us_mainland %>%
    left_join(state_data, by = c("NAME" = "state_name"))
  
  pal <- colorNumeric(palette, domain = merged_state_data$selected_value)
  
  leaflet(data = merged_state_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(selected_value),
      fillOpacity = 0.7, color = "#BDBDC3", weight = 1,
      layerId = ~NAME,
      popup = ~paste(NAME, "<br>", names(allowed_vars)[allowed_vars == state_var], ": ", selected_value)
    ) %>%
    addLegend(
      "bottomright", 
      pal = pal, 
      values = merged_state_data$selected_value, 
      title = names(allowed_vars)[allowed_vars == state_var]
    ) %>%
    addControl(title, position = "topright")
}
cityMap <- function(filtered_cities, map_var, title = "City Map") {
  pal <- colorNumeric(palette = c('#1c96c5', '#FFC0CB'), domain = filtered_cities[[map_var]], na.color = "transparent")
  
  leaflet(filtered_cities) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~lng, lat = ~lat,
      radius = 5,
      color = ~pal(filtered_cities[[map_var]]),
      fillOpacity = 0.7,
      layerId = ~provider_id,  # Set provider_id as the layerId for easy identification
      popup = ~paste(
        "City:", city, "<br>",
        "Hospital:", hospital_name, "<br>",
        "Deaths Among Chronically Ill Patients:", deaths_chronically_ill, "<br>",
        "Average Co-Payments:", avg_copay_total, "<br>",
        paste(names(allowed_vars)[allowed_vars == map_var], ":", filtered_cities[[map_var]])
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = filtered_cities[[map_var]],
      title = names(allowed_vars)[allowed_vars == map_var],
      opacity = 0.7
    ) %>%
    addControl(title, position = "topright")
}

scatterPlot <- function(filtered_cities, scatter_x, scatter_y, title = "Scatter Plot", color = "#1c96c5") {
  plot <- ggplot(filtered_cities, aes_string(x = scatter_x, y = scatter_y)) +
    geom_point(aes(text = paste("City:", city,
                                "<br>Hospital:", hospital_name,
                                "Deaths Among Chronically Ill Patients:",
                                deaths_chronically_ill, "<br>",
                                "Average Co-Payments:", avg_copay_total, "<br>",
                                paste("<br>", names(allowed_vars)[allowed_vars == scatter_x], ":", get(scatter_x)),
                                paste("<br>", names(allowed_vars)[allowed_vars == scatter_y], ":", get(scatter_y)))),
               color = color, size = 2, alpha = 0.6) +
    labs(x = names(allowed_vars)[allowed_vars == scatter_x], 
         y = names(allowed_vars)[allowed_vars == scatter_y], 
         title = title) +
    theme_minimal()
  
  ggplotly(plot, tooltip = "text")
}


ui <- fluidPage(
  titlePanel("Healthcare Data Visualization on Hospitals -- Find your preferred provider"),
  
  fluidRow(
    column(12, 
           h4("This application visualizes healthcare data at both state and hospital levels, the spendings and reimbursement is the key focus here.  
          You can explore whatever variable you like to find customize the selection of hospitals.   You can also explore relationships between different variables in your interests."),
           p("Use the selection options below to choose variables for the state and hospital maps, as well as for the scatter plot. 
         The state map aggregates data by state, the hospital map shows detailed data for hospitals within a selected state, 
         and the scatter plot allows for variable comparisons within the selected state.")
    )
  ),
  
  fluidRow(
    column(4,
           selectInput("state_var", "Select Variable for Maps:", 
                       choices = allowed_vars, selected = "care_intensity_idx")
    ),
    column(4,
           selectInput("scatter_x", "Choose X Variable for Scatter Plot:", 
                       choices = allowed_vars, selected = "care_intensity_idx")
    ),
    column(4,
           selectInput("scatter_y", "Choose Y Variable for Scatter Plot:", 
                       choices = allowed_vars, selected = "medicare_total_reimburse")
    )
  ),
  
  fluidRow(
    column(12, 
           h5("State-Level Map"),
           p("This map shows aggregated healthcare data by state for the selected variable. Click on a state to view hospital-level data."),
           leafletOutput("stateMap", height = 400)
    )
  ),
  
  fluidRow(
    column(6, 
           h5("Hospital Map"),
           p("This map displays detailed healthcare data for hospitals within the selected state in the map above. The data reflects the same variable as the state map, allowing for consistent comparison."),
           leafletOutput("cityMap", height = 400)
    ),
    column(6, 
           h5("Scatter Plot"),
           p("This scatter plot visualizes the relationship between the selected X and Y variables for hospitals within the chosen state. Use this plot to explore correlations or trends."),
           plotlyOutput("scatterPlot")
    )
  ),
  
  fluidRow(
    column(12, 
           h5("Hospital Data Table"),
           p("This table displays detailed healthcare data for each hospital within the selected state, based on the selected variables."),
           DTOutput("cityDataTable")
    )
  )
)

server <- function(input, output, session) {
  output$stateMap <- renderLeaflet({
    stateMap(merged_data, us_mainland, input$state_var, title = "State-Level Healthcare Visualization")
  })
  
  observeEvent(input$stateMap_shape_click, {
    selected_state <- input$stateMap_shape_click$id
    
    filtered_cities <- merged_data %>%
      filter(state_name == selected_state)
    
    if (nrow(filtered_cities) > 0 && all(c("lng", "lat") %in% colnames(filtered_cities))) {
      output$cityMap <- renderLeaflet({
        cityMap(filtered_cities, input$state_var, title = paste("Hospital Map for", selected_state))
      })
    }
    
    output$scatterPlot <- renderPlotly({
      scatterPlot(filtered_cities, input$scatter_x, input$scatter_y)
    })
    
    output$cityDataTable <- renderDT({
      filtered_cities %>%
        select(city, hospital_name, provider_id, all_of(input$state_var), all_of(input$scatter_x), all_of(input$scatter_y),
               medicare_total_reimburse, inpatient_reimburse, outpatient_reimburse,
               snf_reimburse, home_health_reimburse, hospice_reimburse, dme_reimburse,
               hospital_reimburse, per_day_reimburse,avg_copay_total, avg_copay_physician, avg_copay_dme,) %>%
        datatable(options = list(pageLength = 5), rownames = FALSE)
    })
    
    
    
    observeEvent(input$cityMap_marker_click, {
      selected_provider <- input$cityMap_marker_click$id
      
      selected_hospital_data <- filtered_cities %>%
        filter(provider_id == selected_provider)
      
      if (nrow(selected_hospital_data) > 0) {
        output$scatterPlot <- renderPlotly({
          plot <- ggplot(filtered_cities, aes_string(x = input$scatter_x, y = input$scatter_y)) +
            geom_point(aes(
              text = paste(
                "City:", city, "<br>",
                "Hospital:", hospital_name, "<br>",
                names(allowed_vars)[allowed_vars == input$scatter_x], ":", get(input$scatter_x), "<br>",
                names(allowed_vars)[allowed_vars == input$scatter_y], ":", get(input$scatter_y)
              )
            ),
            color = "#1c96c5", size = 2, alpha = 0.6) +
            geom_point(data = selected_hospital_data, 
                       aes_string(x = input$scatter_x, y = input$scatter_y),
                       color = "pink", size = 4, alpha = 1) +
            labs(x = names(allowed_vars)[allowed_vars == input$scatter_x], 
                 y = names(allowed_vars)[allowed_vars == input$scatter_y], 
                 title = paste("Scatter Plot of", names(allowed_vars)[allowed_vars == input$scatter_x], 
                               "vs", names(allowed_vars)[allowed_vars == input$scatter_y])) +
            theme_minimal()
          
          ggplotly(plot, tooltip = "text")
        })
      }
    })
  })
}


shinyApp(ui, server)



library(shiny)
library(plotly)
library(dplyr)
library(leaflet)
library(DT)
library(ggplot2)
library(readr)

# Define helper function to install and load packages
install_packages_if_not_present <- function(x) {
  new_packages <- x[!x %in% installed.packages()[,"Package"]]
  if (length(new_packages) > 0) install.packages(new_packages)
  sapply(x, require, character.only = TRUE)
}

# List of required packages
packages <- c("spData", "leaflet", "leaflet.extras", "shiny", "DT",
              "readxl", "readr", "dplyr", "sf", "ggplot2", "plotly")
install_packages_if_not_present(packages)

data_url <- 'https://raw.githubusercontent.com/wqian26/436-project/refs/heads/main/combined_dataset.csv'
data <- read_csv(data_url)


data <- data %>%
  rename(
    hrr = `HRR`,
    city = `City`,
    state = `State`,
    system = `System`,
    provider_id = `Provider.ID`,
    hospital_name = `Hospital.Name`,
    lat = latitude,
    lng = longitude,
    deaths_chronically_ill = `Number.of.deaths.among.chronically.ill.patients.assigned.to.hospital`,
    inpatient_days = `Inpatient.Days.per.Decedent.during.the.Last.Two.Years.of.Life`,
    icu_deaths = `Percent.of.Deaths.Associated.With.ICU.Admission`,
    hospice_enrolled = `Percent.of.Decedents.Enrolled.In.Hospice.during.the.Last.Six.Months.of.Life`,
    pct_inpatient_days = `Percent.of.enrollees.medical.inpatient.days.at.hospital.to.which.they.were.assigned`,
    care_intensity_idx = `Hospital.Care.Intensity.Index.during.the.Last.Two.Years.of.Life`,
    medicare_total_reimburse = `Total.Medicare.Reimbursements.per.Decedent.during.the.Last.Two.Years.of.Life`,
    inpatient_reimburse = `Inpatient.Sector.Reimbursements.per.Decedent.during.the.Last.Two.Years.of.Life`,
    outpatient_reimburse = `Outpatient.Sector.Reimbursements.per.Decedent.during.the.Last.Two.Years.of.Life`,
    snf_reimburse = `SNF.Long.Term.Care.Sector.Reimbursements.per.Decedent.during.the.Last.Two.Years.of.Life`,
    home_health_reimburse = `Home.Health.Sector.Reimbursements.per.Decedent.during.the.Last.Two.Years.of.Life`,
    hospice_reimburse = `Hospice.Sector.Reimbursements.per.Decedent.during.the.Last.Two.Years.of.Life`,
    dme_reimburse = `Reimbursements.for.Durable.Medical.Equipment.per.Decedent.during.the.Last.Two.Years.of.Life`,
    hospital_reimburse = `Hospital.reimbursements.per.Decedent.during.the.last.two.years.of.life`,
    per_day_reimburse = `Reimbursements.per.patient.day..calculated.`,
    hosp_reimburse_ratio = `Hospital.Reimbursements..Ratio.to.US.Average..calculated.`,
    reimburse_day_ratio = `Reimbursements.per.Day..Ratio.to.US.Average..calculated.`,
    ambulance_spend = `Ambulance.spending.per.Decedent.during.the.last.two.years.of.life`,
    other_spend = `Other.spending.per.Decedent.during.the.last.two.years.of.life`,
    part_b_total_spend = `Part.B.Spending.per.Decedent.during.the.Last.Two.Years.of.Life`,
    part_b_eval_manage_spend = `Part.B.Spending.for.Evaluation...Management.per.Decedent.during.the.Last.Two.Years.of.Life`,
    part_b_procedures_spend = `Part.B.Spending.for.Procedures.per.Decedent.during.the.Last.Two.Years.of.Life`,
    part_b_imaging_spend = `Part.B.Spending.for.Imaging.per.Decedent.during.the.Last.Two.Years.of.Life`,
    part_b_tests_spend = `Part.B.Spending.for.Tests.per.Decedent.during.the.Last.Two.Years.of.Life`,
    part_b_other_spend = `Other.Part.B.spending.per.Decedent.during.the.last.two.years.of.life`,
    physician_payment = `Payments.for.physician.visits.per.Decedent.during.the.last.two.years.of.life`,
    physician_payment_per_visit = `Payments.per.physician.visit..calculated.`,
    physician_payment_ratio = `Physician.visit.payments..Ratio.to.US.Average..calculated.`,
    avg_copay_total = `Average.Co.Payments.per.Decedent.during.the.Last.Two.Years.of.Life`,
    avg_copay_physician = `Average.Co.Payments.for.Physician.Services.per.Decedent.during.the.Last.Two.Years.of.Life`,
    avg_copay_dme = `Average.Co.Payments.for.Durable.Medical.Equipment.per.Decedent.during.the.Last.Two.Years.of.Life`
  ) %>%
  
  # select the economic related variables
  select(
    hrr, city, state, system, provider_id, hospital_name,
    lng, lat,address, website, tele,
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


data <- data %>%
  mutate(region = case_when(
    state %in% c('ME', 'NH', 'VT', 'MA', 'RI', 'CT') ~ 'New England',
    state %in% c('NY', 'NJ', 'PA') ~ 'Middle Atlantic',
    state %in% c('OH', 'MI', 'IN', 'IL', 'WI') ~ 'East North Central',
    state %in% c('MN', 'IA', 'MO', 'ND', 'SD', 'NE', 'KS') ~ 'West North Central',
    state %in% c('DE', 'MD', 'DC', 'VA', 'WV', 'NC', 'SC', 'GA', 'FL') ~ 'South Atlantic',
    state %in% c('KY', 'TN', 'MS', 'AL') ~ 'East South Central',
    state %in% c('AR', 'LA', 'OK', 'TX') ~ 'West South Central',
    state %in% c('MT', 'ID', 'WY', 'NV', 'UT', 'CO', 'AZ', 'NM') ~ 'Mountain',
    state %in% c('WA', 'OR', 'CA', 'AK', 'HI') ~ 'Pacific',
    TRUE ~ 'Other'
  )) %>%
  filter(!(region %in% 'Other'))

state_mapping <- data.frame(state_code = state.abb, state_name = state.name)
data <- data %>%
  left_join(state_mapping, by = c("state" = "state_code"))

us_states <- st_as_sf(spData::us_states)
us_mainland <- us_states %>%
  st_transform(crs = 4326) %>%
  filter(!NAME %in% c("Alaska", "Hawaii"))


allowed_vars <- c(
  "Deaths Among Chronically Ill Patients" = "deaths_chronically_ill",
  "Percent Inpatient Days" = "pct_inpatient_days",
  "Other Spend" = "other_spend",
  "Average Copayment for Physicians" = "avg_copay_physician",
  "Part B Total Spend" = "part_b_total_spend",
  "Average Total Copayment" = "avg_copay_total",
  "Hospice Enrolled" = "hospice_enrolled",
  "Inpatient Days" = "inpatient_days",
  "Total Medicare Reimbursements" = "medicare_total_reimburse",
  "Physician Payment" = "physician_payment",
  "Physician Payment per Visit" = "physician_payment_per_visit",
  "Average Copayment for Durable Medical Equipment" = "avg_copay_dme",
  "Ambulance Spend" = "ambulance_spend",
  "ICU Deaths" = "icu_deaths",
  "Care Intensity Index" = "care_intensity_idx",
  "Hospital Reimbursements" = "hospital_reimburse",
  "Reimbursements per Patient Day" = "per_day_reimburse",
  "Hospital Reimburse Ratio" = "hosp_reimburse_ratio",
  "Reimburse per Day Ratio" = "reimburse_day_ratio"
  
)


### tab1: functions


## funcs used in tab1
# Update the histogram function to include faceting by region
histogram <- function(df, x_var, lower_quantile=0.05, upper_quantile=0.95) {
  filtered <- df %>%
    filter(
      df[[x_var]] >= quantile(df[[x_var]], lower_quantile, na.rm = TRUE) &
        df[[x_var]] <= quantile(df[[x_var]], upper_quantile, na.rm = TRUE)
    )
  ggplot(filtered, aes_string(x = x_var, fill = "region")) +
    geom_histogram(alpha = 0.7, bins = 30) +  # Set a default number of bins for clearer visual distinction
    labs(x = x_var, y = "Frequency") +
    facet_wrap(~ region, scales = "free_y") +  # Add faceting by 'Region'
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 2.scatterplot: with input of variables names and quantiles from users, the points are color coded by states; brushing event can be applied on the scatter plot
scatterplot <- function(df, x_var, y_var, lower_quantile = 0.05, upper_quantile = 0.95) {
  # Calculate the quantiles for x_var and y_var to cut the tails
  lower_x <- quantile(df[[x_var]], lower_quantile, na.rm = TRUE)
  upper_x <- quantile(df[[x_var]], upper_quantile, na.rm = TRUE)
  lower_y <- quantile(df[[y_var]], lower_quantile, na.rm = TRUE)
  upper_y <- quantile(df[[y_var]], upper_quantile, na.rm = TRUE)
  
  filtered <- df %>%
    filter(
      df[[x_var]] >= quantile(df[[x_var]], lower_quantile, na.rm = TRUE) &
        df[[x_var]] <= quantile(df[[x_var]], upper_quantile, na.rm = TRUE)
    )
  
  # Check if the filtered data frame is empty
  if (nrow(filtered) == 0) {
    ggplot() + 
      ggtitle("No data available for selected filters") + 
      theme_void()
  } else {
    ggplot(filtered, aes_string(x = x_var, y = y_var, color = "region")) +
      geom_point() +
      geom_smooth(method = "lm", color = "darkgrey") +  # Adding a linear regression line without confidence interval
      facet_wrap(~ region)
  }
}

boxplot_by_region <- function(df, y_var, region_name) {
  # Filter the dataframe by the specified region
  filtered_df <- df %>%
    filter(region == region_name)
  
  # Check if the filtered dataframe is empty
  if (nrow(filtered_df) == 0) {
    print("No data found for the specified region.")
    return(NULL)  # Returns NULL if no data is found to avoid errors in ggplot
  }
  
  # Plotting the boxplot using aes and dynamic variable names
  ggplot(filtered_df, aes(x = state_name, y = .data[[y_var]], fill = state_name)) +
    geom_boxplot() +
    labs(y = y_var, title = paste("Boxplot of", y_var, "in", region_name)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



### tab2: functions

merged_data<- data

stateMap <- function(merged_data, us_mainland, state_var, title = "US Map", palette = c('#e0ecf4','#9ebcda', '#8856a7','#810f7c')) {
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
      fillOpacity = 0.95, color = "#BDBDC3", weight = 1,
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
  pal <- colorNumeric(palette = c('#e0ecf4','#9ebcda', '#8856a7','#810f7c'), domain = filtered_cities[[map_var]], na.color = "transparent")
  
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

scatterPlot <- function(filtered_cities, scatter_x, scatter_y, title = "Scatter Plot", color = "#9ebcda") {
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




## App


df1<- data
ui <- fluidPage(
  titlePanel("Healthcare Data Visualization on Hospitals -- Find your preferred provider"),
  
  tabsetPanel(
    ### tab1
    tabPanel("General Information",
             sidebarLayout(
               sidebarPanel(
                 h4("Instructions"),
                 p("This application helps you visualize relationships between your selected hospital care variables. Follow these steps to explore the data:"),
                 h5("Step 1: Choose Visualization Type"),
                 p("Select the type of plot you wish to view."),
                 selectInput("plot_type", "Choose Plot Type:", choices = c("Histogram", "Scatterplot")),
                 h5("Step 2: Select Variables"),
                 p("Choose the variables you want to display on the X-axis and Y-axis."),
                 selectInput("x_var", "X-axis variable:", choices = allowed_vars, selected = "deaths_chronically_ill"),
                 selectInput("y_var", "Y-axis variable:", choices = allowed_vars, selected = "care_intensity_idx"),
                 h5("Step 3: Adjust the Data Range"),
                 p("Modify the range sliders below to filter out extreme values from the data."),
                 sliderInput("lower_quantile", "Lower Percentile (Exclude extreme Low-Value Outliers)", 
                             min = 0, max = 0.5, value = 0.05),
                 sliderInput("upper_quantile", "Upper Percentile (Exclude extreme High-Value Outliers)", 
                             min = 0.5, max = 1, value = 0.95) ),
               mainPanel(
                 fluidRow(
                   column(12, plotOutput("plot"))
                 ), 
                 h5("Region and Box Plot"),
                 p("Select a specific geographic area to focus your analysis and view the corresponding box plot."),
                 selectInput("region", "Step 4: Select Region:", choices = unique(df1$region), selected = unique(df1$region)[1]),
                 fluidRow(
                   column(12, plotOutput("boxplot"))
                 )
               )
             )),
    ### tab2
    
    tabPanel("Interactive Maps",
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
                                  choices = allowed_vars, selected = "deaths_chronically_ill")
               ),
               column(4,
                      selectInput("scatter_x", "Choose X Variable for Scatter Plot:", 
                                  choices = allowed_vars, selected = "deaths_chronically_ill")
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
                      h5("Step 1: Use the 'Select Variable for Maps' dropdown menu to choose a variable to visualize (e.g., 'Care Intensity Index'). Hover over a state to view aggregated values in a popup, and click on a state to explore its hospital-level details."),
                      leafletOutput("stateMap", height = 400)
               )
             ),
             
             fluidRow(
               column(6, 
                      h5("Hospital Map"),
                      p("This map displays detailed healthcare data for hospitals within the selected state in the map above. The data reflects the same variable as the state map, allowing for consistent comparison."),
                      h5("Step 2: Each circle represents a hospital. Hover over a marker to see details like the hospital name, city, and metrics. Click on a marker to highlight its data in the scatter plot."),
                      leafletOutput("cityMap", height = 400)
               ),
               column(6, 
                      h5("Scatter Plot"),
                      p("This scatter plot visualizes the relationship between the selected X and Y variables for hospitals within the chosen state. Use this plot to explore correlations or trends."),
                      h5("Step 3: Use the 'Choose X Variable' and 'Choose Y Variable' dropdowns to select variables to compare. Each point represents a hospital, with tooltips showing more details."),
                      plotlyOutput("scatterPlot")
               )
             ),
             
             fluidRow(
               column(12, 
                      h5("Hospital Data Table"),
                      p("This table displays detailed healthcare data for each hospital within the selected state, based on the selected variables."),
                      h5("Step 4: Scroll through the table to review metrics. Use the search bar to filter data by hospital name, city, or other parameters."),
                      DTOutput("cityDataTable")
               )
             )
    )
  )
)


#####
server <- function(input, output) {
  ## tab1
  output$plot <- renderPlot({
    if (input$plot_type == "Histogram") {
      histogram(df1, input$x_var, input$lower_quantile, input$upper_quantile)
    } else {
      scatterplot(df1, input$x_var, input$y_var, input$lower_quantile, input$upper_quantile)
    }
  })
  
  output$boxplot <- renderPlot({
    boxplot_by_region(df1, input$x_var, input$region)
  })
  
  ## tab2
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
        select(state, city, hospital_name, provider_id, all_of(input$state_var), all_of(input$scatter_x), all_of(input$scatter_y),
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
            # c('#e0ecf4','#9ebcda', '#8856a7')
            color = "#9ebcda", size = 2, alpha = 0.6) +
            geom_point(data = selected_hospital_data, 
                       aes_string(x = input$scatter_x, y = input$scatter_y),
                       color = "#dd1c77", size = 4, alpha = 1) +
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

shinyApp(ui = ui, server = server)






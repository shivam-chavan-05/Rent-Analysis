#install.packages(c('tidycensus', 'tidyr','dplyr','leaflet','shiny','shinythemes'))

library(tidycensus)
library(dplyr)
library(leaflet)
library(shiny)
library(shinythemes)

# Set your Census API key
Sys.setenv(TIDYCENSUS_KEY = "81e3f1bd27a2f4ceca9e26cbb3e9cfe21eaf9696")

get_rent_data <- function(state_code, bedrooms, start_year, end_year) {
  variable <- paste0("B25031_00", bedrooms, "E")
  
  rent_data <- purrr::map_df(
    seq(start_year, end_year),
    ~tidycensus::get_acs(
      geography = "county",
      variables = c(median_rent = variable),
      state = state_code,
      year = .x,
      survey = "acs5",
      geometry = TRUE,  # Fetch geographic boundaries
      cache = TRUE
    )
  )
  
  return(rent_data)
}

# Specify the state, number of bedrooms, and years
state_code <- "VA"  # Replace with the actual state code, e.g., "NY" for New York
bedrooms <- "3" # Replace with the desired number of bedrooms (e.g., "04" for 4 bedrooms)
start_year <- 2017
end_year <- 2021

# Shiny app
ui <- fluidPage(
  theme = shinytheme("cyborg"),  # Set the theme to "cyborg"
  titlePanel(
    h2("Insta Rent Visualization", align = "center")
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("stateInput", "Enter State Code (e.g., VA):"),
      textInput("bedroomsInput", "Enter Number of Bedrooms (1-5):"),
      actionButton("plotButton", "View")
    ),
    mainPanel(
      leafletOutput("rentMap"),
      textOutput("errorText")  # Display error message
    )
  )
)

server <- function(input, output) {
  state_code_valid <- reactive({
    req(input$stateInput)
    toupper(input$stateInput) %in% state.abb
  })
  
  output$errorText <- renderText({
    if (!state_code_valid()) {
      "Invalid state code. Please enter a valid state code."
    } else {
      ""
    }
  })
  
  observeEvent(input$plotButton, {
    req(input$stateInput, input$bedroomsInput)  # Require both inputs to be non-empty
    
    # Check if state code is valid
    if (!state_code_valid()) {
      return(NULL)  # Return NULL to prevent further execution
    }
    
    state_code <- toupper(input$stateInput)
    bedroom <- input$bedroomsInput
    if (bedroom==1) {
      bedrooms = 3
    } 
    if (bedroom==2) {
      bedrooms = 4
    }
    if (bedroom==3) {
      bedrooms = 5
    }
    if (bedroom==4) {
      bedrooms = 6
    }
    if (bedroom==5) {
      bedrooms = 7
    }
    
    rent_data <- get_rent_data(state_code, bedrooms, start_year, end_year)
    qpal <- colorQuantile("Blues", rent_data$estimate, n = 5)
    
    output$rentMap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = rent_data, fillOpacity = 0.5, weight = 1, color = "white",
                    fillColor = ~qpal(estimate),
                    popup = ~paste("County: ", NAME, "<br>Median Rent: $", estimate)) %>%
        addLegend(
          "bottomleft",
          title = "Median Rent ($)",
          pal = colorBin('Blues', rent_data$estimate),
          values = rent_data$estimate
        ) %>%
        hideGroup(c('Median Rent')) %>%
        showGroup('medianrent')
    })
  })
}

shinyApp(ui, server)

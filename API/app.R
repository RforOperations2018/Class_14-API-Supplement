# Class 8 API Example
# This App works, as the wprdc API does not require Keys

library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(dplyr)

ckanSQL <- function(url) {
  # Make the Request
  url <- gsub(" ", "%20", url)
  print(url)
  r <- GET(url)
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Create fake inputs
# input <- NULL
# input$dates <- c("2018-09-15", "2018-09-30")
# input$type_select <- c("Potholes","Weeds/Debris")

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(url))
}

types <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_TYPE")$REQUEST_TYPE)

dat <- ckanSQL("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22e03a89dd-134a-4ee8-a2bd-62c40aeebc6f%22%20WHERE%22OFFENSES%22%20LIKE%20%27%Public%20Drunk%%27") 
df <- dat %>%
  rename(ARREST = OFFENSES) %>%
  mutate(Race = case_when(RACE == "B" ~ "Black",
                          RACE == "W" ~ "White",
                          RACE == "H" ~ "Hispanic",
                          TRUE ~ "Other"))

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("City of Pittsburgh 311 Dashboard"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
         dateRangeInput("dates",
                        "Select Dates",
                        start = Sys.Date()-30,
                        end = Sys.Date()),
         selectInput("type_select",
                     "Request Type",
                     choices = types,
                     multiple = TRUE,
                     selectize = TRUE,
                     selected = "Potholes")
      ),
      
      # Tabset Main Panel
      mainPanel(
        tabsetPanel(
          tabPanel("Line Plot",
            plotlyOutput("linePlot")
          ),
          tabPanel("Open/Closed",
                   plotlyOutput("barChart"))
        )
      )
   )
)

# Define server logic
server <- function(input, output) {
   load311 <- reactive({
     # Building an IN selector
     types_filter <- ifelse(length(input$type_select) > 0, paste0("%20AND%20%22REQUEST_TYPE%22%20IN%20(%27", paste(input$type_select, collapse = "%27,%27"),"%27)"), "")
     # Build API Query with proper encodes
     url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", input$dates[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$dates[2], "%27", types_filter)
     
     # Load and clean data
     dat311 <-  ckanSQL(url) %>%
       mutate(date = as.Date(CREATED_ON),
              STATUS = ifelse(STATUS == 1, "Closed", "Open"))
     
     return(dat311)
   })
   output$linePlot <- renderPlotly({
      dat311 <- load311()
      
      # shape the data for chart
      table <- dat311 %>%
        group_by(date) %>%
        summarise(count = n())
      
      # draw plot
      ggplot(table, aes(x = date, y = count)) +
        geom_point(colour = "#d95f02") +
        geom_line(colour = "#d95f02") +
        geom_smooth()
   })
   output$barChart <- renderPlotly({
     dat311 <- load311()
     
     # shape the data for chart
     table <- dat311 %>%
       group_by(STATUS, REQUEST_TYPE) %>%
       summarise(count = n())
     
     # draw plot
     ggplot(table, aes(x = STATUS, y = count, fill = STATUS)) +
       geom_bar(stat = "identity") +
       facet_grid(~REQUEST_TYPE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


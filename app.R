# LIBRARIES ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# APP CATALOG (META DATA) ----
app1 <- list(
    title = "Stock Analyzer",
    subtitle = "MongoDB Atlas",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and MongoDB Atlas Cloud.",
    sub_directory = "stock_analyzer_mongo_atlas",
    tags = tibble(
        tag = c("Stocks", "Shiny", "AWS", "MongoDB", "Auth"),
        color = c("info", "success", "success", "success", "default")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app2 <- list(
    title = "Stock Analyzer",
    subtitle = "Local Data",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and uses internal data storage.",
    sub_directory = "stock_analyzer_local_data",
    tags = tibble(
        tag = c("Stocks", "Shiny", "AWS", "Auth"),
        color = c("info", "success", "success", "default")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app3 <- list(
    title = "Old Faithful",
    subtitle = NA,
    description = "A test application used to validate Shiny Server.",
    sub_directory = "test_app",
    tags = tibble(
        tag = c("Sample Apps", "Shiny", "AWS"),
        color = c("info", "success", "success")
    ) %>% list(),
    img = "test_app.jpg"
)

app_catalog_tbl <- bind_rows(app1, app2, app3, .id = "id")


# FUNCTIONS ----


# UI ----
# Define UI for application that draws a histogram
ui <- fluidPage(
    "Hello Shiny"
)

# SERVER ----
server <- function(session, input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)

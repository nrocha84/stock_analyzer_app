# LIBRARIES ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# SETUP ----
theme <- "paper"
button_theme_search <- "primary"
button_theme_tags   <- "primary"
button_theme_cards  <- "primary" 

# APP CATALOG ----
app1 <- list(
    title = "Stock Analyzer",
    subtitle = "MongoDB Atlas",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and MongoDB Atlas Cloud.",
    sub_directory = "stock_analyzer_mongo_atlas/",
    tags = tibble(
        tag = c("Stocks", "Shiny", "AWS", "MongoDB", "Auth"),
        color = c("info", "success", "success", "success", "danger")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app2 <- list(
    title = "Stock Analyzer",
    subtitle = "Local Data",
    description = "A financial application for analyzing trends in your favorite SP 500 stocks. Leverages AWS EC2 and uses internal data storage.",
    sub_directory = "stock_analyzer_local_data/",
    tags = tibble(
        tag = c("Stocks", "Shiny", "AWS", "Auth"),
        color = c("info", "success", "success", "danger")
    ) %>% list(),
    img = "stock_analyzer.jpg"
)

app3 <- list(
    title = "Old Faithful",
    subtitle = NA,
    description = "A test application used to validate Shiny Server.",
    sub_directory = "test_app/",
    tags = tibble(
        tag = c("Sample Apps", "Shiny", "AWS"),
        color = c("info", "success", "success")
    ) %>% list(),
    img = "test_app.jpg"
)

app_catalog_tbl <- bind_rows(app1, app2, app3, .id = "id")


# FUNCTIONS ----
# https://stackoverflow.com/a/40755608/6713793
navbar_page_with_inputs <- function(..., inputs) {
    navbar <- navbarPage(...)
    form <- tags$form(class = "navbar-form navbar-right", inputs)
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
        navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form)
    navbar
}

make_tags <- function(data) {
    data %>%
        mutate(tag = as_factor(tag)) %>% 
        group_by(tag) %>% 
        group_split() %>%
        map(.f = function(data) {
            span(class = str_glue("label label-{data$color}"), data$tag)
        }) %>%
        tagList()
}

make_cards <- function(data) {
    
    if (nrow(data) == 0) return("")
    
    data %>%
        mutate(id = as_factor(id)) %>%
        group_by(id) %>%
        group_split() %>%
        
        map(.f = function(data) {
            div(
                class = "col-sm-4",
                style = "display:-webkit-flex;",
                div(
                    class = "panel panel-default",
                    div(
                        class = "panel-heading",
                        data %>% pluck("tags", 1) %>% make_tags()
                    ),
                    div(
                        class = "pannel-body", 
                        style = "padding:20px;",
                        img(class = "img img-thumbnail", 
                            src = str_glue("images/{data$img}")
                        ),
                        br(), br(),
                        h4(data$title, br(), 
                           if (!is.na(data$subtitle)) tags$small(data$subtitle)
                        ),
                        p(data$description),
                        a(type   = "button", 
                          class  = str_glue("btn btn-{button_theme_cards}"), 
                          target = "_blank", 
                          href   = str_c("/", data$sub_directory), 
                          "Open")
                    )
                )
            ) 
        }) %>%
        tagList()
}


# UI ----
# Define UI for application that draws a histogram
ui <- fluidPage(
    tagList(
        tags$head(HTML('<title>Apps by Business Science</title>
                       <meta content="/images/stock_analyzer.jpg" property="og:image">'))
    ),
    style = "padding:0px;",
    
    navbar_page_with_inputs(
        
        # Application 
        title = div(
            img(src = "https://www.business-science.io/img/business-science-logo.png",
                width="30", height="30", style="-webkit-filter: drop-shadow(3px 3px 3px #222);"), 
            "Apps by Business Science"
        ),
        collapsible = TRUE,
        theme = shinytheme(theme),
        
        # Search
        inputs = div(
            textInput(inputId = "search_box", label = NULL, placeholder = "Search", width = 200),
            actionButton(inputId = "search_button", label = "Submit", class = str_glue("btn-{button_theme_search}")),
            actionButton(inputId = "clear_button", label = "Clear", class = str_glue("btn-{button_theme_search}"))
        ),
        
        # Tabs
        tabPanel(
            "Library",
            
            # Filters
            br(),
            div(
                class = "container", 
                id = "tag-filters", 
                radioGroupButtons(
                    inputId   = "input_tags", 
                    choices   = c("All", app_catalog_tbl %>% unnest(tags) %>% pull(tag) %>% unique() %>% sort()) %>% str_to_upper(), 
                    justified = TRUE, 
                    selected = "ALL",
                    status    = button_theme_tags
                )
            ),
            br(),
            
            # App Library
            div(
                class = "", 
                id = "app-library",
                uiOutput(outputId = "output_cards")
            )
        )
        
    )
)

# SERVER ----
server <- function(session, input, output) {
    
    reactive_values <- reactiveValues(data = app_catalog_tbl)
    
    # Search Box ----
    observeEvent(eventExpr = input$search_button, {
        
        # updateRadioGroupButtons(session = session, inputId = "input_tags", selected = "ALL")
        
        search_string <- str_to_lower(input$search_box)
        
        reactive_values$data <- app_catalog_tbl %>%
            filter(
                str_to_lower(title) %>% str_detect(pattern = search_string) |
                    str_to_lower(subtitle) %>% str_detect(pattern = search_string) | 
                    str_to_lower(description) %>% str_detect(pattern = search_string)
            )
        
        
        
    })
    
    observeEvent(eventExpr = input$clear_button, {
        reactive_values$data <- app_catalog_tbl
        
        updateTextInput(session = session, inputId = "search_box", value = "", placeholder = "Search")
        updateRadioGroupButtons(session = session, inputId = "input_tags", selected = "ALL")
        
    })
    
    # Search Tags ----
    observeEvent(input$input_tags, {
        
        tag_selected <- str_to_lower(input$input_tags)
        
        if (tag_selected == "all") {
            reactive_values$data <- app_catalog_tbl
        } else {
            selected_app_ids <- app_catalog_tbl %>% 
                unnest(tags) %>%
                filter(str_to_lower(tag) == tag_selected) %>%
                pull(id) %>%
                unique()
            
            reactive_values$data <- app_catalog_tbl %>%
                filter(id %in% selected_app_ids)
        }
        
    })
    
    # Render Cards ----
    output$output_cards <- renderUI({
        
        div(
            class = "container",
            div(
                class = "row",
                style = "display:-webkit-flex; flex-wrap:wrap;",
                reactive_values$data %>% make_cards()
            )
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
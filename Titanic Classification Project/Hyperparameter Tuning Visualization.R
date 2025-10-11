# Importing necessary libraries.
library(tidyverse)
library(shiny)
library(rlang)

# Fetching the tuning results from Titanic data.
data <- read.csv('C:/Users/RAMIL/Python Classification/Titanic Classification Project/titanic_tuning_results.csv')

# Viewing the data.
View(data)

# Fetching the unique values from each hyper parameter.
param_max_depth <- data %>% pull(param_max_depth) %>% unique()
param_min_samples_leaf <- data %>% pull(param_min_samples_leaf) %>% unique()
param_min_samples_split <- data %>% pull(param_min_samples_split) %>% unique()
param_n_estimators <- data %>% pull(param_n_estimators) %>% unique()
params <- list('param_max_depth' = param_max_depth, 'param_min_samples_leaf' = param_min_samples_leaf, 'param_min_samples_split' = param_min_samples_split, 'param_n_estimators' = param_n_estimators)

ui <- fluidPage(
  selectInput(inputId = 'static_param1', label = 'Choose the first static parameter:', choices = names(params)),
  selectInput(inputId = 'static_param1_choices', label = 'Choose its value:', choices = NULL),
  selectInput(inputId = 'static_param2', label = 'Choose the second static parameter:', choices = NULL),
  selectInput(inputId = 'static_param2_choices', label = 'Choose its value:', choices = NULL),
  selectInput(inputId = 'dynamic_param1', label = 'Choose the first dynamic parameter:', choices = NULL),
  selectInput(inputId = 'dynamic_param2', label = 'Second dynamic parameter:', choices = NULL),
  plotOutput('heatmap')
)

server <- function(input, output, session){
  observeEvent(input$static_param1, {
    updateSelectInput(
      session = session,
      inputId = 'static_param2',
      choices = names(params[setdiff(names(params), input$static_param1)])
    )
    updateSelectInput(
      session = session,
      inputId = 'static_param1_choices',
      choices = params[[input$static_param1]]
    )
  })
  
  observeEvent(input$static_param2, {
    updateSelectInput(
      session = session,
      inputId = 'static_param2_choices',
      choices = params[[input$static_param2]]
    )
  })
  
  observe({
    updateSelectInput(
      session = session,
      inputId = 'dynamic_param1',
      choices = names(params[setdiff(names(params), c(input$static_param1, input$static_param2))])
    )
  })
  
  observe({
    updateSelectInput(
      session = session,
      inputId = 'dynamic_param2',
      choices = names(params[setdiff(names(params), c(input$static_param1, input$static_param2, input$dynamic_param1))])
    )
  })
  
  output$heatmap <- renderPlot({
    p1 <- input$static_param1_choices
    p2 <- input$static_param2_choices
    df <- data %>%
      filter(
        !!sym(input$static_param1) == as.numeric(input$static_param1_choices),
        !!sym(input$static_param2) == as.numeric(input$static_param2_choices)
      )
    df %>%
      ggplot(aes(x = factor(!!sym(input$dynamic_param1)),
                 y = factor(!!sym(input$dynamic_param2)),
                 fill = mean_test_score)) +
      geom_tile() +
      geom_text(aes(label = round(mean_test_score, 3)), color = "white", size = 4) +
      labs(
        x = input$dynamic_param1,
        y = input$dynamic_param2,
        fill = "Mean CV Score"
      ) +
      theme_minimal()
  })
}
  
shinyApp(ui, server)

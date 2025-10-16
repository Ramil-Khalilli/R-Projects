library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# --- Load Data ---
metrics <- read.csv("C:/Users/RAMIL/Desktop/Data Science Project 1/metrics.csv", row.names = 1)
conf_matrices <- read.csv("C:/Users/RAMIL/Desktop/Data Science Project 1/conf_matrices.csv", row.names = 1)
tuning_methods <- read.csv("C:/Users/RAMIL/Desktop/Data Science Project 1/tuning_methods.csv", row.names = 1)
perf_by_lr <- read.csv("C:/Users/RAMIL/Desktop/Data Science Project 1/perf_by_lr.csv", row.names = 1)

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(title = "Classification Models Performance Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Model Metrics", tabName = "metrics_tab", icon = icon("chart-bar")),
      menuItem("Tuning Comparison", tabName = "tuning_tab", icon = icon("sliders-h")),
      menuItem("Learning Rate", tabName = "lr_tab", icon = icon("cogs")),
      actionButton("reset", "Reset Filters")
    )
  ),
  
  dashboardBody(
    tabItems(
      # --- Model Metrics Tab ---
      tabItem(tabName = "metrics_tab",
              fluidRow(
                box(width = 4,
                    selectInput("selected_models", "Select Models:",
                                choices = rownames(metrics),
                                multiple = TRUE,
                                selected = rownames(metrics)[1:3]),
                    checkboxGroupInput("selected_metrics", "Select Metrics:",
                                       choices = colnames(metrics),
                                       selected = colnames(metrics)[1:5])
                ),
                box(width = 8,
                    plotlyOutput("metrics_barplot"),
                    DTOutput("conf_matrix_table")
                )
              )
      ),
      
      # --- Tuning Comparison Tab ---
      tabItem(tabName = "tuning_tab",
              fluidRow(
                box(width = 4,
                    selectInput("selected_tuning", "Select Tuning Methods:",
                                choices = rownames(tuning_methods),
                                multiple = TRUE,
                                selected = rownames(tuning_methods)[1:3]),
                    checkboxGroupInput("selected_tuning_metrics", "Select Metrics:",
                                       choices = colnames(tuning_methods),
                                       selected = colnames(tuning_methods)[1:5])
                ),
                box(width = 8,
                    plotlyOutput("tuning_lollipop")
                )
              )
      ),
      
      # --- Learning Rate Tab ---
      tabItem(tabName = "lr_tab",
              fluidRow(
                box(width = 4,
                    sliderInput("lr_slider", "Learning Rate:",
                                min = min(as.numeric(rownames(perf_by_lr))),
                                max = max(as.numeric(rownames(perf_by_lr))),
                                value = min(as.numeric(rownames(perf_by_lr))),
                                step = 0.001)
                ),
                box(width = 8,
                    plotlyOutput("lr_area_chart")
                )
              )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # --- Reset Button ---
  observeEvent(input$reset, {
    updateSelectInput(session, "selected_models", selected = rownames(metrics)[1:3])
    updateCheckboxGroupInput(session, "selected_metrics", selected = colnames(metrics)[1:5])
    updateSelectInput(session, "selected_tuning", selected = rownames(tuning_methods)[1:3])
    updateCheckboxGroupInput(session, "selected_tuning_metrics", selected = colnames(tuning_methods)[1:5])
    updateSliderInput(session, "lr_slider", value = min(as.numeric(rownames(perf_by_lr))))
  })
  
  # --- Metrics Bar Plot ---
  output$metrics_barplot <- renderPlotly({
    req(input$selected_models)
    req(input$selected_metrics)
    
    df <- metrics[input$selected_models, input$selected_metrics, drop = FALSE]
    df_long <- df %>% 
      rownames_to_column("Model") %>% 
      pivot_longer(-Model, names_to = "Metric", values_to = "Value")
    
    plot_ly(df_long, x = ~Model, y = ~Value, color = ~Metric, type = 'bar') %>%
      layout(yaxis = list(title = 'Metric Value'))
  })
  
  # --- Confusion Matrix Table ---
  output$conf_matrix_table <- renderDT({
    req(input$selected_models)
    conf_matrix_df <- conf_matrices[input$selected_models, ]
    datatable(conf_matrix_df, options = list(pageLength = 5))
  })
  
  # --- Tuning Lollipop Chart ---
  output$tuning_lollipop <- renderPlotly({
    req(input$selected_tuning)
    req(input$selected_tuning_metrics)
    
    df <- tuning_methods[input$selected_tuning, input$selected_tuning_metrics, drop = FALSE]
    df_long <- df %>% 
      rownames_to_column("TuningMethod") %>% 
      pivot_longer(-TuningMethod, names_to = "Metric", values_to = "Value")
    
    plot_ly(df_long, x = ~TuningMethod, y = ~Value, color = ~Metric, 
            type = 'scatter', mode = 'markers+lines') %>%
      layout(yaxis = list(title = 'Metric Value'))
  })
  
  # --- Learning Rate Stacked Area Chart ---
  output$lr_area_chart <- renderPlotly({
    lr_val <- input$lr_slider
    
    df_lr <- perf_by_lr[as.numeric(rownames(perf_by_lr)) <= lr_val, ]
    df_lr <- df_lr %>% rownames_to_column("LearningRate")
    
    df_long <- df_lr %>% 
      pivot_longer(-LearningRate, names_to = "Metric", values_to = "Value")
    
    plot_ly(df_long, x = ~LearningRate, y = ~Value, color = ~Metric, 
            type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
      layout(yaxis = list(title = "Metric Value"), xaxis = list(title = "Learning Rate"))
  })
}

# --- Run App ---
shinyApp(ui, server)
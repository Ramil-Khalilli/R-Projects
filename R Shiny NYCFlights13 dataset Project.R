library(tidyverse)
library(ggplot2)
library(shiny)
library(nycflights13)

datasets <- ls("package:nycflights13")
visuals <- c('Histogram', 'Boxplot', 'BarPlot')

ui <- fluidPage(
  titlePanel(title = 'NYC Flights Datasets'),
  selectInput(inputId = 'dataset', label = 'Please, select a dataset', choices = datasets, selected = datasets[1]),
  selectInput(inputId = 'visual', label = 'Choose a visual type', choices = visuals, selected = visuals[3]),
  selectInput(inputId = 'num_col', label = 'Choose a numeric column', choices = NULL),
  selectInput(inputId = 'cat_col', label = 'Choose a categorical column', choices = NULL),
  plotOutput('plot')
)

server <- function(input, output, session){
  observeEvent(input$dataset, {
    df <- get(input$dataset, 'package:nycflights13')
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[!sapply(df, is.numeric)]
    updateSelectInput(
      session = session,
      inputId = 'num_col',
      choices = num_cols,
      selected = num_cols[1]
    )
    updateSelectInput(
      session = session,
      inputId = 'cat_col',
      choices = cat_cols,
      selected = cat_cols[1]
    )})
  observeEvent(input$visual, {
    df <- get(input$dataset, 'package:nycflights13')
    cat_cols <- names(df)[!sapply(df, is.numeric)]
    if (input$visual %in% c('Histogram', 'Boxplot')){
      updateSelectInput(
        session = session,
        inputId = 'cat_col',
        choices = NULL,
        selected = NULL
      )
    }
      else
      {
        updateSelectInput(
          session = session,
          inputId = 'cat_col',
          choices = cat_cols,
          selected = cat_cols[1]
        )
      }
    })
  output$plot <- renderPlot({
    df <- get(input$dataset, 'package:nycflights13')
    if (input$visual == 'Histogram'){
      df %>%
        ggplot(aes_string(x = input$num_col)) +geom_histogram(fill = 'blue')
    }
    else if (input$visual == 'Boxplot'){
      df %>%
        ggplot(aes_string(x = input$num_col)) +geom_boxplot(fill = 'lightblue')
    }
    else {
      df %>%
        ggplot(aes_string(y = input$num_col, x = input$cat_col)) +geom_col(fill = 'green')
    }
  })
}

shinyApp(ui, server)


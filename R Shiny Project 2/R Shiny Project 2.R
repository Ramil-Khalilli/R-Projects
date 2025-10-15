# Loading required libraries.
library(shiny)
library(shinythemes)
library(dplyr)
library(tibble)
library(skimr)
library(inspectdf)
library(ggplot2)

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Exploratory Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Which type of variable?", c("is.numeric", "is.character")),
      numericInput("n", "Number of samples:", 5, min = 1, max = 500)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Glimpse", verbatimTextOutput("glimpse_output")),
        tabPanel("Skim Summary", verbatimTextOutput("skim_output")),
        tabPanel("Missing Data", plotOutput("na_plot")),
        tabPanel("Correlation", plotOutput("cor_plot")),
        tabPanel("Grouped Profit", tableOutput("profit_table"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Directly read from your local path
  data <- read.csv("C:/Users/RAMIL/Downloads/Future-500.csv", stringsAsFactors = FALSE)
  
  # 1. Glimpse
  output$glimpse_output <- renderPrint({
    glimpse(data)
  })
  
  # 2. Skim by variable type
  output$skim_output <- renderPrint({
    if (input$type == "is.numeric") {
      skim(data[sapply(data, is.numeric)])
    } else {
      skim(data[sapply(data, is.character)])
    }
  })
  
  # 3. inspect_na
  output$na_plot <- renderPlot({
    show_plot(inspect_na(data))
  })
  
  # 4. inspect_cor
  output$cor_plot <- renderPlot({
    num_df <- data[sapply(data, is.numeric)]
    if (ncol(num_df) > 1) {
      show_plot(inspect_cor(num_df))
    }
  })
  
  # 5. Group by Industry & Inception, total profit, sort
  output$profit_table <- renderTable({
    req("Industry" %in% names(data), "Inception" %in% names(data), "Profit" %in% names(data))
    data %>%
      group_by(Industry, Inception) %>%
      summarise(Total_Profit = round(sum(Profit, na.rm = TRUE), 2)) %>%
      arrange(desc(Total_Profit)) %>%
      head(input$n)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

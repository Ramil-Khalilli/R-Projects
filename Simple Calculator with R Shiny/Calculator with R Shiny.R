library(shiny)

ui <- fluidPage(
  titlePanel(title = 'Simple Calculator'),
  textInput(inputId = 'n1', label = 'Number 1:', placeholder = ''),
  selectInput(inputId = 'op', label = 'Operation:', choices = c('+', '-', '*', '/')),
  textInput(inputId = 'n2', label = 'Number 2:', placeholder = ''),
  textOutput('result')
)

server <- function(input, output, session){
  output$result <- renderText({
    n1 <- as.integer(input$n1)
    n2 <- as.integer(input$n2)
    op <- input$op
    if (op == '/'){
      validate(
        need(expr = n2 != 0, message = 'Error: Cannot divide by zero!')
      )
    }
    result <- switch(op,
                     '+' = n1 + n2,
                     '-' = n1 - n2,
                     '*' = n1 * n2,
                     '/' = n1 / n2)
    paste("The Result:", result, sep = ' ')
  })
}

shinyApp(ui, server)

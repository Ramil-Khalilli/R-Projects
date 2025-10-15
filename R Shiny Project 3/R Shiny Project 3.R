# 1. Load the three datasets — injuries.csv, products.csv, and population.csv — 
# into R and check their dimensions and data types using an appropriate function.
injuries <- read.csv('C:/Users/RAMIL/Downloads/injuries.csv')
products <- read.csv('C:/Users/RAMIL/Downloads/products.csv')
population <- read.csv('C:/Users/RAMIL/Downloads/population.csv')

# 2. Find the most frequent `prod_code` in the `injuries` dataset.
library(tidyverse)
prod_code_ms <- injuries %>%
  group_by(prod_code) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 1) %>%
  pull(prod_code)
prod_code_ms

# 3. Identify the product name (`title`) that corresponds to the `prod_code` you found.
title_product <- products %>%
  filter(prod_code == prod_code_ms) %>%
  pull(title)
title_product

# 4. Create a separate dataset for the selected `prod_code`.
injuries_1842 <- injuries %>%
  filter(prod_code == prod_code_ms)

injuries_1842 %>% View()

# 5. Calculate the frequency of diagnoses in this subset and display the results 
# in descending order.
injuries_1842 %>%
  group_by(diag) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 6. Determine the distribution of injuries by body part and present the results.
injuries_1842 %>%
  count(body_part, sort = TRUE)

# 7. Analyze and show the distribution of injury locations (`location`).
injuries_1842 %>%
  count(location) %>%
  arrange(desc(n))

# 8. Combine injury data with population data by age and sex, then calculate 
# the injury rate per 10,000 people.
data <- injuries_1842 %>%
  group_by(age, sex) %>%
  summarise(count = n()) %>%
  left_join(population, by = c('age', 'sex')) %>%
  mutate(per = count*10000/population)
data

# 9. Visualize the calculated injury rate by age and sex.
data %>%
  ggplot(aes(x = age, y = per, color = sex)) +
  geom_line()


# 10. Build an interactive Shiny app that dynamically displays diagnosis, 
# body part, location, and age–sex charts based on the selected product.
library(shiny)
ui <- fluidPage(
  fluidRow(
    column(6,selectInput('code','Prodcuts',choices = products$title))
  ),
  fluidRow(
    column(4,tableOutput('diag')),
    column(4,tableOutput('body_part')),
    column(4,tableOutput('location'))
  ),
  fluidRow(column(12,plotOutput('sex_age'))))

server <- function(input, output, session) {
  data <- reactive(injuries %>% left_join(products,by='prod_code') %>% filter(title==input$code)) #left_join mentiqi title-leri gormek ucun,artiq bir title secende butun qrafiklere tesir edecek
  output$diag <- renderTable(data() %>% count(diag,sort=T))
  output$body_part <- renderTable(data() %>% count(body_part,sort=T))
  output$location <- renderTable(data() %>% count(location,sort=T))
  output$sex_age <- renderPlot(data() %>% 
                                 count(age,sex) %>% 
                                 left_join(population,by = c('age','sex')) %>%  
                                 mutate(rate= n/population * 10000) %>% 
                                 ggplot(aes(age,rate,colour = sex)) +
                                 geom_line() + coord_cartesian(xlim=c(0,85))+
                                 labs(y = 'Injuries per 10000 people'))
  
}

shinyApp(ui, server)
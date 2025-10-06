# 1. Import datasets in R.
flights <- read.csv('C:/Users/RAMIL/Downloads/flights.csv')
airlines <- read.csv('C:/Users/RAMIL/Downloads/airlines.csv')
airports <- read.csv('C:/Users/RAMIL/Downloads/airports.csv')

View(flights)
View(airlines)
View(airports)

# 2. Join “airlines”, “airports”, “flights” datasets.
library(tidyverse)
library(ggplot2)
library(naniar)
data <- flights %>% 
  left_join(airlines, by = c('AIRLINE' = 'IATA_CODE')) %>% 
  left_join(airports, by = c('ORIGIN_AIRPORT' = 'IATA_CODE'))

# 3. Select distinct values of the AIR_TIME column.
data %>% select(AIR_TIME) %>% unique()

# 4. Reorder the flights data by DEPARTURE_TIME, then by descending DEPARTURE_DELAY.
flights %>% arrange(DEPARTURE_TIME, desc(DEPARTURE_DELAY))

# 5. Paste together ARRIVEL_TIME and ARRIVEL_DELAY with “<->”.
paste(data$ARRIVAL_TIME, data$ARRIVAL_DELAY, sep = '<->')

# 6. Create histogram of FLIGHT_NUMBER in flights dataset.
flights %>%
  ggplot(aes(x = FLIGHT_NUMBER)) +
  geom_histogram(fill = 'darkblue') +
  labs(title = 'Distribution of Flight Number.')

# 7. Create Barplot of FLIGHT_NUMBER with color fill defined by AIRLINE.
flights %>%
  ggplot(aes(x = AIRLINE, y = FLIGHT_NUMBER)) +
  geom_col()

# 8. Create a scatter plot of SCHEDULED_DEPARTURE versus DEPARTURE_TIME.Afterwards play
# around with alpha and color argument to clarify information.
data %>%
  ggplot(aes(x = SCHEDULED_DEPARTURE, y = DEPARTURE_TIME, color = AIRLINE, alpha = 0.7)) +
  geom_point()

# 9. Explore missingness of arr_delay by carrier using geom_miss_point.
data %>%
  ggplot(aes(x = ARRIVAL_DELAY, y = DEPARTURE_DELAY)) +
  geom_miss_point()


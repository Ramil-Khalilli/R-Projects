library(nycflights13)
library(tidyverse)
library(dplyr)

data(package = "nycflights13")
flights %>%  View()
airports %>% View()



# Assign datasets to your own variables
flights_df   <- flights
airlines_df  <- airlines
airports_df  <- airports
planes_df    <- planes
weather_df   <- weather

flights_df %>% View(title = 'flights')
airlines_df %>% View(title = 'airlines')
airports_df %>% View(title = 'airports')
planes_df %>% View(title = 'planes')
weather_df %>% View(title = 'weather')

# Questions.
# 1. What is the average airtime for each origin airport (names) in each of the months of 2013.
data <- inner_join(x = airports_df, y = flights_df, by = c('faa' = 'origin')) %>%
  group_by(name) %>% 
  summarise(avg_airtime = mean(air_time, na.rm = TRUE)) %>%
  arrange(desc(avg_airtime))

data %>%
  mutate(highest = ifelse(avg_airtime == max(avg_airtime), 'Highest', 'No')) %>%
  ggplot(aes(x = name, y = avg_airtime, fill = highest)) +
  geom_col() +
  labs(title = 'Airports with Average Air Time', x = 'Airport', y = 'Average Air Time') +
  geom_text(aes(label = paste(round(avg_airtime), 'min', sep = ' '), vjust = 1.5)) +
  theme_minimal() +
  scale_fill_manual(values = c('Highest' = 'red', 'No' = 'blue'))

# 2. In the winter months show top 5 carrier type of airplane that was late (arrival) at least for 15 minutes.
winter_months <- c(1, 11, 12)
data <- flights_df %>%
  filter(arr_delay >= 15, month %in% winter_months) %>%
  group_by(carrier) %>%
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_arr_delay)) %>%
  slice_head(n = 5)

data %>%
  mutate(highest = ifelse(avg_arr_delay == max(avg_arr_delay), 'Highest', 'Other')) %>%
  ggplot(aes(x = carrier, y = avg_arr_delay, fill = highest)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c('Highest' = 'red', 'Other' = 'blue')) +
  geom_text(data = . %>% filter(avg_arr_delay == max(avg_arr_delay)),
            aes(label = paste0(round(avg_arr_delay), 'min')), vjust = 1, hjust = -0.4) +
  labs(title = 'Carriers vs Average Arrival Delay', x = 'Carrier', y = 'Arrival Delay') +
  theme(axis.text.x = element_text(angle = 0, vjust = 5))

# 3. Select the airport that was destination of an airplane that was most late in each of the time zone.
data <- inner_join(airports, flights_df, by = c('faa' = 'dest'))
data %>%
  filter(arr_delay > 0) %>%
  group_by(tzone, faa) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(tzone) %>%
  mutate(rank = dense_rank(desc(avg_delay))) %>%
  filter(rank == 1) %>%
  select(-rank) %>%
  ggplot(aes(x = faa, y = avg_delay)) +
  geom_col(fill = 'darkgreen') +
  labs(x = 'Airport', y = 'Average Delay', title = 'Airports vs Average Delay')

# 4. Observe if being late for arrival has some kind of significant relationship to the type of an airplane. (Show average arr_delay for each).
data <- inner_join(planes_df, flights_df, by = 'tailnum')
data %>%
  filter(arr_delay > 0) %>%
  group_by(type) %>%
  summarise(avg_delay = mean(arr_delay)) %>%
  ggplot(aes(x = type, y = avg_delay)) +
  geom_col(fill = 'red') +
  labs(title = 'Plane Type vs Average Arrival Delay', x = 'Plane Type', y = 'Avg Delay') +
  geom_text(aes(label = round(avg_delay), vjust = -0.5))

# 5. See if being late for arrival has something to do with wind speed.
data <- inner_join(flights_df, weather_df, by = 'time_hour')
data %>%
  mutate(late = ifelse(coalesce(arr_delay, 0) > 0, 'Yes', 'No')) %>%
  group_by(late) %>%
  summarise(avg_wind_speed = mean(wind_speed, na.rm = TRUE)) %>%
  ggplot(aes(x = late, y = avg_wind_speed)) +
  geom_col(fill = 'green') +
  labs(title='Being Late vs Wind Speed', x = 'Late?', y = 'Wind Speed', subtitle = 'Wind Speed has some effect on arrival time')

# 6. Show the share of top 3 manufacturers for the number of flights in 2013.
data <- inner_join(flights_df, planes_df, by = 'tailnum')
data %>% filter(year.x == 2013) %>% group_by(manufacturer) %>%
  summarise(n_flights = n()) %>% slice_max(order_by = n_flights, n = 3) %>%
  ggplot(aes(x = '', y = n_flights, fill = manufacturer)) +
  geom_col() +
  coord_polar(theta = 'y') +
  labs(title='Top 3 manufacturer with most flights', y = 'Number of Flights', x = '') +
  geom_text(aes(label = n_flights), position = position_stack(vjust = 0.5))

# 7. Observe in which season planes are more likely to be late for arrival.
flights_df %>% mutate(season = ifelse(month %in% c(3, 4, 5), 'Spring', ifelse(month %in% c(6, 7, 8), 'Summer', ifelse(month %in% c(9, 10, 11), 'Autumn', 'Winter')))) %>%
  mutate(late = ifelse(coalesce(arr_delay, 0) > 0, 'Yes', 'No')) %>%
  group_by(season, late) %>% summarise(n = n(), .groups = 'drop_last') %>%
  mutate(late_ratio = sum(ifelse(late == 'Yes', n, 0))*100/sum(n)) %>%
  filter(late == 'Yes') %>% select(-c(late, n)) %>% arrange(desc(late_ratio)) %>%
  ggplot(aes(x = '', y = late_ratio, fill = season)) +
  geom_col() +
  coord_polar(theta = 'y') +
  geom_text(aes(label = paste0(round(late_ratio), '%')), position = position_stack(vjust = 0.5)) +
  labs(x = '', y = 'Being_Late_Ratio')

# 8. Find the top 3 manufacturers that have fastest airplanes according to average air_time per distance of each manufacturer.
data <- inner_join(flights_df, planes_df, by = 'tailnum')
data %>% mutate(airtime_per_dist = air_time/distance) %>% group_by(manufacturer) %>%
  summarise(avg_airtime_per_dist = mean(airtime_per_dist, na.rm = TRUE)) %>% slice_max(order_by = avg_airtime_per_dist, n = 3) %>%
  ggplot(aes(x = manufacturer, y = avg_airtime_per_dist)) +
  geom_col(fill = 'brown') +
  labs(x = 'Manufacturer', y = 'Average Air Time per Distance') +
  geom_text(aes(label = round(avg_airtime_per_dist, 2), vjust = -0.5))

# 9. Visualize average air time of airplanes flying from JFK airport for each month in order (Jan-Dec).
flights_df %>% filter(origin == 'JFK') %>% group_by(month) %>% summarise(avg_airtime = mean(air_time, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = avg_airtime)) +
  geom_line(color = 'red') + geom_point(color = 'black') +
  theme_minimal() +
  scale_x_discrete(limits = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))

# 10. Which 3 airplane manufacturers' planes are more likely to be both late at departure and arrival at the same time.
data <- inner_join(planes_df, flights_df, by = 'tailnum')
data %>% mutate(late_both = ifelse(coalesce(dep_delay, 0) > 0 & coalesce(arr_delay, 0) > 0, 1, 0)) %>%
  select(manufacturer, late_both) %>% group_by(manufacturer, late_both) %>%
  summarise(n = n()) %>% ungroup() %>% group_by(manufacturer) %>%
  mutate(per_ = sum(ifelse(late_both == 1, n, 0))/sum(n)) %>% filter(late_both == 1) %>%
  ungroup() %>%
  slice_max(order_by = per_, n = 3) %>%
  ggplot(aes(x = manufacturer, y = per_*100)) +
  geom_col(fill = 'yellow') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  labs(y = 'both_arrival_departure_ratio')

# 11. Delay Trends by Airline Over the Year.
# Compute average arrival delay by airline and month. Then visualize the trend lines to compare how different airlines perform across the year.
data <- inner_join(airlines_df, flights_df, by = 'carrier')
data %>% 
  filter(arr_delay > 0) %>% group_by(month, name) %>% summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = avg_arr_delay, color = name)) +
  geom_line()

# 12. Weather Impact on Delays
# Calculate average departure delay vs. temperature buckets (e.g., cut into bins of 5°C). Visualize with a scatter +
# smoothed regression line.
weather_df$temp <- (weather_df$temp - 32)*(5/9)
weather_df$temp_scaled <- round(coalesce(weather_df$temp, mean(weather_df$temp, na.rm = TRUE))/5)*5
data <- inner_join(weather_df, flights_df, by = 'time_hour')
data %>% group_by(temp_scaled) %>% summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = temp_scaled, y = avg_dep_delay)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal()

# 13. Flight Distance vs. Air Time by Manufacturer
# Plot distance vs. air_time, colored by manufacturer (e.g., Boeing vs. Airbus). Add regression lines for each manufacturer.
data <- inner_join(planes_df, flights_df, by = 'tailnum')
data <- data[sample(nrow(data), 1000), ]
data %>%
  filter(manufacturer %in% c('BOEING', 'AIRBUS')) %>%
  mutate(distance = coalesce(distance, mean(distance, na.rm = TRUE)), air_time = coalesce(air_time, mean(air_time, na.rm = TRUE))) %>%
  ggplot(aes(x = distance, y = air_time, color = manufacturer)) +
  geom_point() +
  labs(x = 'Distance', y = 'Air Time')

# 14. Most Reliable Airports.
# Compute average arrival delay by destination airport. Plot the top 15 airports as a horizontal bar chart (ordered by delay).
library(forcats)
flights_df %>% 
  filter(arr_delay > 0) %>%
  group_by(dest) %>% summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  slice_min(order_by = avg_arr_delay, n = 15) %>%
  ggplot(aes(x = avg_arr_delay, y = fct_reorder(dest, avg_arr_delay))) +
  geom_col(fill = 'darkgreen') +
  theme_minimal()

# 15. Distribution of delays by Airline.
# For each airline, visualize the distribution of arrival delays as a boxplot.
# This allows comparison of not just averages but variability and outliers.
flights_df %>%
  ggplot(aes(x = carrier, y = arr_delay)) +
  geom_boxplot() +
  labs(title = 'Airlines vs Arrival Delay', subtitle = 'There are many outliers')

# 16. Heatmap of Delays by Day and Hour.
# Create a heatmap of average departure delay by day and hour across all flights.
flights_df %>% group_by(day, hour) %>% summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = day, y = hour, fill = avg_dep_delay)) +
  geom_tile() +
  scale_fill_gradient(low = 'lightblue', high = 'darkblue')

# 17. Effect of Wind Speed on Flight Duration.
# Plot air_time vs. wind_speed to check if high winds shorten or lengthen flights. Use scatterplot + smoothing curve.
data <- inner_join(flights_df, weather_df, by = c('origin' = 'origin', 'time_hour' = 'time_hour'))
data <- data[sample(nrow(data), 1000), ]
data %>% ggplot(aes(x = wind_speed, y = air_time)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'WInd Speed vs Air Time', x = 'Wind Speed', y = 'Air Time')

# 18. Airline Market Share.
# Compute the percentage of flights by airline across the whole dataset. Display as a bar chart sorted from largest to smallest.
flights_df %>% 
  group_by(carrier) %>% summarise(number_of_flights = n()) %>%
  mutate(color_ = ifelse(number_of_flights == max(number_of_flights), 'h', 'o')) %>%
  ggplot(aes(x = number_of_flights, y = fct_reorder(carrier, number_of_flights), fill = color_)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c('h' = 'green', 'o' = 'blue')) +
  labs(title = 'Airline Market Share', y = 'Airlines', x = 'Flights')

# 19. Faceted Trends: Distance vs. Delays by Carrier
# Plot average arrival delay vs. average distance for each airline. Use facets facet_wrap so each airline gets its own panel.
data <- flights_df[sample(nrow(flights_df), 1000), ]
data %>%
  ggplot(aes(x = distance, y = arr_delay)) +
  geom_point() +
  facet_wrap(~ data$carrier) +
  labs(title = 'Distance vs Delays per Airline', x = 'Distance', y = 'Delay_min')


# 20. Flights Volume vs Delays over Time.
# For each month, compute number of flights + average delay. Create a dual
# visualization: bar chart for flights count + line for average delay.
flights_df %>% group_by(month) %>%
  summarise(flights = n(), avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = month)) +
  geom_col(aes(y = flights/1000), fill = 'blue') +
  geom_line(aes(y = avg_delay), color = 'red', size = 1) +
  labs(x = 'Month', y = 'Flights(k) and Average Delay')

# 21. Airline Punctuality Ranking.
# Compute on-time performance (%) for each airline (flights arriving ≤15 min late).
# Rank carriers and show a horizontal bar chart with percentages.
flights_df %>%
  mutate(late = ifelse(coalesce(arr_delay, 0) <= 15, 'No', 'Yes')) %>%
  group_by(carrier, late) %>%
  summarise(n = n(), .groups = 'drop_last') %>%
  mutate(delay_per = sum(ifelse(late == 'No', n, 0))*100/sum(n)) %>%
  filter(late == 'No') %>%
  select(carrier, delay_per) %>%
  ggplot(aes(y = fct_reorder(carrier, delay_per), x = delay_per)) +
  geom_col(fill = 'darkgreen') +
  labs(title = 'Carrier vs Punctuality Rate', y = 'Carrier', x = 'On Time Rate(%)')

# 22. Departure Delays by Day of Week.
# Which days of the week see the worst delays? Summarize average 
# dep_delay by weekday and visualize with a bar chart ordered Mon–Sun.
flights_df %>%
  mutate(date = make_date(year = year, month = month, day = day), weekday = wday(date, label = TRUE, week_start = 1)) %>%
  group_by(weekday) %>% summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = weekday, y = avg_dep_delay)) +
  geom_col(fill = 'orange') +
  theme_minimal() +
  labs(title = 'Weekdays vs Average Departure Delay', x = 'Weekday', 'y = Avg Departure Delay') +
  geom_text(aes(label = paste(round(avg_dep_delay), 'min', sep = ' '), vjust = -0.5))

# 23. Aircraft Size vs. Number of Flights.
# Do larger planes (by seats) carry proportionally more flights? 
# Join flights + planes, group by seat category (e.g., small <150, medium 150–250, large >250),
# and plot bar chart of total flights.
data <- inner_join(flights_df, planes_df, by = 'tailnum')
data %>% mutate(seat_cat = ifelse(seats < 150, 'small', ifelse(seats > 250, 'large', 'medium'))) %>%
  group_by(seat_cat) %>% summarise(n_flights = n()) %>%
  ggplot(aes(x = n_flights, y = seat_cat)) +
  geom_col(fill = 'darkblue') +
  labs(title = 'Aircraft Size vs Number of Flights', x = 'Flights', y = 'Seat Category') +
  theme_minimal()

# 24. Engine Type and Distance Patterns.
# Compare average distance flown for different engine types from planes.
# Visualize as boxplots to show distribution spread.
data <- inner_join(planes_df, flights_df, by = 'tailnum')
data %>% ggplot(aes(x = engine, y = distance)) +
  geom_boxplot(fill = 'lightblue') +
  labs(title = 'Engine Type vs Distance Covered', x = 'Engine Type', y = 'Distance Dist') +
  theme_minimal()

# 25. Manufacturer Reliability Proxy.
# Which manufacturers’ planes (Boeing, Airbus, Embraer, etc.) tend to have longer
# air_time relative to distance efficiency? Create a scatterplot of distance vs. air_time
# colored by manufacturer.
data <- inner_join(planes_df, flights_df, by = 'tailnum')
data <- data[sample(nrow(data), 1000), ]
data %>% 
  filter(manufacturer %in% c('BOEING', 'AIRBUS', 'EMBRAER')) %>%
  ggplot(aes(x = distance, y = air_time, color = manufacturer)) +
  geom_point() +
  labs(title = 'Distance vs Air Time by Manufacturer', x = 'Distance', y = 'Air Time') +
  theme_minimal()

# 26. Seasonal Weather Impact on Flight Volume.
# Use weather and flights: aggregate number of flights per month vs. avg temperature
# at origin airports. Show line chart for both(flights vs.temp).
flights_df %>% 
  mutate(date = make_date(year = year, month = month, day = day),
         month_name = month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(month_name) %>% summarise(n_flights = n()) %>%
  ggplot(aes(y = month_name, x = n_flights)) +
  geom_col(fill = 'orange') +
  theme_minimal() +
  labs(title = 'Number of Flights in Months', x = 'Flights', y = 'Month') +
  scale_y_discrete(limits = c( 'Dec', 'Nov', 'Oct', 'Sep', 'Aug', 'Jul', 'Jun', 'May', 'Apr', 'Mar', 'Feb', 'Jan'))

data <- inner_join(weather_df, flights_df, by = c('time_hour' = 'time_hour', 'origin' = 'origin'))
data %>% group_by(origin) %>% summarise(avg_temp = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = origin, y = avg_temp)) +
  coord_cartesian(ylim = c(50, 60)) +
  geom_col(fill = 'brown') +
  labs(title = 'Avg Temperature vs Airports', x = 'Airport', y = 'Temperature(F)') +
  geom_text(aes(label = paste(round(avg_temp, 2), 'F', sep = ' '), vjust = 1.5)) +
  theme_minimal()

# 27.  Plane Age and Usage.
#  For each aircraft, compute its age in 2013 (2013 - year) and total flights it
# made. Visualize with a scatterplot (age vs. number of flights).
data <- left_join(planes_df, flights_df, by = 'tailnum')
data %>% mutate(plane_age = 2013 - year.x) %>%
  group_by(plane_age) %>% summarise(total_flights = n()) %>%
  ggplot(aes(x = plane_age, y = total_flights)) +
  geom_point() +
  labs(title = 'Plane Age vs Distance Covered', x = 'Plane Age', y = 'Distance')

# 28.  Weather and Aircraft Interaction.
# Do heavier planes (more seats) fly more consistently in bad weather? Join flights and planes and weather.
# Group by seat category, and plot boxplots of visib during flights.
data <- inner_join(flights_df, planes_df, by = 'tailnum')
data <- inner_join(data, weather_df, by = c('time_hour' = 'time_hour', 'origin' = 'origin'))
data %>% mutate(seat_cat = ifelse(seats < 150, 'small', ifelse(seats > 250, 'large', 'medium'))) %>%
  ggplot(aes(x = seat_cat, y = visib)) +
  geom_boxplot(fill = 'lightblue') +
  theme_minimal()


# 29.  Airlines’ Fleet Composition.
# For each airline, show the proportion of flights by aircraft manufacturer (Boeing,
# Airbus, etc.) using a faceted bar chart (one facet per airline).
data <- inner_join(planes_df, flights_df, by = 'tailnum')
data %>% group_by(carrier, manufacturer) %>% summarise(n_flights = n(), .groups = 'drop_last') %>%
  mutate(flights_per = n_flights*100/sum(n_flights)) %>%
  ggplot(aes(x = manufacturer, y = n_flights)) +
  geom_col() +
  facet_wrap(~ carrier)

# 30. Airport Performance by Distance & Delay.
# Which airports (by origin) handle the longest average flight distances and 
# how does this relate to their average arrival delay?
data <- flights_df %>% group_by(origin, dest) %>%
  summarise(avg_dist = mean(distance, na.rm = TRUE)) %>% 
  ungroup() %>%
  slice_max(order_by = avg_dist, n = 30)

inner_join(data, flights_df, by = c('origin' = 'origin', 'dest' = 'dest')) %>%
  group_by(origin) %>%
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = origin, y = avg_arr_delay)) +
  geom_col() +
  theme_minimal() +
  labs(x = 'Origin', y = 'Average Arrival Delay', subtitle = 'JFK handles longer distances better')

# 31. Airport Connection Network.
# Which airports are the most connected hubs (serving the widest variety of destinations)?
# Preprocessing: from flights, group by origin → count unique dest.
# Visualization: bar chart sorted descending → top 15 airports by unique destinations.
library(forcats)
flights_df %>% group_by(origin) %>% summarise(n_dest = length(unique(dest))) %>%
  ggplot(aes(x = fct_reorder(origin, desc(n_dest)), y = n_dest)) +
  geom_col(fill = 'brown') +
  labs(title = 'Airports vs Number of unique destinations', x = 'Airports', y = 'Destinations')

# 32. Airport Elevation Impact on Flight Operations.
# Does the elevation of airports (alt from airports) impact the average speed (distance/air_time) of flights?
# Preprocessing: join flights + airports on origin → calculate avg flight speed per airport → plot vs airport elevation.
data <- inner_join(airports_df, flights_df, by = c('faa' = 'origin'))
data %>% mutate(speed = distance/air_time) %>%
  group_by(faa, lat) %>%
  summarise(avg_speed = mean(speed, na.rm = TRUE)) %>%
  ggplot(aes(x = lat, y = avg_speed)) +
  geom_point() +
  geom_line() +
  labs(x = 'Latitude', y = 'Average Speed', subtitle = 'There is negativ correlation between these two.')


















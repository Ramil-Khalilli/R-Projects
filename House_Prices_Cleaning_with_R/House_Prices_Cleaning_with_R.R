# Importing necessary libraries.
library(tidyverse)
library(forcats)

# Fetching the data.
data <- read.csv('C:/Users/RAMIL/Desktop/house_prices.csv')
# Observing the data.
head(data)

# Removing unnecessaryv columns for modeling.
data <- data %>% select(-Title, -Description)

# Getting information about the columns of the data.
str(data)

# Getting rid of duplicates in our dataset.
data <- unique(data)

# Seeing what kinds of currencies we have got inside 'Amount.in.rupees.' column.
data %>% pull(Amount.in.rupees.) %>% str_extract('[a-zA-Z]+') %>% unique()

# Converting amounts into US Dollar.
data <- data %>% 
  mutate(amount_unit = str_extract(Amount.in.rupees., '[a-zA-Z]+'),
         amount_value = as.numeric(str_extract(Amount.in.rupees., '[0-9]+'))) %>%
  mutate(amount_usd = ifelse(amount_unit == 'Cr', amount_value*10000000/88.74,
                             ifelse(amount_unit == 'Lac', amount_value*100000/88.74, NA_real_))) %>%
  select(-amount_unit, -amount_value, -Amount.in.rupees.)

# Converting each price to US Dollar in 'Price..in.rupees.' column.
data <- data %>% mutate(price_usd = Price..in.rupees./88.74) %>% select(-Price..in.rupees.)

# Seeing what kinds of length units we have got inside 'Carpet.Area' column.
data %>% pull(Carpet.Area) %>% str_extract('[a-zA-Z]+') %>% unique()

# Converting each length into sqft (square-feet).
data <- data %>%
  mutate(carpet_area_unit = str_extract(Carpet.Area, '[a-zA-Z]+'),
                carpet_area_value = as.numeric(str_extract(Carpet.Area, '[0-9.]+'))) %>%
  mutate(carpet_are_sqft = ifelse(carpet_area_unit == 'sqft', carpet_area_value,
                                  ifelse(carpet_area_unit == 'sqm', carpet_area_value*10.7639,
                                         ifelse(carpet_area_unit == 'sqyrd', carpet_area_value*9,
                                                ifelse(carpet_area_unit == 'acre', carpet_area_value*43560,
                                                       ifelse(carpet_area_unit == 'ground', carpet_area_value*2400,
                                                              ifelse(carpet_area_unit == 'cent', carpet_area_value*435.6,
                                                                     ifelse(carpet_area_unit == 'bigha', carpet_area_value*14400,
                                                                            ifelse(carpet_area_unit == 'marla', carpet_area_value*272.25,
                                                                                   ifelse(carpet_area_unit == 'kanal', carpet_area_value*5445, NA_real_))))))))))
# Getting rid of temporary columns.
data <- data %>% select(-carpet_area_unit, -carpet_area_value)
head(data)

# Extracting both apartment floor and building floor from the 'Floor' column.
data <- data %>% mutate(floor = str_extract(Floor, '[0-9]+'), building_floor = str_extract_all(Floor, '[0-9]+') %>% map_chr(~ .x[2]))
data %>% pull(Floor) %>% str_extract_all('[a-zA-Z]+') %>% map_chr(~ paste(.x[1], .x[2], sep = ' ')) %>% unique()
data$floor <- as.integer(data$floor)
data$building_floor <- as.integer(data$building_floor)
data <- data %>% mutate(
  two_string = str_extract_all(Floor, '[a-zA-Z]+') %>% map_chr(~ paste(.x[1], .x[2], sep = ' ')),
  floor = case_when(
    two_string %in% c('Ground out', 'Ground NA') ~ 0,
    two_string == 'Upper Basement' ~ -1,
    two_string == 'Lower Basement' ~ -2,
    two_string == 'NA NA' ~ as.integer(str_extract(Floor, '[0-9]+')),
    TRUE ~ floor),
  building_floor = case_when(
    two_string == 'Ground out' ~ as.integer(str_extract(Floor, '[0-9]+')),
    two_string == 'Ground NA' ~ NA_integer_,
    two_string == 'Upper Basement' ~ as.integer(str_extract(Floor, '[0-9]+')),
    two_string == 'Lower Basement' ~ as.integer(str_extract(Floor, '[0-9]+')),
    two_string == 'NA NA' ~ NA_integer_,
    TRUE ~ building_floor)
)

# Dropping unnecessary columns.
data <- data %>% select(-Floor, -two_string, -Carpet.Area)
head(data)

# Getting rid of some redundant values inside 'overlooking' column.
data %>% pull(overlooking) %>% unique()
data <- data %>%
  mutate(overlooking = case_when(
    overlooking == '' ~ NA_character_,
    overlooking == 'Main Road, Garden/Park' ~ 'Garden/Park, Main Road',
    overlooking == 'Garden/Park, Pool, Main Road' ~ 'Pool, Garden/Park, Main Road',
    overlooking == 'Pool, Garden/Park' ~ 'Garden/Park, Pool',
    overlooking == 'Main Road, Garden/Park, Pool' ~ 'Pool, Garden/Park, Main Road',
    overlooking == 'Main Road, Pool' ~ 'Pool, Main Road',
    overlooking == 'Main Road, Pool, Garden/Park' ~ 'Pool, Garden/Park, Main Road',
    overlooking == 'Pool, Main Road, Garden/Park' ~ 'Pool, Garden/Park, Main Road',
    TRUE ~ overlooking
  ))
# Observing the difference.
data %>% pull(overlooking) %>% unique()

# Observing unique values of 'Furnishing' column.
data %>% select(Furnishing) %>% unique()

# Mapping them into integers cause there is a logical order, basically doing Ordinal Encoding.
data <- data %>%
  mutate(Furnishing = case_when(
    Furnishing == 'Unfurnished' ~ -1,
    Furnishing == 'Furnishing' ~ 0,
    Furnishing == 'Semi-Furnished' ~ 1,
    Furnishing == 'Furnished' ~ 2,
    TRUE ~ NA_integer_
  ))

# Seeing the unique texts inside 'Car.Parking' column.
data %>% pull(Car.Parking) %>% str_extract('[a-zA-Z]+') %>% unique()

# Extracting number and texts seperately and making meaningful columns for each.
data <- data %>%
  mutate(car_parking_type = str_extract(Car.Parking, '[a-zA-Z]+'),
         car_parking_spot = as.integer(str_extract(Car.Parking, '[0-9]+'))) %>%
  select(-Car.Parking)
head(data)
data <- data %>% select(-Society, -Index)
data

# Observing missing values in our dataset.
colSums(is.na(data))
for (i in 1:length(data)){
  cat(names(data)[i], mean(is.na(data[, i]))*100, '%\n')
}

# Getting rid of those missing values by eaither removing column, imputing them by either mode or mean.
f_mode <- names(table(data$Furnishing)[1])
data$Furnishing[is.na(data$Furnishing)] <- f_mode

o_mode <- names(table(data$overlooking)[1])
data$overlooking[is.na(data$overlooking)] <- o_mode

colSums(is.na(data))
data <- data %>% select(-Dimensions, -Plot.Area, -carpet_are_sqft)

data$amount_usd[is.na(data$amount_usd)] <- mean(data$amount_usd, na.rm = TRUE)
data$price_usd[is.na(data$price_usd)] <- mean(data$price_usd, na.rm = TRUE)

f_mode <- names(table(data$floor)[1])
data$floor[is.na(data$floor)] <- f_mode

b_mode <- names(table(data$building_floor)[1])
data$building_floor[is.na(data$building_floor)] <- b_mode

c_mode <- names(table(data$car_parking_type)[1])
data$car_parking_type[is.na(data$car_parking_type)] <- c_mode

data$car_parking_spot[is.na(data$car_parking_spot)] <- as.integer(mean(data$car_parking_spot, na.rm = TRUE))

# Converting data types of columns into proper ones.
str(data)
data$Furnishing <- as.integer(data$Furnishing)
data$Bathroom <- as.integer(data$Bathroom)
data$Balcony <- as.integer(data$Balcony)
data$floor <- as.integer(data$floor)
data$building_floor <- as.integer(data$building_floor)

# Making a copy so original data stays safe
clean_data <- data


for (i in 1:ncol(data)) {
  
  # Checking if the column is numeric
  if (is.numeric(data[[i]])) {
    
    Q1 <- quantile(data[[i]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[i]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Filtering out outliers for this numeric column
    clean_data <- clean_data[
      clean_data[[i]] >= lower_bound & clean_data[[i]] <= upper_bound, 
    ]
    
    cat("Filtered column:", names(data)[i], "\n")
    
  } else {
    cat("Skipped non-numeric column:", names(data)[i], "\n")
  }
}

# Let's see 5 most expensive locations according to the average house prices in those locations.
data %>%
  group_by(location) %>%
  summarise(avg_price = mean(amount_usd)) %>%
  slice_max(order_by = avg_price, n = 5) %>%
  ggplot(aes(x = fct_reorder(location, desc(avg_price)), y = avg_price)) +
  geom_col(fill = 'green') +
  theme_minimal() +
  labs(title = 'Average Price in different Regions.', x = 'Location', y = 'Avg Price')

str(data)
# Now let's see if there is any price difference between houses facing different directions.
data %>%
  filter(!facing %in% c('', ' ')) %>%
  group_by(facing) %>%
  summarise(avg_price = mean(amount_usd, na.rm = TRUE)) %>%
  ggplot(aes(x = facing, y = avg_price)) +
  geom_col() +
  theme_minimal() +
  labs(title = 'Facing vs Average Price.', subtitle = 'Houses facing North East is the most expensive while the total opposite is the cheapest.', x = 'Direction', y = 'Avg Price')

# Now let's see if ownership affects house price.
data %>%
  filter(Ownership != '') %>%
  group_by(Ownership) %>%
  summarise(avg_price = mean(amount_usd, na.rm = TRUE)) %>%
  ggplot(aes(x = Ownership, y = avg_price)) +
  geom_col(fill = 'green') +
  labs(title = 'Ownership types vs Average Price.', x = 'Ownership Type', y = 'Avg Price') +
  theme_minimal()

# Now let's see if the apartment floor affects its price.
data_sample <- data[sample(nrow(data), 5000), ]
data_sample %>%
  group_by(floor) %>%
  summarise(avg_price = mean(amount_usd, na.rm = TRUE)) %>%
  ggplot(aes(x = floor, y = avg_price)) +
  geom_col(fill = 'gray') +
  scale_x_continuous(breaks = unique(data$floor)) +
  labs(title = 'Floor vs Average Price', x = 'Floor', y = 'Avg Price') +
  theme_minimal()
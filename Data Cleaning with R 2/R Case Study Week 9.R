data <- MO14_Round_1_Dealing_With_Data_Workbook
View(data)
colnames(data) <- 'col'
colnames(data)

library(tidyverse)
data$col <- data$col %>% str_replace_all(pattern = '_', replacement = ' ')
data$col

data$col <- data$col %>% str_trim()
data$col

data$col <- data$col %>% str_squish()
data$col

data <- data %>%
  mutate('am_pm' = case_when(str_detect(string = col, pattern = 'AM') ~ 'AM',
                           str_detect(string = col, pattern = 'PM') ~ 'PM'),
         'kwh' = ifelse(str_detect(string = col, pattern = 'kwh'), 'kwh', ''))

data$col <- data$col %>% str_remove_all(pattern = 'AM|PM|kwh')
View(data)

data <- data %>% 
  mutate(weekday = case_when(str_detect(string = col, pattern = 'Monday|Mon') ~ 'Monday',
                             str_detect(col, 'Tuesday|Tue') ~ 'Tuesday',
                             str_detect(col, 'Wednesday|Wed') ~ 'Wednesday',
                             str_detect(col, 'Thursday|Thu') ~ 'Thursday',
                             str_detect(col, 'Friday|Fri') ~ 'Friday',
                             str_detect(col, 'Saturday|Sat') ~ 'Saturday',
                             str_detect(col, 'Sunday|Sun') ~ 'Sunday'
                             ))

data$col <- data$col %>% str_remove_all('Monday|Mon|Tuesday|Tue|Wednesday|Wed|Thursday|Thu|Friday|Fri|Saturday|Sat|Sunday|Sun')

View(data)  

data$col <- data$col %>% str_squish()  
  
View(data)

data <- data %>% separate(col = col, into = c('Hour', 'Date', 'KWH'), sep = ' ')

data$Date <- data$Date %>% str_remove_all('st|nd|rd|th')

View(data)
data$Date <- dmy(data$Date)

View(data)

data$KWH <- as.numeric(data$KWH)

data$Hour <- as.numeric(data$Hour)

data <- data %>%
  mutate(weekday = ifelse(is.na(weekday), weekdays(Date), weekday))
View(data)

data <- data %>% relocate(am_pm, .after = 1)
View(data)

data <- data %>% rename('usage' = kwh)
View(data)



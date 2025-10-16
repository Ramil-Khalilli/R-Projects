# Importing necessary libraries.
library(tidyverse)
library(stringr)
library(janitor)
library(naniar)
library(mice)
library(fastDummies)

# Fetching the data and viewing it.
data <- read.csv('C:/Users/RAMIL/Desktop/Data Science Project 1/german_credit.csv')
View(data)

# Observing the columns of the dataset.
str(data)

# Getting statistical information about the columns.
summary(data)

# Observing the null values.
vis_miss(data)
# We do not have null values inside our dataset.
# We can check it differently.
sum(is.na(data))

# Removing duplicates.
data <- unique(data)

# Let visualize our outliers in numerical columns with box plot.
num_cols <- data %>% select(where(is.numeric)) %>% colnames()
num_cols
for (i in num_cols){
  p <- data %>% ggplot(aes_string(x = i)) +
    geom_boxplot()
  print(p)
}

# Let's investigate our first column.
data %>%
  pull(checking_status) %>%
  unique()
table(data$checking_status)
# I will encode this column by creating dummy variables even though some of the values inside falls into ordinal logic one
# does not.

# Let's investigate our second column.
table(data$credit_history)
# I will encode this column by creating dummy variables as well.

# 3rd column.
table(data$purpose)
# Encoding by dummy variables as well for this column.

# 4th column.
table(data$savings_status)
# Even though some of the values inside this column have ordinal logic I will use dummy variables to encode this column
# since one of the most common values does not fit into ordinal encoding.

# 5th column.
table(data$employment)
data %>% pull(employment) %>% unique()
# Now we can encode this column in ordinal way.
data <- data %>%
  mutate(employment = case_when((employment == 'unemployed') ~ -1,
                                (employment == '<1') ~ 1,
                                (employment == '1<=X<4') ~ 2,
                                (employment == '4<=X<7') ~ 3,
                                TRUE ~ 4))

# 6th column.
table(data$personal_status)
# I will extract 2 different columns from this column.
data <- data %>%
  mutate(gender = ifelse(str_detect(string = personal_status, pattern = 'female|Feale|FEMALE'), 'female', 'male'),
         marital_status = case_when((str_detect(string = personal_status, pattern = 'div/dep/mar')) ~ 'div/dep/mar',
                                    (str_detect(string = personal_status, pattern = 'div/sep')) ~ 'div/sep',
                                    (str_detect(string = personal_status, pattern = 'mar/wid')) ~ 'mar/wid',
                                    (str_detect(string = personal_status, pattern = 'single')) ~ 'single'))
# Now we can remove the original column we do not need it anymore.
data <- data %>% select(-personal_status)
View(data)


# 7th column.
table(data$other_parties)
# Encoding by dummy variables as well.

# 8th column.
table(data$property_magnitude)
# Encoding by dummy variables as well.

# 9th column.
table(data$other_payment_plans)
# Encoding by dummy variables as well.

# 10th.
table(data$housing)
# Encoding by dummy variables as well.

# 11th column.
table(data$job)
# Let's extract separate columns from this column.
data <- data %>%
  mutate(skill_level = case_when(str_detect(string = job, pattern = 'unskilled') ~ -1,
                                 str_detect(string = job, pattern = 'high') ~ 2,
                                 TRUE ~ 1))
# We already have 'employed' column so I will remove 'job' column.
data <- data %>% select(-job)
class(data$skill_level)

# 12th column.
table(data$own_telephone)
# Reserved for dummy encoding.

# 13th column.
table(data$foreign_worker)
# Reserved for dummy encoding as well.

# The following columns are numeric.

# Now let's check pearson correlation between our numerical columns and target column.
data$class <- as.factor(data$class)
t_test_results <- data.frame(
  variable = character(),
  t_statistic = numeric(),
  p_value = numeric()
)

for (col in num_cols) {
  test <- t.test(data[[col]] ~ data$class)
  t_test_results <- rbind(t_test_results, data.frame(
    variable = col,
    t_statistic = test$statistic,
    p_value = test$p.value
  ))
}

t_test_results <- t_test_results %>%
  arrange(p_value)

print(t_test_results)
# Columns - 'duration', 'credit_amount' and 'age' has significant correlation to our target column.
# While age has negative correlation other 2 have positive correlation.

# Now let's similar with categorical columns using chi-square test.
cat_cols <- data %>%
  select(where(Negate(is.numeric))) %>%
  select(-class) %>%
  colnames()

chi_results <- data.frame(
  variable = character(),
  p_value = numeric()
)

for (col in cat_cols) {
  tbl <- table(data[[col]], data$class)
  if (nrow(tbl) > 1 && ncol(tbl) > 1) {
    test <- chisq.test(tbl)
    chi_results <- rbind(chi_results, data.frame(
      variable = col,
      p_value = test$p.value
    ))
  }
}

chi_results <- chi_results %>% arrange(p_value)
print(chi_results)
# Those with p value less than 0.02 has significant correlation with the target column.

# Let's convert our target column before applying dummy variables across categorical columns.
data %>% pull(class) %>% unique()
data %>% group_by(class) %>% summarise(count = n())
data <- data %>%
  mutate(class = ifelse(class == 'good', 1, 0))

# Let's encode our categorical columns.
data <- dummy_cols(data, remove_first_dummy = T, remove_selected_columns = T)

# Let's scale our numerical columns.
data <- data %>%
  mutate(across(where(is.numeric) & !where(~ all(unique(.) %in% c(0, 1))),
                scale))

head(data, n = 2)
data$class

write.csv(data, 'C:/Users/RAMIL/Desktop/Data Science Project 1/cleaned_data.csv', row.names = F)
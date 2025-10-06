# 1. The data is in .csv format. Enter the date in R.
data <- Superstore_data

# 2. The R cannot read spaces in column names. Therefore replace all column names
# with spaces with "_".
colnames(data) <- sub(pattern = ' ', replacement = '_', colnames(data))

# 3. Find relationship between sales and profit using scatter plot diagram.
# 4. Add geom_smooth.
library(tidyverse)
data %>%
  ggplot(aes(x = Sales, y = Profit)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Sales vs Profit', x = 'Sales', y = 'Profit') +
  theme_minimal()

# 5. Analyze the visualization by segmentation and discounts.
data %>%
  ggplot(aes(x = Sales, y = Profit, color = Segment, size = Discount)) +
  geom_point() +
  geom_smooth() +
  labs(title = 'Sales vs Profit', x = 'Sales', y = 'Profit') +
  theme_minimal()

# 6. Show the relationship between the sales and profit by regions. Then split a plot
# according to categories using facet_grid().
data %>%
  ggplot(aes(x = Sales, y = Profit, color = Region)) +
  geom_point() +
  facet_grid(.~ Category) +
  theme_minimal() +
  labs(title = 'Sales vs Profit by region and category')

# 7. Create histogram of number of sales with color filled by categories.
data %>%
  ggplot(aes(x = Sales, fill = Category)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = 'Distribution of Sales by Category')

# 8. Show the relationship between sales and profit by box-plot and identify outliers.
library(patchwork)
visual1 <- data %>%
  ggplot(aes(y = Sales)) +
  geom_boxplot(fill = 'lightblue') +
  theme_minimal()

visual2 <- data %>%
  ggplot(aes(y = Profit)) +
  geom_boxplot(fill = 'green') +
  theme_minimal()

visual1 + visual2

# 9. Create a histogram of quantity of products filled by type of shipment and split a plot
# by product category using facet_grid().
# 11. Add the title "Shipping by product category".
data %>%
  ggplot(aes(x = Quantity, fill = Ship_Mode)) +
  geom_histogram() +
  facet_grid(.~ Category) +
  labs(title = 'Shipping by Product Category')


# 10. Create violin plot of sales and profit.
graph1 <- data %>%
  ggplot(aes(x = Sales, y = Category)) +
  geom_violin(fill = 'lightblue')

graph2 <- data %>%
  ggplot(aes(x = Profit, y = Category)) +
  geom_violin(fill = 'red')

graph1 + graph2

# 11. Add the title "Shipping by product category".
# Have been added to the task 9.



library(readr)
library(dplyr)
library(ggplot2)

# Set the path to your CSV file
file_path <- "C:/Users/ns14555/Downloads/chronic_disease_data.CSV"

# Load the CSV file into a data frame
data <- read_csv(file_path)

# Display the first few rows of the data frame
head(data)

# Calculate missing values percentage
missing_percent <- colMeans(is.na(data)) * 100

# Create a data frame with the field names and their missing value percentages
missing_data <- data.frame(
  Field = names(missing_percent),
  MissingPercentage = missing_percent
)

# Order the data frame by missing percentage in descending order
missing_data <- missing_data %>%
  arrange(desc(MissingPercentage))

# Create the bar chart for missing values
missing_plot <- ggplot(missing_data, aes(x = reorder(Field, -MissingPercentage), y = MissingPercentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Percentage of Missing Values by Field",
       x = "Field",
       y = "Missing Percentage") +
  theme_minimal() +
  coord_flip()

# List variables with missing values 25% or more
variables_with_25_or_more_missing <- missing_data %>%
  filter(MissingPercentage >= 25)

# Display the list of variables with missing values 25% or more
print("Variables with missing values 25% or more:")
print(variables_with_25_or_more_missing)

# Remove variables with missing values 25% or more
data_cleaned <- data %>%
  select(-one_of(variables_with_25_or_more_missing$Field))

# Count the values in the 'Topic' column
topic_counts <- data_cleaned %>%
  count(Topic) %>%
  arrange(desc(n))

# Create the bar chart for most counted topics
topic_plot <- ggplot(topic_counts, aes(x = reorder(Topic, -n), y = n, fill = Topic)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Count of Topics",
       x = "Topic",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plots
print(missing_plot)
print(topic_plot)

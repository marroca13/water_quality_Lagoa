# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)


# Load the CSV file (update the path if needed)
file_path <- "data_percentage_for_each_variable.csv"
data <- read_csv(file_path)

# Rename the first column (assuming it contains dates)
colnames(data)[1] <- "Date"

# Convert Date to character (in case it's numeric)
data$Date <- as.character(data$Date)

# Extract Year and Month manually
data$Year <- substr(data$Date, 1, 4)   # First 4 characters = Year
data$Month <- substr(data$Date, 5, 6)  # Characters 5-6 = Month

# Convert Month number (01, 02, ...) to proper month names
data$Month <- factor(month.abb[as.numeric(data$Month)], 
                     levels = month.abb)

# Add a new column with value 1
data$Count <- 1

# Check if extraction worked
print(head(data[, c("Date", "Year", "Month", "Count")]))

# Select only the interesting columns
data <- filter(data[25:27])
print(head(data))

# Summing the counts per Year and Month
result <- data %>%
  group_by(Year, Month) %>%
  summarise(Total_Count = sum(Count), .groups = 'drop')

# View the result
print(result)

# Accumulate the counts over months
data <- data %>%
  arrange(Month) %>%
  mutate(Accumulated_Count = cumsum(Count))

labels_n = custom_labels <- c("2017 (n=20)", "2018 (n=45)", "2019 (n=54)", "2020 (n=51)", "2021 (n=46)", "2022 (n=51)", "2023 (n=48)", "2024 (n=44)")


# Create stacked bar plot
ggplot(data, aes(x = Month, y = Count, fill = Year)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#E0F7FA", "#A7E4EB", "#5FC9D8", "#1FB6C1", "#0097A7", "#007488", "#005A6E", "#003F4F"), labels= labels_n) +
  labs(y = "Number of images (n)", x = NULL, fill = "Year") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(size = 15, family = "Roboto"),
        axis.title.y = element_text(size = 13, family = "Roboto"),
        legend.text = element_text(size=12, family = "Roboto"))
print(ggplot)

count_per_year <- data %>%
  group_by(Year) %>%
  summarise(Total_Count = sum(Count), .groups = 'drop')

print(count_per_year)
##"#66c2a5", "#8da0cb", "#FFB3BA","#e5c494", "#fc8d62", "gray80","#ffd92f", "#a6d854"











# Reshape data from wide to long format
data_long <- data %>%
  select(-Date) %>%
  pivot_longer(cols = -c(Year, Month, Count), names_to = "Variable", values_to = "Value")

# Aggregate: Sum the number of images per Month
image_count_per_month <- data_long %>%
  group_by(Month) %>%
  summarise(Total_Images = sum(Value, na.rm = TRUE), Count = sum(Count))

# Print the result
print(image_count_per_month)




# Reshape data from wide to long format
data_long <- data %>%
  select(-Date) %>%
  pivot_longer(cols = -c(Year, Month), names_to = "Variable", values_to = "Value")

# Aggregate: Sum the number of images per Month
image_count_per_month <- data_long %>%
  group_by(Month) %>%
  summarise(Total_Images = sum(Value, na.rm = TRUE))

# Print the result
print(image_count_per_month)










-----------------------------


# Reshape data from wide to long format
data_long <- data %>%
  select(-Date) %>%
  pivot_longer(cols = -c(Year, Month), names_to = "Variable", values_to = "Value")

# Aggregate data by Month and Year
data_summary <- data_long %>%
  group_by(Year, Month) %>%
  summarise(Frequency = mean(Value, na.rm = TRUE), .groups = "drop")

# Convert Year to factor (for correct ordering in the legend)
data_summary$Year <- factor(data_summary$Year)

# Create stacked bar plot
ggplot(data_summary, aes(x = Month, y = Frequency, fill = Year)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#66c2a5", "#8da0cb", "#e78ac3","#e5c494", "#fc8d62", "gray80","#ffd92f", "#a6d854")) +  # Custom colors
  labs(y = "Relative frequency of scenes [%]", x = NULL, fill = "Year") +
  theme_minimal() +
  theme(legend.position = "top")

print(ggplot)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# Load the CSV file
file_path <- "data_percentage_for_each_variable.csv"
data <- read_csv(file_path)



# Reshape data from wide to long format
data_long <- data %>%
  select(-Date) %>%
  pivot_longer(cols = -c(Year, Month), names_to = "Variable", values_to = "Value")

# Aggregate data by Month and Year
data_summary <- data_long %>%
  group_by(Year, Month) %>%
  summarise(Frequency = mean(Value, na.rm = TRUE), .groups = "drop")

# Convert Year to factor (for correct ordering in the legend)
data_summary$Year <- factor(data_summary$Year)

# Create stacked bar plot
ggplot(data_summary, aes(x = Month, y = Frequency, fill = Year)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "gray50", "gray80")) +  # Custom colors
  labs(y = "Relative frequency of scenes [%]", x = NULL, fill = "Year") +
  theme_minimal() +
  theme(legend.position = "top")



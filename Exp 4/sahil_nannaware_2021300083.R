library(ggplot2)
library(dplyr)

crimes <- read.csv("..Datasets/crime_dataset_india.csv")

# Bar Chart
top_cities <- crimes %>%
  group_by(City) %>%
  summarise(Crime_Count = n()) %>%
  arrange(desc(Crime_Count)) %>%
  slice(1:10)  # Select top 10 cities

ggplot(top_cities, aes(x = reorder(City, -Crime_Count), y = Crime_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Cities with Highest Crime Count", x = "City", y = "Count of Crimes") +
  theme_minimal()

# Histogram
ggplot(crimes, aes(x = Victim.Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Victim Ages", x = "Victim Age", y = "Frequency") +
  theme_minimal()

# Pie Chart
crime_domain_counts <- table(crimes$Crime.Domain)
pie(crime_domain_counts, labels = names(crime_domain_counts), 
    main = "Crime Distribution by Domain", col = rainbow(length(crime_domain_counts)))


# Scatter Plot
filtered_data <- crimes %>%
  filter(Victim.Age >= 30 & Victim.Age <= 50,
         Crime.Description %in% c("HOMICIDE", "IDENTITY THEFT"),
         City %in% c("Mumbai"),
         Victim.Gender == "M")

ggplot(filtered_data, aes(x = Victim.Age, y = Crime.Code, color = Crime.Description)) +
  geom_point(alpha = 0.7, size = 4) +
  labs(title = "Filtered Scatter Plot: Homicide and Identity Theft (30-50, Male, Mumbai)", 
       x = "Victim Age", y = "Crime Code") +
  theme_minimal() +
  scale_color_manual(values = c("HOMICIDE" = "red", "IDENTITY THEFT" = "blue"))


# Time Series
crimes$Date.Reported <- as.Date(crimes$Date.Reported, format="%d-%m-%Y %H:%M")

crimes_2023 <- crimes %>%
  filter(format(Date.Reported, "%Y") == "2023")

crimes_2023_monthly <- crimes_2023 %>%
  mutate(Month = format(Date.Reported, "%Y-%m")) %>%
  group_by(Month) %>%
  summarise(Crime_Count = n())  # Count crimes per month

ggplot(crimes_2023_monthly, aes(x = Month, y = Crime_Count, group = 1)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "blue", size = 3) +  # Add points for emphasis
  labs(title = "Crimes Reported Over Time (2023)", x = "Month", y = "Count of Crimes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Bubble Chart
aggregated_data <- crimes %>%
  group_by(City, `Crime.Domain`) %>%
  summarise(crime_count = n()) %>%
  ungroup() %>%
  arrange(desc(crime_count)) %>%
  group_by(City) %>%
  mutate(city_total = sum(crime_count)) %>%
  ungroup() %>%
  filter(City %in% (slice_max(., city_total, n = 15))$City)

ggplot(aggregated_data, aes(x = City, y = `Crime.Domain`, size = crime_count, color = `Crime.Domain`)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1, 20)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Crime Distribution by City and Domain",
       x = "City", 
       y = "Crime Domain", 
       size = "Number of Crimes")

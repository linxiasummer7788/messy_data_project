#Load packages
library(dplyr)
library(lubridate)
library(ggplot2)
set.seed(1234)

## Plot1: Create a plot with counts of Issuing Agency
# Load required libraries
library(dplyr)  # For data manipulation
library(lubridate)  # For date/time manipulation
library(ggplot2)  # For data visualization

# Group the data by Issuing Agency and count the number of violations for each agency
agency_counts <- data %>% 
  group_by(Issuing.Agency) %>% 
  summarize(count = n())

# Create a pie chart of Issuing Agency
my_colors <- c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7", "#F6B4D6", "#B7B7B7", "#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7", "#999999", "#0072B2")
ggplot(agency_counts, aes(x = "", y = count, fill = Issuing.Agency)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = my_colors) +
  theme_void()+
  labs(fill = "Issuing Agency", y = "Count") +
  geom_text(aes(label = ifelse(rank(-count) < 5, count, "")), position = position_stack(vjust = 0.5), size = 3)+
  ggtitle("Distribution of Issuing Agency for Violation")  # Add a title to the plot

# Save the plot as a PNG file
ggsave("figures/issuing_agency_plot.png", width = 8, height = 6, dpi = 300)

## Plot2: Create a plot of distribution of Issuer Commanders
# Group data by Issuer Command and count number of violations
issuer_counts <- data %>% filter(Issuer.Command != "") %>% group_by(Issuer.Command) %>% summarise(count = n())

# Sort data in descending order by count
issuer_counts <- issuer_counts[order(-issuer_counts$count), ]

# Select top 10 Issuer Commands
issuer_counts_top10 <- head(issuer_counts, 10)

# Create a vector of colors
my_colors <- rainbow(length(unique(issuer_counts_top10$Issuer.Command)))

# Create a pie chart for Issuer Command
ggplot(issuer_counts_top10, aes(x = "", y = count, fill = Issuer.Command)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  
  # Set the color scale for the chart
  scale_fill_manual(values = my_colors) +
  
  # Remove axis labels and backgrounds
  theme_void() +
  
  # Set axis labels
  labs(fill = "Issuer Command", y = "Count") +
  
  # Add text labels to the chart
  geom_text(aes(label = ifelse(rank(-count) < 10, count, "")), position = position_stack(vjust = 0.5), size = 2.5) +
  
  # Add chart title
  ggtitle("Distribution of Top 10 Issuer Command for Violation") 

# Save the chart as a png file
ggsave("figures/issuer_command_plot.png", width = 8, height = 6, dpi = 300)


## Plot3: Create a NYC map plot of violation density 
# Install necessary packages
install.packages("maps")
install.packages("ggmap")

# Load necessary libraries
library(ggplot2)
library(ggmap)
library(maps)

# Register Google Maps API key
register_google("YOUR_API_KEY")

# Get a map of New York City
NYC <- get_map("New York City", zoom=12)

# Plot the New York City map using ggmap
map <- ggmap(NYC) 

# Add violation data points to the map as colored circles, with size proportional to count
map <- map + geom_point(data = new_location_data, aes(x = latitude, y = longitude, color = count), size = 4)

# Set the color scale for the violation points
map <- map + scale_colour_gradient(high = "red", low = "beige")

# Remove axis ticks and labels
map <- map + theme(axis.ticks = element_blank(), axis.text = element_blank())

# Add a title to the plot
map <- map + ggtitle("NYC Violation Density Map")

# Remove x and y axis labels
map <- map + xlab("") + ylab("")

# Save the plot as a PNG file
ggsave("figures/map_plot.png", map, width = 8, height = 6, dpi = 300)

## Plot4: Create Borough-wise Plot of Violations by Plate Type and Month 
#Check unique boroughs
unique(data$Violation.Borough)
table(data$Violation.Borough)

# Filter data for relevant boroughs
data_filtered <- data %>% filter(Violation.Borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"))

# Group the data and calculate the count of violations
data_grouped <- data_filtered %>%
  group_by(Violation.Borough, Month, Plate.Type) %>%
  summarize(count = n())

#Create the bar plot
ggplot(data_grouped, aes(x = Month, y = count, fill = Plate.Type)) +
  geom_bar(stat = "identity") +
  facet_grid(Violation.Borough ~ Plate.Type, scales = "free_y") +
  facet_wrap(~Violation.Borough, nrow = 2) +
  labs(x = "Month", y = "Count of Violations") +
  ggtitle("Borough-wise Analysis of Violations by Plate Type and Month") +
  scale_x_discrete(labels = seq_along(unique(data_grouped$Month))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("./figures/violations_by_borough_and_plate_type.png", width = 10, height = 6)

## Plot5: Create Borough-wise Plot of Violations by Vehicle Type and Month 
# Filter data for relevant boroughs
data_filtered <- data %>% filter(Violation.Borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"))

# Group the data and calculate the count of violations
data_grouped <- data_filtered %>%
  group_by(Violation.Borough, Month, Vehicle.Body.Type) %>%
  summarize(count = n())

# Create the bar plot 
ggplot(data_grouped, aes(x = Month, y = count, fill = Vehicle.Body.Type)) +
  geom_bar(stat = "identity") +
  facet_grid(Violation.Borough ~ Vehicle.Body.Type, scales = "free_y") +
  facet_wrap(~Violation.Borough, nrow = 2) +
  labs(x = "Month", y = "Count of Violations") +
  ggtitle("Borough-wise Analysis of Violations by Vehicle Type and Month") +
  scale_x_discrete(labels = seq_along(unique(data_grouped$Month))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("./figures/violations_by_borough_and_vehicle_type.png", width = 10, height = 6)

## Plot6: Create a Line Plot for the Number of Violations
## Plot6: Create a Line Plot for the Number of Violations
# Filter data for relevant boroughs
data_filtered <- data %>% filter(Violation.Borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"))

# Create a new column Hour based on Violation_Time_int
data_filtered <- data_filtered %>% 
  mutate(Hour = ifelse(Violation_Time_int == 0, 0,
                       ifelse(Violation_Time_int > 2400, "Unknown",
                              as.integer(substr(Violation_Time_int, 1, nchar(Violation_Time_int) - 2)))))

# Group the data and calculate the count of violations
data_grouped <- data %>%
  group_by(Month, Hour) %>%
  summarize(count = n())

data_grouped$Hour <- reorder(data_grouped$Hour, data_grouped$count, FUN = median)

ggplot(data_grouped, aes(x = as.numeric(Hour), y = count, group = Month, color = Month)) +
  geom_line(size = 1) +
  labs(x = "Hour", y = "Count of Violations", color = "Month") +
  ggtitle("Count of Violations by Hour and Month") +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_color_hue(name="Month", 
                  breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
                  labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save the plot
ggsave("./figures/violations_by_hour.png", width = 10, height = 6)
# Plot7: Create Borough-wise Plot of Violations by Day_Night and Intersected
# Group the data and calculate the count of violations
data_grouped <- data_filtered %>%
  group_by(Violation.Borough, Day_Night, Intersected) %>%
  summarize(count = n())

# Create a horizontal stacked bar plot for each Borough
ggplot(data_grouped, aes(x = count, y = Day_Night, fill = Intersected)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Violation.Borough, ncol = 3) +
  labs(x = "Count of Violations", y = "Day/Night") +
  ggtitle("Borough-wise Analysis of Violations by Day_Night and Intersected")

# Save the plot
ggsave("./figures/violations_by_Day_Night_and_Intersected.png", width = 10, height = 6)

## Plot8: Create the Violation Code Pie Chart
# Convert Violation.Code to factor
data$Violation.Code <- as.factor(data$Violation.Code)

# Group the data by Violation.Code and count the number of violations
data_grouped <- data %>%
  group_by(Violation.Code) %>%
  summarize(count = n())

# Sort the data by count in descending order
data_sorted <- data_grouped[order(-data_grouped$count),]

# Get the top 10 Violation.Codes for the pie chart
data_top10 <- data_sorted[1:10,]

# Get the counts for the remaining categories
other_count <- sum(data_sorted$count[11:length(data_sorted$count)])

# Combine the remaining categories into "Other"
data_top10 <- rbind(data_top10, data.frame(Violation.Code = "Other", count = other_count))

# Create the pie chart
my_colors <- c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7", "#B7B7B7", "#E69F00", "#56B4E9", "#009E73")
ggplot(data_top10, aes(x = "", y = count, fill = Violation.Code)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = my_colors) +
  labs(title = "Violation Codes", fill = "Violation Code", x = NULL, y = NULL) +
  theme_void() +
  geom_text(aes(label = ifelse(rank(-count) < 7, count, "")), position = position_stack(vjust = 0.5), size = 2) +
  ggtitle("Distribution of Violation Codes") +
  theme(legend.position = "right")

# Save the plot
ggsave("./figures/distribution_of_violation_codes.png")



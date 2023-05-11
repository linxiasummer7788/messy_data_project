# Load package
library(dplyr)
library(lubridate)
library(ROCR)
library(ranger)
library(ggplot2)

#set seeds
set.seed(1024)

# Read in files
all_data <- read.csv("./data/Parking_Violations_Issued_-_Fiscal_Year_2017.csv")

# Make a copy for all_data
data <- all_data

## Step0: Drop columns that has many NAs and irrelevant to our analysis
# Check which columns only have NA values
cols_with_na <- colSums(is.na(data)) == nrow(data)
cols_with_na

# Subset data frame to only include columns without NA values
data <- data[, !cols_with_na]

# Drop columns
data <- data %>% 
  select(-c(`Vehicle.Expiration.Date`, `Violation.Legal.Code`, `House.Number`, 
            `Issuer.Code`, `Issuer.Squad`, `Meter.Number`, `Unregistered.Vehicle.`, 
            `Feet.From.Curb`, `From.Hours.In.Effect`, `To.Hours.In.Effect`, 
            `Street.Code1`, `Street.Code2`, `Street.Code3`, `Violation.Post.Code`, 
            `Violation.Precinct`, `Date.First.Observed`, `Time.First.Observed`, 
            `Sub.Division`, `Days.Parking.In.Effect`, `Violation.Location`,
            `Issuer.Precinct`))

## Step1 - Violation Time: Day/Night
# Split the column into 4-digit integer and character
data$Violation_Time_int <- as.integer(substr(data$Violation.Time, 1, 4))
data$Violation_Time_char <- substr(data$Violation.Time, 5, nchar(data$Violation.Time))

# Adjust abnormal time value
data <- data %>%
  mutate(Violation_Time_int = ifelse(Violation_Time_char == 'P' & Violation_Time_int < 1200,
                                     Violation_Time_int + 1200,
                                     Violation_Time_int))

# Categorize time of the day as Day, Night or Unknown
data <- data %>%
  mutate(Day_Night = case_when(
    Violation_Time_int > 2400 ~ "Unkown",
    Violation_Time_int >= 600 & Violation_Time_int <= 1859 ~ "Day",
    (Violation_Time_int >= 0 & Violation_Time_int < 600) | (Violation_Time_int >= 1900 & Violation_Time_int <= 2400) ~ "Night"
  ))

## Step2 - Generate Borough from Violation.County
data <- data %>% mutate(Violation.Borough = case_when(
  Violation.County %in% c("BX", "BRONX") ~ "Bronx",
  Violation.County %in% c("BK", "KINGS", "K") ~ "Brooklyn",
  Violation.County %in% c("NY", "MN") ~ "Manhattan",
  Violation.County %in% c("Q", "QN", "QNS") ~ "Queens",
  Violation.County %in% c("R", "ST") ~ "Staten Island ",
  TRUE ~ "OTHER")) %>% 
  select(-Violation.County)

## Step3 - Recode Plate Type based on rules
data <- data %>%
  mutate(Plate.Type = recode(Plate.Type, 
                             "PAS" = "passenger", 
                             "COM" = "Commercial Vehicle",
                             "OMT" = "Taxi",
                             "OMS" = "Rental",
                             "SRF" = "Special Passenger - Vanity",
                             "IRP" = "Intl. Registration Plan",
                             "TRC" = "Tractor Regular",
                             "MOT" = "Motorcycle",
                             "OMR" = "Bus",
                             "ORG" = "Organization (Pas)",
                             .default = "Other"
  ))

## Step4 - Vehicle Year: deal with abnormal data 
# Set standard for car age of 12 years 
# Exclude all cars that out of the standard to Others
data <- data %>%
  mutate(Vehicle.Year = case_when(
    Vehicle.Year <= 2017 & Vehicle.Year >= 2005 ~ as.character(Vehicle.Year),
    TRUE ~ "unknown"
  ))

## Step5 - Recode Vehicle Body Type
data <- data %>% mutate(Vehicle.Body.Type = recode(Vehicle.Body.Type, 
                                                   "SUBN" = "Suburban",
                                                   "4DSD" = "Sedan",
                                                   "VAN" = "Van",
                                                   "DELV" = "Truck",
                                                   "SDN" = "Sedan",
                                                   "2DSD" = "Sedan",
                                                   "PICK" = "Truck",
                                                   "REFG" = "Truck",
                                                   "TRAC" = "Truck",
                                                   "TAXI" = "Taxi",
                                                   .default = 'Other'))

## Step6 - Categorize messy Vehicle Color 
data <- data %>% mutate(Vehicle.Color = case_when(
    Vehicle.Color %in% c("BEIGE", "BE", "BIEGE","BIGE") ~ "BEIGE",
    Vehicle.Color %in% c("", "OTHER", "UNKNO", "UN", "UNK", "OTH") ~ "OTHER",
    grepl("^[W].*[H|I|T|E]", Vehicle.Color, ignore.case = TRUE) ~ "WHITE",
    grepl("^[G].*[A|Y]", Vehicle.Color, ignore.case = TRUE) ~ "GRAY",
    grepl("^[B].*[C|K]", Vehicle.Color, ignore.case = TRUE) ~ "BLACK",
    grepl("^[R].*[E|D]", Vehicle.Color, ignore.case = TRUE) ~ "RED",
    grepl("^[S].*[I|L|V|E|R]", Vehicle.Color, ignore.case = TRUE) ~ "SILVER",
    grepl("^[G].*[R|E|N]", Vehicle.Color, ignore.case = TRUE) ~ "GREEN",
    grepl("^[B].*[L|U|E]", Vehicle.Color, ignore.case = TRUE) ~ "BLUE",
    grepl("^[B].*[R|O|W|N]", Vehicle.Color, ignore.case = TRUE) ~ "BROWN",
    grepl("^[P].*[I|N|K]", Vehicle.Color, ignore.case = TRUE) ~ "PINK",
    grepl("^[T].*[A|N]", Vehicle.Color, ignore.case = TRUE) ~ "PINK",
    grepl("^[G].*[O|L|D]", Vehicle.Color, ignore.case = TRUE) ~ "PINK",
    grepl("^[P].*[U|R|L|E]", Vehicle.Color, ignore.case = TRUE) ~ "PURPLE",
    grepl("^[O].*[R|A|N|G|E]", Vehicle.Color, ignore.case = TRUE) ~ "ORANGE",
    grepl("^[Y].*[E|L|O|W]", Vehicle.Color, ignore.case = TRUE) ~ "YELLOW",
    TRUE ~ "OTHER"))

# Test for categories of Vehicle Color
unique(data$Vehicle.Color)

## Step7 - Recode Violation.In.Front.Of.Or.Opposite
data <- data %>%
  mutate(Violation.In.Front.Of.Or.Opposite = recode(Violation.In.Front.Of.Or.Opposite,
                                                    "F" = "Fire Hydrant",
                                                    "O" = "Private Driveway",
                                                    "I" = "Sidewalk Access Ramp",
                                                    "X" = "Pedestrian Ramp",
                                                    "R" = "Curb Cut",
                                                    "0" = "Driveway"))

## Step8 - Add new column for boolean variable Intersected
data <- data %>% mutate(Intersected = ifelse((Street.Name != "" & Intersecting.Street !=""), 1, 0))

## Step9 - Add new column for boolean variable for whether Plate_ID first time violate
data <- data %>%
  group_by(Plate.ID) %>%
  mutate(count = n()) %>%
  mutate(First.Violate.Plate = ifelse(count > 1, 0, 1)) %>%
  select(-count)

## Step10 - Create and Extract Month from Issue.Date
data$Month <- format(as.Date(data$Issue.Date, "%m/%d/%Y"), "%B")

## Step11 - Create column for State NY vs Non-NY
data$NY_Registration <- ifelse(data$Registration.State == "NY", 1, 0)

## Step12 - Create column for Plate.Type PAS or no PAS
data$PAS <- if_else(data$Plate.Type == "PAS", 1, 0)

## Step13 - Convert to factors
data$Month <- factor(data$Month, levels = c("January", "February", "March", 
                                            "April", "May", "June", "July", 
                                            "August", "September", "October", 
                                            "November", "December"))

data$Registration.State <- as.factor(data$Registration.State)

data$Plate.Type <- as.factor(data$Plate.Type)

data$Violation.Code <- as.factor(data$Violation.Code)

data$Vehicle.Body.Type <- as.factor(data$Vehicle.Body.Type)

data$Vehicle.Make <- as.factor(data$Vehicle.Make)

data$Issuing.Agency <- as.factor(data$Issuing.Agency)

data$Violation.In.Front.Of.Or.Opposite <- as.factor(data$Violation.In.Front.Of.Or.Opposite)

data$Vehicle.Color <- as.factor(data$Vehicle.Color)

data$Vehicle.Year <- as.factor(data$Vehicle.Year)

data$Day_Night <- as.factor(data$Day_Night)

## Step14 - Replace "Staten Island " with "Staten Island" and convert to factor
data$Violation.Borough <- factor(trimws(data$Violation.Borough))

## Step15 - Categorize abnormal Hour 
data <- data %>%
  mutate(Hour = ifelse(Violation_Time_int == 0, 0,
                       ifelse(Violation_Time_int > 2400, "Unknown",
                              as.integer(substring(Violation_Time_int, 1, nchar(Violation_Time_int) - 2)))))

data$Hour[is.na(data$Hour)] <- "Unknown"

write.csv(data, file = "./data/data1.csv")

## Step 16 - Generate longtitude and latitude for later map visualization and save into new_location_data
##### It would run for several days so please don't test it 
geocodeAddress <- function(address) {
  base <- "https://maps.googleapis.com/maps/api/geocode/json?address=" 
  key  <- "AIzaSyCQsGAqediHJABcQp7Iy7JR3JPKxfwZi8I"
  url  <- URLencode(paste0(base, address, ", New York City", "&key=", key))
  
  RJSONIO::fromJSON(url, simplify=FALSE)
}

# Convert data to different type
location_data <- data.frame(Street.Name = character(),
                            latitude = numeric(),
                            longitude = numeric(),
                            count = numeric())
# Save the location data
write.csv(location_data, file = "location_data_original.csv")

for(i in 1:nrow(data)) {
  address <- data[i, "Street.Name"]
  street_index <- which(location_data$Street.Name == address)
  
  if (length(street_index) > 0) {
    # Increase the count for this street name
    location_data$count[street_index] <- location_data$count[street_index] + 1
  } else {
    # Geocode the address and add it to location_data
    result <- geocodeAddress(address)
    if (result$status == "OK") {
      result_lat <- result$results[[1]]$geometry$location$lat
      result_lng <- result$results[[1]]$geometry$location$lng
      # Add the results to the location_data data frame
      location_data[nrow(location_data) + 1,] <- c(address, result_lat, result_lng, 1)
    }
  }
}

#convert columns to numeric and save the data
new_location_data <- data.frame(latitude = numeric(),
                                longitude = numeric(),
                                count = numeric())

write.csv(new_location_data, file = "location_data.csv")

tolerance <- 0.0001

for(i in 1:nrow(location_data)) {
  # get longitude and latitude from location_data
  latitude <- location_data[i, "latitude"]
  longitude <- location_data[i, "longitude"]
  # check if there are any rows in new_location_data that's within 0.0001 of difference
  tmp <- location_data[which(abs(new_location_data$longitude - longitude) < tolerance & abs(new_location_data$latitude - latitude) < tolerance),]
  if (nrow(tmp) == 0) {
    # if none rows, calculated the sum of count for all rows in location_data that's within 0.0001 of difference,
    tmp2 <- location_data[which(abs(location_data$longitude - longitude) < tolerance & abs(location_data$latitude - latitude) < tolerance),]
    sum = sum(tmp2$count)
    # insert the sum of count along with longitude and latitude to new_location_data
    new_location_data[nrow(new_location_data) + 1,] <- c(latitude, longitude, sum)
  }
  print(i)
}




# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(tidyverse)

# Function to load and clean individual datasets
load_and_clean_data <- function(file_path, date) {
  read_csv(
    file_path,
    skip = 4,
    col_names = c("Group", "humidity", "temperature")
  )[ , 1:3] %>%
    mutate(Group = as.character(Group)) %>%  # Convert Group column to character
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%  # Replace NA with mean only for numeric columns
    mutate(Date = date)  # Add Date column
}

# Load all datasets with respective dates
ntankro_files <- list.files(
  "/Users/zhengxinyi/Desktop/thesis/ghana_aqi_xinyi/data/ntankro_mod_pm",
  pattern = "DATA_202408(1[3-9])@FW22.csv", 
  full.names = TRUE
)

dates <- seq.Date(as.Date("2024-08-13"), as.Date("2024-08-19"), by = "day")

# Apply the function to each file, adding the respective date
all_data <- mapply(load_and_clean_data, ntankro_files, dates, SIMPLIFY = FALSE)

# Combine all datasets
ntankro_combined <- bind_rows(all_data)

# Averaging across every 12 records for each group and each day
averaged_ntan_combined <- ntankro_combined %>%
  mutate(Group = ceiling((row_number() + 1) / 12)) %>%
  group_by(Date, Group) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Plot humidity and temperature for all days
ggplot(data = averaged_ntan_combined) +
  geom_point(aes(x = Group, y = humidity, color = Date), size = 3) +
  geom_point(aes(x = Group, y = temperature, color = Date), shape = 17, size = 3) +
  labs(title = "Scatterplot of Humidity and Temperature over Time (0813-0819)",
       x = "Time",
       y = "Value",
       color = "Date") +
  theme_minimal()



---
# Load and clean datasets

```{r}
ntankro_240813 <- read_csv(
  "./data/ntankro_mod_pm/DATA_20240813@FW22.csv", 
  skip = 4, 
  col_names = c("Group", "humidity", "temperature")
)[, 1:3]


ntankro_240813_clean <- ntankro_240813 %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

averaged_ntan_0813 <- ntankro_240813_clean %>%
  mutate(Group = ceiling((row_number() + 1) / 12)) %>%
  group_by(Group) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

```

# plot of humidity and temperature for one day
```{r}
ggplot(data = averaged_ntan_0813) +
  geom_point(aes(x = Group, y = humidity), color = "blue", size = 3) + 
  geom_point(aes(x = Group, y = temperature), color = "red", size = 3) +
  labs(title = "Scatterplot of Humidity and Temperature over Time",
       x = "Time",
       y = "Value") +
  theme_minimal()
```

Interpretation:
  x-axis:
  The time progresses from 0 minutes (start of the day) to 1440 minutes (end of the day), covering a full 24-hour period.

y-axis:
  Humidity (blue line) starts high in the morning, drops around noon, and then rises again in the evening.
Temperature (red line) stays steady in the morning, increases around noon, and then drops in the evening.

# Load remaining datasets
```{r}
ntankro_240814 <- read_csv(
  "./data/ntankro_mod_pm/DATA_20240814@FW22.csv", 
  skip = 4, 
  col_names = c("Group", "humidity", "temperature")
)[, 1:3]

ntankro_240815 <- read_csv(
  "./data/ntankro_mod_pm/DATA_20240815@FW22.csv", 
  skip = 4, 
  col_names = c("Group", "humidity", "temperature")
)[, 1:3]


ntankro_240816 <- read_csv(
  "./data/ntankro_mod_pm/DATA_20240816@FW22.csv", 
  skip = 4, 
  col_names = c("Group", "humidity", "temperature")
)[, 1:3]


ntankro_240817 <- read_csv(
  "./data/ntankro_mod_pm/DATA_20240817@FW22.csv", 
  skip = 4, 
  col_names = c("Group", "humidity", "temperature")
)[, 1:3]


ntankro_240818 <- read_csv(
  "./data/ntankro_mod_pm/DATA_20240818@FW22.csv", 
  skip = 4, 
  col_names = c("Group", "humidity", "temperature")
)[, 1:3]


ntankro_240819 <- read_csv(
  "./data/ntankro_mod_pm/DATA_20240819@FW22.csv", 
  skip = 4, 
  col_names = c("Group", "humidity", "temperature")
)[, 1:3]

```

# plot of humidity and temperature for one week(2024-8-13 to 2024-8-19)
```{r}
# Function to load and clean individual datasets
load_and_clean_data <- function(file_path, date) {
  read_csv(
    file_path,
    skip = 4,
    col_names = c("Group", "humidity", "temperature")
  )[ , 1:3] %>%
    mutate(Group = as.character(Group)) %>%  # Convert Group column to character
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%  # Replace NA with mean only for numeric columns
    mutate(Date = date)  # Add Date column
}

# Load all datasets with respective dates
ntankro_files <- list.files(
  "/Users/zhengxinyi/Desktop/thesis/ghana_aqi_xinyi/data/ntankro_mod_pm",
  pattern = "DATA_202408(1[3-9])@FW22.csv", 
  full.names = TRUE
)

dates <- seq.Date(as.Date("2024-08-13"), as.Date("2024-08-19"), by = "day")

# Apply the function to each file, adding the respective date
all_data <- mapply(load_and_clean_data, ntankro_files, dates, SIMPLIFY = FALSE)

# Combine all datasets
ntankro_combined <- bind_rows(all_data)

# Averaging across every 12 records for each group and each day
averaged_ntan_combined <- ntankro_combined %>%
  mutate(Group = ceiling((row_number() + 1) / 12)) %>%
  group_by(Date, Group) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Plot humidity and temperature for all days
ggplot(data = averaged_ntan_combined) +
  geom_point(aes(x = Group, y = humidity, color = Date), size = 3) +
  geom_point(aes(x = Group, y = temperature, color = Date), shape = 17, size = 3) +
  labs(title = "Scatterplot of Humidity and Temperature over Time (0813-0819)",
       x = "Time",
       y = "Value",
       color = "Date") +
  theme_minimal()
```



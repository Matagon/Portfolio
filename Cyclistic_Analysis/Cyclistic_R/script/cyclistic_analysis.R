# Install the following packages:
# install.packages("tidyverse")
# install.packages("here")
# install.packages("scales")

library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(scales)

# Read and store the CSV files containing the data
files <- here::here("data") %>%
  dir(full.names = TRUE)

trip_data <- map(files, read_csv)

# Data cleaning ---------------------------------------------------------------

# Inspecting two elements of the list
trip_data[[1]] %>% glimpse()
trip_data[[2]] %>% glimpse()

# Verifying column compatibility (name and classes)
trip_data %>% compare_df_cols_same(bind_method = "bind_rows")

# Since the column of the tibbles are compatible, it is time to join them
all_trips <- bind_rows(trip_data)

# Remove the latitude and longitude columns, these are not required for
# the analysis.
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

# Inspect the complete data set
glimpse(all_trips)
summary(all_trips)

# Identify unique values per column
all_trips %>%
  summarise_all(n_distinct) %>%
  glimpse()

unique_values <- lapply(all_trips, unique)
glimpse(unique_values)

# Looking for null values in each column
all_trips %>%
  summarise(
    variable = colnames(all_trips),
    null_values = colSums(is.na(all_trips))
  )

# Add a Date, Year, Month, Day of the month, Day of the week, Starting hour
# and Ending hour variables.
weekd <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weeke <- c("Saturday", "Sunday")

all_trips <- all_trips %>% mutate(
  date = date(started_at),
  year = year(started_at),
  month = month(started_at, label = TRUE, abbr = FALSE),
  day = day(started_at),
  day_of_the_week = wday(started_at, label = TRUE, abbr = FALSE),
  start_hour = hour(all_trips$started_at),
  end_hour = hour(all_trips$ended_at),
  weekday = (day_of_the_week %in% weekd),
  weekend = (day_of_the_week %in% weeke)
)

# Calculate the time in seconds of each ride
all_trips <- all_trips %>%
  mutate(ride_length = difftime(ended_at, started_at, units = "sec")) %>%
  mutate(ride_length = as.numeric(ride_length)) # Convert to numeric

# Which trips have a duration under 60 seconds? (These are considered as false
# starts due to user error).
all_trips %>%
  summarise(name = "Total false starts", bad_data = (sum(ride_length < 60)))

# Delete the trips under 0 seconds (New data frame)
all_trips_v2 <- all_trips %>% filter(ride_length >= 60)

all_trips_v2 %>% # No Bad Data
  summarise(
    name = "Total false starts",
    bad_data = (sum(ride_length < 60))
  )

# Identify and remove the maintenance days data (start_station_name &
# end_station_name == NA).
all_trips_v2 %>%
  summarise(
    name = "Maintenance days",
    not_available_start = sum(is.na(start_station_name)),
    not_available_end = sum(is.na(end_station_name))
  )

all_trips_v2 <- all_trips_v2 %>% drop_na(start_station_name, end_station_name)

all_trips_v2 %>%
  summarise(
    name = "Maintenance days",
    not_available_start = sum(is.na(start_station_name)),
    not_available_end = sum(is.na(end_station_name))
  )

all_trips_v2 %>%
  summarise(
    variable = colnames(all_trips_v2),
    not_available = colSums(is.na(all_trips_v2))
  )

# Analyze the data ------------------------------------------------------------
# Trip count analysis ---------------------------------------------------------
# Total number of trips per membership
all_trips_v2 %>%
  ggplot(aes(member_casual, fill = member_casual)) +
  geom_bar() +
  labs(
    title = "Total trips",
    subtitle = "The total bicycle trips, grouped by membership type"
  ) +
  xlab("Membership Type") +
  ylab("Trips") +
  scale_fill_manual("Membership Type", values = c("#f44d4d", "#4aa5ea")) +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  theme_light()

# Total monthly trips per membership type
all_trips_v2 %>%
  ggplot(aes(month, fill = member_casual)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  labs(
    title = "Total monthly trips",
    subtitle = "The total bicycle trips, per month, grouped by membership type"
  ) +
  xlab("") +
  ylab("Trips") +
  scale_fill_manual("Membership Type", values = c("#f44d4d", "#4aa5ea")) +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  theme_light()

# Total trips per day of the week and membership type
all_trips_v2 %>%
  group_by(member_casual, day_of_the_week) %>%
  summarise(total_trips = n()) %>%
  arrange(member_casual, desc(total_trips)) %>%
  print() %>%
  ggplot(aes(day_of_the_week, total_trips, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total daily trips",
    subtitle = "The total daily bicycle trips, grouped by membership type"
  ) +
  xlab("") +
  ylab("Trips") +
  scale_fill_manual("Membership Type", values = c("#f44d4d", "#4aa5ea")) +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  theme_light()

# Total starting trips per hour by day of the week for casual members
weekend_labs <- c("Weekday", "Weekend")
names(weekend_labs) <- c(FALSE, TRUE)

all_trips_v2 %>%
  filter(member_casual == "casual") %>%
  ggplot(aes(start_hour, color = day_of_the_week)) +
  geom_freqpoly(binwidth = 1) +
  facet_wrap(~weekend, labeller = labeller(weekend = weekend_labs)) +
  scale_x_continuous(limits = c(0, 23.99999)) +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_color_discrete(name = "day of the week") +
  labs(
    title = "Casual Member: total daily trips by the hour",
    subtitle = paste(
      "The total hourly bicycle trips per day of the week for",
      " casual members, grouped by day of the week"
    )
  ) +
  xlab("Time of the day") +
  ylab("Trips") +
  theme_light()

# Total trips per hour by average weekday and weekend, for casual members
weekday_data <- all_trips_v2 %>%
  filter(weekend == FALSE, member_casual == "casual") %>%
  group_by(start_hour, day_of_the_week) %>%
  summarise(n = n()) %>%
  group_by(start_hour) %>%
  summarise(avg = mean(n))

weekend_data <- all_trips_v2 %>%
  filter(weekend == TRUE, member_casual == "casual") %>%
  group_by(start_hour, day_of_the_week) %>%
  summarise(n = n()) %>%
  group_by(start_hour) %>%
  summarise(avg = mean(n))

ggplot() +
  geom_freqpoly(
    data = weekday_data,
    aes(x = start_hour, y = avg, color = "Weekday"),
    stat = "identity"
  ) +
  geom_freqpoly(
    data = weekend_data,
    aes(x = start_hour, y = avg, color = "Weekend"),
    stat = "identity"
  ) +
  scale_x_continuous(limits = c(0, 23.99999)) +
  labs(
    title = paste(
      "Casual Members: weekday & weekend average", " hourly trips by the hour"
    ),
    subtitle = paste(
      "The average hourly bicycle trips for casual members, ",
      "grouped by weekday and weekend"
    )
  ) +
  xlab("Time of the day") +
  ylab("Trips") +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_color_manual(
    name = "Days of the week",
    values = c("#f44d4d", "#4aa5ea")
  ) +
  theme_light()

# Total trips per hour by day of the week for members
all_trips_v2 %>%
  filter(member_casual == "member") %>%
  group_by(day_of_the_week) %>%
  ggplot(aes(start_hour, color = day_of_the_week)) +
  geom_freqpoly(binwidth = 1) +
  scale_x_continuous(limits = c(0, 23.99999)) +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_color_discrete(name = "day of the week") +
  xlab("hour of the day") +
  ylab("trips") +
  labs(
    title = "Annual Member: total daily trips by the hour",
    subtitle = paste(
      "The total hourly bicycle trips per day of the week for annual members, ",
      "grouped by day of the week"
    )
  ) +
  theme_light()

# Total trips per hour by average weekday and weekend, for annual members
weekday_data_mem <- all_trips_v2 %>%
  filter(weekend == FALSE, member_casual == "member") %>%
  group_by(start_hour, day_of_the_week) %>%
  summarise(n = n()) %>%
  group_by(start_hour) %>%
  summarise(avg = mean(n))

weekend_data_mem <- all_trips_v2 %>%
  filter(weekend == TRUE, member_casual == "member") %>%
  group_by(start_hour, day_of_the_week) %>%
  summarise(n = n()) %>%
  group_by(start_hour) %>%
  summarise(avg = mean(n))

ggplot() +
  geom_freqpoly(
    data = weekday_data_mem,
    aes(x = start_hour, y = avg, color = "Weekday"),
    stat = "identity"
  ) +
  geom_freqpoly(
    data = weekend_data_mem,
    aes(x = start_hour, y = avg, color = "Weekend"),
    stat = "identity"
  ) +
  scale_x_continuous(limits = c(0, 23.99999)) +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_color_manual(
    name = "day of the week",
    values = c("#f44d4d", "#4aa5ea")
  ) +
  xlab("hour of the day") +
  ylab("trips") +
  labs(
    title = paste(
      "Annual Members: weekday & weekend average",
      " hourly trips by the hour"
    ),
    subtitle = paste(
      "The average hourly bicycle trips for annual members, ",
      "grouped by weekday and weekend"
    )
  ) +
  theme_light()

# Total trips per hour by average weekday, comparing membership type
ggplot() +
  geom_freqpoly(
    data = weekday_data,
    aes(x = start_hour, y = avg, color = "Casual"),
    stat = "identity"
  ) +
  geom_freqpoly(
    data = weekday_data_mem,
    aes(x = start_hour, y = avg, color = "Member"),
    stat = "identity"
  ) +
  scale_x_continuous(limits = c(0, 23.99999)) +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_color_manual(
    name = "Type of Member",
    values = c("#f44d4d", "#4aa5ea")
  ) +
  xlab("hour of the day") +
  ylab("trips") +
  labs(
    title = "Total average weekday trips",
    subtitle = paste(
      "The total average weekday trips, grouped",
      " by casual and annual members"
    )
  ) +
  theme_light()

# Total trips per hour by average weekend, comparing membership type
ggplot() +
  geom_freqpoly(
    data = weekend_data,
    aes(x = start_hour, y = avg, color = "Casual"),
    stat = "identity"
  ) +
  geom_freqpoly(
    data = weekend_data_mem,
    aes(x = start_hour, y = avg, color = "Member"),
    stat = "identity"
  ) +
  scale_x_continuous(limits = c(0, 23.99999)) +
  scale_y_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_color_manual(
    name = "Type of Member",
    values = c("#f44d4d", "#4aa5ea")
  ) +
  xlab("hour of the day") +
  ylab("trips") +
  labs(
    title = "Total average weekend trips",
    subtitle = paste(
      "The total average weekend trips, grouped",
      " by casual and annual members"
    )
  ) +
  theme_light()

# Ride length analysis---------------------------------------------------------

# Statistics about the ride length
(ride_length_stat <- summary(all_trips_v2$ride_length))

all_trips_v2 %>%
  ggplot(aes(ride_length)) +
  geom_boxplot() +
  scale_x_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_y_discrete(label = "") +
  xlab("ride length") +
  labs(
    title = "Ride length",
    subtitle = "The distribution of the ride length variable"
  ) +
  theme_light()

# Assigning the filter limit for the outliers
outlier_limit <- (ride_length_stat["3rd Qu."] + # Removing outliers
  1.5 * IQR(all_trips_v2$ride_length))

# Visualizing ride_length (no outliers)
all_trips_v2 %>%
  filter(ride_length < outlier_limit) %>%
  ggplot(aes(ride_length)) +
  geom_boxplot() +
  scale_x_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_y_discrete(label = "") +
  xlab("ride length") +
  labs(
    title = "Ride length",
    subtitle = paste(
      "The distribution of the ride length",
      " variable without the previous outliers"
    )
  ) +
  theme_light()

# Average trip duration per member type
all_trips_v2 %>%
  filter(ride_length < outlier_limit) %>%
  group_by(member_casual) %>%
  summarise(average_length = mean(ride_length))

all_trips_v2 %>%
  filter(ride_length < outlier_limit) %>%
  ggplot(aes(ride_length, fill = member_casual)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_y_discrete(label = "") +
  scale_fill_discrete(name = "Type of Membership") +
  xlab("ride length") +
  labs(
    title = "Ride length per membership",
    subtitle = paste(
      "The distribution of the ride length variable",
      " (without outliers), grouped by membership"
    )
  ) +
  theme_light()

all_trips_v2 %>%
  filter(ride_length < outlier_limit) %>%
  ggplot(aes(ride_length, fill = member_casual)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~member_casual, nrow = 2) +
  scale_x_continuous(
    labels = comma_format(big.mark = ",", decimal.mark = ".")
  ) +
  scale_fill_manual(
    name = "Membership Type",
    values = c("#f44d4d", "#4aa5ea")
  ) +
  xlab("ride length") +
  labs(
    title = "Density distribution of Ride length",
    subtitle = "The distribution of the ride length, grouped by membership"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.background = element_rect(fill = "white")
  )

# Average trip duration each month per membership
all_trips_v2 %>%
  filter(ride_length < outlier_limit) %>%
  group_by(month, member_casual) %>%
  summarise(ride_length = mean(ride_length)) %>%
  ggplot(aes(month, ride_length, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("ride length") +
  labs(
    title = "Average trip duration per month",
    subtitle = paste(
      "The average trip duration in seconds",
      " per month, grouped by membership type"
    )
  ) +
  scale_fill_manual(
    name = "Membership Type",
    values = c("#f44d4d", "#4aa5ea")
  ) +
  theme_light()

# Average trip duration per membership and day of the week
all_trips_v2 %>%
  filter(ride_length < outlier_limit) %>%
  group_by(member_casual, day_of_the_week) %>%
  summarise(average_duration = mean(ride_length)) %>%
  ggplot(aes(day_of_the_week, average_duration, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("day of the week") +
  ylab("trip duration") +
  labs(
    title = "Average trip duration per day of the week",
    subtitle = paste(
      "The average trip duration in seconds per day ",
      "of the week, grouped by membership type"
    )
  ) +
  scale_fill_manual(
    name = "Membership Type", values = c("#f44d4d", "#4aa5ea")
  ) +
  theme_light()

all_trips_v2 %>%
  filter(ride_length < outlier_limit) %>%
  group_by(member_casual, day_of_the_week) %>%
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, desc(average_duration))

# Popular stations analysis ---------------------------------------------------

# Popular stations among casual users
all_trips_v2 %>% # Casual Stations
  filter(member_casual == "casual") %>%
  count(start_station_name, sort = TRUE) %>%
  top_n(10) %>%
  print() %>%
  ggplot(aes(x = n, y = reorder(start_station_name, n))) +
  geom_bar(stat = "identity", fill = "#f44d4d") +
  xlab("trips") +
  ylab("") +
  labs(
    title = "Casual Members: Popular stations",
    subtitle = "The top ten most popular stations among casual members"
  ) +
  theme_minimal()

# Popular stations among annual membership holders
all_trips_v2 %>%
  filter(member_casual == "member") %>%
  count(start_station_name, sort = TRUE) %>%
  top_n(10) %>%
  print() %>%
  ggplot(aes(x = n, y = reorder(start_station_name, n))) +
  geom_bar(stat = "identity", fill = "#4aa5ea") +
  xlab("trips") +
  ylab("") +
  labs(
    title = "Annual Members: Popular stations",
    subtitle = "The top ten most popular stations among annual members"
  ) +
  theme_minimal()

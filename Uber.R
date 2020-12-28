library(tidyverse)
library(ggthemes)
library(lubridate)
library(DT)
library(scales)

#color setup
colors = c("#CC1011", "#665555", "#05a399","#cfcaca", "#f5e840", "#0683c9", "#e075b0")

#read data in
april_data <- read_csv("uber-raw-data-apr14.csv")           
may_data <- read_csv("uber-raw-data-may14.csv")
june_data <- read_csv("uber-raw-data-jun14.csv")
july_data <- read_csv("uber-raw-data-jul14.csv")
august_data <- read_csv("uber-raw-data-aug14.csv")
september_data <- read_csv("uber-raw-data-sep14.csv")
data_2014 <- bind_rows(april_data, may_data, june_data, july_data,
                       august_data, september_data)

#rename the first column's name 
data_2014 <- data_2014 %>%
  rename(date_time = `Date/Time`)

#extract myd_hms from data frame and mutate data frame
data_2014 <- data_2014 %>%
  mutate(
    date_new = mdy_hms(date_time),
    year = year(date_new),
    month = month(date_new, label = TRUE),
    day_of_month = mday(date_new),
    day_of_week = wday(date_new, label = TRUE),
    hour = hour(date_new),
    minute = minute(date_new),
    second = second(date_new)
  )

#data analysis

#Trips per hour
hour_data <- data_2014 %>%
  group_by(hour) %>%
  summarize(total = n())

#datatable(hour_data) 

ggplot(hour_data, aes(hour, total)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "red") + 
  ggtitle("Trips by Hour") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma)

#Trips by hour and month

month_hour_data <- data_2014 %>%
  group_by(month, hour) %>%
  summarize(total = n())

ggplot(month_hour_data, aes(hour, total,  fill = month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

#Trips by day of month

day_of_month_data <- data_2014 %>%
  group_by(day_of_month) %>%
  summarize(total = n())

#datatable(day_data)

ggplot(day_of_month_data, aes(day_of_month, total)) + 
  geom_bar(stat = "identity", fill = "red") + 
  ggtitle("Trips by Day of Month") + 
  scale_y_continuous(labels = comma)

#Trips by month and day of week

month_week_day_data <- data_2014 %>%
  group_by(month, day_of_week) %>%
  summarize(total = n())

ggplot(month_week_day_data, aes(month, total)) + 
  geom_bar(stat = "identity", 
           aes(fill = as.factor(day_of_week)),
           position = "dodge") + 
  ggtitle("Trips by Day of Week and Month") +
  labs(fill='Day of Week') + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors) 

#Trips during months in a year

month_data <- data_2014 %>%
  group_by(month) %>%
  summarize(total = n()) 

#datatable(month_data)

ggplot(month_data, aes(month, total)) +
  geom_bar(stat = "identity", 
           aes(fill = as.factor(month))) + 
  ggtitle("Trips by Month") +
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

#Trips by bases

ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkblue") + 
  scale_y_continuous(labels = comma) + 
  ggtitle("Trips by Base")

#Trips by bases and month
ggplot(data_2014, aes(Base)) + 
  geom_bar(position = "dodge",
           aes(fill = as.factor(month))) + 
  scale_y_continuous(labels = comma) + 
  ggtitle("Trips by Base and Month") + 
  labs(fill='Month') +
  scale_fill_manual(values = colors)

#Trips by bases and day of week
ggplot(data_2014, aes(Base)) + 
  geom_bar(position = "dodge",
           aes(fill = as.factor(day_of_week))) + 
  scale_y_continuous(labels = comma) + 
  ggtitle("Trips by Base and Day of Week") + 
  labs(fill='Day of Week') +
  scale_fill_manual(values = colors)

#Heatmap by day of month and hour
day_hour_data <- data_2014 %>%
  group_by(day_of_month, hour) %>%
  summarize(total = n())

ggplot(day_hour_data, aes(day_of_month, hour, fill = total)) + 
  geom_tile(color = "White") + 
  ggtitle("Heat Map by Hour and Day of Month")

#Heatmap by month and day of month
month_day_data <- data_2014 %>%
  group_by(month, day_of_month) %>%
  summarize(total = n()) 

ggplot(month_day_data, aes(day_of_month, month, fill = total))+
  geom_tile(color = "White") + 
  ggtitle("Heat Map by Day of Month and Month")

#Heatmap by month and day of week
ggplot(month_week_day_data, aes(day_of_week, month, fill = total)) + 
  geom_tile(color = "White") + 
  ggtitle("Heat Map by Day of Week and Month")

#Heatmap by month and base
month_base_data <- data_2014 %>%
  group_by(Base, month) %>%
  summarize(total = n())

ggplot(month_base_data, aes(Base, month, fill = total)) + 
  geom_tile(color = "White") + 
  ggtitle("Heat Map by Month and Base")

#Heatmap by day of week and base
day_of_week_base_data <- data_2014 %>%
  group_by(Base, day_of_week) %>%
  summarize(total = n())

ggplot(day_of_week_base_data, aes(Base, day_of_week, fill = total)) +
  geom_tile(color = "White") + 
  ggtitle("Heat Map by Day of Week and Base")

#Geoplot based on a sample from data_2014
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

sample <- data_2014 %>%
  sample_frac(.025)

ggplot(sample, aes(Lon, Lat)) + 
  geom_point(size = 1, color = "chocolate") + 
  scale_x_continuous(limits = c(min_long, max_long)) + 
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() + 
  ggtitle("NYC Map Based on Uber Rides Data (April-September 2014)")

ggplot(sample, aes(Lon, Lat, color = Base)) + 
  geom_point(size = 1) + 
  scale_x_continuous(limits = c(min_long, max_long)) + 
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() + 
  ggtitle("NYC Map Based on Uber Rides Data by Base (April-September 2014)")


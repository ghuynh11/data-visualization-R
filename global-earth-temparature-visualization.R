### Global Earth Temperature Analysis & Visualization ###

### Install necessary R packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization

install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)
library(dplyr)

#------
# I. COLLECT DATA
#------

# Upload Global Earth Temperature (csv file) and inspect the dataset
global_temps <- read_csv("Global_Earth_Temperature_Kaggle.csv")
View(global_temps)
colnames(global_temps)
nrow(global_temps)
dim(global_temps)
head(global_temps)
str(global_temps)
summary(global_temps)

#------
# II. WRANGLE & CLEAN UP DATA FOR ANALYSIS
#------

# Replace space in column names with underscores
global_temps <- clean_names(global_temps)
head(global_temps)

# Drop certain columns which are not necessary for the analysis and assign the updated dataset to a new data frame
global_temps_1 <- global_temps %>%
  select(year, month, monthly_anomaly, annual_anomaly)
head(global_temps_1)
summary(global_temps_1)

# Drop rows which contain NaN values
global_temps_1 <- global_temps_1 %>% 
  filter(annual_anomaly != "NaN")

#------
# III. CONDUCT DESCRIPTIVE DATA ANALYSIS
#------

# Inspect the updated data frame
head(global_temps_1)
summary(global_temps_1)
View(global_temps_1)
str(global_temps_1)
table(global_temps_1$year)

# Calculate yearly average anomaly temperatures & assign them to a new dataframe
global_temps_2 <- aggregate(global_temps_1$monthly_anomaly ~ global_temps_1$year, FUN = mean)
colnames(global_temps_2) <- c("year", "yearly_avg_anomaly")
head(global_temps_2)
summary(global_temps_2)

# Time Series Line Plot
ggplot(global_temps_2, aes(x = year, y = yearly_avg_anomaly)) +
  geom_line() +
  labs(x = "Year", y = "Yearly Temperature avg. Anomaly", title = "Yearly Temperature Anomalies 1850 - 2022")

# Scatter Plot
ggplot(global_temps_2, aes(x = year, y = yearly_avg_anomaly)) +
  geom_point() +
  labs(x = "Year", y = "Yearly Temperature avg. Anomaly", title = "Yearly Temperature Anomalies 1850 - 2022")

# Scatter Plot with Linear Regression Line
install.packages("viridis")
library(viridis)
ggplot(global_temps_2, aes(x = year, y = yearly_avg_anomaly, group = yearly_avg_anomaly, color = yearly_avg_anomaly)) +
  geom_point() + 
  scale_color_viridis(name = "Average Temperature Anomalies", option = "turbo") +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  theme(axis.line = element_line(color = "lightblue", linewidth = 1)) +
  scale_x_continuous(breaks = seq(1851, 2022, by = 20)) +
  scale_y_continuous() +
  theme(panel.background = element_blank()) +
  theme(legend.position = "bottom", axis.title = element_blank(),
        axis.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  labs(title = "Yearly Temperature Anomalies", subtitle = "1850 - 2022")

# Line Plot

# 1. Transform data

# Drop column annual_anomaly from the data frame global_temps_1
global_temps_1 <- global_temps_1 %>% 
  select(year, month, monthly_anomaly)
head(global_temps_1, n = 20)
str(global_temps_1)
count(global_temps_1, month)
View(global_temps_1)
# Transform numerical month names into abbreviated month names
global_temps_1$month <- month.abb[global_temps_1$month]

# 2. Create Line Plot of Annual Temperature Anomalies

# Create a data frame containing monthly anomalies from Dec of previous year
last_Dec <- global_temps_1 %>% 
  filter(month == "Dec") %>% 
  mutate(year = year + 1,
         month = "last_Dec")
head(last_Dec)
count(last_Dec, month)

# Create a data frame containing monthly anomalies from Jan of next year
next_Jan <- global_temps_1 %>% 
  filter(month == "Jan") %>% 
  mutate(year = year - 1,
         month = "next_Jan") # Assign number to month for line plot
head(next_Jan)
count(next_Jan, month)

# Concatenate 3 data frames
# "last_Dec" to "month" to "next_Jan" ranges from 1 to 14
# Mutate the "month" column to use coord_cartesian() later to set x axis limit ranging from 1 to 12
global_temps_3 <- bind_rows(last_Dec, global_temps_1, next_Jan) %>% 
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) - 1,
         this_year = year == 2022) 
head(global_temps_3)

# Create annotation to indicate the latest year of the data frame
annotation <- global_temps_3 %>% 
  slice_max(year) %>% 
  slice_max(month)

# Create line plot
global_temps_3 %>% 
  ggplot(aes(x = month_number, y = monthly_anomaly, group = year, 
             color = year, size = this_year)) +
  geom_hline(yintercept = 0, color = "white") + # Add a white line at 0 point on y axis
  geom_line(show.legend = FALSE) +
  geom_text(data = annotation,
            aes(x = month_number, y = monthly_anomaly, group = year, color = year, label = 2022),
            inherit.aes = FALSE,
            hjust = 0, size = 4, nudge_x = 0.15, fontface = "bold") + # Annotation for the latest year
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb,
                     sec.axis = dup_axis(name = NULL, labels = NULL)) + # Add ticks to all 4 sides of the panel
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) + # Add ticks to all 4 sides of the panel
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.25, 0.8),
                    guide = "none") + # Adjust the size of the lines in the panel
  scale_color_viridis_c(breaks = seq(1850, 2020, 15),
                        guide = guide_colorbar(frame.colour = "white",
                                               frame.linewidth = 0.5)) + # Adjust the appearance of the legend
  coord_cartesian(xlim = c(1,12)) + # Set the limit for x axis
  labs(x = NULL,
       y = "Temperature change since pre-industrial time [\u00B0C]",
       title = "Global temperature change since 1850 by month") +
  theme(
    plot.background = element_rect(fill = "#555555"),
    panel.background = element_rect(fill = "black", color = "white", linewidth = 1),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white", size = 8),
    axis.ticks = element_line(color = "white"),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "white", size = 11),
    plot.title = element_text(color = "white", hjust = 0.5, size = 15),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "white"),
    legend.key.height = unit(55, "pt")
  )

ggsave("Global_Temperature/GLobal_Temperature_Line_Plot.png", width = 8, height = 4.5)

# The Climate Spiral
install.packages("gganimate")
install.packages("gifski")
install.packages("transformr")
install.packages("av")
library(gganimate)
library(gifski)
library(transformr)
library(av)

# 1. Transform data
# Create a data frame containing monthly anomalies from Jan of next year
next_Jan_1 <- global_temps_1 %>% 
  filter(month == "Jan") %>% 
  mutate(year = year - 1,
         month = "next_Jan")

# Concatinate two data frames global_temps_1 and next_Jan_1, arrange by year and month and add step number for the animation
global_temps_4 <- bind_rows(global_temps_1, next_Jan_1) %>% 
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>% 
  arrange(year, month) %>% 
  filter(year != 1849) %>% 
  mutate(step_number = 1:nrow(.))

head(global_temps_4)
tail(global_temps_4)
View(global_temps_4)

# 2. Create spiral plot

# Create data frame for the labels of geom_hline
temp_lines <- tibble(
  x = 12,
  y = c(1.5, 2.0),
  labels = c("1.5 \u00B0C", "2.0 \u00B0C")
)

# Align month labels to be tangential to the spiral
# Create a data frame with the months and their positions
month_labels <- tibble(
  x = 1:12,
  labels = month.abb,
  y = 2.8
) # Increase y value to move the month labels outward. Remember to extend the limits on the y axis accordingly

# Create annotation to add year label to the center of the label
annotation <- global_temps_4 %>% 
  slice_max(year) %>% 
  slice_max(month)

# Create spiral plot with animation
a <- global_temps_4 %>% 
  ggplot(aes(x = month_number, y = monthly_anomaly, group = year, color = year)) +
  geom_col(data = month_labels, aes(x = x + 0.5, y = 2.5), fill = "black", # Add a unit of 0.5 to the pie chart to close the gap. Increase y value to make the pie chart bigger, meaning more black background
           width = 1,
           inherit.aes = FALSE) + # Create circular plotting panel for the outside (pie chart). A circle can be created by creating a pie chart, width 100%
  geom_col(data = month_labels, aes(x = x + 0.5, y = -2), fill = "black", # Add a unit of 0.5 to the pie chart to close the gap
           width = 1,
           inherit.aes = FALSE) + # Create a circular plotting panel for the inside (pie chart) from 0 to negative values
  geom_hline(yintercept = c(1.5, 2.0), color = "red") + # Create labelled isotherm lines
  geom_label(data = temp_lines, aes(x = x, y = y, label = labels),
             color = "red", fill = "black", label.size = 0,
             inherit.aes = FALSE) + # Create labels for isotherm lines using temp_lines data frame
  geom_text(data = month_labels, aes(x = x, y = y, label = labels),
            color = "white",
            inherit.aes = FALSE,
            angle = seq(360 - 360/12, 0, length.out = 12)) + # Align month labels to be tangential to the spiral
  geom_label(aes(x = 1, y = -1.3, label = year),
             color = "white", fill = "black",
             label.padding = unit(50, "pt"), label.size = 0,
             size = 6) + # Add label of the latest year to the center of the spiral
  geom_line() +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb, expand = c(0, 0),
                     sec.axis = dup_axis(name = NULL, labels = NULL)) + # Add ticks to all 4 sides of the panel
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-2, 2.8), expand = c(0, -0.7), # Expand/move the month labels outward to the extra space around the plot
                     sec.axis = dup_axis(name = NULL, labels = NULL)) + # Add ticks to all 4 sides of the panel
  scale_color_viridis_c(breaks = seq(1850, 2020, 15),
                        guide = "none") +
  coord_polar(start = 2*pi/12) +
  labs(x = NULL,
       y = NULL,
       title = "Global temperature change from 1850 to 2022") +
  theme(
    plot.background = element_rect(fill = "#555555", color = "#555555"),
    panel.background = element_rect(fill = "#555555"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "white", size = 11),
    plot.title = element_text(color = "white", hjust = 0.5, size = 15),
  ) +
  transition_manual(frames = year, cumulative = TRUE)

# Save the GIF, modify its appearance
animate(a, width = 4.155, height = 4.5, unit = "in", res = 300)
anim_save("Global_Temperature/GLobal_Climate_Spiral_with_Animation.gif")

#------
# Trying a different approach to generate a black background circle
# global_temps_4 %>%
#   ggplot(aes(x = month_number, y = monthly_anomaly, group = year, color = year)) +
#   # geom_rect(aes(xmin = 1, xmax = 13, ymin = -2, ymax = 2.5),
#   #           color = "black", fill = "black",
#   #           inherit.aes = FALSE) + # Add black background circle
#   geom_col(data = month_labels, aes(x = x + 0.5, y = 2.5), fill = "black", # Add a unit of 0.5 to the pie chart to close the gap. Increase y value to make the pie chart bigger, meaning more black background
#            width = 1,
#            inherit.aes = FALSE) + # Create circular plotting panel for the outside (pie chart). A circle can be created by creating a pie chart, width 100%
#   geom_col(data = month_labels, aes(x = x + 0.5, y = -2), fill = "black", # Add a unit of 0.5 to the pie chart to close the gap
#            width = 1,
#            inherit.aes = FALSE) + # Create a circular plotting panel for the inside (pie chart) from 0 to negative values
#   geom_hline(yintercept = c(1.5, 2.0), color = "red") + # Create labelled isotherm lines
#   geom_label(data = temp_lines, aes(x = x, y = y, label = labels),
#              color = "red", fill = "black", label.size = 0,
#              inherit.aes = FALSE) + # Create labels for isotherm lines using temp_lines data frame
#   geom_text(data = month_labels, aes(x = x, y = y, label = labels),
#             color = "white",
#             inherit.aes = FALSE,
#             angle = seq(360 - 360/12, 0, length.out = 12)) + # Align month labels to be tangential to the spiral
#   geom_label(aes(x = 1, y = -1.3, label = year),
#              color = "white", fill = "black",
#              label.padding = unit(50, "pt"), label.size = 0,
#              size = 6) + # Add label of the latest year to the center of the spiral
#   geom_line() +
#   scale_x_continuous(breaks = 1:12,
#                      labels = month.abb, expand = c(0, 0),
#                      sec.axis = dup_axis(name = NULL, labels = NULL)) + # Add ticks to all 4 sides of the panel
#   scale_y_continuous(breaks = seq(-2, 2, 0.2),
#                      limits = c(-2.5, 2.8), expand = c(0, -0.7), # Expand/move the month labels outward to the extra space around the plot
#                      sec.axis = dup_axis(name = NULL, labels = NULL)) + # Add ticks to all 4 sides of the panel
#   scale_color_viridis_c(breaks = seq(1850, 2020, 15),
#                         guide = "none") +
#   coord_polar(start = 2*pi/12) +
#   labs(x = NULL,
#        y = NULL,
#        title = "Global temperature change from 1850 to 2022") +
#   theme(
#     plot.background = element_rect(fill = "#555555", color = "#555555"),
#     panel.background = element_rect(fill = "#555555"),
#     panel.grid = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.title.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title = element_text(color = "white", size = 11),
#     plot.title = element_text(color = "white", hjust = 0.5, size = 15),
#   ) +
#   transition_manual(frames = year, cumulative = TRUE)



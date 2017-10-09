## LOAD PACKAGES ####
library(tidyverse)


## READ IN DATA ####
solar_permits <- read_csv("data/solar_permits_issued_2016_datasd.csv")


## ORGANIZE DATA ####
# Get status summary
solar_permits_sum <- solar_permits %>%
  # Get total number of applications
  mutate(total_num_applications = n()) %>%
  # Get number of applications by status
  group_by(status, total_num_applications) %>%
  summarise(num_applications = n()) %>%
  ungroup() %>%
  # Get percentage of applications by status
  mutate(pct_applications = num_applications / total_num_applications)

# San Diego map
sandiego_map <- ggmap::get_googlemap(center = c(lon = -117.1, lat = 32.8),
                                     maptype = "roadmap",
                                     zoom = 10,
                                     size = c(500, 500),
                                     color = "bw")


## MAKE FIGURES ####
# Statuses
statuses_plot <- ggplot(solar_permits_sum, aes(x = status,
                                              y = pct_applications,
                                              fill = status)) +
  # Make a barplot
  geom_bar(stat = "identity") +
  # Make y-axis use %s
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  # Set axis labels
  labs(x = "Status",
       y = "% of applications") +
  # Use classic theme and put legend on top
  theme_classic() +
  theme(legend.position = "top")

statuses_plot

# Plot all locations (map)
locations_plot <- ggmap::ggmap(sandiego_map, extent = "device") +
  # Make scatterplot
  geom_point(data = solar_permits, aes(x = longitude,
                                      y = latitude,
                                      color = status)) +
  # Put legend on top
  theme(legend.position = "top")

locations_plot


## SAVE ENVIRONMENT ####
save.image("environment.RData")

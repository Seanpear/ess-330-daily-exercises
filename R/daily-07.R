# Name: Sean Pearson
# Date: 03/25/2025
# Purpose: Practice reading in Covid-19 data

library(tidyverse)

# Read in the state-level COVID data
covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Get the top 6 states with the most cases on the most recent date
top_states <- covid_states %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  summarize(max_cases = max(cases)) %>%
  arrange(desc(max_cases)) %>%
  slice_head(n = 6) %>%
  pull(state)

# Filter for only those states
filtered_data <- covid_states %>%
  filter(state %in% top_states)

# Faceted plot with one color per state
plot <- ggplot(filtered_data, aes(x = date, y = cases, color = state)) +
  geom_line(size = 1.2) +
  facet_wrap(~ state, scales = "free_y") +
  labs(
    title = "Cumulative Case Counts: COVID-19 Pandemic",
    x = "Date",
    y = "Cases",
    caption = "Daily Exercise 06"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 13, color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "white"),
    plot.caption = element_text(hjust = 0.5, color = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", color = NA),
    strip.background = element_rect(fill = "black")
  ) +
  guides(color = "none")  # Hides legend since each facet is labeled

# Save plot
ggsave("img/day-07-covid-facet.png", plot = plot, width = 10, height = 6)

# Question 2: Column plot of national total cases

# Read in national COVID data
covid_us <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")

# Plot it
us_plot <- ggplot(covid_us, aes(x = date, y = cases)) +
  geom_col(fill = "firebrick", alpha = 0.7) +
  labs(
    title = "National Cumulative Case Counts: COVID-19 Pandemic",
    x = "Date",
    y = "Cases",
    caption = "Daily Exercise 06"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "white"),
    plot.caption = element_text(hjust = 0.5, color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", color = NA)
  )

# Save the plot
ggsave("img/day-07-us-column-plot.png", plot = us_plot, width = 8, height = 5)

title: "Daily 8 Exercise"
author: "Sean Pearson"
date: "2025-03-27"
format: html
execute:
  echo: true


# Install tidyverse if it's not already installed
install.packages("tidyverse")

# Load tidyverse (which includes tibble and other necessary packages)
library(tidyverse)

# Create a simple tibble to test
tibble_test <- tibble(x = 1:5, y = letters[1:5])

# Print the tibble
print(tibble_test)

# Manually assign regions to states using tibble
region_map <- tibble(
  state = state.name,
  region = c(
    rep("Northeast", 9),
    rep("South", 16),
    rep("North Central", 12),
    rep("West", 13)
  )
)

# Check if region_map was created correctly
head(region_map)

# Join the region data to the COVID data
covid_joined <- covid_states %>%
  left_join(region_map, by = "state")

# Check if the region data was successfully added
head(covid_joined)

# Summarize the data by region and date
covid_region <- covid_joined %>%
  group_by(region, date) %>%
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    .groups = "drop"
  )

# Pivot the data to long format for plotting
covid_long <- covid_region %>%
  pivot_longer(cols = c(cases, deaths),
               names_to = "type",
               values_to = "count")

# Filter out NA regions if needed
covid_long_filtered <- covid_long %>%
  filter(!is.na(region), region != "NA")

# Facet by `type` (rows) and `region` (columns)
ggplot(covid_long_filtered, aes(x = date, y = count)) +
  geom_line(size = 1, color = "steelblue") +
  facet_grid(type ~ region, scales = "free_y") +
  labs(
    title = "Cumulative COVID-19 Cases & Deaths by Region",
    subtitle = "Data from NY Times",
    x = "Date",
    y = "Cumulative Count"
  ) +
  # Format the x-axis dates
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  # Show large numbers in short format (e.g., 20M)
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  theme_light(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"  # We only have one line color here, so no legend needed
  )

ggsave("covid_facet_plot_cases_deaths_separate.png", width = 12, height = 8)

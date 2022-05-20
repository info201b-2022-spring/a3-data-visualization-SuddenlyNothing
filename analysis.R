library(tidyverse)

incarceration_trends <- read.csv("incarceration_trends.csv")

# Summary info
summary_info <- list()
summary_info$columns <- colnames(incarceration_trends)
summary_info$min_year <- min(incarceration_trends$year)
summary_info$max_year <- max(incarceration_trends$year)
summary_info$num_states <- length(unique(incarceration_trends$state))
summary_info$num_counties <- length(unique(incarceration_trends$county_name))

# Trends over time chart
# Compare male to female prison proportion population over time
male_female_prison_prop <- incarceration_trends %>%
  select(year, total_prison_pop, female_prison_pop, male_prison_pop) %>%
  na.omit() %>%
  group_by(year) %>%
  mutate(total_prison_pop = sum(total_prison_pop),
        female_prison_pop = sum(female_prison_pop),
        male_prison_pop = sum(male_prison_pop)) %>%
  mutate(female_prison_prop = female_prison_pop / total_prison_pop,
         male_prison_prop = male_prison_pop / total_prison_pop) %>%
  gather(key = Legend, value = Rate, c("male_prison_prop", "female_prison_prop"))
# Create plot
male_female_prison_prop_lines <- ggplot(data = male_female_prison_prop,
                                        aes(
                                          x=year,
                                          y = Rate,
                                          group = Legend,
                                          color = Legend)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Prison Proportion", title = "Prison Proportion\nBy Gender") +
  theme(plot.title = element_text(hjust = 0.5))

library(tidyverse)
library(usmap)

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
  summarize(total_prison_pop = sum(total_prison_pop),
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
  labs(y = "Prison Proportion", x = "Year", title = "Prison Proportion\nBy Gender") +
  theme(plot.title = element_text(hjust = 0.5))

# Variable comparison chart
# Compares female jail population and female prison population
male_female_prison_2016 <- incarceration_trends %>%
  select(year, female_prison_pop, male_prison_pop) %>%
  filter(year == 2016) %>%
  group_by(year) %>%
  summarize(Male = sum(male_prison_pop, na.rm = T),
            Female = sum(female_prison_pop, na.rm = T)) %>%
  select(Male, Female) %>%
  pivot_longer(cols = everything())
# Create bar graph
female_male_prison_2016 <- ggplot(male_female_prison_2016, aes(x=name, y=value)) +
  geom_col() +
  labs(y = "Number of People", x = element_blank(),
      title = "Prison Populations\nBy Sex") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 12))

# Proportion prison grouped by state
state_incarcerations_prop <- incarceration_trends %>%
  filter(male_prison_pop != 0, female_prison_pop != 0) %>%
  mutate(male_to_female_ratio = male_prison_pop / female_prison_pop) %>%
  na.omit() %>%
  group_by(state) %>%
  summarize(male_to_female_ratio = sum(male_to_female_ratio))

state_ratio_map <- plot_usmap(data = state_incarcerations_prop, 
                              values = "male_to_female_ratio",
                              include = state_incarcerations_prop$state) +
  scale_fill_continuous(low = "white", high = "red", name = "Males Per Female")

# Relative proportion prison grouped by state
state_incarcerations_rel_prop <- incarceration_trends %>%
  select(state, total_prison_pop, female_prison_pop, male_prison_pop) %>%
  na.omit() %>%
  group_by(state) %>%
  summarize(total_prison_pop = sum(total_prison_pop),
            male_prison_pop = sum(male_prison_pop)) %>%
  mutate(rel_prop = male_prison_pop / total_prison_pop)

state_incarcerations_rel_prop <- plot_usmap(data = state_incarcerations_rel_prop, 
                              values = "rel_prop",
                              include = state_incarcerations_rel_prop$state) +
  scale_fill_continuous(low = "white", high = "red",
                        name = "Male Percent\nof Prison\nPopulation")

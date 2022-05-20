library(tidyverse)

# incarceration_trends <- read.csv("incarceration_trends.csv")

# Summary info
summary_info <- list()
summary_info$columns <- colnames(incarceration_trends)
summary_info$min_year <- min(incarceration_trends$year)
summary_info$max_year <- max(incarceration_trends$year)
summary_info$num_states <- length(unique(incarceration_trends$state))
summary_info$num_counties <- length(unique(incarceration_trends$county_name))


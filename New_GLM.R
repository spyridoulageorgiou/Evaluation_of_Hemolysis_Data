library(dplyr)
library(lubridate)

# Define a function to determine the season
get_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Autumn")
  }
}

# Extract season and time of day from anforderungsdatum
combined_data <- combined_data %>%
  mutate(
    season = sapply(anforderungsdatum, get_season),
    time_of_day = case_when(
      hour(anforderungsdatum) >= 5 & hour(anforderungsdatum) < 12 ~ "Morning",
      hour(anforderungsdatum) >= 12 & hour(anforderungsdatum) < 17 ~ "Afternoon",
      hour(anforderungsdatum) >= 17 & hour(anforderungsdatum) < 21 ~ "Evening",
      TRUE ~ "Night"
    )
  )

# Convert season and time_of_day to factors
combined_data$season <- factor(combined_data$season, levels = c("Winter", "Spring", "Summer", "Autumn"))
combined_data$time_of_day <- factor(combined_data$time_of_day, levels = c("Morning", "Afternoon", "Evening", "Night"))

# Fit GLM for seasons
glm_season <- glm(wert ~ season, data = combined_data, family = gaussian())

# Fit GLM for time of day
glm_time_of_day <- glm(wert ~ time_of_day, data = combined_data, family = gaussian())

# Print the summaries of the models
summary(glm_season)
summary(glm_time_of_day)


# Fit GLM for sex
glm_sex <- glm(wert ~ geschlecht, data = combined_data, family = gaussian())

# Print the summary of the model
summary(glm_sex)


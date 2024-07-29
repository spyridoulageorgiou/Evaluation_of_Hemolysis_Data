# Load necessary packages
library(dplyr)
library(writexl)

# Define a function to compute descriptive statistics for each clinic
describe_clinic <- function(data) {
  data %>%
    group_by(einsender_klinik) %>%
    summarize(
      count = n(),
      min_wert = min(wert, na.rm = TRUE),
      max_wert = max(wert, na.rm = TRUE),
      mean_wert = mean(wert, na.rm = TRUE),
      median_wert = median(wert, na.rm = TRUE),
      sd_wert = sd(wert, na.rm = TRUE),
      gender_F = sum(geschlecht == "F", na.rm = TRUE),
      gender_M = sum(geschlecht == "M", na.rm = TRUE),
      gender_missing = sum(is.na(geschlecht)),
      case_type_stationär = sum(fallart == "stationär", na.rm = TRUE),
      case_type_teilstationär = sum(fallart == "teilstationär", na.rm = TRUE),
      case_type_ambulant = sum(fallart == "ambulant", na.rm = TRUE)
    ) %>%
    arrange(desc(count))
}

# Apply the function to the combined data
clinic_stats <- describe_clinic(combined_data)

# View the summarized statistics for each clinic
print(clinic_stats)

# Write the descriptive statistics to an Excel file
write_xlsx(clinic_stats, "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/clinic_descriptive_stats.xlsx")




### ER ###

# Define the list of ER clinics
er_clinics <- c("Universitäres Notfallzentrum", "Universitätsklinik für Notfallmedizin", "Notfallzentrum Kinder & Jugendliche")

# Separate the data into ER and non-ER groups
er_data <- combined_data %>% filter(einsender_klinik %in% er_clinics)
non_er_data <- combined_data %>% filter(!einsender_klinik %in% er_clinics)

# Calculate descriptive statistics for ERs
er_stats <- er_data %>%
  summarize(
    count = n(),
    min_wert = min(wert, na.rm = TRUE),
    max_wert = max(wert, na.rm = TRUE),
    mean_wert = mean(wert, na.rm = TRUE),
    median_wert = median(wert, na.rm = TRUE),
    sd_wert = sd(wert, na.rm = TRUE)
  )

# Calculate descriptive statistics for Non-ERs
non_er_stats <- non_er_data %>%
  summarize(
    count = n(),
    min_wert = min(wert, na.rm = TRUE),
    max_wert = max(wert, na.rm = TRUE),
    mean_wert = mean(wert, na.rm = TRUE),
    median_wert = median(wert, na.rm = TRUE),
    sd_wert = sd(wert, na.rm = TRUE)
  )

# Print the descriptive statistics for comparison
print("Descriptive statistics for ER clinics:")
print(er_stats)

print("Descriptive statistics for Non-ER clinics:")
print(non_er_stats)

# Optionally, save the results to an Excel file
write_xlsx(list(ERs = er_stats, Non_ERs = non_er_stats), "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/ER_vs_NonER_stats.xlsx")

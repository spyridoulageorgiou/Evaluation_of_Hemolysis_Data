library(readxl)
library(writexl)
library(dplyr)
#install.packages("lubridate")
library(lubridate)


# File paths
file1 <- "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/idsc202101580_data_210610.xlsx"
file2 <- "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/ritm0143077_main_28032024.xlsx"

# Read all sheets from the first file and combine them into one data frame (2019-20-21)
sheets <- excel_sheets(file1)
data1 <- bind_rows(lapply(sheets, function(sheet) {
  read_excel(file1, sheet = sheet)
}))

# Second file (2023-24)
data2 <- read_excel(file2)

# Convert anforderungsdatum in data2 to datetime
data2 <- data2 %>%
  mutate(anforderungsdatum = as.POSIXct(anforderungsdatum, format = "%Y-%m-%d %H:%M:%S"))

# Fix columns order
data2 <- data2 %>% 
  rename(
    pseudo_pid = pseudo_pid,
    anforderungsdatum = anforderungsdatum,
    fallart = fallart,
    geschlecht = geschlecht,
    einsender = einsender,
    einsender_name = einsender_name,
    einsender_klinik = einsender_klinik,
    einsender_typ = analyse_nr,
    material = analyse_kbz,
    wert = analyse_wert
  )

# Select and reorder columns in both data frames
data1 <- data1 %>%
  select(
    pseudo_pid,
    anforderungsdatum,
    fallart,
    geschlecht,
    einsender,
    einsender_name,
    einsender_klinik,
    einsender_typ,
    material,
    wert
  )

data2 <- data2 %>%
  select(
    pseudo_pid,
    anforderungsdatum,
    fallart,
    geschlecht,
    einsender,
    einsender_name,
    einsender_klinik,
    einsender_typ,
    material,
    wert
  )

# Combine dfs
combined_data <- bind_rows(data1, data2)

# Split the data into two parts cause they don't fit in a single excel
split_data <- function(data) {
  half <- ceiling(nrow(data) / 2)
  part1 <- data[1:half, ]
  part2 <- data[(half + 1):nrow(data), ]
  list(part1 = part1, part2 = part2)
}

# Split the combined data into two parts
data_parts <- split_data(combined_data)

# Write each part to a separate excel
write_xlsx(data_parts$part1, "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/combined_data_part1.xlsx")
write_xlsx(data_parts$part2, "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/combined_data_part2.xlsx")


### Overview ###

# Summary statistics for the entire dataset
summary(combined_data) # mainly care about the column wert


# Check for missing values in each column
missing_values <- sapply(combined_data, function(column) {
  sum(is.na(column))
})

missing_values


# Split wert into hemolysis categories
combined_data <- combined_data %>%
  mutate(hemolysis_category = case_when(
    wert < 20 ~ "Not Hemolytic",
    wert >= 20 & wert < 50 ~ "Mild to Moderate Hemolysis",
    wert >= 50 ~ "Significant Hemolysis"
  ))


library(ggplot2)

# Calculate frequencies of each hemolysis category
hemolysis_counts <- combined_data %>%
  count(hemolysis_category)

# Wert barplot
ggplot(combined_data, aes(x = reorder(hemolysis_category, -as.numeric(hemolysis_category)), fill = hemolysis_category)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 2.5) +  # Add text labels above each bar
  labs(title = "Distribution of Hemolysis Categories", x = "Hemolysis Category", y = "Frequency") +
  scale_fill_manual(values = c("Not Hemolytic" = "violet", "Mild to Moderate Hemolysis" = "deepskyblue", "Significant Hemolysis" = "green")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels as comma-separated




# Recode NAs in geschlecht column to "Unknown"
combined_data$geschlecht[is.na(combined_data$geschlecht)] <- "Unknown"

# Geschlecht barplot
ggplot(combined_data, aes(x = factor(geschlecht), fill = geschlecht)) +
  geom_bar() +
  labs(title = "Distribution of Gender (Geschlecht)", x = "Gender", y = "Count") +
  scale_fill_manual(values = c("F" = "violet", "M" = "deepskyblue", "Unknown" = "grey")) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 3, color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# List of unique clinics (einsender_klinik)
unique_clinics <- unique(combined_data$einsender_klinik)
print("List of Clinics:")
print(unique_clinics)

# Count the number of entries for each unique clinic
clinic_counts <- combined_data %>%
  count(einsender_klinik) %>%
  arrange(desc(n))

# Print the counts
print("Number of entries for each unique clinic:")
print(clinic_counts)

write_xlsx(clinic_counts, "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/clinic_counts.xlsx")





### Identify and extract duplicates ###

# Count occurrences of each pseudo_pid
pid_counts <- combined_data %>%
  count(pseudo_pid)

# Filter to keep only those with more than 1 occurrence
duplicates <- pid_counts %>%
  filter(n > 1) %>%
  select(pseudo_pid)

# Extract rows with duplicated pseudo_pid
duplicated_rows <- combined_data %>%
  filter(pseudo_pid %in% duplicates$pseudo_pid)

# Split the duplicated rows into two parts because they don't fit in a single excel
split_data <- function(data) {
  half <- ceiling(nrow(data) / 2)
  part1 <- data[1:half, ]
  part2 <- data[(half + 1):nrow(data), ]
  list(part1 = part1, part2 = part2)
}

# Split the duplicated rows into two parts
duplicated_parts <- split_data(duplicated_rows)

# Write each part to a separate excel
write_xlsx(duplicated_parts$part1, "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/duplicated_pseudo_pid_rows_part1.xlsx")
write_xlsx(duplicated_parts$part2, "C:/Users/Spyridoula/OneDrive/Desktop/Universität/Master Thesis/Archiv/duplicated_pseudo_pid_rows_part2.xlsx")

library(dplyr)

calculate_summary_stats <- function(data, category_name) {
  data <- data %>%
    mutate(
      Actual_Costs = exp(LOG.STAFF.COSTS) / 1e6, # transform logarithmic values back to normal scale
      Predicted_Costs = exp(pred_LOG.STAFF.COSTS) / 1e6
    )
  
  # calculate some summary statistics
  data %>%
    summarise(
      Category = category_name,
      Mean_Actual = mean(Actual_Costs),
      Median_Actual = median(Actual_Costs),
      Std_Dev_Actual = sd(Actual_Costs),
      Mean_Predicted = mean(Predicted_Costs),
      Median_Predicted = median(Predicted_Costs),
      Std_Dev_Predicted = sd(Predicted_Costs),
      MAE = mean(abs(Actual_Costs - Predicted_Costs)),
      MAPE = mean(abs((Actual_Costs - Predicted_Costs) / Actual_Costs)) * 100,
      R_squared = 1 - (sum((Actual_Costs - Predicted_Costs)^2) / sum((Actual_Costs - mean(Actual_Costs))^2))
    )
}

files_and_categories <- list(
  "Allgemeine_Verwaltung.csv" = "Allgemeine Verwaltung",
  "FinanzenSteuern.csv" = "FinanzenSteuern",
  "Gesundheit.csv" = "Gesundheit",
  "KultSportFrei.csv" = "KultSportFrei",
  "Ordnung_Sicherheit.csv" = "Ordnung Sicherheit",
  "Soziale Sicherheit.csv" = "Soziale Sicherheit",
  "Umwelt_Raumordnung.csv" = "Umwelt Raumordnung",
  "VerkehrNachrichten.csv" = "VerkehrNachrichten",
  "Volkswirtschaft.csv" = "Volkswirtschaft"
)

all_results <- data.frame()

for (file_name in names(files_and_categories)) {
  category_name <- files_and_categories[[file_name]]
  
  data <- read.csv(file_name)
  
  summary_stats <- calculate_summary_stats(data, category_name)
  
  all_results <- bind_rows(all_results, summary_stats)
}

print(all_results)

write.csv(all_results, "Summary_Statistics_Millions.csv", row.names = FALSE)

##########################################
#
# Stats Lab
# Predicting FTEs
# 
##########################################

######### PACKAGES ###########
library(readxl)
#############################

# Objective: use staff costs per department to predict FTEs per department
# Municipalities: the 24 included in Federas Excel sheets (muni_excels.zip)
# Predictors: costs per department from stats office
# DV: FTEs (Stellenprozente) per department from Federas excels
# Model: lm with intercept and slope (simple linear regression)


names <- c("Erlenbach", "Volketswil_3", "Staaefa", "Volketswil", "Oberglatt", 
           "Niederglatt", "Herrliberg", "Bubikon", "Aesch_ZH", "Volketswil_2", 
           "Rifferswil", "Bassersdorf")
path <- "/Users/lea/Desktop/"

data_list <- list()

for (name in names) {
  file_path <- paste0(path, name, ".xlsx")
  try({
    df <- read_excel(file_path)
    data_list[[name]] <- df
  }, silent = TRUE) 
}


# Financial data per department
department.financial.data # STAFF_COSTS not in log scale

# Extracting Relevant Data for each Gemeinde
# Not Considered Gemeinden: oberglatt, stäfa, volketswil 3, volketswil 2, volketswil, Stäfa 2
bassersdorf <- data_list$Bassersdorf[2:4, c(2,4, 8, 10,12)] # 2023, use 2022
bassersdorf <- rbind(bassersdorf, rep(2022, nrow(bassersdorf)))

bubikon <- data_list$Bubikon[c(1,2,4), c(2,4,6,8)] # 2021
bubikon <- rbind(bubikon, rep(2021, nrow(bubikon)))

rifferswil <- data_list$Rifferswil[2:4, c(2,4,6,8,10)] # 2022
rifferswil <- rbind(rifferswil, rep(2022, nrow(rifferswil)))

Aesch_ZH <- data_list$Aesch_ZH[c(1,2,5), c(2,4,6,8)] # 2020, use 2021
Aesch_ZH <- rbind(Aesch_ZH, rep(2021, nrow(Aesch_ZH)))

erlenbach <- data_list$Erlenbach[2:4, c(2,4,6,8)] # 2022
erlenbach <- rbind(erlenbach, rep(2022, nrow(erlenbach)))

niederglatt <- data_list$Niederglatt[2:4, c(2, 4, 6, 8, 10, 12)] # 2022
niederglatt <- rbind(niederglatt, rep(2022, nrow(niederglatt)))


# Herliberg has Strange Format
gemeinden_herrliberg <- colnames(data_list$Herrliberg) # 2018
data_list$Herrliberg <- rbind(gemeinden_herrliberg, data_list$Herrliberg)
herrliberg <- data_list$Herrliberg[c(1,2,5), c(2,5, 8, 11, 14)]
herrliberg <- rbind(herrliberg, rep(2018, nrow(herrliberg)))


gemeinde_list <- list(bubikon, bassersdorf, rifferswil, Aesch_ZH, erlenbach, herrliberg, niederglatt)
total_df <- data.frame()
col_names <- c("Gemeinde/Stadt", "Einwohnerzahl", "Stellenprozente", "Jahr")


# Create Dataframe with Gemeinde, Einwohnerzahl, Stellenprozente
for (i in seq_along(gemeinde_list)) {
  df_matrix <- as.matrix(gemeinde_list[[i]])
  df_transposed <- t(df_matrix)
  
  df_transposed_2 <- as.data.frame(df_transposed, row.names = FALSE, stringsAsFactors = FALSE)
  
  colnames(df_transposed_2) <- col_names
  
  total_df <- rbind(total_df, df_transposed_2)
}


# Convert to Numeric
total_df$Einwohnerzahl <- as.numeric(total_df$Einwohnerzahl)
total_df$Stellenprozente <- as.numeric(total_df$Stellenprozente)
total_df$Jahr <- as.numeric(total_df$Jahr)

# remove duplicate columns (choose row with highest stellenprozente)
rows_to_exclude <- c(26, 23, 29, 31, 27, 33, 32, 24, 30)
total_df <- total_df[-rows_to_exclude, ]
total_df <- total_df[order(total_df$Einwohnerzahl),]

# Editing Gemeinden Names
gemeinden <- total_df$`Gemeinde/Stadt`
patterns <- c(" \\(EHG\\)", "  \\(pol\\. Gemeinde\\)", " an der Limmat", " am Albi", " ZH")
for (pattern in patterns) {
  gemeinden <- gsub(pattern, "", gemeinden)
}

gemeinden[gemeinden == "Affolterns"] <- "Affoltern a.A."
gemeinden[gemeinden == "Oetwil"] <- "Oetwil a.d.L."
gemeinden[gemeinden == "Aeugsts"] <- "Aeugst a.A."

total_df$`Gemeinde/Stadt` <- gemeinden

jahr <- unique(total_df$Jahr)


# change stellenprozente to FTE
total_df$Stellenprozente <- total_df$Stellenprozente/100

finacial.data <- list()
for (names in names(department.financial.data)) {
  df <- department.financial.data[[names]]
  
  filtered_df <- df[df$GEMEINDE %in% gemeinden & df$YEAR %in% jahr,]

  finacial.data[[names]] <- filtered_df
}


# merging and rearranging
key_data <- total_df[, c("Gemeinde/Stadt", "Einwohnerzahl", "Stellenprozente", "Jahr")]
colnames(key_data) <- c("GEMEINDE", "Einwohnerzahl", "Stellenprozente", "YEAR")

total.df <- list()
for (names in names(finacial.data)) {
  df <- finacial.data[[names]]
  
  merged_df <- merge(df, key_data, by = c("GEMEINDE", "YEAR"))
  
  total.df[[names]] <- merged_df[ ,c(2,1,4,5,3)]
  colnames(total.df[[names]]) <- c("year", "municipality", "population", "ftes", "staff_costs")
}


#################### FITTING FTE MODEL AND PREDICTING FTE ###############################

# Fitting the Model
fit_models <- list()

for (name in names(total.df)) {
  df <- total.df[[name]]
  
  df$log_ftes <- log(df$ftes)
  df$log_staff_costs <- log(df$staff_costs)
  
  fit <- lm(log_ftes ~ log_staff_costs, data = df)
  r22 <- summary(fit)$r.squared
  
  plot(log(df$staff_costs), log(df$ftes),
       xlab = "log Total staff costs in million CHF (2018)",
       ylab = "log Total FTEs",
       main = paste("log(Staff costs) vs. log(FTEs) -", name),
       pch = 19, cex = 0.6)
  text(x = min(log(df$staff_costs)) + 1, y = max(log(df$ftes)) - 1, paste("r2=", round(r22, 3)))
  abline(fit, col = "red")
  
  plot(fit, which = 1, main = paste("TA-Plots", name))
  
  fit_models[[name]] <- fit
}


# Predictions using above model for all municiaplities and all years
predictions_fte <- list()  # Initialize an empty list to store predictions

for (department in names(department.financial.data)) {

  df <- department.financial.data[[department]]
  
  newdata <- data.frame(log_staff_costs = log(df$STAFF_COSTS))
  pred_log_ftes <- predict(fit_models[[department]], newdata = newdata)
  pred_ftes <- exp(pred_log_ftes)
  
  predictions_fte[[department]] <- cbind(df, pred_ftes)
  colnames(predictions_fte[[department]]) <- c("YEAR", "GEMEINDE", "STAFF_COSTS", "pred_fte")
}

# Save the predictions to CSV files
for (department in names(predictions_fte)) {
  write.csv(predictions_fte[[department]], paste0(department, "_ALL_predicted_FTEsFINAL.csv"))
}

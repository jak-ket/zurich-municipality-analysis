# naming of excel files for simplicity when reading in

# A2_Gemeindevergleich unbereinigt VS = Volketswil
# A2 Gemeindevergleich Volketswil Liegenschaften  = Volketswil_2
# A2_Gemeindevergleich Volketswil_korr = Volketswil_3
# A2_Gemeindevergleich Oberglatt = Oberglatt
# A2_Gemeindevergleich Bubikon = Bubikon
# A2_Gemeindevergleich Herrliberg = Herrliberg
# A2_Gemeindevergleich unbereinigt SP = Stäfa
# A2_Gemeindevergleich unbereinigt Liegenschaften Hochbau = Stäfa 2
# A2_Gemeindevergleich Aesch ZH = Aesch ZH
# A2 Gemeindevergleich Rifferswil unbereinigt = Rifferswil
# A2 Gemeindevergleich Bassersdorf = Bassersdorf
# A2_Gemeindevergleich Niederglatt = Niederglatt

# important: the following files are the same ones:
# A2_Gemeindevergleich Herrliberg = A2_Zusammenzug Gemeindevergleich (A3 Format) Lindau
# A2_Gemeindevergleich volki Hochbau = A2_Gemeindevergleich Bubikon
#Berechnungen Koeffizienten Niederglatt = A2_Gemeindevergleich Niederglatt


#read all files

names <- c("Erlenbach", "Volketswil_3", "Staefa", "Volketswil", "Oberglatt", "Niederglatt", "Herrliberg", "Bubikon", "Aesch_ZH", "Volketswil_2", "Rifferswil", "Bassersdorf")
writeLines(names, "muni_names.txt")
library(readxl)

path <- "/Users/jakob/OneDrive/_SS24/StatsLab/renamed/" # correct here filepath when reading in files
data_list <- list()

for (name in names) {
  file_path <- paste0(path, name, ".xlsx")
    try({
    df <- read_excel(file_path)
    data_list[[name]] <- df
  }, silent = TRUE) 
}


## select relevant data for each excel file manually, cause there is inconsistency

bassersdorf <- data_list$Bassersdorf[2:4, c(2,4, 8, 10,12)] # attention: thalwil is not considered here, cause there is no data for total stellenprozente
bubikon <- data_list$Bubikon[c(1,2,4), c(2,4,6,8)]
rifferswil <- data_list$Rifferswil[2:4, c(2,4,6,8,10)]
Aesch_ZH <- data_list$Aesch_ZH[c(1,2,5), c(2,4,6,8)]
erlenbach <- data_list$Erlenbach[2:4, c(2,4,6,8)]
niederglatt <- data_list$Niederglatt[2:4, c(2, 4, 6, 8, 10, 12)]

#correct some stuff for herrliberg (gemeinde_names were not within dataset, but read as column names)
gemeinden_herrliberg <- colnames(data_list$Herrliberg)
data_list$Herrliberg <- rbind(gemeinden_herrliberg, data_list$Herrliberg)
herrliberg <- data_list$Herrliberg[c(1,2,5), c(2,5, 8, 11, 14)]

# the dataset oberglatt contains only data for immobilien --> hence not considered
# the dataset stäfa contains only data for the department "Personal" --> hence not considered
# dataset volketswil 3 only contains values for "finanzen" --> hence not considered
# dataset volketswil 2 only contains values for ableitung "liegenschaften" --> hence not considered
# dataset "volketswil" only contains values for abteilung sicherheit --> hence not considered
# dataset "Stäfa 2" only contains values for Abteilung "Hochbau"--> hence not considered



# store all dataframes within a list to loop through
gemeinde_list <- list(bubikon, bassersdorf, rifferswil, Aesch_ZH, erlenbach, herrliberg, niederglatt)
total_df <- data.frame()

col_names <- c("Gemeinde/Stadt", "Einwohnerzahl", "Stellenprozente")

str(gemeinde_list)


for (i in seq_along(gemeinde_list)) {
  df_matrix <- as.matrix(gemeinde_list[[i]])
  df_transposed <- t(df_matrix)
  
  df_transposed_2 <- as.data.frame(df_transposed, row.names = FALSE, stringsAsFactors = FALSE)
  
  colnames(df_transposed_2) <- col_names
  
  total_df <- rbind(total_df, df_transposed_2)
}

# change data to numeric
total_df$Einwohnerzahl <- as.numeric(total_df$Einwohnerzahl)
total_df$Stellenprozente <- as.numeric(total_df$Stellenprozente)


# order dataset by einwohnerzahl and remove duplicate columns (choose the row that has the highest stellenprozente among the duplicates)
rows_to_exclude <- c(26, 23, 29, 31, 27, 33, 32, 24, 30)
total_df <- total_df[-rows_to_exclude, ]
total_df <- total_df[order(total_df$Einwohnerzahl),]
nrow(total_df)
total_df

# add total costs of staff for 2018
total_staff_costs <- read.csv("data/total_staff_costs.csv")
total_staff_costs$total_staff_costs_2018 <- gsub("'", "", total_staff_costs$total_staff_costs_2018)
total_staff_costs$total_staff_costs_2018 <- gsub(" ", "", total_staff_costs$total_staff_costs_2018)
total_staff_costs$total_staff_costs_2018 <- sapply(total_staff_costs$total_staff_costs_2018, as.integer)
# append costs
total_df <- cbind(total_df, total_staff_costs$total_staff_costs_2018)

# change stellenprozente to fte
total_df$Stellenprozente <- total_df$Stellenprozente/100

# change staff costs to million
total_df[,"total_staff_costs$total_staff_costs_2018"] <- total_df[,"total_staff_costs$total_staff_costs_2018"]/1e6

# rename 
colnames(total_df) <- c("municipality", "population", "ftes", "staff_costs")


# plot population vs. FTE
plot(total_df$population, total_df$ftes, 
     xlab = "Population", 
     ylab = "Total FTEs", 
     main = "Population vs. FTEs",
     pch = 19, cex = 0.6) 

fit0 <- lm(total_df$ftes ~ total_df$population)
abline(fit0)
summary(fit0)
legend("bottomright", legend = paste("n =", nrow(total_df)), bty = "n")  

# approach 1: total staff costs vs. FTE
plot(total_df$staff_costs, total_df$ftes,
     xlab = "Total staff costs in million CHF (2018)",
     ylab = "Total FTEs",
     main = "Staff costs vs. FTEs",
     pch = 19, cex = 0.6)

fit1 <- lm(total_df$ftes ~ total_df$staff_costs) # removing intercept can increase R²
abline(fit1)
legend("bottomright", legend = paste("1/slope =", round(1/fit1$coefficients[2]*1e6), "CHF"), bty = "n")  
summary(fit1)
dim(total_df)

hist(total_df$ftes, breaks=10, xlab="Total FTEs", main="Histogram of Total FTEs")

# Create histogram
library(MASS)
hist(total_df$ftes, 
     breaks=10, freq = FALSE, 
     main = "Histogram of Total FTEs with Lognormal Fit", ylim=c(0,0.03), xlab="Total FTEs")
fit <- fitdistr(total_df$ftes, "lognormal")
mu <- fit$estimate[1]
sigma <- fit$estimate[2]
x <- seq(min(total_df$ftes), max(total_df$ftes), length.out = 100)
density <- dlnorm(x, meanlog = mu, sdlog = sigma)
lines(x, density, col = "red", lwd = 2, cex.axis = 1.5)
legend("topright", legend = "Lognormal Fit", col = "red", lty = 1, lwd = 2)

hist(fit1$residuals, probability = T, ylim=c(0,0.045))
lines(density(fit1$residuals), col = 4, lwd = 2)

plot(fit1)



# approach 2: total staff costs vs. log(FTE) => log-transform of the target
plot(total_df$staff_costs, log(total_df$ftes),
     xlab = "Total staff costs in million CHF (2018)",
     ylab = "log Total FTEs",
     main = "Staff costs vs. log(FTEs)",
     pch = 19, cex = 0.6)

fit2 <- lm(log(total_df$ftes) ~ total_df$staff_costs)
abline(lm(log(total_df$ftes) ~ total_df$staff_costs))
summary(fit2)

plot(fit2, which=2)

hist(log(total_df$ftes),  xlab="Total FTEs", main="Histogram of Total FTEs")

log_data <- log(total_df$ftes)
hist(log_data, freq = FALSE, main = "Histogram of log(FTEs) with Normal Fit", xlab="log(FTEs)")
fit <- fitdistr(log_data, "normal")
mean <- fit$estimate["mean"]
sd <- fit$estimate["sd"]
x <- seq(min(log_data), max(log_data), length.out = 100)
density <- dnorm(x, mean = mean, sd = sd)
lines(x, density, col = "red", lwd = 2)
legend("topright", legend = "Normal Fit", col = "red", lty = 1, lwd = 2)

hist(fit2$residuals, probability = T, ylim=c(0,0.85))
lines(density(fit2$residuals), col = 4, lwd = 2)


# approach 3: log(total staff costs) vs. log(FTE) => log-transform of the target and covariable
plot(log(total_df$staff_costs), log(total_df$ftes),
     xlab = "log Total staff costs in million CHF (2018)",
     ylab = "log Total FTEs",
     main = "log Staff costs vs. log FTEs",
     pch = 19, cex = 0.6)

fit3 <- lm(log(total_df$ftes) ~ log(total_df$staff_costs))
abline(lm(log(total_df$ftes) ~ log(total_df$staff_costs)))
summary(fit3)

plot(fit3, which=1)


hist(fit3$residuals, probability = T)
lines(density(fit3$residuals), col = 4, lwd = 2)

# approach 4: total staff costs vs. FTE
plot(total_df$staff_costs, sqrt(total_df$ftes),
     xlab = "Total staff costs in million CHF (2018)",
     ylab = "Total FTEs",
     main = "Staff costs vs. FTEs",
     pch = 19, cex = 0.6)

fit4 <- lm(sqrt(total_df$ftes) ~ total_df$staff_costs)
abline(fit4)

plot(fit4)


# mean average salaries over municipalities
hist(total_df$staff_costs*1e3/total_df$ftes, main="Histogram of Mean Annual Salary", xlab="Mean annual salary in 1000 CHF")

plot(total_df$population, total_df$staff_costs, main="Population vs. Staff Costs", xlab="Population", ylab="Staff costs in Million CHF")

hist(total_df$population, breaks=10, main="Histogram of Population", xlab="Population")
hist(total_df$staff_costs, breaks=7, main="Histogram of Staff Costs", xlab="Staff Costs in Million CHF")

# geographic heatmaps
# install.packages("ggplot2")
# install.packages("swissmaps")


##############################################
#  Stats Lab
#  Predicting Staff Costs per Department
# Data Exploratory Analysis
#############################################

####### PACKAGES #######
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(car)
library(carData)
#######################

# Loading Processed Data
data.predictors.scaled 
all.departments.combined.data

############################# IDENTIFYING + SHIFTING NEGATIVE VALUES IN PREDICTORS DATA #######################################

data.predictors.filled.shifted <- data.predictors.scaled # Initialize

columns.with.negatives <- c() # store the names of columns with negative values

for(col in names(data.predictors.filled.shifted)) { # Loops through each column to check neg. values
  if(any(data.predictors.filled.shifted[[col]] < 0, na.rm = TRUE)) {
    columns.with.negatives <- c(columns.with.negatives, col) # adds column name with neg values
  }
}
columns.with.negatives # 248

# stores  minimum values for columns with negatives
min_values_for_negatives <- setNames(numeric(length(columns.with.negatives)), columns.with.negatives)

for(col in columns.with.negatives) { # loops to find minimum values
  min_values_for_negatives[col] <- min(data.predictors.filled.shifted[[col]], na.rm = TRUE)
}


# Add each minimum vlaue to its corresponding column
for(col in columns.with.negatives) {
  if(min_values_for_negatives[col] < 0) {
    data.predictors.filled.shifted[[col]] <- data.predictors.filled.shifted[[col]] - min_values_for_negatives[col] # add smallest number
  }
}

######################### HISTOGRAMS OF PREDICTOR VARIABLES ###################################


# Histogram of unshifted predictors
for(i in 3:(ncol(data.predictors.scaled))) {
  hist(data.predictors.scaled[[i]], main=paste("Histogram of", names(data.predictors.scaled[i])),
       xlab=names(data.predictors.scaled[i]), col="blue")
}

# Log Transforming Shifted Predictors 
log.data.predictors.filled <- data.predictors.filled.shifted

for(predictor in names(data.predictors.filled.shifted)) {
  if(is.numeric(data.predictors.filled.shifted[[predictor]])) {
    log.data.predictors.filled[[predictor]] <- log(data.predictors.filled.shifted[[predictor]] + 1)
  }
}

# Histogram of Log Transformed Predictors
for(i in 3:(ncol(log.data.predictors.filled))) {
  hist(log.data.predictors.filled[[i]], 
       main = paste("Histogram of", names(log.data.predictors.filled[i])),
       xlab = names(log.data.predictors.filled[i]), 
       col = "orange", 
       xlim = range(log.data.predictors.filled[[i]], na.rm = TRUE))
}

####################### IDENTIFYING AND SHIFTING NEGATIVE VALUES OF FINANCIAL DATA ##############################

department.financial.data <- list(
  Gesundheit = data.financial.Gesundheit,
  KultSportFrei = data.financial.KultSportFrei,
  OrdnungSicherheit = data.financial.OrdnungSicherheit,
  SozialeSicherheit = data.financial.SozialeSicherheit,
  AllgemeineVerwaltung = data.financial.AllgemeineVerwaltung,
  FinanzenSteuern = data.financial.FinanzenSteuern,
  UmweltRaumordnung = data.financial.UmweltRaumordnung,
  VerkehrNachrichten = data.financial.VerkehrNachrichten,
  Volkswirtschaft = data.financial.Volkswirtschaft
)

# Identifying Minimum Value in Financial Data
global_min_staff_costs <- Inf # Initialize

for(department in names(department.financial.data)) { # Loop thrpugh each department to find min staff costs
  department_data <- department.financial.data[[department]]
  department_min <- min(department_data$STAFF_COSTS, na.rm = TRUE)
  
  if(department_min < global_min_staff_costs) { # Update global minimum 
    global_min_staff_costs <- department_min
  }
}

global_min_staff_costs #-134711.2,  Wiesendagen (2014)

# Number of Negative Values in Each department
# Gesundheit: n = 0, 
# KultSportFrei: n = 0
# OrdnungSicherheit: n = 0
# SozialeSicherheit: n = 0, 
# AllgemeineVerwaltung: n = 0
# FinanzenSteuern: n = 1 # Gemeinde: Adliswil (2015), -3512.40
# UmweltRaumordnung: n = 0
# VerkehrNachrichten: n = 1 # Volketswil (2015), -134711.2
# Volkswirtschaft: n = 0,

# SUGGESTION: Remove all negative values since there are only 2
department.financial.data[["FinanzenSteuern"]] <- subset(department.financial.data[["FinanzenSteuern"]], !(GEMEINDE == "Adliswil" & YEAR == 2015))
department.financial.data[["VerkehrNachrichten"]] <- subset(department.financial.data[["VerkehrNachrichten"]], !(GEMEINDE == "Volketswil" & YEAR == 2015))

global_min_staff_costs <- Inf
for(department in names(department.financial.data)) { # Loop thrpugh each department to find min staff costs
  department_data <- department.financial.data[[department]]
  department_min <- min(department_data$STAFF_COSTS, na.rm = TRUE)
  
  if(department_min < global_min_staff_costs) { # Update global minimum 
    global_min_staff_costs <- department_min
  }
}

global_min_staff_costs # 0.25, which is positive

# Shift financial data by adding minimum value
department.financial.data.shifted <- department.financial.data # Initialize

for(department in names(department.financial.data.shifted)) {  
  if(global_min_staff_costs < 0) { # Check, only add global min if its negative
    department.financial.data.shifted[[department]]$STAFF_COSTS <- department.financial.data.shifted[[department]]$STAFF_COSTS - global_min_staff_costs
  }
}

####################### HISTOGRAM OF FINANCIAL DATA ############################################################

# Histogram of Staff Costs per Department
for(department in names(department.financial.data.shifted)) {
  hist(department.financial.data.shifted[[department]]$STAFF_COSTS, 
       main = paste("Staff Costs for Department", department),
       xlab = "Staff Costs", 
       col = "green",
       xlim = range(department.financial.data.shifted[[department]]$STAFF_COSTS, na.rm = TRUE))
}

# Log Transform Staff Costs 
for (department in names(department.financial.data.shifted)) {
  department.financial.data.shifted[[department]]$LOG.STAFF.COSTS <- log(department.financial.data.shifted[[department]]$STAFF_COSTS + 1)
}

# Histogram of log transformed Staff Costs
for(department in names(department.financial.data.shifted)) {
  hist(department.financial.data.shifted[[department]]$LOG.STAFF.COSTS, 
       main = paste("Staff Costs for Department", department),
       xlab = "Log Staff Costs", 
       col = "yellow",
       xlim = range(department.financial.data.shifted[[department]]$LOG.STAFF.COSTS, na.rm = TRUE))
} 

##################### COMBINING SHIFTED PREDICTOR + FINANCIAL DATASETS ##############################

all.departments.combined.shifted.data <- list() # Initialize

for(department_name in names(department.financial.data.shifted)) { # Loop through each department

    dept_data <- department.financial.data.shifted[[department_name]] %>%
    select(YEAR, GEMEINDE, STAFF_COSTS, LOG.STAFF.COSTS)
  
    # merge
  merged_dept_data <- left_join(data.predictors.filled.shifted, dept_data, 
                                by = c("YEAR", "GEMEINDE"))
  
  all.departments.combined.shifted.data[[department_name]] <- merged_dept_data # store
}


# Includes NAs for Gemeinden where certain years do not have financial data specific to the department
# This may chnage as we decide which financial data to include
View(all.departments.combined.shifted.data$Gesundheit)
View(all.departments.combined.shifted.data$KultSportFrei)

################################################ COMBINING log(FINANCIAL DATA) AND RAW PREDICTOR DATA ###############

all.departments.combined.data2 <- all.departments.combined.data

for(department_name in names(department.financial.data.shifted)) { # Loop through each department
  
  dept_data <- department.financial.data.shifted[[department_name]] %>%
    select(YEAR, GEMEINDE, STAFF_COSTS, LOG.STAFF.COSTS)
  
  # merge
  merged_dept_data <- left_join(data.predictors.scaled, dept_data, 
                                by = c("YEAR", "GEMEINDE"))
  
  all.departments.combined.data2[[department_name]] <- merged_dept_data # store
}

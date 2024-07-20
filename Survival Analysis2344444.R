# Load required packages
install.packages(c("devtools", "survival", "tidyverse", "survminer"))
library(devtools)
library(survival)
library(tidyverse)
library(survminer)

# Load the dataset
dataset_path <- "C:\\Users\\Edgar\\Documents\\echocardiogram2.csv"
echocardiogram2_data <- read.csv(dataset_path)

# Explore the data
str(echocardiogram2_data)
summary(echocardiogram2_data)


# Select numeric columns
numeric_columns <- sapply(echocardiogram2_data, is.numeric)

# Check for missing values
missing_values <- colSums(is.na(echocardiogram2_data))

# Display missing values
print("Missing Values:")
print(missing_values)

# Replace missing values with 0 (or any other appropriate method)
echocardiogram2_data[is.na(echocardiogram2_data)] <- 0

# Perform correlation analysis
correlation_matrix <- cor(echocardiogram2_data[, numeric_columns, drop = FALSE])

# Visualize the correlation matrix
corrplot::corrplot(correlation_matrix)


install.packages("knitr")

# Load the necessary packages
library(knitr)


# Use kable() to visualize the data
kable(head(echocardiogram2_data))




# Question:
# "What is the probability of survival over time for individuals in different treatment groups in the echocardiogram2 dataset?"

# Our dataset have a variable named 'survival' representing the time to event
# and a variable named 'alive' representing the event status (1 for event, 0 for censored)

# Load the survival package
library(survival)

# Create a Surv object using the Surv() function
surv_object <- Surv(time = echocardiogram2_data$survival, event = echocardiogram2_data$alive)

# Print the Surv object
print(surv_object)

# Analyze the data with survfit()
survfit_object <- survfit(surv_object ~ 1)  # ~1 assumes a single group, adjust if needed

# Display summary of the survfit object
summary(survfit_object)

# Load the necessary libraries
library(survival)
library(survminer)


# Assuming you have a Surv object named 'surv_object' (replace it with your actual Surv object)
# If you don't have a 'surv_object', use the one created earlier

# Generate a Kaplan-Meier curve
survfit_object <- survfit(surv_object ~ 1)  # Assuming a single group


# Display summary of the survfit object
summary(survfit_object)

# Check the structure of 'echocardiogram2_data'
str(echocardiogram2_data)



# Ensure that 'surv_object' is correctly defined
surv_object <- Surv(time = echocardiogram2_data$survival, event = echocardiogram2_data$alive)

# Generate a new Kaplan-Meier curve with stratification
ggsurvplot(survfit(surv_object ~ group, data = echocardiogram2_data), 
           title = "Kaplan-Meier Survival Curve", 
           xlab = "Time", ylab = "Survival Probability",
           pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           xlim = c(0, 200), ylim = c(0, 1),
           risk.table.title = "Number at Risk",
           risk.table.col = "strata",
           risk.table.fontsize = 4,
           ggtheme = theme_minimal()
)




#Generate K-M Curves with the Predictor:




# Ensure that 'surv_object' and 'echocardiogram2_data' are correctly defined
# (replace 'survival' and 'alive' with your actual column names)
surv_object <- Surv(time = echocardiogram2_data$survival, event = echocardiogram2_data$alive)

# Generate a new Kaplan-Meier curve with stratification by 'group'
survfit_object <- survfit(surv_object ~ group, data = echocardiogram2_data)

# Display the strata using summary()
summary(survfit_object)

# Plot the strata using ggsurvplot()
ggsurvplot(survfit_object, data = echocardiogram2_data,
           title = "Kaplan-Meier Survival Curve (Stratified by Group)",
           xlab = "Time", ylab = "Survival Probability",
           pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           xlim = c(0, 200), ylim = c(0, 1),
           risk.table.title = "Number at Risk",
           risk.table.col = "strata",
           risk.table.fontsize = 4,
           ggtheme = theme_minimal()
)

# Plot the overall survival, stratified by 'group'
ggsurvplot(survfit_object, data = echocardiogram2_data, 
           title = "Kaplan-Meier Survival Curve (Overall)",
           xlab = "Time", ylab = "Survival Probability",
           pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           xlim = c(0, 200), ylim = c(0, 1),
           risk.table.title = "Number at Risk",
           risk.table.col = "strata",
           risk.table.fontsize = 4,
           ggtheme = theme_minimal()
)


# Perform Log-Rank Analysis:


# Ensure that 'surv_object' and 'echocardiogram2_data' are correctly defined
# (replace 'survival' and 'alive' with your actual column names)
surv_object <- Surv(time = echocardiogram2_data$survival, event = echocardiogram2_data$alive)

# Fit Kaplan-Meier survival curves
survfit_object <- survfit(surv_object ~ group, data = echocardiogram2_data)

# Perform log-rank test
log_rank_test <- survdiff(surv_object ~ group, data = echocardiogram2_data)
print(log_rank_test)

# Plot Kaplan-Meier curve with log-rank test
ggsurvplot(survfit_object, data = echocardiogram2_data,
           title = "Kaplan-Meier Survival Curve with Log-Rank Test",
           pval = TRUE,              # Display p-value for survival curves
           conf.int = TRUE,          # Display confidence intervals
           risk.table = TRUE,        # Display risk table
           xlim = c(0, 200),         # Set x-axis limits
           ylim = c(0, 1),           # Set y-axis limits
           risk.table.title = "Number at Risk",
           risk.table.col = "strata",
           risk.table.fontsize = 4,
           ggtheme = theme_minimal(),  # Use a minimal theme, adjust as needed
           surv.median.line = "hv",   # Add median survival line
           pval.method = TRUE,         # Use log-rank test for p-value
           pval.size = 4               # Adjust p-value font size
)


# Plot Kaplan-Meier curve without median line
ggsurvplot(survfit_object, data = echocardiogram2_data,
           title = "Kaplan-Meier Survival Curve with Log-Rank Test",
           pval = TRUE,              # Display p-value for survival curves
           conf.int = TRUE,          # Display confidence intervals
           risk.table = TRUE,        # Display risk table
           xlim = c(0, 200),         # Set x-axis limits
           ylim = c(0, 1),           # Set y-axis limits
           risk.table.title = "Number at Risk",
           risk.table.col = "strata",
           risk.table.fontsize = 4,
           ggtheme = theme_minimal(),  # Use a minimal theme, adjust as needed
           pval.method = TRUE,         # Use log-rank test for p-value
           pval.size = 4               # Adjust p-value font size
)


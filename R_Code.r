rm(list = ls())

if(!require(dplyr)) install.packages("dplyr")
if(!require(statmod)) install.packages("statmod")
if(!require(caret)) install.packages("caret")
library(dplyr)
library(statmod)
library(caret)

file_path <- file.choose()

data <- read.csv(file_path, sep = ";")

data$Traffic_Zone <- case_when(
  data$City == "Istanbul" ~ "High_Stress_Zone",
  data$City %in% c("Ankara", "Izmir", "Bursa") ~ "Medium_Density",
  TRUE ~ "Quiet_Zone"
)

cols_to_factor <- c("Driver_Profile", "Vehicle_Class", "Traffic_Zone", "Safety_Package_Level", "City")
data[cols_to_factor] <- lapply(data[cols_to_factor], as.factor)


set.seed(123)
trainIndex <- createDataPartition(data$Claim_Count, p = .8, list = FALSE, times = 1)
dataTrain <- data[trainIndex,]
dataTest  <- data[-trainIndex,]

freq_formula <- Claim_Count ~ Driver_Profile * Vehicle_Class + Traffic_Zone + Safety_Package_Level + offset(log(Exposure))
model_freq <- glm(freq_formula, data = dataTrain, family = poisson(link = "log"))

train_sev <- subset(dataTrain, Claim_Count > 0)
sev_formula <- Claim_Amount ~ Vehicle_Class + Safety_Package_Level 
model_sev <- glm(sev_formula, data = train_sev, family = Gamma(link = "log"))

data$Pred_Frequency <- predict(model_freq, newdata = data, type = "response")
data$Pred_Severity <- predict(model_sev, newdata = data, type = "response")
data$Risk_Premium <- data$Pred_Frequency * data$Pred_Severity


final_columns <- c("Policy_ID", "City", "Safety_Package_Level", 
                   "Driver_Profile", "Vehicle_Class", "Traffic_Zone", 
                   "Exposure", "Claim_Count", "Claim_Amount", "Risk_Premium")

final_dataset <- data[, intersect(final_columns, names(data))]

write.csv(final_dataset, file.choose(new = TRUE), row.names = FALSE)


###############################################################################
# VOL2 ADAS Pricing Paradox — Advanced Actuarial GLM & Interactions
# Pipeline: SQL_Output2.csv -> [THIS SCRIPT] -> R_Output.csv
###############################################################################

rm(list = ls())

# --- Paketler ---
required_pkgs <- c("dplyr", "statmod", "caret", "ggplot2", "jsonlite", "gridExtra", "corrplot")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# --- Dizin Ayarlari ---
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# --- Veri Yukle ---
file_path <- "SQL_Output2.csv"
if (!file.exists(file_path)) {
  file_path <- "ham_data_final.csv" 
}
first_line <- readLines(file_path, n = 1)
if (grepl(";", first_line)) {
  data <- read.csv(file_path, sep = ";")
} else {
  data <- read.csv(file_path, sep = ",")
}

# Exposure eger tam sayiysa (SQL export hatasi) 100'e bolerek 0-1 arasina cek
if (max(data$Exposure, na.rm = TRUE) > 10) {
  data$Exposure <- data$Exposure / 100
}

# SQL_Code tarafindan eklenmeyen Traffic_Zone varsa biz ekleyelim
if (!"Traffic_Zone" %in% names(data)) {
  data$Traffic_Zone <- case_when(
    data$City == "Istanbul" ~ "High_Stress_Zone",
    data$City %in% c("Ankara", "Izmir", "Bursa") ~ "Medium_Density",
    TRUE ~ "Quiet_Zone"
  )
}

cols_to_factor <- c("Driver_Profile", "Vehicle_Class", "Traffic_Zone", "Safety_Package_Level", "City")
for (col in cols_to_factor) {
  if (col %in% names(data)) data[[col]] <- as.factor(data[[col]])
}

###############################################################################
# BOLUM 0: KESIFSEL VERI ANALIZI (EDA)
###############################################################################
cat("\n===== EDA (KESIFSEL VERI ANALIZI) =====\n")
png("outputs/figures/eda_distributions.png", width = 1000, height = 400, res = 120)
par(mfrow = c(1, 2))
if("Claim_Count" %in% names(data)) {
  barplot(table(data$Claim_Count), main = "Frekans Dagilimi", col = "skyblue", border = "white")
}
if("Claim_Amount" %in% names(data)) {
  hist(data$Claim_Amount[data$Claim_Amount > 0], main = "Hasar Tutari Dagilimi (Sifir Haric)", 
       xlab = "Tutar (TL)", col = "lightcoral", border = "white", breaks = 30)
}
dev.off()
cat("  eda_distributions.png kaydedildi.\n")

###############################################################################
# BOLUM 1: TRAIN / TEST SPLIT
###############################################################################
set.seed(123)
trainIndex <- createDataPartition(data$Claim_Count, p = .8, list = FALSE, times = 1)
dataTrain <- data[trainIndex,]
dataTest  <- data[-trainIndex,]
cat(sprintf("\nTrain seti: %d gozlem | Test seti: %d gozlem\n", nrow(dataTrain), nrow(dataTest)))

###############################################################################
# BOLUM 2: FREKANS MODELI (Poisson GLM with Interactions)
###############################################################################
cat("\n===== FREKANS MODELI (Poisson GLM) =====\n")
# Sadece 2 veya daha fazla seviyesi olan faktorleri kullan
valid_vars <- c("Safety_Package_Level")
if ("Driver_Profile" %in% names(data) && length(levels(dataTrain$Driver_Profile)) > 1 &&
    "Vehicle_Class" %in% names(data) && length(levels(dataTrain$Vehicle_Class)) > 1) {
  valid_vars <- c(valid_vars, "Driver_Profile * Vehicle_Class")
} else {
  if ("Driver_Profile" %in% names(data) && length(levels(dataTrain$Driver_Profile)) > 1) valid_vars <- c(valid_vars, "Driver_Profile")
  if ("Vehicle_Class" %in% names(data) && length(levels(dataTrain$Vehicle_Class)) > 1) valid_vars <- c(valid_vars, "Vehicle_Class")
}
if ("Traffic_Zone" %in% names(data) && length(levels(dataTrain$Traffic_Zone)) > 1) {
  valid_vars <- c(valid_vars, "Traffic_Zone")
}

freq_formula_str <- paste("Claim_Count ~", paste(valid_vars, collapse = " + "), "+ offset(log(Exposure))")
freq_formula <- as.formula(freq_formula_str)

model_freq <- glm(freq_formula, data = dataTrain, family = poisson(link = "log"))

freq_summary <- capture.output(summary(model_freq))
writeLines(freq_summary, "outputs/freq_model_summary.txt")

###############################################################################
# BOLUM 3: SIDDET MODELI (Gamma GLM)
###############################################################################
cat("\n===== SIDDET MODELI (Gamma GLM) =====\n")
train_sev <- subset(dataTrain, Claim_Count > 0 & Claim_Amount > 0)

sev_vars <- c("Safety_Package_Level")
if ("Vehicle_Class" %in% names(data) && length(levels(train_sev$Vehicle_Class)) > 1) {
  sev_vars <- c(sev_vars, "Vehicle_Class")
}
if ("Driver_Profile" %in% names(data) && length(levels(train_sev$Driver_Profile)) > 1) {
  sev_vars <- c(sev_vars, "Driver_Profile")
}

sev_formula_str <- paste("Claim_Amount ~", paste(sev_vars, collapse = " + "))
sev_formula <- as.formula(sev_formula_str)

model_sev <- glm(sev_formula, data = train_sev, family = Gamma(link = "log"))

sev_summary <- capture.output(summary(model_sev))
writeLines(sev_summary, "outputs/sev_model_summary.txt")

###############################################################################
# BOLUM 4: RMSE VE GINI (PERFORMANS TESTI)
###############################################################################
cat("\n===== TEST SETI PERFORMANSI =====\n")
dataTest$Pred_Freq <- predict(model_freq, newdata = dataTest, type = "response")
dataTest$Pred_Sev  <- predict(model_sev, newdata = dataTest, type = "response")
dataTest$Pred_Risk_Premium <- dataTest$Pred_Freq * dataTest$Pred_Sev

rmse_freq <- sqrt(mean((dataTest$Claim_Count - dataTest$Pred_Freq)^2))
cat(sprintf("Frekans Modeli RMSE: %.4f\n", rmse_freq))

# Gini Index / Lorenz Curve Hesaplamasi (Risk Primine gore siralama)
dataTest <- dataTest[order(dataTest$Pred_Risk_Premium), ]
dataTest$Cum_Premium <- cumsum(as.numeric(dataTest$Pred_Risk_Premium)) / sum(as.numeric(dataTest$Pred_Risk_Premium))
dataTest$Cum_Claims <- cumsum(as.numeric(dataTest$Claim_Amount)) / sum(as.numeric(dataTest$Claim_Amount))
dataTest$Cum_Pop <- seq(1, nrow(dataTest)) / nrow(dataTest)

gini_index <- 1 - 2 * sum((dataTest$Cum_Claims[-1] + dataTest$Cum_Claims[-nrow(dataTest)]) / 2 * diff(dataTest$Cum_Pop))
cat(sprintf("Gini Index (Test Seti): %.3f\n", gini_index))

png("outputs/figures/gini_lift_chart.png", width = 600, height = 600, res = 120)
plot(dataTest$Cum_Pop, dataTest$Cum_Claims, type = "l", col = "blue", lwd = 2,
     xlab = "Kumulatif Nufus (Risk Primine Gore Sirali)", ylab = "Kumulatif Gercek Hasar",
     main = paste("Lorenz Eğrisi (Gini:", round(gini_index, 3), ")"))
abline(0, 1, col = "red", lty = 2)
dev.off()
cat("  gini_lift_chart.png kaydedildi.\n")

###############################################################################
# BOLUM 5: TUM VERIDE TAHMIN VE PARADOKS ANALIZI
###############################################################################
data$Pred_Frequency <- predict(model_freq, newdata = data, type = "response")
data$Pred_Severity <- predict(model_sev, newdata = data, type = "response")
data$Risk_Premium <- data$Pred_Frequency * data$Pred_Severity

paradox <- data %>%
  group_by(Safety_Package_Level) %>%
  summarise(
    Police_Sayisi   = n(),
    Ort_Frekans     = mean(Pred_Frequency),
    SE_Frekans      = sd(Pred_Frequency) / sqrt(n()),
    Ort_Siddet      = mean(Pred_Severity),
    SE_Siddet       = sd(Pred_Severity) / sqrt(n()),
    Ort_Risk_Primi  = mean(Risk_Premium),
    SE_Risk_Primi   = sd(Risk_Premium) / sqrt(n()),
    .groups = "drop"
  )

write.csv(as.data.frame(paradox), "outputs/paradox_summary.csv", row.names = FALSE)

# GGPLOT ile Paradoks Grafikleri
plot_freq <- ggplot(paradox, aes(x = paste("ADAS", Safety_Package_Level), y = Ort_Frekans, fill = Safety_Package_Level)) +
  geom_col() +
  geom_errorbar(aes(ymin = Ort_Frekans - 1.96 * SE_Frekans, ymax = Ort_Frekans + 1.96 * SE_Frekans), width = 0.2) +
  scale_fill_manual(values = c("#2ecc71", "#f39c12", "#e74c3c")) +
  labs(title = "Frekans", x = NULL, y = "Ort. Tahmin Edilen Frekans") +
  theme_minimal() + theme(legend.position = "none")

plot_sev <- ggplot(paradox, aes(x = paste("ADAS", Safety_Package_Level), y = Ort_Siddet, fill = Safety_Package_Level)) +
  geom_col() +
  geom_errorbar(aes(ymin = Ort_Siddet - 1.96 * SE_Siddet, ymax = Ort_Siddet + 1.96 * SE_Siddet), width = 0.2) +
  scale_fill_manual(values = c("#2ecc71", "#f39c12", "#e74c3c")) +
  labs(title = "Siddet", x = NULL, y = "Ort. Tahmin Edilen Siddet (TL)") +
  theme_minimal() + theme(legend.position = "none")

plot_prem <- ggplot(paradox, aes(x = paste("ADAS", Safety_Package_Level), y = Ort_Risk_Primi, fill = Safety_Package_Level)) +
  geom_col() +
  geom_errorbar(aes(ymin = Ort_Risk_Primi - 1.96 * SE_Risk_Primi, ymax = Ort_Risk_Primi + 1.96 * SE_Risk_Primi), width = 0.2) +
  scale_fill_manual(values = c("#2ecc71", "#f39c12", "#e74c3c")) +
  labs(title = "Risk Primi", x = NULL, y = "Ort. Risk Primi (TL)") +
  theme_minimal() + theme(legend.position = "none")

png("outputs/figures/paradox_main.png", width = 1200, height = 450, res = 120)
gridExtra::grid.arrange(plot_freq, plot_sev, plot_prem, ncol = 3, top = "ADAS Paradoksu: Vol 2 (+/-%95 Guven Araligi)")
dev.off()

###############################################################################
# BOLUM 6: FINAL CIKTI YAZDIRMA (Power BI Icin)
###############################################################################
final_columns <- c("Policy_ID", "City", "Safety_Package_Level", 
                   "Driver_Profile", "Vehicle_Class", "Traffic_Zone", 
                   "Exposure", "Claim_Count", "Claim_Amount", "Pred_Frequency", "Pred_Severity", "Risk_Premium")

final_dataset <- data[, intersect(final_columns, names(data))]

# file.choose() yerine sabit isimle yazdiriyoruz. 
# Power BI bu sabit dosyayi okuyacak.
write.csv(final_dataset, "R_Output.csv", row.names = FALSE)

results_json <- list(
  metrics = list(rmse_freq = rmse_freq, gini_index = gini_index),
  paradox = as.data.frame(paradox)
)
writeLines(jsonlite::toJSON(results_json, pretty = TRUE, auto_unbox = TRUE), "outputs/results.json")

cat("\n===== ANALIZ TAMAMLANDI =====\n")
cat("Output dosyasi 'R_Output.csv' olarak kaydedildi.\n")


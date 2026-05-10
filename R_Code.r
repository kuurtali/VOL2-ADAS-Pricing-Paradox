## ---- VOL2: ADAS Pricing Paradox — Advanced Actuarial GLM & Interactions ----
## Pipeline: SQL_Output2.csv -> [THIS SCRIPT] -> R_Output.csv

rm(list = ls())

# Set working directory to script location (works when run from any folder)
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
setwd(script_dir)
cat("Calisma dizini:", getwd(), "\n")

required_pkgs <- c("dplyr", "statmod", "caret", "ggplot2",
                   "jsonlite", "gridExtra", "corrplot", "scales")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

## ---- 1: Data Load ----

file_path <- "SQL_Output2.csv"
if (!file.exists(file_path)) file_path <- "ham_data_final.csv"

first_line <- readLines(file_path, n = 1)
data <- if (grepl(";", first_line)) read.csv(file_path, sep = ";") else read.csv(file_path, sep = ",")

if (max(data$Exposure, na.rm = TRUE) > 10) data$Exposure <- data$Exposure / 100

if (!"Traffic_Zone" %in% names(data)) {
  data$Traffic_Zone <- dplyr::case_when(
    data$City == "Istanbul"                          ~ "High_Stress_Zone",
    data$City %in% c("Ankara", "Izmir", "Bursa")    ~ "Medium_Density",
    TRUE                                             ~ "Quiet_Zone"
  )
}

cols_to_factor <- c("Driver_Profile", "Vehicle_Class", "Traffic_Zone", "Safety_Package_Level", "City")
for (col in cols_to_factor) {
  if (col %in% names(data)) data[[col]] <- as.factor(data[[col]])
}

## ---- 2: Exploratory Data Analysis (EDA) ----

cat("\n===== EDA =====\n")

num_vars <- data %>% dplyr::select(where(is.numeric), -dplyr::any_of("Policy_ID"))
cor_matrix <- cor(num_vars, use = "complete.obs")
png("outputs/figures/eda_correlation.png", width = 800, height = 800, res = 120)
corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                   tl.col = "black", tl.srt = 45, addCoef.col = "black",
                   number.cex = 0.7, main = "Korelasyon Matrisi", mar = c(0, 0, 2, 0))
dev.off()
cat("  eda_correlation.png kaydedildi.\n")

p_eda1 <- ggplot(data, aes(x = factor(Claim_Count))) +
  geom_bar(fill = "#3498db", color = "white") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Hasar Frekans Dagilimi", x = "Hasar Sayisi", y = "Police Sayisi") +
  theme_minimal(base_size = 13)

p_eda2 <- ggplot(data[data$Claim_Amount > 0, ], aes(x = Claim_Amount)) +
  geom_histogram(fill = "#e74c3c", color = "white", bins = 30) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Hasar Tutari Dagilimi (> 0)", x = "Tutar (TL)", y = "Frekans") +
  theme_minimal(base_size = 13)

png("outputs/figures/eda_distributions.png", width = 1000, height = 450, res = 120)
gridExtra::grid.arrange(p_eda1, p_eda2, ncol = 2)
dev.off()
cat("  eda_distributions.png kaydedildi.\n")

## ---- 3: Train / Test Split ----

set.seed(123)
trainIndex <- caret::createDataPartition(data$Claim_Count, p = .8, list = FALSE, times = 1)
dataTrain  <- data[ trainIndex, ]
dataTest   <- data[-trainIndex, ]
cat(sprintf("\nTrain seti: %d gozlem | Test seti: %d gozlem\n", nrow(dataTrain), nrow(dataTest)))

## ---- 4: Frequency Model (Poisson GLM with Interactions) ----

cat("\n===== FREKANS MODELI (Poisson GLM) =====\n")

valid_vars <- c("Safety_Package_Level")
if ("Driver_Profile" %in% names(data) && length(levels(dataTrain$Driver_Profile)) > 1 &&
    "Vehicle_Class"  %in% names(data) && length(levels(dataTrain$Vehicle_Class))  > 1) {
  valid_vars <- c(valid_vars, "Driver_Profile * Vehicle_Class")
} else {
  if ("Driver_Profile" %in% names(data) && length(levels(dataTrain$Driver_Profile)) > 1)
    valid_vars <- c(valid_vars, "Driver_Profile")
  if ("Vehicle_Class" %in% names(data) && length(levels(dataTrain$Vehicle_Class)) > 1)
    valid_vars <- c(valid_vars, "Vehicle_Class")
}
if ("Traffic_Zone" %in% names(data) && length(levels(dataTrain$Traffic_Zone)) > 1)
  valid_vars <- c(valid_vars, "Traffic_Zone")

freq_formula <- as.formula(paste("Claim_Count ~", paste(valid_vars, collapse = " + "), "+ offset(log(Exposure))"))
model_freq   <- glm(freq_formula, data = dataTrain, family = poisson(link = "log"))

writeLines(capture.output(summary(model_freq)), "outputs/freq_model_summary.txt")

## ---- 5: Severity Model (Gamma GLM) ----

cat("\n===== SIDDET MODELI (Gamma GLM) =====\n")

train_sev <- subset(dataTrain, Claim_Count > 0 & Claim_Amount > 0)

sev_vars <- c("Safety_Package_Level")
if ("Vehicle_Class"  %in% names(data) && length(levels(train_sev$Vehicle_Class))  > 1)
  sev_vars <- c(sev_vars, "Vehicle_Class")
if ("Driver_Profile" %in% names(data) && length(levels(train_sev$Driver_Profile)) > 1)
  sev_vars <- c(sev_vars, "Driver_Profile")

sev_formula <- as.formula(paste("Claim_Amount ~", paste(sev_vars, collapse = " + ")))
model_sev   <- glm(sev_formula, data = train_sev, family = Gamma(link = "log"))

writeLines(capture.output(summary(model_sev)), "outputs/sev_model_summary.txt")

## ---- 6: Model Performance — RMSE & Gini Index ----

cat("\n===== TEST SETI PERFORMANSI =====\n")

dataTest$Pred_Freq         <- predict(model_freq, newdata = dataTest, type = "response")
dataTest$Pred_Sev          <- predict(model_sev,  newdata = dataTest, type = "response")
dataTest$Pred_Risk_Premium <- dataTest$Pred_Freq * dataTest$Pred_Sev

rmse_freq <- sqrt(mean((dataTest$Claim_Count - dataTest$Pred_Freq)^2))
cat(sprintf("Frekans Modeli RMSE: %.4f\n", rmse_freq))

dataTest <- dataTest[order(dataTest$Pred_Risk_Premium), ]
dataTest$Cum_Premium <- cumsum(as.numeric(dataTest$Pred_Risk_Premium)) / sum(as.numeric(dataTest$Pred_Risk_Premium))
dataTest$Cum_Claims  <- cumsum(as.numeric(dataTest$Claim_Amount))      / sum(as.numeric(dataTest$Claim_Amount))
dataTest$Cum_Pop     <- seq(1, nrow(dataTest)) / nrow(dataTest)

gini_index <- 1 - 2 * sum(
  (dataTest$Cum_Claims[-1] + dataTest$Cum_Claims[-nrow(dataTest)]) / 2 * diff(dataTest$Cum_Pop)
)
cat(sprintf("Gini Index (Test Seti): %.3f\n", gini_index))

p_gini <- ggplot(dataTest, aes(x = Cum_Pop, y = Cum_Claims)) +
  geom_line(color = "#2980b9", linewidth = 1.2) +
  geom_abline(intercept = 0, slope = 1, color = "#e74c3c", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 0.25, y = 0.75,
           label = paste0("Gini = ", round(gini_index, 3)),
           size = 5, color = "#2c3e50", fontface = "bold") +
  labs(title = "Lorenz Egrisi — Model Ayirt Edicilik Gucu",
       x = "Kumulatif Nufus (Risk Primine Gore)",
       y = "Kumulatif Gercek Hasar") +
  theme_minimal(base_size = 13)

png("outputs/figures/gini_lorenz.png", width = 700, height = 600, res = 120)
print(p_gini)
dev.off()
cat("  gini_lorenz.png kaydedildi.\n")

dataTest$Decile <- cut(dataTest$Pred_Risk_Premium,
                       breaks = quantile(dataTest$Pred_Risk_Premium, probs = seq(0, 1, 0.1)),
                       include.lowest = TRUE, labels = 1:10)

lift_data <- dataTest %>%
  group_by(Decile) %>%
  summarise(Avg_Predicted = mean(Pred_Risk_Premium),
            Avg_Actual    = mean(Claim_Amount),
            .groups = "drop")

p_lift <- ggplot(lift_data, aes(x = Decile)) +
  geom_col(aes(y = Avg_Predicted, fill = "Tahmin"), position = "dodge", alpha = 0.85) +
  geom_col(aes(y = Avg_Actual,    fill = "Gercek"),  position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("Tahmin" = "#3498db", "Gercek" = "#e74c3c")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Lift Chart — Decile Bazli Model Performansi",
       x = "Decile (Dusuk Risk  →  Yuksek Risk)",
       y = "Ortalama Hasar (TL)", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

png("outputs/figures/lift_chart.png", width = 800, height = 500, res = 120)
print(p_lift)
dev.off()
cat("  lift_chart.png kaydedildi.\n")

## ---- 7: Full-Data Predictions & Paradox Analysis ----

data$Pred_Frequency <- predict(model_freq, newdata = data, type = "response")
data$Pred_Severity  <- predict(model_sev,  newdata = data, type = "response")
data$Risk_Premium   <- data$Pred_Frequency * data$Pred_Severity

paradox <- data %>%
  group_by(Safety_Package_Level) %>%
  summarise(
    Police_Sayisi  = n(),
    Ort_Frekans    = mean(Pred_Frequency),
    SE_Frekans     = sd(Pred_Frequency)  / sqrt(n()),
    Ort_Siddet     = mean(Pred_Severity),
    SE_Siddet      = sd(Pred_Severity)   / sqrt(n()),
    Ort_Risk_Primi = mean(Risk_Premium),
    SE_Risk_Primi  = sd(Risk_Premium)    / sqrt(n()),
    .groups = "drop"
  )

write.csv(as.data.frame(paradox), "outputs/paradox_summary.csv", row.names = FALSE)

adas_colors <- c("0" = "#e74c3c", "1" = "#f39c12", "2" = "#2ecc71")

plot_freq <- ggplot(paradox, aes(x = paste("ADAS", Safety_Package_Level),
                                 y = Ort_Frekans, fill = Safety_Package_Level)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = Ort_Frekans - 1.96 * SE_Frekans,
                    ymax = Ort_Frekans + 1.96 * SE_Frekans), width = 0.2) +
  scale_fill_manual(values = adas_colors) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  labs(title = "Hasar Frekansi (+/-%95 GA)", x = NULL,
       y = "Ort. Tahmin Edilen Frekans") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")

plot_sev <- ggplot(paradox, aes(x = paste("ADAS", Safety_Package_Level),
                                y = Ort_Siddet, fill = Safety_Package_Level)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = Ort_Siddet - 1.96 * SE_Siddet,
                    ymax = Ort_Siddet + 1.96 * SE_Siddet), width = 0.2) +
  scale_fill_manual(values = adas_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Hasar Siddeti (+/-%95 GA)", x = NULL,
       y = "Ort. Tahmin Edilen Siddet (TL)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")

plot_prem <- ggplot(paradox, aes(x = paste("ADAS", Safety_Package_Level),
                                 y = Ort_Risk_Primi, fill = Safety_Package_Level)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = Ort_Risk_Primi - 1.96 * SE_Risk_Primi,
                    ymax = Ort_Risk_Primi + 1.96 * SE_Risk_Primi), width = 0.2) +
  scale_fill_manual(values = adas_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Risk Primi Net Etki (+/-%95 GA)", x = NULL,
       y = "Ort. Risk Primi (TL)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")

png("outputs/figures/paradox_main.png", width = 1200, height = 450, res = 120)
gridExtra::grid.arrange(plot_freq, plot_sev, plot_prem, ncol = 3,
                        top = "ADAS Fiyatlama Paradoksu: Vol 2 (+/-%95 Guven Araligi)")
dev.off()
cat("  paradox_main.png kaydedildi.\n")

segment_city <- data %>%
  group_by(City, Safety_Package_Level) %>%
  summarise(Ort_Risk_Primi = mean(Risk_Premium), .groups = "drop")

p_city <- ggplot(segment_city, aes(x = City, y = Ort_Risk_Primi, fill = Safety_Package_Level)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = adas_colors,
                    labels = paste("ADAS", levels(data$Safety_Package_Level))) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Sehir Bazinda ADAS Risk Primi",
       x = "Sehir", y = "Ort. Risk Primi (TL)", fill = "ADAS Seviyesi") +
  theme_minimal(base_size = 13)

png("outputs/figures/paradox_city.png", width = 1000, height = 500, res = 120)
print(p_city)
dev.off()
cat("  paradox_city.png kaydedildi.\n")

## ---- 8: Export for Power BI ----

final_columns <- c("Policy_ID", "City", "Safety_Package_Level",
                   "Driver_Profile", "Vehicle_Class", "Traffic_Zone",
                   "Exposure", "Claim_Count", "Claim_Amount",
                   "Pred_Frequency", "Pred_Severity", "Risk_Premium")

write.csv(data[, intersect(final_columns, names(data))], "R_Output.csv", row.names = FALSE)

results_json <- list(
  metrics = list(rmse_freq = rmse_freq, gini_index = gini_index),
  paradox  = as.data.frame(paradox)
)
writeLines(jsonlite::toJSON(results_json, pretty = TRUE, auto_unbox = TRUE), "outputs/results.json")

cat("\n===== ANALIZ TAMAMLANDI =====\n")
cat("Output dosyasi 'R_Output.csv' olarak kaydedildi.\n")

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)

# ==========================================
# 1. REAL GLM COEFFICIENTS (from R_Code.r output)
# ==========================================
# Frequency Model (Poisson, log link) — 22 coefficients incl. interactions
freq_intercept <- -2.006584
freq_coefs <- list(
  Safety_Package_Level1 = -0.119342,
  Safety_Package_Level2 = -0.356726,
  Driver_ProfileRisky_History       = 0.171814,
  Driver_ProfileSenior_Driver       = 0.272100,
  Driver_ProfileYoung_Female        = 0.534868,
  Driver_ProfileYoung_Male_HighRisk = 0.598738,
  Driver_ProfileExperienced_Safe    = 0,  # baseline
  Vehicle_ClassLuxury_Comfort   = 0.027866,
  Vehicle_ClassPerformance_Car  = 0.270656,
  Vehicle_ClassStandard_Economy = 0.020300,
  Vehicle_ClassCompact_Urban    = 0,  # baseline
  # Interaction terms (Driver x Vehicle)
  "Risky_History:Luxury_Comfort"       = 0.100152,
  "Senior_Driver:Luxury_Comfort"       = 0.006039,
  "Young_Female:Luxury_Comfort"        = -0.092630,
  "Young_Male_HighRisk:Luxury_Comfort" = 0.140751,
  "Risky_History:Performance_Car"       = 0.058887,
  "Senior_Driver:Performance_Car"       = 0.083870,
  "Young_Female:Performance_Car"        = -0.050327,
  "Young_Male_HighRisk:Performance_Car" = 0.095154,
  "Risky_History:Standard_Economy"       = 0.001688,
  "Senior_Driver:Standard_Economy"       = 0.030690,
  "Young_Female:Standard_Economy"        = 0.010795,
  "Young_Male_HighRisk:Standard_Economy" = 0.039310
)

# Severity Model (Gamma, log link)
sev_intercept <- 14.72397
sev_coefs <- list(
  Safety_Package_Level1 = 0.11294,
  Safety_Package_Level2 = 0.39551,
  Vehicle_ClassLuxury_Comfort   = 0.64167,
  Vehicle_ClassPerformance_Car  = -0.01083,
  Vehicle_ClassStandard_Economy = -0.36458,
  Vehicle_ClassCompact_Urban    = 0,
  Driver_ProfileRisky_History       = 0.04048,
  Driver_ProfileSenior_Driver       = 0.02104,
  Driver_ProfileYoung_Female        = 0.02848,
  Driver_ProfileYoung_Male_HighRisk = -0.01138,
  Driver_ProfileExperienced_Safe    = 0
)

# Model Performance Metrics (from results.json)
RMSE_FREQ  <- 0.2762
GINI_INDEX <- 0.3247

# ==========================================
# 2. CALCULATION FUNCTION
# ==========================================
calculate_premium <- function(driver, vehicle, adas) {
  results <- data.frame(ADAS = c("0", "1", "2"), Freq = 0, Sev = 0, Premium = 0)
  
  for (i in 1:nrow(results)) {
    a <- results$ADAS[i]
    
    # --- Frequency (Poisson, log link) ---
    lp_freq <- freq_intercept
    
    # ADAS
    adas_key <- paste0("Safety_Package_Level", a)
    if (adas_key %in% names(freq_coefs)) lp_freq <- lp_freq + freq_coefs[[adas_key]]
    
    # Driver Profile
    drv_key <- paste0("Driver_Profile", driver)
    if (drv_key %in% names(freq_coefs)) lp_freq <- lp_freq + freq_coefs[[drv_key]]
    
    # Vehicle Class
    veh_key <- paste0("Vehicle_Class", vehicle)
    if (veh_key %in% names(freq_coefs)) lp_freq <- lp_freq + freq_coefs[[veh_key]]
    
    # Interaction: Driver x Vehicle
    int_key <- paste0(driver, ":", vehicle)
    if (int_key %in% names(freq_coefs)) lp_freq <- lp_freq + freq_coefs[[int_key]]
    
    freq_val <- exp(lp_freq)
    
    # --- Severity (Gamma, log link) ---
    lp_sev <- sev_intercept
    
    adas_s <- paste0("Safety_Package_Level", a)
    if (adas_s %in% names(sev_coefs)) lp_sev <- lp_sev + sev_coefs[[adas_s]]
    
    veh_s <- paste0("Vehicle_Class", vehicle)
    if (veh_s %in% names(sev_coefs)) lp_sev <- lp_sev + sev_coefs[[veh_s]]
    
    drv_s <- paste0("Driver_Profile", driver)
    if (drv_s %in% names(sev_coefs)) lp_sev <- lp_sev + sev_coefs[[drv_s]]
    
    sev_val <- exp(lp_sev)
    
    results$Freq[i]    <- freq_val
    results$Sev[i]     <- sev_val
    results$Premium[i] <- freq_val * sev_val
  }
  return(results)
}

# ==========================================
# 3. UI
# ==========================================
ui <- page_navbar(
  title = "ADAS Fiyatlama Paradoksu (Vol 2) — Gercek GLM",
  theme = bs_theme(version = 5, bootswatch = "cyborg", primary = "#ff416c"),
  
  nav_panel("Canli Fiyatlama",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        h4("Police Girdileri"),
        hr(),
        selectInput("driver", "Surucu Profili:", choices = c(
          "Deneyimli Guvenli" = "Experienced_Safe",
          "Kıdemli Surucu" = "Senior_Driver",
          "Riskli Gecmis" = "Risky_History",
          "Genc Kadin" = "Young_Female",
          "Genc Erkek (Yuksek Risk)" = "Young_Male_HighRisk"
        ), selected = "Risky_History"),
        
        selectInput("vehicle", "Arac Sinifi:", choices = c(
          "Kompakt / Sehir Araci" = "Compact_Urban",
          "Standart / Ekonomi" = "Standard_Economy",
          "Luks / Konfor" = "Luxury_Comfort",
          "Performans / Spor" = "Performance_Car"
        ), selected = "Standard_Economy"),
        
        hr(),
        h5("Model Bilgisi"),
        p(style = "color:#888; font-size:0.85rem;",
          paste0("Frekans: Poisson GLM (22 katsayi)\n",
                 "Siddet: Gamma GLM (11 katsayi)\n",
                 "Interaction: Driver × Vehicle\n",
                 "RMSE: ", RMSE_FREQ, " | Gini: ", GINI_INDEX)),
        hr(),
        actionButton("calc_btn", "Hesapla", class = "btn-primary w-100")
      ),
      
      card(
        card_header("Risk Primi Sonuc Ekrani (Gercek GLM Katsayilari)", class = "bg-primary text-white"),
        card_body(
          uiOutput("price_display"),
          hr(),
          uiOutput("info_boxes"),
          hr(),
          plotOutput("waterfall_plot", height = "380px")
        )
      )
    )
  ),
  
  nav_panel("Paradoks Ozeti",
    card(
      card_header("ADAS Fiyatlama Paradoksu — Gercek Veriden"),
      card_body(
        markdown(paste0("
        **200.000 sentetik police** uzerinde egitilmis **Poisson-Gamma Two-Part GLM** sonuclari:
        
        | ADAS Seviyesi | Police Sayisi | Ort. Frekans | Ort. Siddet (TL) | Ort. Risk Primi (TL) |
        |:---:|:---:|:---:|:---:|:---:|
        | ADAS 0 (Yok) | 90,251 | 0.0875 | 2,499,554 | 220,080 |
        | ADAS 1 (Temel) | 69,723 | 0.0776 (-11.3%) | 2,794,633 (+11.8%) | 217,861 (-1.0%) |
        | ADAS 2 (Ileri) | 40,025 | 0.0613 (-30.0%) | 3,707,888 (+48.3%) | 228,551 (+3.8%) |
        
        **Paradoks:** ADAS 2, kaza frekansini **%30 azaltmasina** ragmen, onarim maliyetini **%48 artirdigi** icin toplam risk primi **yükseliyor**.
        
        **Model Performansi:**
        - Frekans RMSE: ", RMSE_FREQ, "
        - Gini Index: ", GINI_INDEX, " (+14% iyilesme Vol 1'e gore)
        "))
      )
    )
  ),
  
  nav_panel("Model Detaylari",
    card(
      card_header("Poisson-Gamma GLM Altyapisi"),
      card_body(
        markdown("
        Bu fiyatlama motoru **gercek GLM katsayilarini** kullanir (R_Code.r ciktisi).
        
        **Frekans Modeli (Poisson, log link):**
        - 22 katsayi (intercept + 5 driver + 3 vehicle + 3 ADAS + 12 interaction)
        - Interaction terms: Driver_Profile × Vehicle_Class
        - Offset: log(Exposure)
        - AIC: 89,777
        
        **Siddet Modeli (Gamma, log link):**
        - 11 katsayi
        - Dispersion: 0.715
        - AIC: 377,952
        
        **ADAS Fiyatlama Paradoksu:**
        - ADAS, kaza frekansini dusurur (negatif katsayi: -0.119, -0.357)
        - Ama sensor/radar maliyetleri nedeniyle hasar siddetini arttirir (+0.113, +0.396)
        - Net etki: ADAS 1 hafif dusus, ADAS 2 net ARTIS → **Paradoks**
        ")
      )
    )
  )
)

# ==========================================
# 4. SERVER
# ==========================================
server <- function(input, output, session) {
  
  observeEvent(input$calc_btn, ignoreNULL = FALSE, {
    
    res <- calculate_premium(
      driver  = input$driver,
      vehicle = input$vehicle,
      adas    = "compare"
    )
    
    base_prem <- res$Premium[1]   # ADAS 0
    adas1_prem <- res$Premium[2]  # ADAS 1
    adas2_prem <- res$Premium[3]  # ADAS 2
    
    output$price_display <- renderUI({
      HTML(paste0(
        "<div style='text-align:center; padding:15px;'>",
        "<h3 style='color:#888;'>Secili Profil: ", input$driver, " + ", input$vehicle, "</h3>",
        "<table style='width:100%; text-align:center; margin:10px 0;'>",
        "<tr>",
        "<td style='padding:15px;'><h4 style='color:#aaa;'>ADAS 0</h4>",
        "<h2 style='color:#ccc;'>", prettyNum(round(base_prem), big.mark="."), " TL</h2></td>",
        "<td style='padding:15px;'><h4 style='color:#f39c12;'>ADAS 1</h4>",
        "<h2 style='color:#f39c12;'>", prettyNum(round(adas1_prem), big.mark="."), " TL</h2>",
        "<p style='color:", ifelse(adas1_prem < base_prem, "#00cc66", "#ff4444"), ";'>",
        ifelse(adas1_prem < base_prem, "⬇", "⬆"), " %", 
        round(abs(adas1_prem/base_prem - 1)*100, 1), "</p></td>",
        "<td style='padding:15px;'><h4 style='color:#2ecc71;'>ADAS 2</h4>",
        "<h2 style='color:", ifelse(adas2_prem > base_prem, "#ff4444", "#00cc66"), ";'>", 
        prettyNum(round(adas2_prem), big.mark="."), " TL</h2>",
        "<p style='color:", ifelse(adas2_prem > base_prem, "#ff4444", "#00cc66"), ";'>",
        ifelse(adas2_prem > base_prem, "⬆", "⬇"), " %", 
        round(abs(adas2_prem/base_prem - 1)*100, 1), " — PARADOKS!</p></td>",
        "</tr></table></div>"
      ))
    })
    
    output$info_boxes <- renderUI({
      HTML(paste0(
        "<div style='display:flex; gap:10px; justify-content:center;'>",
        "<div style='flex:1; text-align:center; background:#1a1a2e; padding:12px; border-radius:8px;'>",
        "<h5 style='color:#888;'>Frekans (ADAS 0→2)</h5>",
        "<p style='color:#00cc66; font-size:1.3rem;'>", 
        round(res$Freq[1], 4), " → ", round(res$Freq[3], 4), "</p>",
        "<p style='color:#00cc66;'>⬇ %", round((1 - res$Freq[3]/res$Freq[1])*100, 1), " azalma</p></div>",
        
        "<div style='flex:1; text-align:center; background:#1a1a2e; padding:12px; border-radius:8px;'>",
        "<h5 style='color:#888;'>Siddet (ADAS 0→2)</h5>",
        "<p style='color:#ff4444; font-size:1.3rem;'>", 
        prettyNum(round(res$Sev[1]), big.mark="."), " → ", prettyNum(round(res$Sev[3]), big.mark="."), " TL</p>",
        "<p style='color:#ff4444;'>⬆ %", round((res$Sev[3]/res$Sev[1] - 1)*100, 1), " artis</p></div>",
        
        "<div style='flex:1; text-align:center; background:#1a1a2e; padding:12px; border-radius:8px; border:1px solid #ff416c;'>",
        "<h5 style='color:#888;'>Net Risk Primi</h5>",
        "<p style='color:#ff416c; font-size:1.3rem;'>", 
        prettyNum(round(base_prem), big.mark="."), " → ", prettyNum(round(adas2_prem), big.mark="."), " TL</p>",
        "<p style='color:#ff416c;'>", ifelse(adas2_prem > base_prem, "⬆ PARADOKS", "⬇ Azaldi"), "</p></div>",
        "</div>"
      ))
    })
    
    # Waterfall Chart
    output$waterfall_plot <- renderPlot({
      freq_effect <- (res$Freq[3] - res$Freq[1]) * res$Sev[1]
      sev_effect  <- res$Freq[3] * (res$Sev[3] - res$Sev[1])
      
      wf <- data.frame(
        Category = factor(c("ADAS 0 (Baz Prim)", "Frekans Etkisi\n(Kaza Azalisi)", 
                           "Siddet Etkisi\n(Sensor Maliyeti)", "ADAS 2 Net Prim"),
                         levels = c("ADAS 0 (Baz Prim)", "Frekans Etkisi\n(Kaza Azalisi)",
                                   "Siddet Etkisi\n(Sensor Maliyeti)", "ADAS 2 Net Prim")),
        Value = c(base_prem, freq_effect, sev_effect, adas2_prem)
      )
      
      wf$End   <- cumsum(wf$Value)
      wf$Start <- c(0, wf$End[1:2], 0)
      wf$End[4] <- adas2_prem; wf$Start[4] <- 0
      wf$Type <- c("Net", ifelse(freq_effect < 0, "Decrease", "Increase"),
                   ifelse(sev_effect > 0, "Increase", "Decrease"), "Net")
      
      ggplot(wf, aes(x = Category, fill = Type)) +
        geom_rect(aes(xmin = as.numeric(Category) - 0.4, xmax = as.numeric(Category) + 0.4,
                      ymin = Start, ymax = End)) +
        scale_fill_manual(values = c("Decrease" = "#00cc66", "Increase" = "#ff4444", "Net" = "#cccccc")) +
        geom_text(aes(y = pmax(Start, End) + max(adas2_prem, base_prem) * 0.04,
                      label = paste0(ifelse(Value > 0 & Type != "Net", "+", ""),
                                    prettyNum(round(Value), big.mark=".", decimal.mark=","), " TL")),
                  color = "white", size = 5, fontface = "bold") +
        theme_minimal() +
        labs(x = NULL, y = "Saf Risk Primi (TL)",
             title = "ADAS Fiyatlama Paradoksu — Waterfall Analizi") +
        scale_y_continuous(labels = comma) +
        theme(
          plot.background = element_rect(fill = "#111111", color = NA),
          panel.background = element_rect(fill = "#111111", color = NA),
          panel.grid.major = element_line(color = "#333333"),
          panel.grid.minor = element_blank(),
          text = element_text(color = "#eeeeee"),
          axis.text = element_text(color = "#eeeeee", size = 12),
          axis.title = element_text(color = "#eeeeee", size = 14),
          plot.title = element_text(color = "#ff416c", size = 16, face = "bold"),
          legend.position = "none"
        )
    })
  })
}

shinyApp(ui, server)

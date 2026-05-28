library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# Simulated base parameters based on the R_Code.r models
# Poisson Freq base ~ 0.05
# Gamma Sev base ~ 15000 TL

ui <- page_navbar(
  title = "ADAS Fiyatlama Motoru (Vol 2)",
  theme = bs_theme(version = 5, bootswatch = "cyborg", primary = "#ff416c"),
  
  nav_panel("Canlı Fiyatlama",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Poliçe Girdileri"),
        hr(),
        selectInput("adas_level", "ADAS Seviyesi (Otonomi):", 
                    choices = c("Seviye 0 (Yok)" = "0", "Seviye 1 (Temel)" = "1", "Seviye 2 (Gelişmiş)" = "2")),
        selectInput("city", "Şehir (Trafik Bölgesi):", 
                    choices = c("Istanbul", "Ankara", "Izmir", "Bursa", "Antalya", "Diger")),
        selectInput("driver", "Sürücü Profili:", 
                    choices = c("Genc_Riskli", "Yetiskin_Normal", "Deneyimli_Guvenli")),
        selectInput("vehicle", "Araç Sınıfı:", 
                    choices = c("Sedan", "SUV", "Hatchback", "Premium")),
        hr(),
        actionButton("calc_btn", "Fiyatı Hesapla", class = "btn-primary w-100")
      ),
      
      card(
        card_header("Risk Primi Sonuç Ekranı", class = "bg-primary text-white"),
        card_body(
          uiOutput("price_display"),
          hr(),
          plotOutput("waterfall_plot")
        )
      )
    )
  ),
  nav_panel("Model Hakkında",
    card(
      card_header("Poisson-Gamma GLM Altyapısı"),
      card_body(
        markdown("
        Bu fiyatlama motoru, arka planda **Frekans (Poisson)** ve **Şiddet (Gamma)** Genelleştirilmiş Doğrusal Modellerini (GLM) kullanarak saf risk primini hesaplar.
        
        **ADAS Fiyatlama Paradoksu:**
        ADAS sistemleri kaza frekansını düşürse de, sensör ve radar maliyetleri nedeniyle hasar şiddetini artırmaktadır. Bu model, bu iki zıt gücü dengeleyerek nihai primi belirler.
        ")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Uygulama acildiginda otomatik hesaplamasi icin butonu tetikleyelim
  observeEvent(input$calc_btn, ignoreNULL = FALSE, {
    
    # Simulating GLM coefficients
    base_freq <- 0.05
    base_sev <- 15000
    
    # City multiplier
    city_mult <- if(input$city == "Istanbul") 1.4 else if(input$city %in% c("Ankara", "Izmir")) 1.2 else 1.0
    
    # Driver multiplier
    drv_mult <- if(input$driver == "Genc_Riskli") 1.6 else if(input$driver == "Deneyimli_Guvenli") 0.8 else 1.0
    
    # Vehicle multiplier
    veh_mult <- if(input$vehicle %in% c("Premium", "SUV")) 1.3 else 1.0
    
    # ADAS Effects (The Paradox)
    # Level 1: Freq drops by 15%, Sev increases by 20%
    # Level 2: Freq drops by 30%, Sev increases by 45%
    if(input$adas_level == "0") {
      adas_freq_m <- 1.0
      adas_sev_m <- 1.0
    } else if(input$adas_level == "1") {
      adas_freq_m <- 0.85
      adas_sev_m <- 1.20
    } else {
      adas_freq_m <- 0.70
      adas_sev_m <- 1.45
    }
    
    final_freq <- base_freq * city_mult * drv_mult * adas_freq_m
    final_sev <- base_sev * veh_mult * adas_sev_m
    risk_premium <- final_freq * final_sev
    
    output$price_display <- renderUI({
      HTML(paste0(
        "<div style='text-align:center; padding: 20px;'>",
        "<h2 style='color:#888;'>Tahmini Saf Risk Primi</h2>",
        "<h1 style='color:#00FF00; font-size:4rem; font-weight:bold;'>", format(round(risk_premium, 0), big.mark=".", decimal.mark=","), " TL</h1>",
        "<p>Beklenen Frekans: <b>", format(round(final_freq, 3), decimal.mark=","), "</b> | Beklenen Şiddet: <b>", format(round(final_sev, 0), big.mark=".", decimal.mark=","), " TL</b></p>",
        "</div>"
      ))
    })
    
    output$waterfall_plot <- renderPlot({
      # A simple bar chart to show Freq vs Sev effect compared to Base
      base_prem <- base_freq * base_sev * city_mult * drv_mult * veh_mult
      
      df <- data.frame(
        Category = c("ADAS'sız Prim (Seviye 0)", paste("Nihai Prim (Seviye", input$adas_level, ")")),
        Premium = c(base_prem, risk_premium),
        Color = c("Base", "Final")
      )
      # Factor to preserve order
      df$Category <- factor(df$Category, levels = df$Category)
      
      ggplot(df, aes(x = Category, y = Premium, fill = Color)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_text(aes(label = paste0(format(round(Premium,0), big.mark=".", decimal.mark=","), " TL")), vjust = -0.5, color="white", size=6) +
        scale_fill_manual(values = c("Base" = "#888888", "Final" = "#ff416c")) +
        theme_minimal() +
        labs(title = "ADAS Paradoks Etkisi (Fiyat Değişimi)", x = "", y = "Prim (TL)") +
        theme(
          plot.background = element_rect(fill = "#1E1E1E", color = NA),
          panel.background = element_rect(fill = "#1E1E1E", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white", size=12),
          plot.title = element_text(size = 18, face = "bold"),
          legend.position = "none"
        )
    })
  })
}

shinyApp(ui, server)

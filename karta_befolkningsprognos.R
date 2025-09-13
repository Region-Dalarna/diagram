


karta_befolkningsprognos <- function(
    region_vekt = "20",                # skickas länskoder med så hämtas alla kommuner i länet, kommunkoder används som de är
    start_ar = "9999",                 # "9999" är senaste tillgängliga år för folkmängd för helår, annars går det att skicka med ett enskilt år från år 1968
    till_ar = "+10",                   # +10 innebär att det är 10 år efter startåret, går att skicka med ett enskilt år
    returnera_ggobj = TRUE,            # returnerar kartan som ggplot-plotobjekt
    skriv_karta_png = TRUE,            # skriver en png-fil av ggplot-objektet
    skriv_karta_html = TRUE,           # skriver en leaflet-karta till en html-fil
    filnamns_prefix = NA,              # om man vill döpa filerna själv ges möjlighet här, NA = de döps automatiskt
    kommuner_egetnamn = NA,            # om man vill döpa en samling kommuner själv, annars sker det automatiskt
    output_mapp = NA,                  # om man sparar filer så behövs anges en sökväg här
    prognosskapare = "Region Dalarna", # ändras till SCB om deras prognos används
    karta_capt_gg = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna",          # <prognos_ar> byts ut till prognosåret om det finns ett sådant
    karta_capt_leaflet = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna"
  ) {
  
  if (!returnera_ggobj & !skriv_karta_png & !skriv_karta_html) stop("Välj TRUE på något av parametrarna returnera_ggobj, skriv_karta_png eller skriv_karta_html, annars körs inte skriptet.")
  
  library(tidyverse)
  library(glue)
  library(mapview)
  library(leaflet)
  library(htmltools)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
  
  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_karta_png | skriv_karta_html) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(utskriftsmapp())) {
        output_mapp <- utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }
  
  kommunnyckel <- hamtaregtab() %>%                              # hämta alla kommunkoder
    filter(nchar(regionkod) == 4) %>% 
    rename(Kommun = region,
           Kommunkod = regionkod)
  
  region_kommuner <- region_vekt[nchar(region_vekt) == 4]            # lägg alla kommunkoder i en vektor
  region_lan <- region_vekt[nchar(region_vekt) == 2]                 # lägg alla länskoder i en vektor
  kommunkoder <- hamtakommuner(region_lan, F, F) %>%                 # hämta alla kommuner som finns i län med länskod
    c(., region_kommuner)                                            # och lägg ihop med skickade kommunkoder
  
  kommunkoder <- kommunkoder[kommunkoder %in% kommunnyckel$Kommunkod]      # behåll bara kommunkoder som är giltiga
  
  filnamn_kommuner <- ar_alla_kommuner_i_ett_lan(kommunkoder, returnera_text = TRUE, returtext = kommunkoder %>% paste0(collapse = "_")) %>% str_replace_all(" ", "_")
  
  if (is.na(kommuner_egetnamn)) {
    retur_kommuner <- hamtaregion_kod_namn(kommunkoder)$region %>% list_komma_och()
    kommuner_egetnamn <- ar_alla_kommuner_i_ett_lan(kommunkoder, returnera_text = TRUE, returtext = retur_kommuner)
  } 
  
  if (any(str_sub(kommunkoder, 1, 2) != "20")) {
    befprogn <- hamta_befprognos_data(region_vekt = kommunkoder,
                                      url_prognos_vektor = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                      tid_vekt = till_ar)
  } else {
    befprogn <- hamta_befprognos_data(region_vekt = kommunkoder,
                                      tid_vekt = till_ar)
  }
  
  
  befutfall <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = kommunkoder,
                                                    alder_koder = "*",
                                                    tid_koder = start_ar) %>% 
    mutate(prognos_ar = "utfall")
  
  bef_diff <- befutfall %>% 
    bind_rows(befprogn) %>% 
    group_by(år, regionkod, region, ålder, prognos_ar) %>% 
    summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")
  
  min_ar <- min(bef_diff$år)
  max_ar <- max(bef_diff$år)
  prognos_ar <- unique(befprogn$prognos_ar)
  if (!is.na(filnamns_prefix)) {
    filnamn_pre <- glue("befprogn_karta_{filnamn_kommuner}_ar{min_ar}-{max_ar}")
  } else {
    filnamn_pre <- filnamns_prefix
  }
  
    
  bef_diff_tot <- bef_diff %>% 
    filter(ålder != "totalt ålder") %>% 
    group_by(år, regionkod, region) %>% 
    summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop") %>% 
    mutate(aldergrp = "totalt")
  
  bef_diff_aldergrp <- bef_diff %>% 
    filter(ålder != "totalt ålder") %>% 
    mutate(aldergrp = skapa_aldersgrupper(ålder, c(0, 20, 66, 80))) %>% 
    group_by(år, regionkod, region, aldergrp) %>% 
    summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")
  
  bef_df <- bef_diff_tot %>% 
    bind_rows(bef_diff_aldergrp) %>% 
    pivot_wider(names_from = "år", values_from = "Folkmängd") %>% 
    mutate(diff = .data[[max_ar]] - .data[[min_ar]],
           diff_proc = (.data[[max_ar]] - .data[[min_ar]])/.data[[min_ar]] * 100) %>% 
    mutate(beskrivning = glue("Befolkningsförändring år {min_ar}-{max_ar}"))
  
  kommuner_sf <- hamta_karta("kommuner", regionkoder = kommunkoder)
  
  bef_karta <- kommuner_sf %>% 
    left_join(bef_df, by = c("knkod" = "regionkod")) %>% 
    filter(aldergrp == "totalt")
  
  if (skriv_karta_png | returnera_ggobj) {
    # ================= karta gglot
    karta_capt_gg <- karta_capt_gg %>% 
      str_replace_all("<prognos_ar>", "{prognos_ar}") %>% 
      str_replace_all("<prognosskapare>", "{prognosskapare}") %>% 
      glue()
    
    befprognos_gg <- ggplot(bef_karta, aes(fill = diff_proc)) +
      geom_sf()+
      coord_sf(expand = FALSE) +
      scale_fill_gradient2(low = "#F15060", mid = "white", high = "#0e5a4c", midpoint = 0) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = margin(1, 1, 1, 1),
            panel.background = element_rect(fill = "white", colour = 'white'),
            plot.caption = element_text(size = 7, hjust = 0),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption.position = "plot",
            legend.position = "bottom",
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 8),
            #legend.box.margin = margin(5, 5, 5, 5),
            legend.key.height = unit(0.5, "cm"),
            legend.key.width  = unit(0.6, "cm")
            ) +
      labs(title = paste0("Prognosticerad befolkningsutveckling i ", kommuner_egetnamn),
           subtitle = paste0("år ", min_ar, "-", max_ar),
           yaxis = "",
           fill = "Förändring (%)",
           caption = karta_capt_gg)   
    
    if (skriv_karta_png) {
      ggsave(paste0(output_mapp, filnamn_pre, ".png"),
             width = 6,
             height = 7)
    }
  } # slut test om man ska skapa en gg-karta

  # ========= leaflet-karta
  
  if (skriv_karta_html) {
    karta_capt_leaflet <- karta_capt_leaflet %>% 
      str_replace_all("<prognos_ar>", "{prognos_ar}") %>% 
      str_replace_all("<prognosskapare>", "{prognosskapare}") %>% 
      glue()
    
    max_abs <- max(abs(bef_karta$diff_proc), na.rm = TRUE)
    
    fargskala <- colorNumeric(
      palette = c("#F15060", "white", "#0e5a4c"),
      domain = c(-max_abs, max_abs)
    )
    
    bef_leaflet <- bef_karta %>% 
      st_transform(crs = 4326)
    
    befprognos_leaflet <- leaflet(bef_leaflet) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~fargskala(diff_proc),
        weight = 1,
        color = "black",
        fillOpacity = 0.8,
        label = ~glue("{knnamn}: {round(diff_proc, 1)} % ({diff} invånare)"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto")
      ) %>%
      addLegend(
        pal = fargskala,
        values = bef_karta$diff_proc,
        #title = "Förändring (%)",
        title = HTML(glue("Förändring (%)<br/><span style='font-size:11px; font-weight:normal'>år {min_ar}-{max_ar} enligt<br>befolkningsprognos</span>")),
        position = "bottomright"
      ) %>% 
      addControl(
        html = karta_capt_leaflet,
        position = "bottomleft"
      )
    
    saveWidget(befprognos_leaflet, paste0(output_mapp, filnamn_pre, ".html"), selfcontained = TRUE)
  }
  
  if (returnera_ggobj) return(befprognos_gg)
}

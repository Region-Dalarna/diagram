
diag_storsta_yrke_per_geografi <- function(
                              region_vekt = "20",           # kan vara en geografi
                              gruppera_namn = NA,             # om NA görs ett diagram per geografi, annars grupperas de ihop och döps till gruppera_namn
                              tid_koder = "9999",                   # "NA"9999" = senaste år  
                              konsuppdelat = TRUE,
                              antal_yrken = 15,               # antal av största yrken som man tar med
                              manual_color = NA,              # om man vill skicka med en egen färgpalett till diagram 1
                              manual_color_kon = NA,          # egen färgpalett för könsuppdelat
                              kortnamn_lan = TRUE,            # TRUE så tas "län" bort ur länsnamn, annars inte
                              output_mapp = NA,
                              returnera_dataframe_global_environment = FALSE,          
                              ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
                              visa_dataetiketter = FALSE,
                              facet_ovanpa_varandra = FALSE,                           # lägg facets ovanpå varandra istället för bredvid varandra
                              storre_text = FALSE,                                     # större text, passar bättre i markdownrapporter
                              ta_med_logga = TRUE,
                              logga_sokvag = NA,
                              diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                              skriv_till_diagramfil = TRUE     # 
                                            ) {

  # ============================================================================================================
  #
  # Skriv ut de x antal största yrkena SSYK3 (default är 15 största yrken) för valfritt län eller kommun. 
  #
  # vald_geografi - går att skicka med flera, ett diagram per geografi skrivs ut om man inte skickar med ett värde
  #                 för gruppera_namn, då grupperas de ihop till en geografi
  #
  # gruppera_namn - default är NA och då grupperas inget. Skickas namn med så grupperas alla geografier i 
  #                 vald_geografi ihop till en geografi som döps till värdet för gruppera_namn
  #
  # valt_ar - det år man vill få diagram över x antal största yrken för. Skickas flera år med så skrivs ett 
  #           diagram per år ut
  #
  # konsuppdelat - TRUE om man vill ha x största yrken för varje kön. Vid könsuppdelat skrivs ett facetdiagram
  #                ut med ett diagram per kön. Då används olika färger per kön också och varje facetdiagram
  #                sorteras med stösta yrket överst
  #
  # antal_yrken - default är 15. Hur många yrken som tas med i diagrammet
  #
  # manual_color - om man vill ha annan färg än default som är region dalarnas blå från grafiska profilen
  # manual_color_kon - om man vill ha andra färger för kvinnor och män än vår standard för könsuppdelad statistik
  #
  # output_mapp - sökväg till mapp där diagrammen skrivs ut
  # 
  # diagram_capt - diagrambeskrivning
  #
  # skriv_till_diagramfil - om man vill skriva ut diagramfiler, annars returneras endast en lista med ggplot-objekt
  #
  # ============================================================================================================
  library(pxweb)
  library(httr)
  library(tidyverse)
  library(tidytext)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)         # använder funktion för att ta alla namn i en vektor och lägga som lista med komma mellan samt och mellan näst sista och sista element
  options(dplyr.summarise.inform = FALSE)
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(manual_color))) {
    if (exists("diagramfarger", mode = "function")) {
      manual_color <- diagramfarger("rus_sex")
    } else {
      manual_color <- hue_pal()(9)
    }
  }
  
  if (all(is.na(manual_color_kon))) {
    if (exists("diagramfarger", mode = "function")) {
      manual_color_kon <- diagramfarger("kon")
    } else {
      manual_color_kon <- hue_pal()(9)
    }
  }
  
  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_till_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(utskriftsmapp())) {
        output_mapp <- utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }
  # ================== hämta data ===================================
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_yrke_region_ssyk3_kon_tid_YREG58BAS_YREG58N_YREG58_scb.R")

  px_df <- hamta_yrke_region_ssyk3_kon_tid_scb(region_vekt = region_vekt,
                                               kon_klartext = if(konsuppdelat) c("män", "kvinnor") else NA,
                                               tid_koder = tid_koder)
  
  kol_rename <- names(px_df)[str_detect(tolower(names(px_df)), "ssyk")]
  
  px_df <- px_df %>% 
    rename(yrke = !!sym(kol_rename)) 
  
  if (kortnamn_lan) px_df <- px_df %>% mutate(region = region %>% skapa_kortnamn_lan())
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
  # om man skickat med gruppera_namn så grupperas alla geografier ihop till en geografi
  if (!is.na(gruppera_namn)) {
    px_df <-  px_df %>% 
      group_by(yrkeskod, yrke, kön, år) %>% 
      summarise(Antal = sum(Antal, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(regionkod = "XXXX",
             region = gruppera_namn)
  }
  
  # bearbeta df:n beroende på om den ska vara könsuppdelad eller inte
  if (konsuppdelat) {
      kon_var <- unique(px_df$kön)             # vektor med båda könen
      
      # skapa en lista med två vektorer som innehåller yrkeskoder för de antal_yrken största yrkena per kön 
      storsta_yrke <- map(kon_var, ~ px_df %>% 
                            filter(kön == .x,
                                   yrkeskod != "0002") %>% 
                            arrange(desc(Antal)) %>% 
                            slice(1:antal_yrken) %>% 
                            select(yrkeskod) %>% 
                            dplyr::pull())
      
      # filtrera ut de största yrkena per kön och lägg i en df
      chart_df <- map2_dfr(storsta_yrke, kon_var, ~ px_df %>%
                             filter(yrkeskod %in% .x,
                                    kön == .y))
      
  } else {
    chart_df <- px_df %>% 
      filter(yrkeskod != "0002") %>% 
      group_by(år, regionkod, region, yrkeskod, yrke) %>% 
      summarise(Antal = sum(Antal, 
                                                                               na.rm = TRUE)) %>% 
      ungroup() %>% 
      arrange(desc(Antal)) %>% 
      slice(1:antal_yrken)
  }

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("storsta_yrken_per_geografi_df", chart_df, envir = .GlobalEnv)
  }
  
  # ========================= Skapa själva diagrammen ==============================================================

  # skriv ut varje geografi för sig (om vi inte grupperat ihop dem ovan, men då hanteras de som en och samma här)
  # och varje år för sig
    
  for (skickat_ar in unique(chart_df$år)) {
    for (geo_namn in unique(chart_df$region)){
        
      diagramtitel <- paste0("Störst yrken i ", geo_namn, " år ", skickat_ar)
      filnamn <- paste0(antal_yrken, "_storsta_yrken_", geo_namn, ifelse(konsuppdelat, "_kon", ""), "_", skickat_ar,".png")
        
      gg_obj <- SkapaStapelDiagram(skickad_df = chart_df %>% 
                                     filter(år == skickat_ar,
                                            region == geo_namn), 
                        skickad_x_var = "yrke",
                        skickad_y_var = "Antal",
                        diagram_titel = diagramtitel,
                        diagram_capt = diagram_capt,
                        output_mapp = output_mapp,
                        x_var_fokus = ifelse(konsuppdelat, "kön", NA),         # vi använder fokusvariabel endast i könsuppdelade diagram (för att få grönt för män och gul för kvinnor)
                        x_axis_sort_value = TRUE,
                        x_axis_lutning = 0,
                        manual_x_axis_title = "",
                        manual_y_axis_title = "Antal sysselsatta",
                        filnamn_diagram = filnamn,
                        manual_color = if (konsuppdelat) manual_color_kon else manual_color,         # grön/gul för könsuppdelat, blå när det inte är det (om man inte väljer andra färgskalor som parameter i funktionen)
                        stodlinjer_avrunda_fem = TRUE,
                        diagram_liggande = TRUE,
                        diagram_facet = konsuppdelat,
                        lagg_pa_logga = ta_med_logga,
                        logga_path = logga_sokvag,
                        utan_diagramtitel = ta_bort_diagramtitel,                            # FALSE så skrivs ingen diagramtitel ut
                        dataetiketter = visa_dataetiketter,
                        facet_grp = ifelse(konsuppdelat, "kön", NA),                          # kör facet, ett för varje kön om könsuppdelat, inte könsuppdelalt annars
                        facet_sort = TRUE,
                        diagramfil_hojd = ifelse(storre_text, 11, 7),
                        facet_x_axis_storlek = ifelse(storre_text, 10.5, 8),
                        facet_y_axis_storlek = ifelse(storre_text, 10.5, 8),
                        facet_kolumner = if (facet_ovanpa_varandra) 1 else NULL,
                        facet_scale = "free_y",                                   # konstanthåller skala för antal i yrket men låter vilka yrken som är med vara "free", så att det kan bli två olika uppsättningar av yrken
                        skriv_till_diagramfil = skriv_till_diagramfil
                        )
    
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- filnamn %>% str_remove(".png")
      
    } # slut for-loop för alla geografier som har skickats med
  } # slut for-loop för alla år som har skickats med
  
  # sist av allt returnerar vi en lista med diagram
  return(gg_list)
  
} # slut funktion

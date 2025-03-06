diag_gini_SCB <- function(region_vekt = hamtaAllaLan(tamedriket = TRUE), # De regioner som skall jämföras i stapeldiagram.
                          diagram_fokus = "20", # Vilken region skall fokus ligga på i stapeldiagrammet. Måste vara en av de som finns ovan
                          region_vekt_linje = c("20","00"), # Vilka regioner skall jämföras i linjediagrammet. Måste vara två av de som finns ovan
                          output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                          spara_diagrambildfil = FALSE,
                          diagram_capt = "Källa: SCB \nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: För att redovisa ojämnheten i inkomstfördelningen används gini-koefficienten.\nKoefficienten kan anta ett värde mellan 0 och 1.\nEtt högt värde på koefficienten visar på större ojämnhet än ett lågt värde",
                          diag_fargvekt_linje = NA,
                          diag_fargvekt_stapel = NA,
                          inkomsttyp_klartext = "disponibel inkomst per k.e. inkl. kapitalvinst",			 #  Finns: "faktorinkomst per k.e. inkl. kapitalvinst", "faktorinkomst per k.e. exkl. kapitalvinst", "disponibel inkomst per k.e. inkl. kapitalvinst", "disponibel inkomst per k.e. exkl. kapitalvinst",
                          diag_tidsserie = TRUE,
                          diag_jmfr_senastear = TRUE,
                          returnera_data = FALSE
) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         scales)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  # if (demo){
  #   demo_url <- 
  #     c("https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_ar2007_2022.png",
  #       "https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_jmfr_riket_ar2007-2022.png")
  #   walk(demo_url, ~browseURL(.x))
  #   if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  #   stop_tyst()
  # }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt_linje))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt_linje <- diagramfarger("rus_sex")
    } else {
      diag_fargvekt_linje <- hue_pal()(9)
    }
  }
  
  if (all(is.na(diag_fargvekt_stapel))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt_stapel <- diagramfarger("rus_tva_fokus")
    } else {
      diag_fargvekt_stapel <- hue_pal()(9)
    }
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_inkomstfordelning_region_inkomsttyp_tid_TabVX1DispInkN_HE0110_HE0110F_scb.R")
  
  gini_df <- hamta_inkomstfordelning_region_inkomsttyp_tid_scb(region_vekt = region_vekt,
                                                               inkomsttyp_klartext = inkomsttyp_klartext,
                                                               cont_klartext = "Gini-koefficient") %>% 
    mutate(region = region %>% skapa_kortnamn_lan())
  
  
  if (diag_tidsserie) {
    
    diagramtitel <- "Ginikoefficient"
    diagramfil <- paste0("gini_tidsserie_",paste(region_vekt_linje,collapse="_"),".png")
    
    gini_tidsserie <- gini_df %>% 
      filter(regionkod %in% region_vekt_linje)
    
    if(returnera_data == TRUE & diag_tidsserie == TRUE & diag_jmfr_senastear == FALSE){
      assign("gini_df", gini_tidsserie, envir = .GlobalEnv)
    }
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = gini_tidsserie,
                                skickad_x_var = "år",
                                skickad_y_var = "Gini-koefficient",
                                diagram_titel = diagramtitel,
                                skickad_x_grupp = "region",
                                stodlinjer_avrunda_fem = TRUE,
                                diagram_capt = diagram_capt,
                                manual_color = diag_fargvekt_linje,
                                manual_y_axis_title = "Ginikoefficient",
                                output_mapp = output_mapp,
                                filnamn_diagram = diagramfil,
                                skriv_till_diagramfil = spara_diagrambildfil
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  } # slut if-sats om diag_tidsserie
  
  if (diag_jmfr_senastear) {
    
    senaste_ar = max(gini_df$år)
    diagramtitel <- glue("Ginikoefficient år {senaste_ar}")
    diagramfil <- paste0("gini_jmf_senastear.png")
    
    if(returnera_data == TRUE & diag_tidsserie == TRUE & diag_jmfr_senastear == TRUE){
      assign("gini_df", gini_df, envir = .GlobalEnv)
    }
    
    region_fokus = skapa_kortnamn_lan(hamtaregion_kod_namn("20")[2])
    
    gg_obj <- SkapaStapelDiagram(skickad_df = gini_df %>% 
                                   filter(år == max(år)) %>% 
                                   mutate(fokus = ifelse(region == region_fokus,1,0)),
                                 skickad_x_var = "region",
                                 skickad_y_var = "Gini-koefficient",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 x_var_fokus = "fokus",
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title = "Ginikoefficient",
                                 manual_color = diag_fargvekt_stapel,
                                 output_mapp = output_mapp,
                                 skriv_till_diagramfil = spara_diagrambildfil
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  } # slut if-sats om diag_tidsserie
  
  return(gg_list)
  
} # slut diag-funktion

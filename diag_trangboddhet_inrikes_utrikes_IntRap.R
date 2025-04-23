diag_trangboddhet_inrikes_utrikes <- function(region = "20", # Enbart ett i taget.
                                              visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                              logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                              output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                              skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                              diag_antal = TRUE, # Antal
                                              diag_andel = TRUE, # Andel, summerar till 100 procent
                                              returnera_data_rmarkdown = FALSE,
                                              demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  #
  # Två diagram för trångboddhet uppdelat på inrikes och utrikes födda, finns som antal respektive andel
  # Används primärt i integrationsrapporten
  #
  # Alla åldrar
  # =======================================================================================================================
  
  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
    walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    stop_tyst()
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  
  gg_list <- list()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_integration_trangboddhet_region_fodelseregion_alder_kon_tid_LE0105Boende02_scb.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)
  
  # Hämtar data
  trangboddhet_df <- hamta_integration_trangboddhet_region_trangboddhet_fodelseregion_alder_kon_tid_scb (region_vekt = "20",
                                                                                                         fodelseregion_klartext = "*",
                                                                                                         alder_klartext = "totalt",	
                                                                                                         kon_klartext = "totalt",
                                                                                                         cont_klartext = "Samtliga i populationen",
                                                                                                         trangboddhet_klartext = "*",
                                                                                                         tid_koder = c("2018","9999")) %>%
    filter(födelseregion != "totalt",
           trångboddhet != "totalt") %>% 
    mutate(trångboddhet = case_when(
      trångboddhet == "trångbodda enligt norm 2" ~ "Trångbodda",
      trångboddhet == "ej trångbodda enligt norm 2" ~ "Ej trångbodda",
      TRUE ~ trångboddhet
    ),
    födelseregion = ifelse(födelseregion == "Sverige", "Inrikes född", "Utrikes född"),
    region = skapa_kortnamn_lan(region)) %>% 
    group_by(region, år, födelseregion,trångboddhet,) %>% 
    summarise(varde = sum(`Samtliga i populationen`,na.rm = TRUE)) %>% 
    mutate(andel = varde/sum(varde)*100) %>% 
    ungroup() 
  
  
  
  if(returnera_data_rmarkdown == TRUE){
    assign("trangboddhet_df", trangboddhet_df, envir = .GlobalEnv)
  }
  
  
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Trångbodd enligt norm 2. Norm två infördes på 1960-talet och definieras\nsom högst två personer per rum och dessutom ska det finnas ett kök och ett vardagsrum."
  
  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  trangboddhet_df$trångboddhet <- factor(trangboddhet_df$trångboddhet, levels = rev(c("Trångbodda","Ej trångbodda",
                                                                                      "uppgift saknas")))
  
  if(diag_antal == TRUE){
    
    diagramtitel <- paste0("Antal trångbodda och ej trångbodda hushåll i  ",unique(trangboddhet_df$region))
    diagramfilnamn <- paste0("trangboddhet_antal_",unique(trangboddhet_df$region),".png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df = trangboddhet_df %>% 
                                   filter(trångboddhet != "uppgift saknas"),
                                 skickad_x_var = "trångboddhet",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "år",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_facet = TRUE,
                                 facet_grp = "födelseregion",
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE, 
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "",
                                 manual_x_axis_title = "",
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  if(diag_andel == TRUE){
    diagramtitel <- paste0("Andel trångbodda och ej trångbodda hushåll i  ",unique(trangboddhet_df$region))
    diagramfilnamn <- paste0("trangboddhet_andel_",unique(trangboddhet_df$region),".png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df = trangboddhet_df ,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "trångboddhet",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_facet = TRUE,
                                 facet_grp = "födelseregion",
                                 facet_scale = "fixed",
                                 geom_position_stack = TRUE,
                                 facet_legend_bottom = TRUE, 
                                 stodlinjer_avrunda_fem = TRUE,
                                 procent_0_100_10intervaller = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "",
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
}

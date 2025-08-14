diag_huv_ink_kalla_bakgrund_scb <- function(region = "20", # Enbart ett i taget.
                                            visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                            logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                            output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                            skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                            diag_totalt = TRUE,
                                            diag_kon = TRUE,
                                            alder_klartext = "20-64 år",			 #  Finns: "15-19 år", "20-24 år", "25-54 år", "55-64 år", "65-74 år", "15-74 år", "16-64 år", "16-65 år", "20-64 år", "20-65 år" 
                                            returnera_data_rmarkdown = FALSE,
                                            demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  #
  # Två diagram för huvudsaklig inkomstkälla (exkl förvärvsarbetande) kopplat till bakgrund och kön
  # Från integrationsrapporten (därav IntRap i namnet)
  #
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_huvink_region_huvudfot1m_kon_alder_fodelseregion_tid_ArbStatFoT1_scb.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)
  
  huvud_ink_df <- hamta_bas_huvink_region_huvudfot1m_kon_alder_fodelseregion_tid_scb(region_vekt = region,
                                                                                     kon_klartext = "*",
                                                                                     huvudfot1m_klartext = "*",
                                                                                     alder_klartext = alder_klartext,
                                                                                     fodelseregion_klartext = "*",
                                                                                     cont_klartext = "antal totalt",
                                                                                     tid_koder = "9999") %>%
    mutate(`huvudsaklig inkomstkälla` = case_when(
      grepl("arbete", `huvudsaklig inkomstkälla`) ~ "Arbete",
      grepl("arbetslöshet", `huvudsaklig inkomstkälla`) ~ "Arbetslöshet",
      grepl("studier", `huvudsaklig inkomstkälla`) ~ "Studier",
      grepl("^pension$", `huvudsaklig inkomstkälla`) ~ "Pension",
      grepl("långvarigt nedsatt arbetsförmåga", `huvudsaklig inkomstkälla`) ~ "Långvarigt nedsatt arbetsförmåga",
      grepl("sjukdom", `huvudsaklig inkomstkälla`) ~ "Ersättning vid sjukdom",
      grepl("föräldraledighet|närståendeomvårdnad", `huvudsaklig inkomstkälla`) ~ "Föräldraledighet m.m.",
      grepl("ekonomiskt stöd", `huvudsaklig inkomstkälla`) ~ "Ekonomiskt stöd",
      grepl("saknar ersättningar", `huvudsaklig inkomstkälla`) ~ "Saknar ersättningar",
      TRUE ~ NA_character_
    )) %>% 
    filter(födelseregion != "totalt") %>%
    group_by(region, månad, ålder, kön, födelseregion,`huvudsaklig inkomstkälla`) %>%
    summarize(`antal totalt` = sum(`antal totalt`, na.rm = TRUE)) %>% 
    mutate(andel = round((`antal totalt` / sum(`antal totalt`, na.rm = TRUE))*100,1)) %>% 
    ungroup() %>% 
    filter(`huvudsaklig inkomstkälla` != "Arbete") %>% 
    manader_bearbeta_scbtabeller()
  
  if(returnera_data_rmarkdown == TRUE){
    assign("huv_ink_df", huvud_ink_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  huvud_ink_df$`huvudsaklig inkomstkälla` <- factor(huvud_ink_df$`huvudsaklig inkomstkälla`, levels = c("Studier","Föräldraledighet m.m.",
                                                                                                        "Ersättning vid sjukdom","Arbetslöshet",
                                                                                                        "Långvarigt nedsatt arbetsförmåga","Ekonomiskt stöd",
                                                                                                        "Pension","Saknar ersättningar"))
  
  diagramtitel <- paste0("Huvudsaklig inkomstkälla (exkl förvärvsarbetande) i ",skapa_kortnamn_lan(unique(huvud_ink_df$region))," i ",unique(huvud_ink_df$månad), " ", unique(huvud_ink_df$år), ", ",unique(huvud_ink_df$ålder))
  
  
  if(diag_totalt){
    diagramfilnamn <- paste0("huvud_ink_bakgrund_",skapa_kortnamn_lan(unique(huvud_ink_df$region)),".png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =huvud_ink_df %>%
                                   filter(kön == "totalt"),
                                 skickad_x_var = "huvudsaklig inkomstkälla",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "födelseregion",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = rev(diagramfarger("rus_sex")[1:2]),
                                 manual_y_axis_title = "procent",
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 y_axis_100proc = FALSE,
                                 x_axis_lutning = 45,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
  if(diag_kon){
    
    diagramfilnamn <- paste0("huvud_ink_kon_bakgrund_",skapa_kortnamn_lan(unique(huvud_ink_df$region)),".png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =huvud_ink_df %>%
                                   filter(kön != "totalt"),
                                 skickad_x_var = "huvudsaklig inkomstkälla",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = rev(diagramfarger("kon")),
                                 manual_y_axis_title = "procent",
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = TRUE,
                                 facet_grp = "födelseregion",
                                 facet_scale = "fixed",
                                 y_axis_100proc = FALSE,
                                 x_axis_lutning = 45,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
}

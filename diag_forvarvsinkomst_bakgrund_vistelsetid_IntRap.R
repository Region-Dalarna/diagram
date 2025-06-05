diag_inkomst_bakgrund_scb <- function(region = "20", # Enbart ett i taget.
                                      visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                      logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                      output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                      inkomst_typ = "Medianinkomst, tkr", # Finns "Medianinkomst, tkr", "Medelinkomst, tkr". Max 1 åt gången
                                      skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                      alder_klartext = "20-64 år",			 #  Finns: "20+ år", "20-64 år", "20-65 år", "65+ år", "66+ år". Max 1 åt gången
                                      returnera_data_rmarkdown = FALSE,
                                      demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  #
  # Ett diagram för förvärvsinkomst kopplad till bakgrund (vistelsetid)
  # Från integrationsrapporten (därav IntRap i namnet)
  #
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
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_forvarvsinkomst_region_kon_fodelseregion_vistelsetid_HE0110_scb.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl)
  
  # Före 2022
  forvarvsinkomst_df <- hamta_forvarvsinkomst_region_kon_fodelseregion_vistelsetid_scb(region_vekt = region,
                                                                                       kon_klartext = "*",
                                                                                       alder_klartext = alder_klartext,
                                                                                       vistelsetiduf_klartext = "*",
                                                                                       cont_klartext = inkomst_typ,
                                                                                       tid_koder = "9999") %>%
    rename(vistelsetid = `vistelsetid år`) %>%
    mutate(vistelsetid = ifelse(födelseregion == "födda i Sverige","Inrikes född",vistelsetid)) %>%
    filter(födelseregion %in% c("födda i Sverige","utrikes födda"),
           vistelsetid != "samtliga") %>%
    mutate(vistelsetid = case_when(
      vistelsetid == "1-2 år i Sverige" ~ "1-2 år",
      vistelsetid == "3-4 år i Sverige" ~ "3-4 år",
      vistelsetid == "5-9 år i Sverige" ~ "5-9 år",
      vistelsetid == "10-19 år i Sverige" ~ "10-19 år",
      vistelsetid == "20- år i Sverige" ~ "20- år",
      TRUE ~ vistelsetid
    ))
  
  # !(is.na(`Medianinkomst, tkr`)),
  
  
  if(returnera_data_rmarkdown == TRUE){
    assign("forvarvsinkomst_df", forvarvsinkomst_df, envir = .GlobalEnv)
  }
  
  gg_list <- list()
  
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nSammanräknad förvärvsinkomst, dvs. alla skattepliktiga inkomster före skatt (dock ej kapitalinkomster)."
  
  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  forvarvsinkomst_df$vistelsetid <- factor(forvarvsinkomst_df$vistelsetid, levels = c("1-2 år","3-4 år",
                                                                                      "5-9 år","10-19 år",
                                                                                      "20- år","Inrikes född"))
  variabel = sub(",.*", "", last(names(forvarvsinkomst_df)))
  diagramtitel <- paste0(variabel," (", unique(forvarvsinkomst_df$ålder),") i Dalarna ",max(forvarvsinkomst_df$år)," efter vistelsetid")
  #diagramtitel <- str_wrap(diagramtitel,60)
  diagramfilnamn <- paste0(variabel,"_bakgrund.png")
  
  # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
  gg_obj <- SkapaStapelDiagram(skickad_df =forvarvsinkomst_df %>%
                                 filter(kön != "totalt"),
                               skickad_x_var = "vistelsetid",
                               skickad_y_var = last(names(forvarvsinkomst_df)),
                               skickad_x_grupp = "kön",
                               # manual_x_axis_text_vjust=0.9,
                               manual_color = diagramfarger("kon"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               manual_x_axis_title = "Vistelsetid i Sverige",
                               y_axis_100proc = FALSE,
                               x_axis_lutning = 0,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               lagg_pa_logga = visa_logga_i_diagram,
                               logga_path = logga_sokvag,
                               skriv_till_diagramfil = skriv_diagrambildfil)
  
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
}

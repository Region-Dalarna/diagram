diag_SFI_bakgrund <- function(region = "20", # Enbart ett i taget.
                              diag_vistelsetid_senaste_ar = TRUE,
                              diag_vistelsetid_tidsserie = TRUE,
                              visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                              logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                              valda_farger = diagramfarger("rus_sex"),
                              output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                              skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                              returnera_data_rmarkdown = FALSE,
                              demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  #
  # Två diagram godkända i SFI efter vistelsetid i Sverige
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_SFI_godkand_region_kon_bakgrund_tid_IntGr8LanKON3_scb.R")
  
  valt_lan <- skapa_kortnamn_lan(hamtaregion_kod_namn(region)$region)
  # if (!require("pacman")) install.packages("pacman")
  # pacman::p_load(tidyverse,
  #                pxweb,
  #                readxl)
  
  
  
  # Hämtar data
  SFI_df <- hamta_SFI_genomfort_region_kon_bakgrund_tid_scb(region_vekt = region,
                                                            kon_klartext = "*",
                                                            bakgrund_klartext = c("utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning"),
                                                            cont_klartext = "Vistelsetid för godkända i sfi, median i antal dagar",
                                                            tid_koder = "*") %>%
    mutate(variabel = sub("utbildningsnivå: ", "", variabel),
           variabel = str_to_sentence(variabel))
  
  
  if(returnera_data_rmarkdown == TRUE){
    assign("SFI_df", SFI_df, envir = .GlobalEnv)
  }
  
  
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  #Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  SFI_df$variabel <- factor(SFI_df$variabel, levels = unique(SFI_df$variabel))
  
  # Namn på variabel som används i diagramtitel
  variabel_namn <- sub("sfi", "SFI", sub(",.*", "", last(names(SFI_df))))
  
  if(diag_vistelsetid_senaste_ar){
    
    diagramtitel <- glue("{variabel_namn} i {valt_lan} år {max(SFI_df$år)}")
    diagramfilnamn <- paste0("sfi_vistelsetid_senastear_",valt_lan,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =SFI_df %>%
                                   filter(kön != "män och kvinnor",
                                          år == max(år)),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = last(names(SFI_df)),
                                 skickad_x_grupp = "kön",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 #manual_y_axis_title = "procent",
                                 facet_scale = "fixed",
                                 #y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  if(diag_vistelsetid_tidsserie){
    
    diagramtitel <- glue("{variabel_namn} i {valt_lan}")
    diagramfilnamn <- paste0("sfi_vistelsetid_tidsserie_",valt_lan,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =SFI_df %>%
                                   filter(kön == "män och kvinnor"),
                                 skickad_x_var = "år",
                                 skickad_y_var = last(names(SFI_df)),
                                 skickad_x_grupp = "variabel",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = valda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_axis_lutning = 45,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
}
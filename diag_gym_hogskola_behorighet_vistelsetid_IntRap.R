diag_gymnasiebehorighet_mm <- function(region = "20", # Enbart ett i taget.
                                       diag_kon_gym= TRUE,
                                       diag_kon_hogskola = TRUE,
                                       diag_vistelsetid_gym = TRUE, # Ej könsuppdelat
                                       jmf_ar = 2018, # År som senaste år skall jämföras med vid könsuppdelat diagram                              # sökväg till logga som ska visas i diagrammet
                                       output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                       skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                       returnera_data_rmarkdown = FALSE,
                                       demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  #
  # Två diagram för behörighet till gymnasiet och ett för behörighet till högskola som används i integrationsrapporten
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_integration_gymn_hogsk_behorighet_region_kon_bakgrund_tid_IntGr8RikKON2_IntGr8LanKON2_scb.R")
  
  # Hämtar data
  behorighet_gym_df <- hamta_integration_gymn_hogsk_behorighet_region_kon_bakgrund_tid_scb  (region_vekt = c(region),
                                                                                             kon_klartext = "*",
                                                                                             bakgrund_klartext  = c("födelseregion: Sverige","samtliga utrikes födda","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                                             cont_klartext = "Andel behöriga till gymnasium, procent",
                                                                                             tid_koder = "*") %>%
    mutate(region = skapa_kortnamn_lan(region),
           variabel = case_when(
             variabel == "vistelsetid 0-1 år" ~ "0-1 år",
             variabel == "vistelsetid 2-3 år" ~ "2-3 år",
             variabel == "vistelsetid 4-9 år" ~ "4-9 år",
             variabel == "vistelsetid 10- år" ~ "10- år",
             variabel == "samtliga utrikes födda" ~ "Utrikes född",
             variabel == "födelseregion: Sverige" ~ "Inrikes född",
             TRUE ~ variabel
           ),
           kön = case_when(
             kön == "kvinnor" ~ "flickor",
             kön == "män" ~ "pojkar",
             kön == "män och kvinnor" ~ "pojkar och flickor",
             TRUE ~ kön
           )) %>%
    rename(Andel_behoriga = `Andel behöriga till gymnasium, procent`)
  
  
  if(returnera_data_rmarkdown == TRUE){
    assign("behorighet_gym_df", behorighet_gym_df, envir = .GlobalEnv)
  }
  
  if(diag_kon_gym){
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    # syssgrad_df$bakgrundsvariabel <- factor(syssgrad_df$bakgrundsvariabel, levels = c("0-1 år","2-3 år",
    #                                                                                   "4-9 år","10- år",
    #                                                                                   "Inrikes född"))
    
    diagramtitel <- paste0("Andel behöriga till gymnasiet i ",unique(behorighet_gym_df$region))
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("behorighet_gymnasiet_jmf_ar.png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df = behorighet_gym_df %>%
                                   filter(kön != "pojkar och flickor",
                                          variabel %in% c("Utrikes född","Inrikes född"),
                                          år %in% c(jmf_ar,max(år))),
                                 skickad_x_var = "år",
                                 skickad_y_var = "Andel_behoriga",
                                 skickad_x_grupp = "kön",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 diagram_facet = TRUE,
                                 facet_grp = "variabel",
                                 facet_legend_bottom = TRUE,
                                 facet_scale = "fixed",
                                 #manual_x_axis_title = "Vistelsetid i Sverige",
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_vistelsetid_gym){
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    behorighet_gym_df <- behorighet_gym_df %>%
      filter(variabel != "Utrikes född",
             region != "Riket")
    
    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    behorighet_gym_df$variabel <- factor(behorighet_gym_df$variabel, levels = c("0-1 år","2-3 år",
                                                                                "4-9 år","10- år",
                                                                                "Inrikes född"))
    
    diagramtitel <- paste0("Andel behöriga till gymnasiet i Dalarna"," ",max(behorighet_gym_df$år)," uppdelat på vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("behorighet_gymnasiet_vistelsetid.png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df = behorighet_gym_df %>%
                                   filter(kön == "pojkar och flickor",
                                          !(is.na(Andel_behoriga)),
                                          år == max(år)),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = "Andel_behoriga",
                                 #skickad_x_grupp = "kön",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 facet_scale = "fixed",
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_kon_hogskola){
    
    behorighet_hogskola_df <- hamta_integration_gymn_hogsk_behorighet_region_kon_bakgrund_tid_scb  (region_vekt = c(region),
                                                                                                    kon_klartext = c("män","kvinnor"),
                                                                                                    bakgrund_klartext  = c("födelseregion: Sverige","samtliga utrikes födda"),
                                                                                                    cont_klartext = "Andel behöriga till högskola, procent",
                                                                                                    tid_koder = "*") %>%
      mutate(region = skapa_kortnamn_lan(region),
             variabel = case_when(variabel == "samtliga utrikes födda" ~ "Utrikes född",
                                  variabel == "födelseregion: Sverige" ~ "Inrikes född",
                                  TRUE ~ variabel
             )) %>%
      rename(Andel_behoriga = `Andel behöriga till högskola, procent`)
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    if(returnera_data_rmarkdown == TRUE){
      assign("behorighet_hogskola_df", behorighet_hogskola_df, envir = .GlobalEnv)
    }
    
    diagramtitel <- paste0("Andel behöriga till högskola i ",unique(behorighet_hogskola_df$region))
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- "behorighet_högksola_jmf_ar.png"
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df = behorighet_hogskola_df %>%
                                   filter(kön != "män och kvinnor",
                                          variabel %in% c("Utrikes född","Inrikes född"),
                                          år %in% c(jmf_ar,max(år))),
                                 skickad_x_var = "år",
                                 skickad_y_var = "Andel_behoriga",
                                 skickad_x_grupp = "kön",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 diagram_facet = TRUE,
                                 facet_grp = "variabel",
                                 facet_legend_bottom = TRUE,
                                 facet_scale = "fixed",
                                 #manual_x_axis_title = "Vistelsetid i Sverige",
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
}

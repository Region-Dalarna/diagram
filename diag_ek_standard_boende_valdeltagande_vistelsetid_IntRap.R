test <- diagram_diverse_vistelsetid(typ_av_val = c("Valdeltagande i val till riksdag","Valdeltagande i val till region", "Valdeltagande i val till kommun"))

diagram_diverse_vistelsetid <-function(region_vekt = c("20"),# Max 1,
                                       diag_ek_standard = TRUE,
                                       alder_ek_standard = "20-64 år",			 #  Finns: "20- år", "20-64 år", "65- år", "20-29 år", "30-49 år", "50-64 år", "65-79 år", "80- år"
                                       jmf_ar = 2017, # År att jämföra senaste år med (för diagrammet med ekonomisk standard)
                                       diag_boendetyp = TRUE,
                                       diag_valdeltagande = TRUE,
                                       typ_av_val = "Valdeltagande i val till riksdag",# Finns även: "Valdeltagande i val till region", "Valdeltagande i val till kommun",
                                       output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                       skriv_diagrambildfil = FALSE, # Sparar figuren till output_mapp_figur
                                       returnera_figur = TRUE, # Returnerar en figur
                                       diag_fargvekt = NA, # Gäller samtliga diagram
                                       returnera_dataframe_global_environment = FALSE) # Skall data returneras)
{
  
  ## =================================================================================================================
  # Diagram kopplade till tre figurer från integrationsrapporten
  # - Ekonomisk Ett standard: Facet-diagram (stapel) som jämför andel personer med låg ekonomisk standard i olika grupper
  # - Boendeform: Ett Stapeldiagram som visar boendeform baserat på vistelsetid (och inrikes födda)
  # - Valdeltagande: Upp till tre Stapeldiagram som visar valdeltagande i olika val (riksdag, region och kommun) baserat på vistelsetid (och inrikes födda)
  # =================================================================================================================
  # Skript som skapar tre diagram kopplade till låg ekonomisk standard
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)
  
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("rus_sex")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  if(diag_ek_standard == TRUE){
    
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_ekonomisk_standard_region_alder_sysselsattning_utlbakgrund_inkomsttyp_tid_HE0110__HE0110F_scb.R", encoding = "utf-8", echo = FALSE)
    
  
 
    
    # Hämta data
    ekonomisk_standard_bakgrund_df <- hamta_ekonomisk_standard_region_alder_sysselsattning_utlbakgrund_inkomsttyp_tid_scb(region_vekt = region_vekt,
                                                                                                                          alder_klartext = alder_ek_standard,
                                                                                                                          inkomsttyp_klartext = "disponibel inkomst per k.e. inkl. kapitalvinst",
                                                                                                                          sysselsattning_klartext = c("samtliga personer", "förvärvsarbetande", "icke förvärvsarbetande"),
                                                                                                                          cont_klartext = "Inkomst < 60 procent",
                                                                                                                          utlbakgrund_klartext = c("utrikes födda","född i Sverige"),
                                                                                                                          tid_koder = c(jmf_ar,"9999")) %>%
      mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE),
             sysselsättning = paste0(toupper(substr(sysselsättning,1,1)),substr(sysselsättning,2,nchar(sysselsättning))),
             `utländsk/svensk bakgrund` = ifelse(`utländsk/svensk bakgrund` == "född i Sverige","inrikes födda",`utländsk/svensk bakgrund`)) %>%
      rename(bakgrund = `utländsk/svensk bakgrund`)
    
    if(returnera_dataframe_global_environment == TRUE){
      assign("lag_ek_standard_bakgrund_df", ekonomisk_standard_bakgrund_df, envir = .GlobalEnv)
    }
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    
    ekonomisk_standard_bakgrund_df$sysselsättning <- factor(ekonomisk_standard_bakgrund_df$sysselsättning,
                                                            levels = c("Samtliga personer","Förvärvsarbetande","Icke förvärvsarbetande"))
    
    regioner <- paste(unique(ekonomisk_standard_bakgrund_df$region), collapse = "_")
    
    diagram_titel = paste0("Andel personer i hushåll med låg ekonomisk standard i ",unique(ekonomisk_standard_bakgrund_df$region))
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Med låg ekonomisk standard menas att\ninkomsten är lägre än 60 procent av medianen."
    diagramfilnamn <- paste0("diagram_lagekstandard_bakgrund_",regioner,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = ekonomisk_standard_bakgrund_df,
                                 skickad_x_var = "sysselsättning",
                                 skickad_y_var = "Inkomst < 60 procent",
                                 skickad_x_grupp = "år",
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 diagram_capt = diagram_capt,
                                 diagram_titel = diagram_titel,
                                 diagram_facet = TRUE,
                                 # manual_x_axis_text_vjust = 1,
                                 # manual_x_axis_text_hjust = 1,
                                 facet_grp =  "bakgrund",
                                 x_axis_lutning = 0,
                                 facet_scale = "fixed",
                                 procent_0_100_10intervaller = TRUE,
                                 facet_legend_bottom = TRUE,
                                 manual_color = diag_fargvekt,
                                 manual_y_axis_title = "procent",
                                 lagg_pa_logga = FALSE,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_boendetyp == TRUE){
    
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_integration_boende_region_kon_bakgrund_tid_IntGr6LanKon_IntGr6RikKon_scb.R")
    
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(tidyverse,
                   readxl)
    
    
    
    tid_koder = "9999"
    # Av oklar anledning saknas mycket data för senaste år. Jag gör därför ett enklare uttag för senaste år och om det saknas data väljs året innan
    boende_test <- hamta_integration_region_kon_bakgrund_tid_scb (region_vekt = region_vekt,
                                                                  kon_klartext = "män och kvinnor",
                                                                  bakgrund_klartext = c("födelseregion: Sverige","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                  cont_klartext = c("Andel boende i egna hem, procent", "Andel boende i hyresrätt, procent", "Andel boende i bostadsrätt, procent"),
                                                                  tid_koder = tid_koder)
    
    # Check if there exists na:s in any of the variables in the datafram boende_test
    if(length(which(is.na(boende_test)))!= 0 ){
      tid_koder = as.integer(unique(boende_test$år))-1
    }
    
    
    # Hämtar data
    boende_df <- hamta_integration_region_kon_bakgrund_tid_scb (region_vekt = region_vekt,
                                                                kon_klartext = "män och kvinnor",
                                                                bakgrund_klartext = c("födelseregion: Sverige","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                cont_klartext = c("Andel boende i egna hem, procent", "Andel boende i hyresrätt, procent", "Andel boende i bostadsrätt, procent"),
                                                                tid_koder = tid_koder) %>%
      mutate(variabel = case_when(
        variabel == "vistelsetid 0-1 år" ~ "0-1 år",
        variabel == "vistelsetid 2-3 år" ~ "2-3 år",
        variabel == "vistelsetid 4-9 år" ~ "4-9 år",
        variabel == "vistelsetid 10- år" ~ "10- år",
        variabel == "födelseregion: Sverige" ~ "Inrikes född",
        TRUE ~ variabel
      ),
      bakgrund = case_when(
        bakgrund == "Andel boende i egna hem, procent" ~ "Äganderätt",
        bakgrund == "Andel boende i hyresrätt, procent" ~ "Hyresrätt",
        bakgrund == "Andel boende i bostadsrätt, procent" ~ "Bostadsrätt",
        TRUE ~ bakgrund
      )
      ) %>%
      mutate(region = skapa_kortnamn_lan(region))
    
    boende_df <- pivot_wider(boende_df, names_from=bakgrund, values_from=varde) %>%
      mutate(Okänd = 100 - Äganderätt - Hyresrätt - Bostadsrätt) %>%
      pivot_longer(cols=6:9,names_to = "bakgrund",values_to = "varde")
    
    
    
    if(returnera_dataframe_global_environment == TRUE){
      assign("boendtyp_df", boende_df, envir = .GlobalEnv)
    }
    
    
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    boende_df$variabel <- factor(boende_df$variabel, levels = c("0-1 år","2-3 år",
                                                                "4-9 år","10- år",
                                                                "Inrikes född"))
    
    boende_df$bakgrund <- factor(boende_df$bakgrund, levels = c("Okänd","Hyresrätt","Bostadsrätt","Äganderätt"))
    
    diagramtitel <- paste0("Boende per upplåtelseform i ",unique(boende_df$region)," ",max(boende_df$år)," efter vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("boendetyp_vistelsetid_inrikes.png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =boende_df %>%
                                   filter(kön != "totalt"),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "bakgrund",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diag_fargvekt,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 geom_position_stack = TRUE,
                                 legend_vand_ordning = TRUE,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_valdeltagande == TRUE){
    
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_integration_valdeltagande_region_bakgrund_kon_tid_IntGr11Riket1_IntGr11Lan1_IntGr11Kom1_scb.R")
    map_list <- list()

    # Hämtar data
    valdeltagande_df <- hamta_integration_valdeltagande_region_bakgrund_kon_tid_scb (region_vekt = region_vekt,
                                                                                     kon_klartext = "män och kvinnor",
                                                                                     bakgrund_klartext = c("födelseregion: Sverige","samtliga utrikes födda", "vistelsetid 10- år", "vistelsetid < 10 år"),
                                                                                     cont_klartext = "*",
                                                                                     tid_koder = "*") %>%
      mutate(variabel = case_when(
        variabel == "vistelsetid < 10 år" ~ "< 10 år",
        variabel == "vistelsetid 10- år" ~ "10- år",
        variabel == "födelseregion: Sverige" ~ "Inrikes född",
        TRUE ~ variabel
      )) %>%
      mutate(region = skapa_kortnamn_lan(region),
             val = str_extract(val, "^[^,]+"))
    
    
    if(returnera_dataframe_global_environment == TRUE){
      assign("valdeltagande_df", valdeltagande_df, envir = .GlobalEnv)
    }
    
    
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    valdeltagande_df$variabel <- factor(valdeltagande_df$variabel, levels = c("< 10 år","10- år",
                                                                              "samtliga utrikes födda","Inrikes född"))
    
    skapa_diagram <- function(val_vilket){
      
      diagramtitel <- paste0(val_vilket," i ",unique(valdeltagande_df$region)," efter vistelsetid")
      #diagramtitel <- str_wrap(diagramtitel,60)
      diagramfilnamn <- paste0(gsub(" ", "_", val_vilket),".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df =valdeltagande_df %>%
                                     filter(val == val_vilket),
                                   skickad_x_var = "variabel",
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = "år",
                                   # manual_x_axis_text_vjust=0.9,
                                   manual_color = diag_fargvekt,
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   manual_y_axis_title = "procent",
                                   manual_x_axis_title = "Vistelsetid i Sverige",
                                   y_axis_100proc = TRUE,
                                   x_axis_lutning = 0,
                                   output_mapp = output_mapp_figur,
                                   filnamn_diagram = diagramfilnamn,
                                   lagg_pa_logga = visa_logga_i_diagram,
                                   skriv_till_diagramfil = skriv_diagrambildfil)
      
      
      map_list <- c(map_list, list(gg_obj))
      names(map_list)[[length(map_list)]] <- diagramfilnamn %>% str_remove(".png")
      return(map_list)
    }
    
    retur_list <- map(typ_av_val, ~skapa_diagram(val_vilket = .x)) %>% purrr::flatten()
    
    gg_list <- c(gg_list, retur_list)
    
  }
  
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

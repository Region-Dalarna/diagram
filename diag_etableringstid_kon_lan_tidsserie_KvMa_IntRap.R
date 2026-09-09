diag_etablering_utb_kon_scb <- function(region = "20", # Enbart ett i taget.
                                        diag_kon = TRUE, # Jämför kvinnor/män för senaste år
                                        diag_utbildning = TRUE, # Jämför olika utbildningsnivåer för senaste år
                                        utbildningsniva_jmf = c("utbildningsnivå: eftergymnasial utbildning","utbildningsnivå: gymnasial utbildning" ), # Finns även "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", Skriv i den ordning de skall visas i diagram
                                        facet_kolumner = NULL,# Välj antalet kolumner som skall visas i Facet-diagramet (diag_utbildning)
                                        diag_tidsserie = TRUE, # Undersöker etableringstiden över tid (baserat på vistelsetid)
                                        visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                        logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                        output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                        skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                        excel_mapp = NA,                                   # mapp där excelfil ska sparas, NA = sparas ingen fil
                                        returnera_data_rmarkdown = FALSE,
                                        demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  #
  # Tre diagram kopplade till invandringsetablering som används i Kvinnor och män samt integrationsrapporten.
  # Finns även ett liknande diagramskript, med tre andra diagram som används i RUS-uppföljningen:
  # diag_etableringstid_kon_lan_tidsserie
  #
  # Uppdaterat med ny version av PXweb 2026-07-03, Jon
  # Rättat felaktigheter kopplat till bindestreck 2026-09-09, Jon
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
    p_load_gh("FaluPeppe/pxweb2r")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")

  # Nya PXweb 
  # Split codes by digit length
  lan_koder    <- region[nchar(region) == 2]
  kommun_koder <- region[nchar(region) == 4]
  
  bakgrundsvariabler <- c("vistelsetid 0–1 år", "vistelsetid 2–3 år", "vistelsetid 4–9 år",
                          "vistelsetid 10– år", "födelseregion: Sverige")
  
  # Container for results
  resultat <- list()
  
  # Före 2022
  
  # Call region API once, only if there are region codes
  if (length(lan_koder) > 0) {
    resultat$lan <- pxweb2_get_data(
      table = "TAB389",
      query = list(
        Region = lan_koder,
        Kon = "*",
        UtbNiv = "*",
        BakgrVar = bakgrundsvariabler,
        ContentsCode = "Andel förvärvsarbetande (ny definition från och med 2019)",
        Tid = "*"
      )
    )
  }
  
  # Call municipality API once, only if there are municipality codes
  if (length(kommun_koder) > 0) {
    resultat$kommun <- pxweb2_get_data(
      table = "TAB4881",
      query = list(
        Region = kommun_koder,
        Kon = "*",
        UtbNiv = "*",
        BakgrVar = bakgrundsvariabler,
        ContentsCode = "Andel förvärvsarbetande (ny definition från och med 2019)",
        Tid = "*"
      )
    )
  }

  # 2022 och senare
  # Call region API once, only if there are region codes
  if (length(lan_koder) > 0) {
    resultat$lan_22 <- pxweb2_get_data(
      table = "TAB6384",
      query = list(
        Region = lan_koder,
        Kon = "*",
        UtbNiv = "*",
        BakgrVar = bakgrundsvariabler,
        ContentsCode = "Andel sysselsatta",
        Tid = "*"
      )
    )
  }
  
  # Call municipality API once, only if there are municipality codes
  if (length(kommun_koder) > 0) {
    resultat$kommun_22 <- pxweb2_get_data(
      table = "TAB6383",
      query = list(
        Region = kommun_koder,
        Kon = "*",
        UtbNiv = "*",
        BakgrVar = bakgrundsvariabler,
        ContentsCode = "Andel sysselsatta",
        Tid = "*"
      )
    )
  }
  
  # If you want a single combined data frame afterwards (assuming same column structure):
  etablering_df <- dplyr::bind_rows(resultat) %>% 
    rename(andel = value,
           regionkod = region_kod) %>%
      mutate(bakgrundsvariabel = ifelse(bakgrundsvariabel == "födelseregion: Sverige","Inrikes född",bakgrundsvariabel)) %>% 
        mutate(bakgrundsvariabel =  sub("^vistelsetid ", "", bakgrundsvariabel)) %>% 
          select(-tabellinnehåll) %>%
            mutate(region = skapa_kortnamn_lan(region))
    
  
  # etablering_df <- rbind(etablering_2021,etablering_2022_) %>%
  #   mutate(region = skapa_kortnamn_lan(region))

  if(returnera_data_rmarkdown == TRUE){
    assign("etablering_df", etablering_df, envir = .GlobalEnv)
  }
  
  gg_list <- list()
  
  if(diag_kon == TRUE){
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    etablering_df$bakgrundsvariabel <- factor(etablering_df$bakgrundsvariabel, levels = c("0–1 år","2–3 år",
                                                                                          "4–9 år","10– år",
                                                                                          "Inrikes född"))
    
    diagramtitel <- paste0("Andel förvärvsarbetande 20-65 år bland utrikes födda i Dalarna"," ",max(etablering_df$år)," efter vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("etablering_lan_kon.png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =etablering_df %>%
                                   filter(år==max(år),
                                          utbildningsnivå=="samtliga utbildningsnivåer",
                                          kön != "män och kvinnor"),
                                 skickad_x_var = "bakgrundsvariabel",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 y_axis_100proc = TRUE,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = FALSE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_utbildning == TRUE){

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, BAS.\nBearbetning: Samhällsanalys, Region Dalarna."
    diagramtitel <- paste0("Andel förvärvsarbetande 20-65 år bland utrikes födda i Dalarna"," ",max(etablering_df$år))
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("etablering_lan_utb.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =etablering_df %>%
                                   filter(år == max(år),
                                          kön != "män och kvinnor",
                                          utbildningsnivå %in% utbildningsniva_jmf) %>% 
                                   mutate(utbildningsnivå = factor(utbildningsnivå, levels = utbildningsniva_jmf)),
                                 skickad_x_var = "bakgrundsvariabel",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = TRUE,
                                 facet_grp = "utbildningsnivå",
                                 facet_legend_bottom = TRUE,
                                 facet_scale = "fixed",
                                 facet_kolumner = facet_kolumner,
                                 x_axis_sort_value = FALSE,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_tidsserie == TRUE){
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Fram till och med 2021, 20-64 år och data från RAMS. Från 2022, 20-65 år och data från BAS."
    
    etablering_df_tid <- etablering_df %>%
      filter(bakgrundsvariabel != "10– år",
             bakgrundsvariabel != "Inrikes född",
             utbildningsnivå == "samtliga utbildningsnivåer",
             kön == "män och kvinnor")
    
    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    etablering_df_tid$bakgrundsvariabel <- factor(etablering_df_tid$bakgrundsvariabel, levels = c("0–1 år","2–3 år",
                                                                                                  "4–9 år"))
    
    diagramtitel <- paste0("Andel förvärvsarbetande bland utrikes födda i Dalarna efter vistelsetid i Sverige")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("etablering_tid.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =etablering_df_tid,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "bakgrundsvariabel",
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_sort_value = FALSE,
                                 x_axis_lutning = 45,
                                 y_axis_100proc = TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
}

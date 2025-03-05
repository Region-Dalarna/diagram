diag_etablering_diverse_scb <- function(region = "20", # Enbart ett i taget.
                                        diag_alla_lan = TRUE, # Skapar ett diagram där länen jämförs för för vald vistelsetid
                                        vald_vistelsetid = "10- år", # Vistelsetid som ska visas i länsdiagrammet. Finns även: "0-1 år", "2-3 år", "4-9 år"
                                        diag_tidsserie = TRUE, # Skapar ett diagram
                                        diag_facet = TRUE,
                                        visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                        logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                        diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                                        output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                        skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                        excel_mapp = NA,                                   # mapp där excelfil ska sparas, NA = sparas ingen fil
                                        returnera_data_rmarkdown = FALSE,
                                        demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  #
  # Tre diagram kopplade till invandringsetablering som används i Rus-uppföljningen
  #
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_IntGr1LanKonUtb_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_IntGr1KomKonUtb_ny_BAS_scb.R")
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 pxweb,
                 readxl) 
  
  if(diag_alla_lan) {
    region_fokus <- region
    region <- hamtaAllaLan(tamedriket = FALSE)}else {
      region_fokus <- region
    }
  
  skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region) 
  # Före 2022
  etablering_2021 <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb(region_vekt = region,
                                                                                kon_klartext = "*",
                                                                                utbniv_klartext = "samtliga utbildningsnivåer",
                                                                                bakgrvar_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                                cont_klartext = "Andel förvärvsarbetande (ny definition från och med 2019)",
                                                                                tid_koder = "*") %>% 
    rename(Andel_forvarvsarbetande = `Andel förvärvsarbetande (ny definition från och med 2019)`)
  
  # 2022 och senare
  etablering_2022_ <- hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_tid_scb_ny(region_vekt = region,
                                                                                    kon_klartext = "*",
                                                                                    utbniv_klartext = "samtliga utbildningsnivåer",
                                                                                    bakgrvar_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                                                    cont_klartext = "Andel sysselsatta",
                                                                                    tid_koder = "*") %>% 
    rename(Andel_forvarvsarbetande = `Andel sysselsatta`)
  
  etablering <- rbind(etablering_2021,etablering_2022_) %>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  if(returnera_data_rmarkdown == TRUE){
    assign("etablering", etablering, envir = .GlobalEnv)
  }
  
  #diag_fargvektor <- if (all(is.na(diag_fargvektor)) & exists("diagramfarger")) diagramfarger("rus_sex") else c("darkred", "yellow", "darkgreen")
  
  gg_list <- list()
  
  if(diag_tidsserie == TRUE){
    
    diagramtitel <- glue("Etablering på arbetsmarknaden efter vistelsetid för invandrade i {skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region)}")
    diagramfil <- glue("etablering_vistelsetid_{skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region)}.png")
    
    
    gg_obj <- SkapaStapelDiagram(skickad_df = etablering %>% 
                                   filter(region == skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region), 
                                          kön %in% c("män och kvinnor")) %>% 
                                   mutate(år = år %>% as.character(),
                                          bakgrundsvariabel = bakgrundsvariabel %>% str_remove("vistelsetid ")) %>% 
                                   filter(år > (max(as.integer(år))-12)),
                                 skickad_x_var = "bakgrundsvariabel",
                                 skickad_y_var = "Andel_forvarvsarbetande",
                                 skickad_x_grupp = "år",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("rus_gradient"),
                                 manual_y_axis_title = "procent",
                                 procent_0_100_10intervaller = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 legend_rader = 2,
                                 legend_byrow = TRUE,
                                 x_axis_sort_value = TRUE,
                                 vand_sortering = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  }
  
  if(diag_facet == TRUE){
    diagramtitel <- glue("Etablering på arbetsmarknaden efter vistelsetid för invandrade i {skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region)}")
    diagramfil <- glue("etablering_vistelsetid_kon_{skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region)}.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = etablering %>% 
                                   filter(region == skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region), 
                                          kön %in% c("män","kvinnor")) %>% 
                                   mutate(år = år %>% as.character(),
                                          bakgrundsvariabel = bakgrundsvariabel %>% str_remove("vistelsetid "),
                                          kön = str_to_title(kön)) %>% 
                                   filter(år > (max(as.integer(år))-12)),
                                 skickad_x_var = "bakgrundsvariabel",
                                 skickad_y_var = "Andel_forvarvsarbetande",
                                 skickad_x_grupp = "år",
                                 legend_rader = 2,
                                 legend_byrow = TRUE,
                                 diagram_facet = TRUE,
                                 facet_grp = "kön",                     
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("rus_gradient"),
                                 manual_y_axis_title = "procent",
                                 procent_0_100_10intervaller = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 x_axis_sort_value = TRUE,
                                 vand_sortering = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  }
  
  if(diag_alla_lan == TRUE){
    
    
    diagram_titel = paste0("Arbetsmarknadsetablering med ",vald_vistelsetid, "s vistelsetid i Sverige år ",max(etablering$år))
    diagram_capt = paste0("Källa: SCB\n Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andelen etablerade på arbetsmarknaden efter  ",vald_vistelsetid, "s vistelsetid i landet")
    diagramfil <- ("etablering_vistelsetid_allalan.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = etablering %>% 
                                   mutate( fokus = ifelse(region == skapa_kortnamn_lan(hamtaregion_kod_namn(region_fokus)$region), "1", "0"),
                                           år = år %>% as.character(),
                                           bakgrundsvariabel = bakgrundsvariabel %>% str_remove("vistelsetid "),
                                           region = skapa_kortnamn_lan(region)) %>%
                                   filter(år==max(år), 
                                          kön== "män och kvinnor", 
                                          bakgrundsvariabel == vald_vistelsetid),
                                 skickad_x_var = "region",
                                 skickad_y_var = "Andel_forvarvsarbetande",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_lutning = 45,
                                 manual_color = diagramfarger("rus_tva_fokus"),
                                 manual_y_axis_title = "procent",
                                 procent_0_100_10intervaller = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 x_axis_sort_value = TRUE,
                                 x_var_fokus= "fokus",
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  }
  
  return(gg_list)
  
}

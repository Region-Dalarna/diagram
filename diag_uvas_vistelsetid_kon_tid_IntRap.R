diag_UVAS_bakgrund_vistelsetid <- function(region = "20", # Enbart ett i taget.
                                           diag_vistelsetid = TRUE,
                                           diag_utbniva = TRUE,
                                           #visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                           diag_senaste_ar = TRUE,
                                           diag_tidsserie = TRUE,
                                           valda_farger = diagramfarger("rus_sex"),
                                           #logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet. Krävs om visa_logga_i_diagram är TRUE. Om NA så blir det en konstig prick i diagrammet.
                                           output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                           skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                           returnera_data_rmarkdown = FALSE,
                                           demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  #
  # Två diagram för unga som varken arbetar eller studerar kopplat till vistelsetid i Sverige
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
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_andel_studerande_UVAS_region_kon_bakgrund_tid_IntGr8LanKON1N_scb.R")
  
  valt_lan <- skapa_kortnamn_lan(hamtaregion_kod_namn(region)$region)
  # if (!require("pacman")) install.packages("pacman")
  # pacman::p_load(tidyverse,
  #                pxweb,
  #                readxl)
  
  
  
  # Hämtar data
  UVAS_df <- hamta_UVAS_mm_region_kon_bakgrund_tid_scb  (region_vekt = region,
                                                         kon_klartext = "*",
                                                         bakgrund_klartext = c("födelseregion: Sverige","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
                                                         cont_klartext = c("Andel personer 16-19 år som varken förvärvsarbetar eller studerar, procent", "Andel personer 20-25 år som varken förvärvsarbetar eller studerar, procent"),
                                                         tid_koder = "*") %>%
    mutate(variabel = case_when(
      variabel == "vistelsetid 0-1 år" ~ "0-1 år",
      variabel == "vistelsetid 2-3 år" ~ "2-3 år",
      variabel == "vistelsetid 4-9 år" ~ "4-9 år",
      variabel == "vistelsetid 10- år" ~ "10- år",
      variabel == "födelseregion: Sverige" ~ "Inrikes född",
      TRUE ~ variabel
    ))
  
  
  if(returnera_data_rmarkdown == TRUE){
    assign("UVAS_df", UVAS_df, envir = .GlobalEnv)
  }
  
  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  UVAS_df$variabel <- factor(UVAS_df$variabel, levels = c("0-1 år","2-3 år",
                                                          "4-9 år","10- år",
                                                          "Inrikes född"))
  
  if(diag_senaste_ar){
    
    diagramtitel <- glue("Unga som varken arbetar eller studerar i {valt_lan} år {max(UVAS_df$år)} efter vistelsetid")
    diagramfilnamn <- paste0("UVAS_vistelsetid_inrikes_senastear_",valt_lan,".png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =UVAS_df %>%
                                   filter(kön != "män och kvinnor",
                                          år == max(år)) %>% 
                                   mutate(sysselsattning = str_extract(sysselsattning, "\\d{2}-\\d{2} år")),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "kön",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = diagramfarger("kon"),
                                 diagram_facet = TRUE,
                                 facet_grp = "sysselsattning",
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Vistelsetid i Sverige",
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  if(diag_tidsserie){
    
    diagramtitel <- glue("Unga som varken arbetar eller studerar i {valt_lan} efter vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("UVAS_vistelsetid_inrikes_tid_",valt_lan,".png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- SkapaStapelDiagram(skickad_df =UVAS_df %>%
                                   filter(kön == "män och kvinnor",
                                          variabel %in% c("0-1 år","2-3 år","4-9 år")) %>% 
                                   mutate(sysselsattning = str_extract(sysselsattning, "\\d{2}-\\d{2} år")),
                                 skickad_x_var = "år",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel",
                                 # manual_x_axis_text_vjust=0.9,
                                 manual_color = valda_farger,
                                 diagram_facet = TRUE,
                                 facet_grp = "sysselsattning",
                                 facet_legend_bottom = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "procent",
                                 #manual_x_axis_title = "Vistelsetid i Sverige",
                                 facet_scale = "fixed",
                                 facet_rader = 2,
                                 y_axis_100proc = TRUE,
                                 x_axis_lutning = 45,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
}

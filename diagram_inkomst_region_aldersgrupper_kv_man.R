diag_inkomst_scb <- function(regionvekt = "20", # Enbart ett i taget. går även att välja kommuner, men då genereras inget kommundiagram
                             visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                             logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                             output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                             inkomst_typ = "Medianinkomst, tkr", # Finns "Medianinkomst, tkr", "Medelinkomst, tkr". Max 1 åt gången
                             diag_tid = TRUE,
                             diag_linje = TRUE,
                             diag_kommun = TRUE,
                             skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                             alder_klartext = c("20-64 år"),			 #  Finns: "20+ år", "20-64 år", "20-65 år", "65+ år", "66+ år". OBS!! Funkar ej med "*"
                             returnera_data_rmarkdown = FALSE
) {
  
  
  # =======================================================================================================================
  #
  # Tre diagram per åldersgrupp för inkomst. Finns på såväl län som kommunnivå. Används i första hand rapporten "Kvinnor och män i Dalarna".
  #
  #
  # =======================================================================================================================
  
 
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_forvarvsinkomst_region_kon_fodelseregion_vistelsetid_HE0110_scb.R")
  
  # Före 2022
  forvarvsinkomst_df <- hamta_forvarvsinkomst_region_kon_fodelseregion_vistelsetid_scb(region_vekt = hamtakommuner(regionvekt,tamedriket = FALSE),
                                                                                       kon_klartext = "*",
                                                                                       alder_klartext = alder_klartext,
                                                                                       fodelseregion_klartext = "samtliga",
                                                                                       vistelsetiduf_klartext = "samtliga",
                                                                                       cont_klartext = inkomst_typ,
                                                                                       tid_koder = "*") %>%
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE)) 
  
  
  if(returnera_data_rmarkdown == TRUE){
    assign("forvarvsinkomst_df", forvarvsinkomst_df, envir = .GlobalEnv)
  }
  
  gg_list <- list()
  objektnamn <- c()
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nSammanräknad förvärvsinkomst, dvs. alla skattepliktiga inkomster före skatt (dock ej kapitalinkomster)."
  
  skapa_diagram <- function(df, vald_aldersgrupp){ # Start map-funktion
    
    if(diag_tid){
      df_lan <- df %>%
        filter(regionkod == regionvekt,
               ålder == vald_aldersgrupp)
      
      variabel = sub(",.*", "", last(names(df_lan)))
      diagramtitel <- paste0(variabel," (", unique(df_lan$ålder),") i ",unique(df_lan$region)," ",max(df_lan$år))
      diagramfilnamn <- paste0(variabel,"_",str_replace_all(unique(df_lan$ålder), c(" år" = "", "\\+" = "", "-" = "_")),"_",unique(df_lan$region),"_tid.png")
      objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df_lan %>%
                                     filter(kön != "totalt"),
                                   skickad_x_var = "år", 
                                   skickad_y_var = inkomst_typ,
                                   skickad_x_grupp = "kön",
                                   manual_color = diagramfarger("kon"),
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   stodlinjer_avrunda_fem = TRUE,
                                   vand_sortering = TRUE,
                                   x_axis_lutning = 45,
                                   manual_y_axis_title = "Tusentals kronor",
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = skriv_diagrambildfil)
      
      
      gg_list <- c(gg_list, list(gg_obj))
    }
    
    
    if(diag_linje){
      df_lan <- df %>%
        filter(regionkod == regionvekt,
               ålder == vald_aldersgrupp)
      
      variabel = sub(",.*", "", last(names(forvarvsinkomst_df)))
      diagramtitel <- paste0(variabel," (", unique(df_lan$ålder),") i ",unique(df_lan$region)," ",max(df_lan$år))
      diagramfilnamn <- paste0(variabel,"_",str_replace_all(unique(df_lan$ålder), c(" år" = "", "\\+" = "", "-" = "_")),"_",unique(df_lan$region),"_tid_linje.png")
      objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
      
      gg_obj <- SkapaLinjeDiagram(skickad_df = df_lan %>%
                                    filter(kön != "totalt"),
                                  skickad_x_var = "år", 
                                  skickad_y_var = inkomst_typ,
                                  skickad_x_grupp = "kön",
                                  manual_color = diagramfarger("kon"),
                                  diagram_titel = diagramtitel,
                                  diagram_capt =  diagram_capt,
                                  stodlinjer_avrunda_fem = TRUE,
                                  berakna_index = TRUE,
                                  x_axis_lutning = 45,
                                  manual_y_axis_title = "Tusentals kronor",
                                  output_mapp = output_mapp,
                                  filnamn_diagram = diagramfilnamn,
                                  skriv_till_diagramfil = skriv_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
    }
    
    if(diag_kommun && nchar(regionvekt)<3){
      df_kommun <- df %>%
        filter(ålder == vald_aldersgrupp)
      
      variabel = sub(",.*", "", last(names(df_kommun)))
      diagramtitel <- paste0(variabel," (", unique(df_kommun$ålder),") år ",max(df_kommun$år))
      diagramfilnamn <- paste0(variabel,"_",str_replace_all(unique(df_kommun$ålder), c(" år" = "", "\\+" = "", "-" = "_")),"_",first(df_kommun$region),"_kommun.png")
      objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df_kommun %>%
                                     filter(kön != "totalt",
                                            år == max(år)),
                                   skickad_x_var = "region", 
                                   skickad_y_var = inkomst_typ,
                                   skickad_x_grupp = "kön",
                                   manual_color = diagramfarger("kon"),
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   x_axis_sort_value = TRUE,
                                   x_axis_sort_grp = 1,
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   x_axis_lutning = 45,
                                   vand_sortering = TRUE,
                                   manual_y_axis_title = "Tusentals kronor",
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = skriv_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
    }
    
    names(gg_list) <- objektnamn
    return(gg_list)
    
  }
  
  diag <- map(alder_klartext, ~ skapa_diagram(forvarvsinkomst_df, .x)) %>% flatten()
  
  return(diag)
  
}

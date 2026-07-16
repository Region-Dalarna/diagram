diag_foretagarna <- function(region_vekt = "20", # Vilken region skall vi välja
                             output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             output_mapp_data = NA, # Här hamnar data som sparas
                             filnamn_data = "data_foretagarna_ut.xlsx", # Filnamn Excel
                             spara_figur = TRUE, # Skall figuren sparas
                             valda_farger = diagramfarger("rus_tre_fokus"),
                             diag_arbetsstallen = TRUE, # Figur över arbetsstälen
                             diag_nyforetagsamma = TRUE, # Nyförtagssamma 
                             diag_foretagsamma = TRUE, # Företagssamma
                             returnera_data = FALSE){ # Skall data läggas i R-studios globala miljö
  
  # ========================================== Allmän info ============================================
  
  # Diagram för antal företagssamma, antal arbetsställen och andel företagssamma. Data hämtas automatiskt från företagarna:
  # https://www.foretagsklimat.se/ladda-ner/, Välj "Statistikfaktorer.xlsx".
  # Enbart senaste år
  #
  # Ersätter ett tidigare skript diagram_arb_stallen_mm_ejAPI.R där man behövde ladda hem data manuellt.
  # ========================================== Inställningar ============================================
  
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse,
                 readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Text till diagram
  diagram_capt <- c("Källa: SCB (via WWW.foretagsklimat.se).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Gäller arbetsställen med minst 1 anställd",
                    "Källa: UC AB (via WWW.foretagsklimat.se).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Nyföretagsamma personer är sådana som inte klassades som företagsamma förra året,\nmen som gör det under innevarande år.",
                    "Källa: UC, Kreicbergs Utredning & Opinion AB (via WWW.foretagsklimat.se).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: De invånare som innehar F-skattesedel, är delägare i ett aktivt handelsbolag\neller är vd eller styrelseordförande i ett aktivt aktiebolag räknas som företagsamma.\n")
  
  # Läser in län som vi är intresserade av
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  list_data <- list() # Skapar en tom lista som används för att spara data 
  objektnamn <- c()
  
  # ========================================== Läser in data ============================================
  # Vilka kommuner skall väljas
  kommuner = skapa_kortnamn_lan(hamtaregion_kod_namn(hamtakommuner(region_vekt,tamedlan = TRUE,tamedriket = TRUE))$region,byt_ut_riket_mot_sverige = TRUE)
  
  # Url till datafilen
  url <- "https://www.foretagsklimat.se/ladda-ner/mgj4d2_statistikfaktorerxlsx_1231710.html/Statistik.xlsx?forceDownloadFileName=Statistik.xlsx"
  
  # Laddar ned filen och läser in den som en temporär fil
  tmpfile <- tempfile(fileext = ".xlsx")
  curl::curl_download(url, tmpfile)
  tmp <- tempfile()
  unzip(tmpfile, exdir = tmp)

  # Tar hem ett grunddataset som används i samtliga 
  grunddata_df <- read_xlsx(tmpfile, sheet = "Sheet1", col_types = "text") %>% 
    # reshape year columns into rows
    pivot_longer(
      cols = matches("^\\d{4}$"),      # matches 2003, 2004, ... whatever years you have
      names_to = "year",
      values_to = "varde"
    ) %>%
      mutate(year = as.integer(year),
             varde = as.numeric(varde),
             Kommun = skapa_kortnamn_lan(Kommun)) %>%
      # drop years where EVERY kommun is NA, within a given Faktor + Delserie
        group_by(Faktor, Delserie, year) %>%
          filter(!all(is.na(varde))) %>%
            ungroup() %>% 
              filter(Kommun %in% kommuner)
  # ===================================================================================================
  
  # Antal arbetsställen per 1000 invånare
  if(diag_arbetsstallen==TRUE){
    
    # Väljer ut data för arbetsställen
    arbetsstallen_df <- grunddata_df %>%
      filter(Faktor == "Arbetsställen",
             Delserie == "Minst en anställd") %>% 
        filter(year == max(year)) %>% 
          select(-Kommungrupp) %>% 
            rename(Arb_stallen = varde)
 
    # Returnerar till global environment
    if(returnera_data == TRUE){
      assign("Arbetsstallen", arbetsstallen_df, envir = .GlobalEnv)
    }
    
    # Sparar till Excel
    if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Arbetsstallen" = arbetsstallen_df))
    }
    
    diagramtitel <- paste0("Antal privata arbetsställen per 1000 invånare i ",skapa_kortnamn_lan(na.omit(unique(arbetsstallen_df$Län)))  ," år " ,max(arbetsstallen_df$year))
    diagramfilnamn <- paste0("antal_arbetsstallen_",skapa_kortnamn_lan(na.omit(unique(arbetsstallen_df$Län))),".png")
    objektnamn <- c(objektnamn,"arbetsställen_",skapa_kortnamn_lan(na.omit(unique(arbetsstallen_df$Län))))
    
    gg_obj <- SkapaStapelDiagram(skickad_df =arbetsstallen_df %>% 
                                   mutate(fokus = ifelse(Kommun == "Sverige", 2,
                                                         ifelse(Kommun == skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region),1,0))),
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Arb_stallen",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = valda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[1],
                                 manual_y_axis_title = "Antal arbetsställen per 1000 invånare",
                                 diagram_facet = FALSE,
                                 x_var_fokus = "fokus",
                                 x_axis_lutning = 45,
                                 x_axis_sort_value = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  # Antal nyföretagssamma individer per 1000 invånare
  if(diag_nyforetagsamma==TRUE){
    
    # Väljer ut data för nyföretagssamhet
    nyforetagsamma_df <- grunddata_df %>%
      filter(Faktor == "Nyföretagsamhet",
             Delserie == "Antal nyföretagsamma individer") %>% 
      filter(year == max(year)) %>% 
      select(-Kommungrupp) %>% 
      rename(Antal_nyforetagsamma = varde)
    
    if(returnera_data == TRUE){
      assign("Nyföretagssamma", nyforetagsamma_df, envir = .GlobalEnv)
    }
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Nyforetagsamma" = nyforetagsamma_df))
    }
    
    diagramtitel <- paste0("Antal nyföretagsamma per 1000 invånare i ",skapa_kortnamn_lan(na.omit(unique(nyforetagsamma_df$Län)))  ," år " ,max(nyforetagsamma_df$year))
    diagramfilnamn <- paste0("antal_nyforetagsamma_",skapa_kortnamn_lan(na.omit(unique(nyforetagsamma_df$Län))),".png")
    objektnamn <- c(objektnamn,"nyföretagssamma_",skapa_kortnamn_lan(na.omit(unique(nyforetagsamma_df$Län))))

    gg_obj <- SkapaStapelDiagram(skickad_df =nyforetagsamma_df %>% 
                                   mutate(fokus = ifelse(Kommun == "Sverige", 2,
                                                         ifelse(Kommun == skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region),1,0))),
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Antal_nyforetagsamma",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = valda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[2],
                                 manual_y_axis_title = "Antal nyföretagsamma per 1000 invånare",
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 45,
                                 x_var_fokus = "fokus",
                                 x_axis_sort_value = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  # Andel företagssamma
  if(diag_foretagsamma==TRUE){
    
    # Väljer ut data för företagssamhet
    foretagsamma_df <- grunddata_df %>%
      filter(Faktor == "Företagsamhet",
             Delserie == "Andel företagsamma individer") %>% 
      filter(year == max(year)) %>% 
      select(-Kommungrupp) %>% 
      rename(Andel_foretagsamma = varde)

    
    if(returnera_data == TRUE){
      assign("företagssamma", foretagsamma_df, envir = .GlobalEnv)
    }
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Foretagsamma" = foretagsamma_df))
    }
    
    diagramtitel <- paste0("Andel företagsamma i ",skapa_kortnamn_lan(na.omit(unique(foretagsamma_df$Län)))  ," år " ,max(foretagsamma_df$year))
    diagramfilnamn <- paste0("foretagsamma_andel_",skapa_kortnamn_lan(na.omit(unique(foretagsamma_df$Län))),".png")
    objektnamn <- c(objektnamn,"Andel_företagsamma_",skapa_kortnamn_lan(na.omit(unique(foretagsamma_df$Län))))

    gg_obj <- SkapaStapelDiagram(skickad_df =foretagsamma_df %>% 
                                   mutate(fokus = ifelse(Kommun == "Sverige", 2,
                                                         ifelse(Kommun == skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region),1,0))) ,
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Andel_foretagsamma",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = valda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[3],
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_y_axis_title = "procent",
                                 x_var_fokus = "fokus",
                                 diagram_facet = FALSE,
                                 x_axis_lutning = 45,
                                 x_axis_sort_value = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  # Sparar data
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }
  
  names(gg_list) <- objektnamn
  
  return(gg_list)
}

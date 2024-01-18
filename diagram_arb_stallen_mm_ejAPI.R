diag_foretagarna <- function(region_vekt = "20", # Vilken region skall vi välja
                             output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             output_mapp_data = NA, # Här hamnar data som sparas
                             filnamn_data = "data_foretagarna_ut.xlsx", # Filnamn Excel
                             returnera_figur = TRUE, # Skall en figur returneras
                             spara_figur = TRUE, # Skall figuren sparas
                             valda_farger = diagramfarger("rus_tre_fokus"),
                             valda_farger_foretagssamma = diagramfarger("rus_sex")[1],
                             diag_arbetsstallen = TRUE, # Figur över arbetsstälen
                             diag_nyforetagsamma = TRUE, # Nyförtagssamma 
                             diag_foretagsamma = TRUE, # Företagssamma
                             returnera_data = FALSE){ # Skall data läggas i R-studios globala miljö
  
  # ========================================== Allmän info ============================================
  
  # Diagram för antal företagssamma, antal arbetsställen och andel företagssamma. Data hämtas från företagarna:
  # https://www.foretagsklimat.se/downloads
  # Enbart senaste år
  # Data uppdaterades senast 2023-11-16
  # ========================================== Inställningar ============================================
  
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Text till diagram
  diagram_capt <- c("Källa: SCB.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Gäller arbetsställen med minst 1 anställd",
                    "Källa: UC AB.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Nyföretagsamma personer är sådana som inte klassades som företagsamma förra året, men som gör det under innevarande år.",
                    "Källa: UC, Kreicbergs Utredning & Opinion AB\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: De invånare som innehar F-skattesedel, är delägare i ett aktivt handelsbolag\neller är vd eller styrelseordförande i ett aktivt aktiebolag räknas som företagsamma.\n")
  
  # Läser in län som vi är intresserade av
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  list_data <- list() # Skapar en tom lista som används för att spara data 
  objektnamn <- c()
  # ========================================== Läser in data ============================================
  # Vilka kommuner skall väljas
  kommuner = hamtaregion_kod_namn(hamtakommuner(region_vekt,tamedlan = FALSE,tamedriket = FALSE))[2]
  
  if(diag_arbetsstallen==TRUE){
    
    arbetsstallen_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/Arbetsställen_2002-2023.xlsx",startRow=5)
    
    # Pivoterar data och för vissa justeringar
    arbetsstallen_df<-arbetsstallen_df %>%
      select(-3) %>% 
      pivot_longer(3:(ncol(arbetsstallen_df)-1),names_to="year",values_to = "Arb_stallen") %>% 
      rename("Kategorier"=Delserie.1)
    
    # Filtrerar ut kommuner. Vill ha arbetsställen med minst 1 anställd
    
    arbetsstallen_df_kommun<-arbetsstallen_df %>% 
      filter(Kommun %in% kommuner$region,Kategorier == "Minst en anställd",year %in% max(year))
    
    # Beräknar genomsnittet för valt län
    arbetsstallen_df_kommun_sum <- arbetsstallen_df_kommun %>%
      group_by(year,Kategorier) %>%
      summarize(Arb_stallen = mean(Arb_stallen)) %>% 
      mutate("Kommun" = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region))
    
    # Beräknar genomsnittet för riket
    arbetsstallen_df_riket <- arbetsstallen_df %>%
      filter(Kategorier == "Minst en anställd",year == max(year)) %>%
      group_by(year,Kategorier) %>%
      summarize(Arb_stallen=mean(Arb_stallen)) %>% 
      mutate("Kommun" = "Sverige")
    
    arbetsstallen_df_utskrift<-rbind(arbetsstallen_df_kommun,arbetsstallen_df_kommun_sum,arbetsstallen_df_riket)
    
    if(returnera_data == TRUE){
      assign("Arbetsstallen", arbetsstallen_df_utskrift, envir = .GlobalEnv)
    }
    
    if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Arbetsstallen" = arbetsstallen_df_utskrift))
    }
    
    diagramtitel <- paste0("Antal privata arbetsställen per 1000 invånare år ",max(arbetsstallen_df_utskrift$year))
    diagramfilnamn <- paste0("antal_arbetsstallen.png")
    objektnamn <- c(objektnamn,"arbetsställen")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =arbetsstallen_df_utskrift %>% 
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

  if(diag_nyforetagsamma==TRUE){
    
    nyforetagsamma_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/Nyföretagsamhet_2002-2023.xlsx",startRow=5)
    # Pivoterar data och gör vissa justeringar
    nyforetagsamma_df <- nyforetagsamma_df %>%
      select(-3) %>% 
      pivot_longer(3:(ncol(nyforetagsamma_df)-1),names_to = "year",values_to = "Antal_nyforetagsamma") %>% 
      rename("Kategorier" = Delserie.1)
    
    # Filtrerar ut Dalarnas kommuner
    nyforetagsamma_df_kommun <- nyforetagsamma_df %>% 
      filter(Kommun %in% kommuner$region,Kategorier== "Antal nyföretagsamma individer",year %in% max(year))
    
    # Beräknar genomsnittet för Dalarna
    nyforetagsamma_df_kommun_sum <- nyforetagsamma_df_kommun %>%
      group_by(year,Kategorier) %>%
      summarize(Antal_nyforetagsamma = mean(Antal_nyforetagsamma)) %>% 
      mutate("Kommun" = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region))
    
    # Beräknar genomsnittet för riket
    nyforetagsamma_df_riket <- nyforetagsamma_df %>%
      filter(Kategorier == "Antal nyföretagsamma individer",year %in% max(year)) %>% 
      group_by(year,Kategorier) %>%
      summarize(Antal_nyforetagsamma = mean(Antal_nyforetagsamma)) %>% 
      mutate("Kommun" = "Sverige")
    
    nyforetagsamma_df_utskrift<-rbind(nyforetagsamma_df_kommun,nyforetagsamma_df_kommun_sum,nyforetagsamma_df_riket)
    
    if(returnera_data == TRUE){
      assign("Nyföretagssamma", nyforetagsamma_df_utskrift, envir = .GlobalEnv)
    }

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Nyforetagsamma" = nyforetagsamma_df_utskrift))
    }
    
    diagramtitel <- paste0("Antal nyföretagsamma per 1000 invånare i Dalarnas län ",max(nyforetagsamma_df_utskrift$year))
    diagramfilnamn <- paste0("antal_nyforetagsamma.png")
    objektnamn <- c(objektnamn,"nyföretagssamma")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =nyforetagsamma_df_utskrift %>% 
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

  if(diag_foretagsamma==TRUE){
    
    foretagsamma_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/Företagsamhet_2002-2023.xlsx",startRow=5)

    # Pivoterar data och gör vissa justeringar
    foretagsamma_df <- foretagsamma_df %>%
      select(-3) %>% 
      pivot_longer(3:(ncol(foretagsamma_df)-1),names_to="year",values_to = "Andel_foretagsamma") %>% 
      rename("Kategorier"=Delserie.1)
    
    # Filtrerar ut kommuner
    foretagsamma_df_kommun <- foretagsamma_df %>% 
      filter(Kommun %in% kommuner$region,Kategorier == "Andel företagsamma individer",year %in% max(year))
    
    if(returnera_data == TRUE){
      assign("företagssamma", foretagsamma_df_kommun, envir = .GlobalEnv)
    }

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Foretagsamma" = foretagsamma_df_kommun))
    }
    
    diagramtitel <- paste0("Andel företagsamma ",max(foretagsamma_df_kommun$year))
    diagramfilnamn <- paste0("foretagsamma_andel.png")
    objektnamn <- c(objektnamn,"Andel_företagsamma")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =foretagsamma_df_kommun ,
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Andel_foretagsamma",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = valda_farger_foretagssamma,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[3],
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_y_axis_title = "procent",
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
  
  if (returnera_figur == TRUE) return(gg_list)
}

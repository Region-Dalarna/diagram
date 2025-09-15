diagram_utrikes_fodda_tidsserie <-function(region_vekt = c("20"),# Max 1, län
                                           kon_klartext = NA, # #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
                                           diag_antal = TRUE, # Hela populationen
                                           diag_forandring_kommuner = TRUE, # Hela populationen
                                           diag_forandring_lan = TRUE, # Förändring för valt åldersspann
                                           diag_forandring_prognos = TRUE, # Prognos för valt åldersspann, enbart län och enbart båda könen
                                           diag_antal_uppdelat = TRUE,
                                           output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                           spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                           fodelseregion_klartext = "*", # NA = tas inte med i uttaget,  Finns: "Född i Sverige", "Utrikes född"
                                           tid_koder = "*", # Finns från 2000 och framåt
                                           visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                           logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet. 
                                           ta_bort_diagramtitel = FALSE,
                                           ta_bort_caption = FALSE,
                                           x_axis_storlek = 10.5,
                                           alder_grupp = c(16,65), # Spann som skall användas i diagrammen diag_forandring_lan respektive _prognos. Vill man ha 16-64 år skriv c(16,65)
                                           prognos_ar = 2034, # Prognosår
                                           returnera_figur = TRUE, # Returnerar en figur
                                           valda_farger = diagramfarger("rus_sex"),
                                           returnera_data = FALSE) # Skall data returneras)
{
  
  ## =================================================================================================================
  # Funktion som skapar fem diagram, två för antal utrikes födda i valt län (enbart utrikes eller utrikes och inrikes), ett för förändring i utrikes födda mellan första och sista år i länets kommuner,
  # ett för förändring i utrikes födda i valt län och ett för förändring i utrikes födda i valt län för åldersgruppen 16-64 år.
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_region_alder_kon_fodelseregion_tid_InrUtrFoddaRegAlKon_scb.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()
  region_namn <- skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  # Används till diagrammet förändring län
  region_vekt_lan = region_vekt
  
  if(diag_forandring_kommuner) region_vekt = hamtakommuner(region_vekt,tamedriket = FALSE)
  
  # Hämta data
  antal_inrikes_utrikes_df <- hamta_bef_region_alder_kon_fodelseregion_tid_scb(region_vekt = region_vekt,
                                                                               alder_koder = NA,
                                                                               kon_klartext = kon_klartext,
                                                                               tid_koder = tid_koder) %>%
    mutate(region = skapa_kortnamn_lan(region))
  
  
  if(diag_antal){
    antal_utrikes_region_df <- antal_inrikes_utrikes_df %>% filter(födelseregion == "utrikes född",region == region_namn)
    
    if(returnera_data == TRUE){
      assign("antal_utrikes_region_df", antal_utrikes_region_df, envir = .GlobalEnv)
    }
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = paste0("Antal utrikes födda i ",region_namn)
    diagramfilnamn <- paste0("utrikes_fodda_antal_",region_namn,".png")
    
    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }
    
    if(ta_bort_caption){
      diagram_capt = ""
    }
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_utrikes_region_df ,
                                 skickad_x_var = "år",
                                 skickad_y_var = "Antal",
                                 manual_color = valda_farger,
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 x_axis_storlek = x_axis_storlek,
                                 stodlinjer_avrunda_fem = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 manual_y_axis_title = "",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_antal_uppdelat){
    
    if(returnera_data == TRUE){
      assign("antal_inrikes_utrikes_df", antal_inrikes_utrikes_df, envir = .GlobalEnv)
    }
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = paste0("Folkmängd i ",region_namn," ",min(antal_inrikes_utrikes_df$år),"-",max(antal_inrikes_utrikes_df$år))
    diagramfilnamn <- paste0("utrikes_inrikes_antal_",region_namn,".png")
    
    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }
    
    if(ta_bort_caption){
      diagram_capt = ""
    }
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_inrikes_utrikes_df %>%
                                   filter(region == region_namn) %>% 
                                   mutate(födelseregion = factor(födelseregion, c("utrikes född","född i Sverige"))),
                                 skickad_x_var = "år",
                                 skickad_y_var = "Antal",
                                 skickad_x_grupp = "födelseregion",
                                 geom_position_stack = TRUE,
                                 manual_color = valda_farger,
                                 diagram_titel = diagram_titel,
                                 legend_vand_ordning = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 x_axis_storlek = x_axis_storlek,
                                 stodlinjer_avrunda_fem = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 manual_y_axis_title = "",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_forandring_kommuner){
    
    first_year <- min(antal_inrikes_utrikes_df$år)
    last_year <- max(antal_inrikes_utrikes_df$år)
    
    # Beräknar förändring i antal
    antal_forandring_df <- antal_inrikes_utrikes_df %>%
      filter(år %in% c(min(år),max(år))) %>%
      pivot_wider(names_from = år, values_from = Antal) %>%
      mutate(forandring = get(last_year) - get(first_year))
    
    
    
    # Calculate share of utrikes födda by using a pivot wider
    # andel_utrikes_df <- antal_inrikes_utrikes_df %>%
    #   pivot_wider(names_from = födelseregion, values_from = Antal) %>%
    #     mutate(andel_utrikes = (`Utrikes född`/`Född i Sverige`)*100) %>%
    #       select(region,år,andel_utrikes)
    
    if(returnera_data == TRUE){
      assign("antal_forandring_df", antal_forandring_df, envir = .GlobalEnv)
    }
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = paste0("Befolkningsförändring ",first_year,"-",last_year," i ",region_namn)
    diagramfilnamn <- paste0("befolkningsforandring_",region_namn,".png")
    
    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }
    
    if(ta_bort_caption){
      diagram_capt = ""
    }
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_forandring_df %>%
                                   filter((region != region_namn)),
                                 skickad_x_var = "region",
                                 skickad_y_var = "forandring",
                                 skickad_x_grupp = "födelseregion",
                                 manual_color = rev(valda_farger[1:2]),
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_storlek = x_axis_storlek,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 stodlinjer_avrunda_fem = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 manual_y_axis_title = "",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_forandring_lan){
    
    antal_inrikes_utrikes_df <- hamta_bef_region_alder_kon_fodelseregion_tid_scb(region_vekt = region_vekt_lan,
                                                                                 alder_koder = "*",
                                                                                 kon_klartext = kon_klartext,
                                                                                 tid_koder = tid_koder) %>%
      mutate(region = skapa_kortnamn_lan(region),
             alder_grupper =skapa_aldersgrupper(ålder,alder_grupp)) %>%
      group_by(region,år,födelseregion,alder_grupper) %>%
      summarise(Antal = sum(Antal)) %>%
      ungroup() %>%
      filter(alder_grupper == unique(alder_grupper)[2]) # Väljer åldersgruppen i mitten av spannet, skriver man exempelvis c(16,65) blir det 16-65 år
    
    
    #antal_utrikes_region_df <- antal_inrikes_utrikes_df %>% filter(födelseregion == "Utrikes född",region == region_namn)
    
    # Beräknar förändring i antal
    antal_forandring_lan_df <- antal_inrikes_utrikes_df %>%
      group_by(födelseregion) %>%
      arrange(år,.by.group=TRUE) %>%
      mutate(forandring = Antal - lag(Antal)) %>%  # Compute difference from previous year
      ungroup()
    
    antal_forandring_lan_kumulativ <- antal_forandring_lan_df %>%
      filter(år>min(år)) %>%
      group_by(födelseregion) %>%
      arrange(år,.by.group=TRUE) %>%
      mutate(kumulativ_summa = cumsum(forandring)) %>%
      ungroup()
    
    if(returnera_data == TRUE){
      assign("antal_forandring_lan_kumulativ", antal_forandring_lan_kumulativ, envir = .GlobalEnv)
    }
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Kumulativ förändring, dvs förändringen summeras varje år"
    diagram_titel = paste0("Befolkningsförändring ",unique(antal_inrikes_utrikes_df$alder_grupper), " i ",region_namn)
    diagramfilnamn <- paste0("befolkningsforandring_lan_",region_namn,".png")
    
    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }
    
    if(ta_bort_caption){
      diagram_capt = ""
    }
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_forandring_lan_kumulativ,
                                 skickad_x_var = "år",
                                 skickad_y_var = "kumulativ_summa",
                                 skickad_x_grupp = "födelseregion",
                                 manual_color = rev(valda_farger[1:2]),
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_storlek = x_axis_storlek,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 stodlinjer_avrunda_fem = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 manual_y_axis_title = "",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_forandring_prognos){
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_befprogn_region_inrikesutrikes_kon_alder_tid_BefProgRegFakN_scb.R")
    
    befprognos_df <- hamta_befprogn_region_inrikesutrikes_kon_alder_tid_scb(region_vekt = region_vekt_lan,			# Val av region.
                                                                            inrikesutrikes_klartext = c("inrikes födda", "utrikes födda"),			 #  NA = tas inte med i uttaget,  Finns: "inrikes födda", "utrikes födda", "inrikes och utrikes födda"
                                                                            kon_klartext = "*",
                                                                            alder_koder = "*") %>% 
      mutate(region = skapa_kortnamn_lan(region),
             alder_grupper = skapa_aldersgrupper(ålder,alder_grupp)) %>%
      rename(födelseregion = `inrikes/utrikes född`) %>% 
      group_by(region,år,födelseregion,alder_grupper) %>%
      summarise(Antal = sum(Antal)) %>%
      ungroup()
    
    befprognos_df <- befprognos_df %>%           
      filter(alder_grupper == unique(befprognos_df$alder_grupper)[2])
    
    antal_forandring_prognos_df <- befprognos_df %>%
      group_by(födelseregion) %>%
      arrange(år,.by.group=TRUE) %>%
      mutate(forandring = Antal - lag(Antal)) %>%  # Compute difference from previous year
      ungroup()
    
    antal_forandring_prognos_kumulativ <- antal_forandring_prognos_df %>%
      filter(år>min(år)) %>%
      group_by(födelseregion) %>%
      arrange(år,.by.group=TRUE) %>%
      mutate(kumulativ_summa = cumsum(forandring)) %>%
      ungroup()
    
    
    if(returnera_data == TRUE){
      assign("antal_forandring_prognos_kumulativ", antal_forandring_prognos_kumulativ, envir = .GlobalEnv)
    }
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Kumulativ förändring, dvs förändringen summeras varje år"
    diagram_titel = paste0("Befolkningsprognos ",unique(befprognos_df$alder_grupper), " i ",unique(antal_forandring_prognos_kumulativ$region))
    diagramfilnamn <- paste0("befolkningsprognos_lan_",unique(antal_forandring_prognos_kumulativ$region),".png")
    
    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }
    
    if(ta_bort_caption){
      diagram_capt = ""
    }
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_forandring_prognos_kumulativ %>% 
                                   filter(år <= prognos_ar),
                                 skickad_x_var = "år",
                                 skickad_y_var = "kumulativ_summa",
                                 skickad_x_grupp = "födelseregion",
                                 manual_color = rev(valda_farger[1:2]),
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_storlek = x_axis_storlek,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 stodlinjer_avrunda_fem = FALSE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 manual_y_axis_title = "",
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

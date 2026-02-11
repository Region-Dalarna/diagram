diag_ohalsotal_sjukpenningtal <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                          diag_ohalsotal = TRUE,
                                          diag_sjukpenningtal = TRUE,
                                          output_mapp = NA,
                                          spara_diagrambildfil = FALSE,
                                          spara_dataframe_till_global_environment = FALSE){
  
  ## =================================================================================================================
  # Skript som skapar två diagram för ohälsotal och två diagram för sjukpenningtal i valt län.
  # Används i första hand i rapporten "Kvinnor och män i Dalarna"
  # Skapad av Jon Frank 2025-07-04
  # Reviderad av Peter Möller 2025-12-08
  # =============================================== Uttag ===============================================
  
  # # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         glue,
         here)
  
  if (is.na(output_mapp) & spara_diagrambildfil){
    if (file.exists(utskriftsmapp())) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Parametern 'output_mapp' måste anges om en diagrambild ska sparas.")
    }
    
  }
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R")
  
  # # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/ohm-ohalsotal/SJPohttal.xlsx","https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/ohm-sjptal/SJPsjptal.xlsx")
  
  # # Med Peters nya skript
  flik_lista = list()
  gg_list = list()

  vald_region_txt <- hamtaregion_kod_namn(region_vekt)$region %>% 
    skapa_kortnamn_lan() %>% 
    list_komma_och()
  
  vald_region_filnamn <- vald_region_txt %>% 
    tolower() %>% 
    svenska_tecken_byt_ut()
  
  lan_txt <- hamtaregion_kod_namn(str_sub(region_vekt, 1, 2))$region %>% 
    skapa_kortnamn_lan() %>% 
    list_komma_och()
  
  lan_filnamn <- lan_txt %>% 
    tolower() %>% 
    svenska_tecken_byt_ut()
  
  # skapa variabel med regionkoder för län + kommuner för vald region
  region_vekt_lan <- hamtakommuner(lan = str_sub(region_vekt, 1, 2),
                                   tamedlan = TRUE, 
                                   tamedriket = FALSE)
  
  if(diag_ohalsotal == TRUE){
    
    databas_finns <- tryCatch({
      # Din kodrad här
      ohalsotal_df <- oppnadata_hamta("forsakringskassan", "ohalsotal", 
                                               query = glue("WHERE regionkod IN ({glue_collapse(glue(\"'{region_vekt_lan}'\"), sep = ', ')}) AND ålder = 'Samtliga 16-64 år' and kön != 'Kvinnor och män'"))
      TRUE  # Om det fungerar
    }, error = function(e) {
      FALSE # Om det blir fel, tex om det inte finns någon databas
    })
    

    if (!databas_finns) {    
      
      manad_nyckel <- format(as.Date(paste0(1:12, "-01"), format = "%m-%d"), "%b")
      # Om datasetet inte finns i databasen oppna_data så hämtas det direkt från Försäkringskassan
      ohalsa_lista = hamta_excel_dataset_med_url(path[1],skippa_rader = 2)
      ohalsotal_df <- bind_rows(ohalsa_lista) %>% 
        rename_with(~ tolower(.x)) %>% 
        rename(ohalsotal = ohälsotalet) %>% 
        rename(region = kommun) %>% 
        mutate(region = if_else(region == "Riket", "00 Riket", region)) %>% 
        separate_wider_delim(region,
                             delim = " ",
                             names = c("regionkod", "region"),
                             too_many = "merge") %>%  
        select(-c(län, kolumnnamn)) %>% 
        filter(regionkod %in% region_vekt_lan,
               ålder == 'Samtliga 16-64 år',
               kön != 'Kvinnor och män') %>% 
        mutate(månad_txt = manad_nyckel[as.integer(månad)]) %>% 
        relocate(månad_txt, .after = månad) 
    }
    
    # För att kunna skriva ut i caption hur många månader som det senaste året består av
    senaste_manad <- ohalsotal_df %>% mutate(månad_namn = format(as.Date(paste0(år,"-", månad, "-01")), "%B")) %>% .$månad_namn %>% unique() %>% first()
    
    # skapa variabler med text som används i diagramtitlar och filnamn
    alder_txt <- unique(ohalsotal_df$ålder) %>% 
      str_remove("Samtliga ")
    
    ar_txt <- max(ohalsotal_df$år)
    
    # Bearbetar data
    ohalsotal_df <- ohalsotal_df %>% 
            group_by(år, regionkod, region, ålder, kön) %>% 
             summarize(Ohälsotalet_medel = mean(ohalsotal), .groups = "drop")
      
    # Omvandla kolumnnamn
    
    if(spara_dataframe_till_global_environment) {
      assign("ohalsotal_df", ohalsotal_df, envir = .GlobalEnv)
    }
    
    # Ohälsotal tidsserie
    diagram_capt_ohälsa <- glue("Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ohälsotalet: hur många dagar under en tolvmånadersperiod\nFörsäkringskassan betalar ut ersättning för nedsatt arbetsförmåga\ni förhållande till antalet försäkrade i åldrarna 16-64 år. Data för år {max(ohalsotal_df$år)} till och med {senaste_manad}.")

    diagramtitel <- paste0("Genomsnittligt ohälsotal (", alder_txt, ") per år i " , vald_region_txt)
    diagramfilnamn <- paste0("ohalsotal_", vald_region_filnamn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = ohalsotal_df %>%
                                          mutate(kön = tolower(kön)) %>% 
                                          filter(regionkod %in% region_vekt), 
                                        skickad_x_var = "år", 
                                        skickad_y_var = "Ohälsotalet_medel", 
                                        skickad_x_grupp = "kön",
                                        x_axis_lutning = 45,
                                        manual_color = diagramfarger("kon"),
                                        manual_y_axis_title = "",
                                        diagram_titel = diagramtitel,
                                        diagram_capt =  diagram_capt_ohälsa,
                                        stodlinjer_avrunda_fem = TRUE,
                                        output_mapp = output_mapp,
                                        filnamn_diagram = diagramfilnamn,
                                        skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    #Ohälsotal per kommun i samma län
    
    diagramtitel <- paste0("Genomsnittligt ohälsotal (16-64 år) i " , lan_txt," år ", ar_txt)
    diagramfilnamn <- paste0("ohalsotal_kommun_", lan_filnamn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = ohalsotal_df %>%
                                                 filter(år == max(år)) %>% 
                                                 mutate(kön = tolower(kön)), 
                                               skickad_x_var = "region", 
                                               skickad_y_var = "Ohälsotalet_medel", 
                                               skickad_x_grupp = "kön",
                                               manual_x_axis_text_vjust = 1,
                                               manual_x_axis_text_hjust = 1,
                                               x_axis_lutning = 45,
                                               x_axis_sort_value = TRUE,
                                               x_axis_sort_grp = 1,
                                               vand_sortering = TRUE,
                                               manual_color = diagramfarger("kon"),
                                               manual_y_axis_title = "",
                                               diagram_titel = diagramtitel,
                                               diagram_capt =  diagram_capt_ohälsa,
                                               stodlinjer_avrunda_fem = TRUE,
                                               diagram_facet = FALSE,
                                               berakna_index = FALSE,
                                               output_mapp = output_mapp,
                                               filnamn_diagram = diagramfilnamn,
                                               skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  if(diag_sjukpenningtal == TRUE){
    
    
    databas_finns <- tryCatch({
      # Din kodrad här
      sjukpenningtal_df <- oppnadata_hamta("forsakringskassan", "sjukpenningtal", 
                                      query = glue("WHERE regionkod IN ({glue_collapse(glue(\"'{region_vekt_lan}'\"), sep = ', ')}) AND ålder = 'Samtliga 16-64 år' and kön != 'Kvinnor och män'"))
      TRUE  # Om det fungerar
    }, error = function(e) {
      FALSE # Om det blir fel, tex om det inte finns någon databas
    })
    
    
    if (!databas_finns) {    
      
      manad_nyckel <- format(as.Date(paste0(1:12, "-01"), format = "%m-%d"), "%b")
      # Om datasetet inte finns i databasen oppna_data så hämtas det direkt från Försäkringskassan
      sjp_lista = hamta_excel_dataset_med_url(path[2],skippa_rader = 2)
      sjukpenningtal_df <- bind_rows(sjp_lista) %>% 
        rename_with(~ tolower(.x)) %>% 
        rename(sjukpenningtal = `sjukpenningtal 1.0`) %>% 
        rename(region = kommun) %>% 
        mutate(region = if_else(region == "Riket", "00 Riket", region)) %>% 
        separate_wider_delim(region,
                             delim = " ",
                             names = c("regionkod", "region"),
                             too_many = "merge") %>%  
        select(-c(län, kolumnnamn)) %>% 
        filter(regionkod %in% region_vekt_lan,
               ålder == 'Samtliga 16-64 år',
               kön != 'Kvinnor och män') %>% 
        mutate(månad_txt = manad_nyckel[as.integer(månad)]) %>% 
        relocate(månad_txt, .after = månad) 
    }
    
    # För att kunna skriva ut i caption hur många månader som det senaste året består av
    senaste_manad <- sjukpenningtal_df %>% mutate(månad_namn = format(as.Date(paste0(år,"-", månad, "-01")), "%B")) %>% .$månad_namn %>% unique() %>% first()
    
    # Bearbetar data
    sjukpenningtal_df <- sjukpenningtal_df %>% 
      group_by(år, regionkod, region, ålder, kön) %>% 
      summarize(Sjukpenningtal_medel = mean(sjukpenningtal), .groups = "drop")
    
    
    if(spara_dataframe_till_global_environment) {
      assign("sjukpenningtal_df", sjukpenningtal_df, envir = .GlobalEnv)
    }
    
    # Sjukpenningtal tidsserie
    diagram_capt_sjukpenning <- glue("Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring:Sjukpenningtalet är antalet dagar med sjukpenning och rehabiliteringspenning\nsom har betalats ut under en 12-månaders period. Den summan delas med antalet försäkrade i\nSverige som är i åldrarna 16–64 år. Data för år {max(sjukpenningtal_df$år)} till och med {senaste_manad}.")
    diagramtitel <- paste0("Genomsnittligt sjukpenningtal (16-64 år) per år i " , vald_region_txt)
    diagramfilnamn <- paste0("sjukpenningtal_", vald_region_filnamn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sjukpenningtal_df %>%
                                               filter(regionkod %in% region_vekt) %>% 
                                               mutate(kön = tolower(kön)), 
                                             skickad_x_var = "år", 
                                             skickad_y_var = "Sjukpenningtal_medel", 
                                             skickad_x_grupp = "kön",
                                             x_axis_lutning = 45,
                                             manual_color = diagramfarger("kon"),
                                             manual_y_axis_title = "",
                                             diagram_titel = diagramtitel,
                                             diagram_capt =  diagram_capt_sjukpenning,
                                             stodlinjer_avrunda_fem = TRUE,
                                             diagram_facet = FALSE,
                                             berakna_index = FALSE,
                                             output_mapp = output_mapp,
                                             filnamn_diagram = diagramfilnamn,
                                             skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")

    # Sjukpenningtal för samtliga kommuner i länet för vald region
    diagramtitel <- paste0("Genomsnittligt sjukpenningtal (16-64 år) i " , lan_txt," år ", ar_txt)
    diagramfilnamn <- paste0("sjukpenningtal_kommun_", lan_filnamn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sjukpenningtal_df %>%
                                              filter(år == max(år)) %>% 
                                              mutate(kön = tolower(kön)), 
                                            skickad_x_var = "region", 
                                            skickad_y_var = "Sjukpenningtal_medel", 
                                            skickad_x_grupp = "kön",
                                            manual_x_axis_text_vjust = 1,
                                            manual_x_axis_text_hjust = 1,
                                            x_axis_lutning = 45,
                                            x_axis_sort_value = TRUE,
                                            x_axis_sort_grp = 1,
                                            vand_sortering = TRUE,
                                            manual_color = diagramfarger("kon"),
                                            stodlinjer_avrunda_fem = TRUE,
                                            manual_y_axis_title = "",
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt_sjukpenning,
                                            output_mapp = output_mapp,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  return(gg_list)
  
}

diag_foraldrapenning_vab <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                     diag_foraldrapenning = TRUE,
                                     diag_vab = TRUE,
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     spara_diagrambildfil = FALSE,
                                     spara_dataframe_till_global_environment = FALSE){
  
  ## =================================================================================================================
  # Skript som skapar tre diagram för föräldrapenning och två diagram för vård av barn (VAB) i valt län.
  # Används i första hand i rapporten "Kvinnor och män i Dalarna"
  # Skapad av Jon Frank 2025-07-04
  # =============================================== Uttag ===============================================
  
  # # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         here)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/fp-antal-mottagare-nettodagar-belopp/FPAntalDagarBeloppLanKommun.xlsx",
           "https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/tfp-vab-antal-mottagare-belopp/tfpVabAntalDagarBeloppLanKommun.xlsx")
  
  # Med Peters nya skript
  flik_lista = list()
  
  gg_list = list()
  
  if(diag_foraldrapenning == TRUE){

    foraldrapenning_df = hamta_excel_dataset_med_url(path[1],skippa_rader = 2) %>% 
      filter(substr(Län,1,2) %in% region_vekt) %>%
        mutate(Län = str_replace(Län, "^[^\\p{L}]*", ""),
               Kommun = str_replace(Kommun, "^[^\\p{L}]*", "")) %>% 
          mutate(Kommun = ifelse(Kommun ==  hamtaregion_kod_namn(region_vekt) %>% .$region, "Samtliga kommuner",  Kommun)) %>% 
            rename(Antal_mottagare = `Antal mottagare`,
                   Andel = `Andel nettodagar per kön`)%>% 
            select(-kolumnnamn)
              
      # Omvandla kolumnnamn
    
    if(spara_dataframe_till_global_environment) {
      assign("foraldrapenning_df", foraldrapenning_df, envir = .GlobalEnv)
    }
    
    # Antal nettodagar
    diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
    diagramtitel <- paste0("Antal mottagare av föräldrapenning i  " ,skapa_kortnamn_lan(unique(foraldrapenning_df$Län)))
    diagramfilnamn <- paste0("Foraldrapenning_antal_",skapa_kortnamn_lan(unique(foraldrapenning_df$Län)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_df %>%
                                   filter(Kommun == "Samtliga kommuner",
                                          Kön != "Kvinnor och män") %>% 
                                   mutate(Kön = tolower(Kön),
                                          Antal_mottagare = as.numeric(Antal_mottagare)), 
                                 skickad_x_var = "År", 
                                 skickad_y_var = "Antal_mottagare", 
                                 skickad_x_grupp = "Kön",
                                 x_axis_lutning = 45,
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_y_axis_title = "Antal mottagare",
                                 manual_color = diagramfarger("kon"),
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # Andel nettodagar
    diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
    diagramtitel <- paste0("Andel nettodagar per kön i " ,skapa_kortnamn_lan(unique(foraldrapenning_df$Län)))
    diagramfilnamn <- paste0("Foraldrapenning_andel_",skapa_kortnamn_lan(unique(foraldrapenning_df$Län)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_df%>%
                                         filter(Kommun == "Samtliga kommuner",
                                                Kön != "Kvinnor och män") %>% 
                                         mutate(Kön = tolower(Kön),
                                                Andel = as.numeric(Andel)), 
                                       skickad_x_var = "År", 
                                       skickad_y_var = "Andel", 
                                       skickad_x_grupp = "Kön",
                                       x_axis_lutning = 45,
                                       manual_x_axis_text_vjust=1,
                                       manual_x_axis_text_hjust=1,
                                       manual_color = diagramfarger("kon"),
                                       stodlinjer_avrunda_fem = TRUE,
                                       manual_y_axis_title = "",
                                       diagram_titel = diagramtitel,
                                       diagram_capt =  diagram_capt,
                                       output_mapp = output_mapp,
                                       filnamn_diagram = diagramfilnamn,
                                       skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # Andel per kommun
    diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
    diagramtitel <- paste0("Andel nettodagar per kön i " ,skapa_kortnamn_lan(unique(foraldrapenning_df$Län)), " år ",max(foraldrapenning_df$År))
    diagramfilnamn <- paste0("Foraldrapenning_andel_kommun_",skapa_kortnamn_lan(unique(foraldrapenning_df$Län)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_df%>%
                                         filter(Kommun != "Samtliga kommuner",
                                                Kön != "Kvinnor och män",
                                                År == max(År)) %>% 
                                         mutate(Kön = tolower(Kön),
                                                Andel = as.numeric(Andel)), 
                                       skickad_x_var = "Kommun", 
                                       skickad_y_var = "Andel", 
                                       skickad_x_grupp = "Kön",
                                       x_axis_lutning = 45,
                                       manual_color = diagramfarger("kon"),
                                       manual_y_axis_title = "",
                                       manual_x_axis_text_vjust=1,
                                       manual_x_axis_text_hjust=1,
                                       x_axis_sort_value = TRUE,
                                       stodlinjer_avrunda_fem = TRUE,
                                       x_axis_sort_grp = 1,
                                       vand_sortering = FALSE,
                                       diagram_titel = diagramtitel,
                                       diagram_capt =  diagram_capt,
                                       output_mapp = output_mapp,
                                       filnamn_diagram = diagramfilnamn,
                                       skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  if(diag_vab == TRUE){
    
    vab_df = hamta_excel_dataset_med_url(path[2],skippa_rader = 2) %>% 
      filter(substr(Län,1,2) %in% region_vekt) %>%
        mutate(Län = str_replace(Län, "^[^\\p{L}]*", ""),
               Kommun = str_replace(Kommun, "^[^\\p{L}]*", "")) %>% 
          mutate(Kommun = ifelse(Kommun ==  hamtaregion_kod_namn(region_vekt) %>% .$region, "Samtliga kommuner",  Kommun)) %>%  
            rename(Antal_mottagare = `Antal mottagare`,
                   Antal_nettodagar = `Antal nettodagar`,
                   Andel = `Andel nettodagar per kön`) %>% 
              select(-kolumnnamn)
    
    if(spara_dataframe_till_global_environment) {
      assign("vab_df", vab_df, envir = .GlobalEnv)
    }
    
    # Antal uttagna nettodagar
    
    diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
    diagramtitel <- paste0("Vård av barn, antal uttagna nettodagar i " ,skapa_kortnamn_lan(unique(vab_df$Län)))
    diagramfilnamn <- paste0("vab_antal_",skapa_kortnamn_lan(unique(vab_df$Län)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = vab_df%>%
                                    filter(Kommun == "Samtliga kommuner",
                                           Kön != "Kvinnor och män") %>% 
                                    mutate(Kön = tolower(Kön),
                                           Antal_nettodagar = as.numeric(Antal_nettodagar)), 
                                  skickad_x_var = "År", 
                                  skickad_y_var = "Antal_nettodagar", 
                                  skickad_x_grupp = "Kön",
                                  x_axis_lutning = 45,
                                  manual_x_axis_text_vjust=1,
                                  manual_x_axis_text_hjust=1,
                                  manual_y_axis_title = "",
                                  manual_color = diagramfarger("kon"),
                                  diagram_titel = diagramtitel,
                                  diagram_capt =  diagram_capt,
                                  stodlinjer_avrunda_fem = TRUE,
                                  output_mapp = output_mapp,
                                  filnamn_diagram = diagramfilnamn,
                                  skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
    diagramtitel <- paste0("Vård av barn, förändring i antal uttagna nettodagar i " ,skapa_kortnamn_lan(unique(vab_df$Län)))
    diagramfilnamn <- paste0("Vab_antal_linje",skapa_kortnamn_lan(unique(vab_df$Län)),".png")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = vab_df%>%
                                         filter(Kommun == "Samtliga kommuner",
                                                Kön != "Kvinnor och män") %>% 
                                         mutate(Kön = tolower(Kön),
                                                `Antal nettodagar` = as.numeric(Antal_nettodagar),
                                                "år" = År), 
                                       skickad_x_var = "år", 
                                       skickad_y_var = "Antal nettodagar", 
                                       skickad_x_grupp = "Kön",
                                       manual_color = diagramfarger("kon"),
                                       berakna_index = TRUE,
                                       x_axis_lutning = 45,
                                       diagram_titel = diagramtitel,
                                       diagram_capt =  diagram_capt,
                                       stodlinjer_avrunda_fem = TRUE,
                                       output_mapp = output_mapp,
                                       filnamn_diagram = diagramfilnamn,
                                       skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    
  }
  
  return(gg_list)

}

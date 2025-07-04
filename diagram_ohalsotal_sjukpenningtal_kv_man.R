diag_ohalsotal_sjukpenningtal <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                          diag_ohalsotal = TRUE,
                                          diag_diag_sjukpenningtal = TRUE,
                                          output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                          spara_diagrambildfil = FALSE,
                                          spara_dataframe_till_global_environment = FALSE){
  
  ## =================================================================================================================
  # Skript som skapar två diagram för ohälsotal och två diagram för sjukpenningtal i valt län.
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
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/ohm-ohalsotal/SJPohttal.xlsx","https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/ohm-sjptal/SJPsjptal.xlsx")
  
  # Med Peters nya skript
  flik_lista = list()
  
  gg_list = list()
  
  if(diag_ohalsotal == TRUE){
    
    # Dataset är så stort att det tar ut i en lista
    ohalsa_lista = hamta_excel_dataset_med_url(path[1],skippa_rader = 2)
    
    # Extrahera dataset från lista och sätt ihop till en dataframe
    j=1
    ohalsotal_df=c()
    while(j<=length(ohalsa_lista)){
      ohalsotal_df <- rbind(ohalsotal_df,ohalsa_lista[[j]])
      j=j+1
    }
    
    # För att kunna skriva ut i caption hur många månader som det senaste året består av
    senaste_manad <- ohalsotal_df %>% mutate(månad_namn = format(as.Date(paste0(År,"-", Månad, "-01")), "%B")) %>% .$månad_namn %>% unique() %>% first()
    
    # Bearbetar data
    ohalsotal_df <- ohalsotal_df %>% 
      filter(substr(Län,1,2) %in% region_vekt,
             Ålder == "Samtliga 16-64 år") %>%
        mutate(Län = str_replace(Län, "^[^\\p{L}]*", ""),
               Kommun = str_replace(Kommun, "^[^\\p{L}]*", "")) %>% 
          mutate(Kommun = ifelse(Kommun ==  hamtaregion_kod_namn(region_vekt) %>% .$region, "Samtliga kommuner",  Kommun),
                 Ohälsotalet = as.numeric(Ohälsotalet))  %>% 
            group_by(År,Län,Kommun,Kön) %>% 
             summarize(Ohälsotalet_medel=mean(Ohälsotalet))
      
    
    # Omvandla kolumnnamn
    
    if(spara_dataframe_till_global_environment) {
      assign("ohalsotal_df", ohalsotal_df, envir = .GlobalEnv)
    }
    
    # Ohälsotal tidsserie
    diagram_capt_ohälsa <- glue("Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ohälsotalet: hur många dagar under en tolvmånadersperiod\nFörsäkringskassan betalar ut ersättning för nedsatt arbetsförmåga\ni förhållande till antalet försäkrade i åldrarna 16-64 år. Data för {max(ohalsotal_df$År)} till och med {senaste_manad}.")

    diagramtitel <- paste0("Genomsnittligt ohälsotal (16-64 år) per år i " ,skapa_kortnamn_lan(unique(ohalsotal_df$Län)))
    diagramfilnamn <- paste0("ohalsotal_",skapa_kortnamn_lan(unique(ohalsotal_df$Län)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = ohalsotal_df %>%
                                          filter(Kommun == "Samtliga kommuner",
                                                 Kön != "Kvinnor och män") %>% 
                                          mutate(Kön = tolower(Kön)), 
                                        skickad_x_var = "År", 
                                        skickad_y_var = "Ohälsotalet_medel", 
                                        skickad_x_grupp = "Kön",
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
    
    #Ohälsotal kommun
    diagramtitel <- paste0("Genomsnittligt ohälsotal (16-64 år) i " ,skapa_kortnamn_lan(unique(ohalsotal_df$Län))," år ",max(ohalsotal_df$År))
    diagramfilnamn <- paste0("ohalsotal_kommun_",skapa_kortnamn_lan(unique(ohalsotal_df$Län)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = ohalsotal_df %>%
                                                 filter(Kommun != "Samtliga kommuner",
                                                        Kön != "Kvinnor och män",
                                                        År == max(.$År)) %>% 
                                                 mutate(Kön = tolower(Kön)), 
                                               skickad_x_var = "Kommun", 
                                               skickad_y_var = "Ohälsotalet_medel", 
                                               skickad_x_grupp = "Kön",
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
    
    # Dataset är så stort att det tar ut i en lista
    sjp_lista = hamta_excel_dataset_med_url(path[2],skippa_rader = 2)
    
    # Extrahera dataset från lista och sätt ihop till en dataframe
    j=1
    sjukpenningtal_df=c()
    while(j<=length(sjp_lista)){
      sjukpenningtal_df <- rbind(sjukpenningtal_df,sjp_lista[[j]])
      j=j+1
    }
    
    # För att kunna skriva ut i caption hur många månader som det senaste året består av
    senaste_manad <- sjukpenningtal_df %>% mutate(månad_namn = format(as.Date(paste0(År,"-", Månad, "-01")), "%B")) %>% .$månad_namn %>% unique() %>% first()
    
    # Bearbetar data
    sjukpenningtal_df <- sjukpenningtal_df %>% 
      filter(substr(Län,1,2) %in% region_vekt,
             Ålder == "Samtliga 16-64 år") %>%
        mutate(Län = str_replace(Län, "^[^\\p{L}]*", ""),
               Kommun = str_replace(Kommun, "^[^\\p{L}]*", "")) %>% 
          mutate(Kommun = ifelse(Kommun ==  hamtaregion_kod_namn(region_vekt) %>% .$region, "Samtliga kommuner",  Kommun),
                 Sjukpenningtal = as.numeric(`Sjukpenningtal 1.0`))  %>% 
            group_by(År,Län,Kommun,Kön) %>% 
              summarize(Sjukpenningtal_medel=mean(Sjukpenningtal))
    
    if(spara_dataframe_till_global_environment) {
      assign("sjukpenningtal_df", sjukpenningtal_df, envir = .GlobalEnv)
    }
    
    # Sjukpenningtal tidsserie
    diagram_capt_sjukpenning <- glue("Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring:Sjukpenningtalet är antalet dagar med sjukpenning och rehabiliteringspenning\nsom har betalats ut under en 12-månaders period. Den summan delas med antalet försäkrade i\nSverige som är i åldrarna 16–64 år. Data för {max(sjukpenningtal_df$År)} till och med {senaste_manad}.")
    diagramtitel <- paste0("Genomsnittligt sjukpenningtal (16-64 år) per år i " ,skapa_kortnamn_lan(unique(sjukpenningtal_df$Län)))
    diagramfilnamn <- paste0("sjukpenningtal_",skapa_kortnamn_lan(unique(sjukpenningtal_df$Län)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sjukpenningtal_df %>%
                                               filter(Kommun == "Samtliga kommuner",
                                                      Kön != "Kvinnor och män") %>% 
                                               mutate(Kön = tolower(Kön)), 
                                             skickad_x_var = "År", 
                                             skickad_y_var = "Sjukpenningtal_medel", 
                                             skickad_x_grupp = "Kön",
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

    # Sjukpenningtal kommun
    diagramtitel <- paste0("Genomsnittligt sjukpenningtal (16-64 år) i " ,skapa_kortnamn_lan(unique(sjukpenningtal_df$Län))," år ",max(sjukpenningtal_df$År))
    diagramfilnamn <- paste0("sjukpenningtal_kommun_",skapa_kortnamn_lan(unique(sjukpenningtal_df$Län)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sjukpenningtal_df %>%
                                              filter(Kommun != "Samtliga kommuner",
                                                     Kön != "Kvinnor och män",
                                                     År == max(.$År)) %>% 
                                              mutate(Kön = tolower(Kön)), 
                                            skickad_x_var = "Kommun", 
                                            skickad_y_var = "Sjukpenningtal_medel", 
                                            skickad_x_grupp = "Kön",
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

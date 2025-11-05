diag_narstaendepenning <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   diag_stapel = TRUE,
                                   diag_forandring = FALSE,
                                   variabel = "Antal vårdare", # Finns även "Antal sjuka", "Belopp","Nettodagar" Går att välja flera
                                   spara_diagrambildfil = FALSE,
                                   spara_dataframe_till_global_environment = FALSE){
  
  ## =================================================================================================================
  # Skript som skapar två diagram för närståendepenning (stapeldiagram för antal/belopp och linjediagram). Går att få ut för de fyra variablerna:
  # "Antal vårdare", "Antal sjuka", "Belopp" och "Nettodagar". Flera kan väljas samtidigt.
  # Används i första hand i rapporten "Kvinnor och män i Dalarna"
  # Skapad av Jon Frank 2025-11-04
  # Källa: https://www.dataportal.se/datasets/547_13998
  # =============================================== Uttag ===============================================
  
  # # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         here,
         glue)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/np-antal-mottagare-belopp-dagar/NPAntalBeloppDagarLan.xlsx")
  
  # Med Peters nya skript
  flik_lista = list()

  gg_list = list()
  objektnamn <- c()
  
  narstaendepenning_df = hamta_excel_dataset_med_url(path[1],skippa_rader = 2) %>% 
    filter(substr(Län,1,2) %in% region_vekt) %>%
    mutate(Län = str_replace(Län, "^[^\\p{L}]*", "")) %>%
    select(-kolumnnamn)
  
  if(spara_dataframe_till_global_environment){
    assign("narstaendepenning_df", narstaendepenning_df, envir = .GlobalEnv)
  }
    
  skapa_diagram <- function(df, vald_variabel){
      
       df = df %>% 
        select(År,Kön,Län,all_of(vald_variabel))
       
       file_fragment <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(iconv(names(df)[ncol(df)], to = "ASCII//TRANSLIT"))))
       
       if(diag_stapel == TRUE){
      
      # Omvandla kolumnnamn
      
       if(vald_variabel == "Belopp") df$Belopp = df$Belopp/1000 # Omvandla till tusentals kronor
      
      # Antal nettodagar
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- glue("Närståendepenning, {tolower(last(names(df)))} i  " ,skapa_kortnamn_lan(unique(df$Län)))
      diagramfilnamn <- paste0("narstaendepenning_",file_fragment,"_",skapa_kortnamn_lan(unique(df$Län)),".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df %>%
                                     filter(Kön != "Kvinnor och män") %>% 
                                     mutate(Kön = tolower(Kön)), 
                                   skickad_x_var = "År", 
                                   skickad_y_var = last(names(df)), 
                                   skickad_x_grupp = "Kön",
                                   x_axis_lutning = 45,
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_y_axis_title = ifelse(vald_variabel == "Belopp", "Tusentals kronor", ""),
                                   manual_color = diagramfarger("kon"),
                                   stodlinjer_avrunda_fem = TRUE,
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      #objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
       }
       
       if(diag_forandring == TRUE){
         
         # Omvandla kolumnnamn
         
         #if(vald_variabel == "Belopp") df$Belopp = df$Belopp/1000 # Omvandla till tusentals kronor
         
         # Antal nettodagar
         diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
         diagramtitel <- glue("Närståendepenning, förändring i {tolower(last(names(df)))} i  " ,skapa_kortnamn_lan(unique(df$Län)))
         diagramfilnamn <- paste0("narstaendepenning_forandring_",file_fragment,"_",skapa_kortnamn_lan(unique(df$Län)),".png")
         
         gg_obj <- SkapaLinjeDiagram(skickad_df = df %>%
                                        filter(Kön != "Kvinnor och män") %>%
                                        rename(år = År) %>% 
                                        mutate(Kön = tolower(Kön)), 
                                      skickad_x_var = "år", 
                                      skickad_y_var = last(names(df)), 
                                      skickad_x_grupp = "Kön",
                                      x_axis_lutning = 45,
                                      manual_y_axis_title = "Index, startår 1999",
                                      manual_color = diagramfarger("kon"),
                                      stodlinjer_avrunda_fem = TRUE,
                                      berakna_index = TRUE,
                                      diagram_titel = diagramtitel,
                                      diagram_capt =  diagram_capt,
                                      output_mapp = output_mapp,
                                      filnamn_diagram = diagramfilnamn,
                                      skriv_till_diagramfil = spara_diagrambildfil)
         
         gg_list <- c(gg_list, list(gg_obj))
         #objektnamn <- c(objektnamn,diagramfilnamn %>% str_remove(".png"))
         names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
       }
       
    return(gg_list)   
      
  }
    
  diag <- map(variabel, ~ skapa_diagram(narstaendepenning_df, .x)) %>% flatten()
  return(diag)
}

diag_demografi <-function(region = hamtakommuner("22",tamedlan = TRUE,tamedriket = TRUE), # Val av region. 
                          outputmapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                          output_mapp_data = NA, # Ändra om data skall sparas
                          filnamn_data = "medelalder_demo.xlsx",
                          diag_forsorjningskvot = TRUE, # Demografisk försörjningskvot
                          diag_medelalder = TRUE, # Medelålder
                          konsuppdelat = FALSE, # Om TRUE jämförs kön för senaste år, om FALSE jämförs första och sista år i sample
                          valda_farger = diagramfarger("rus_sex"),
                          tid = 2007:2100, # Tid. Ett högt sistavärde ger senaste år
                          spara_figur = TRUE, # Skall figuren sparas
                          returnera_figur = TRUE, # Skall figur returneras från funktion
                          returnera_data = FALSE){ # Om TRUE returneras data till R-Studios globala miljö
  
  # ===========================================================================================================  # 
  # Skript som skriver ut diagram för demografisk försörjningskvot och medelålder. Går att välja med eller utan uppdelning på kön
  # Data för län som helhet finns enbart från 2007
  # Skapad av Jon Frank
  # Senast ändrad: 2023-12-08
  # ===========================================================================================================
  
  
  # ========================================== Inställningar ============================================
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/JonFrank81/funktioner/main/func_API_alternativ.R")
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge
  list_data <- lst() # Skapa tom lista som används för att spara till Excel.

  # =============================================== API-uttag ===============================================
  
  # Hämtar data
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_medelald_demfors_kolada.R")
  
  df_list = hamta_data_medel_demo(region = region, 
                                  konsuppdelat = konsuppdelat,
                                  cont_cod = c("N00959","N00927"), 
                                  tid = tid,
                                  returnera_data = TRUE)

  
  if(diag_forsorjningskvot == TRUE){
    
    demo_df <- df_list[["Demografisk_forsorjningskvot"]] %>% 
      mutate(municipality = skapa_kortnamn_lan(byt_namn_lan_kolada(municipality),TRUE),
             value = value*100)
    
    if(returnera_data == TRUE){
      assign("demografisk_forsorjningskvot", demo_df, envir = .GlobalEnv)
    }
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Demografisk försörjningskvot" = demo_df))
    }
    
    if(konsuppdelat == FALSE){
      demo_df <- demo_df %>% 
        filter(year %in% c(min(year),max(year)))
      diag_farger = valda_farger
      diagram_titel <- "Demografisk försörjningskvot"
      
      
    } else{
      demo_df <- demo_df %>% filter(year == max(year))
      diag_farger = diagramfarger("kon")
      diagram_titel <- paste0("Demografisk försörjningskvot år ",unique(demo_df$year))
    } 
    
    
    diagram_typ <- "demo_fors"
    diagramfil <- "demo_fors.png"
    objektnamn <- c(objektnamn,diagram_typ)
    
    diagram_capt <- "Källa: SCB (via RKA/Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Den demografiska försörjningskvoten beräknas som summan av antal personer 0-19 år och antal personer 65 år och äldre dividerat med antal personer 20-64 år.\nEtt värde över 100 innebär att gruppen äldre och yngre är större än den i arbetsför ålder." 
    
    gg_obj <- SkapaStapelDiagram(skickad_df = demo_df ,
                                 skickad_x_var = "municipality", 
                                 skickad_y_var = "value", 
                                 skickad_x_grupp = ifelse(konsuppdelat == FALSE,"year","gender"),
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diag_farger,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 2,
                                 vand_sortering = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_y_axis_title= "",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = outputmapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }

  
  if(diag_medelalder == TRUE){
    
    medelalder_df <- df_list[["Medelalder"]] %>% 
      mutate(municipality = skapa_kortnamn_lan(byt_namn_lan_kolada(municipality),TRUE))
    
    if(returnera_data == TRUE){
      assign("medelalder", medelalder_df, envir = .GlobalEnv)
    }
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Medelålder" = medelalder_df))
    }
    
    if(konsuppdelat == FALSE){
      medelalder_df <- medelalder_df %>% 
        filter(year%in%c(min(year),max(year)))
      diag_farger = valda_farger
      diagram_titel <- "Medelålder"
      
      
    } else{
      medelalder_df <- medelalder_df %>% filter(year == max(year))
      diag_farger = diagramfarger("kon")
      diagram_titel <- paste0("Medelålder år ",unique(medelalder_df$year))
    } 
    
    diagram_typ <- "medelalder"
    diagramfil <- "medelalder.png"
    objektnamn <-  c(objektnamn,diagram_typ)
    diagram_capt <- "Källa: SCB (via RKA/Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\n" 
    
    gg_obj <- SkapaStapelDiagram(skickad_df = medelalder_df ,
                                 skickad_x_var = "municipality", 
                                 skickad_y_var = "value", 
                                 skickad_x_grupp = ifelse(konsuppdelat == FALSE,"year","gender"),
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diag_farger,
                                 x_axis_sort_value = TRUE,
                                 dataetiketter = FALSE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 dataetiketter_antal_dec = 1,
                                 manual_y_axis_title="Medelålder",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = outputmapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- c(objektnamn)
  
  if(returnera_figur == TRUE) return(gg_list)
  
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }
  
}

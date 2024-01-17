diagram_andel_offentligt <- function(region_vekt = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av kommuner
                                     alder_klartext = "*", # Ålder. Andra val: 16-19 år, 20-24 år, 25-34 år, 35-44 år, 45-54 år, 55-59 år, 60-64 år, 65+ år. Max 1 åt gången
                                     output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Vart hamnar figur om den skall sparas
                                     output_mapp_data = NA, # Vart hamnar data om den skall sparas. NA medför att data inte sparas
                                     filnamn_data = "andel_offentligt.xlsx", # Filnamn för sparad data
                                     vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                     spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                     returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                     returnera_data = FALSE, # True om användaren vill returnera data från funktionen
                                     diag_totalt = TRUE, # Skriver ut diagram för kön totalt
                                     diag_kon = TRUE # Skriver ut diagram uppdelat på kön
){
  
  # ===========================================================================================================
  #
  # Skript som skapar diagram för andelen som arbetar inom offentlig sektor. Funkar med och utan könsuppdelning men enbart för senaste år
  # Går även att använda olika åldersspann
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse,
         openxlsx)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  list_data <- list() # Skapar en tom lista som används för att spara data 
  objektnamn <- c() # Används för att namnge
  
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_forvarvsarbetande_sektor_SCB.R")
  options(dplyr.summarise.inform = FALSE)
  
  andel_df <- hamta_data_forv_sektor(region = region_vekt,
                                     alder_klartext = alder_klartext,
                                     output_mapp =  output_mapp_data,
                                     arbetssektor_klartext = "*",
                                     kon_klartext = c("män","kvinnor"),
                                     returnera_data = TRUE,
                                     tid = "9999")
  
  if(alder_klartext == "*") alder_klartext <- ("16-74 år")
  
  if(diag_totalt == TRUE){
  
    andel_totalt <- andel_df %>% 
      mutate(`arbetsställets sektortillhörighet` = 
                                 ifelse(`arbetsställets sektortillhörighet` %in% 
                                          c("statlig förvaltning","statliga affärsverk","primärkommunal förvaltning","regioner","övriga offentliga institutioner"),"Offentlig sektor","Övriga")) %>% 
        group_by(regionkod, region,`arbetsställets sektortillhörighet`,år) %>% 
         summarize("Förvärvsarbetande" = sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`)) %>% 
           ungroup()
  
    
    # Beräknar andelar      
    andel_totalt_utskrift <- andel_totalt %>%
      group_by(region,år) %>% 
        mutate(Andel_forv = (Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01,
             region = region %>% skapa_kortnamn_lan()) %>% 
         rename(sektor = `arbetsställets sektortillhörighet`) %>% 
          ungroup()

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Totalt" = andel_totalt_utskrift))
    }
    
    if(returnera_data == TRUE){
      assign("andel_offentligt", andel_totalt_utskrift, envir = .GlobalEnv)
    }
      
    diagram_titel <- paste0("Andel offentligt anställda (",alder_klartext,")" ," år ",unique(andel_totalt_utskrift$år))
    diagramfilnamn <- "andel_offentligt_totalt.png"
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    objektnamn = c(objektnamn,"andel_off_totalt")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = andel_totalt_utskrift  %>% 
                                   mutate(sektor = factor(`sektor`, levels = c("Offentlig sektor","Övriga")[2:1]),
                                          region = ifelse(region == "Riket","Sverige",region)),
                                 skickad_x_var = "region", 
                                 skickad_y_var = "Andel_forv", 
                                 skickad_x_grupp = "sektor",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = vald_farg,
                                 stodlinjer_avrunda_fem = TRUE,
                                 geom_position_stack = TRUE,
                                 legend_vand_ordning = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 1,
                                 manual_y_axis_title =" procent",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
   
  }
  
  if(diag_kon == TRUE){
    
    andel_kon <- andel_df %>% 
      mutate(`arbetsställets sektortillhörighet` = 
               ifelse(`arbetsställets sektortillhörighet` %in% 
                        c("statlig förvaltning","statliga affärsverk","primärkommunal förvaltning","regioner","övriga offentliga institutioner"),"Offentlig sektor","Övriga")) %>% 
      group_by(regionkod, region,kön,`arbetsställets sektortillhörighet`,år) %>% 
      summarize("Förvärvsarbetande" = sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`)) %>% 
      ungroup()
    
    
    # Beräknar andelar      
    andel_kon_utskrift <- andel_kon %>%
      group_by(region,år,kön) %>% 
        mutate(Andel_forv = (Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01,
               region = region %>% skapa_kortnamn_lan()) %>% 
          rename(sektor = `arbetsställets sektortillhörighet`) %>% 
            ungroup()

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Kön" = andel_kon_utskrift))
    }
    
    if(returnera_data == TRUE){
      assign("andel_offentligt_kon", andel_kon_utskrift, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Andel offentligt anställda (",alder_klartext,")" ," år ",unique(andel_kon_utskrift$år))
    diagramfilnamn <- "andel_offentligt_kon.png"
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    objektnamn = c(objektnamn,"andel_off_kon")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = andel_kon_utskrift  %>% 
                                   filter(sektor == "Offentlig sektor") %>% 
                                    mutate(region = ifelse(region == "Riket","Sverige",region)),
                                 skickad_x_var = "region", 
                                 skickad_y_var = "Andel_forv", 
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagramfarger("kon"),
                                 stodlinjer_avrunda_fem = TRUE,
                                 legend_vand_ordning = TRUE,
                                 vand_sortering = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 1,
                                 manual_y_axis_title =" procent",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }
  
  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
 
}



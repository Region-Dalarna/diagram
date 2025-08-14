diagram_langtidsarbetslohet_tidsserie <-function(region_vekt = "20",
                                                 output_mapp_data = NA, # Outputmapp för data
                                                 output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                                 spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                                 returnera_figur = TRUE, # Returnerar en figur
                                                 valda_farger = diagramfarger("kon"),
                                                 returnera_data = FALSE) # Skall data returneras)
{
  
  ## =================================================================================================================
  
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()
  
  # Hämtar data för långtidsarbetslöshet
  långtidsarbetslöshet <- hamta_kolada_df(kpi_id = c("N03926"),
                                          valda_kommuner = region_vekt,
                                          valda_ar = 2011:2100,
                                          konsuppdelat = TRUE) %>%
    mutate(kon = tolower(kon))
  
  if(returnera_data == TRUE){
    assign("långtidsarbetslöshet", långtidsarbetslöshet, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: Arbetsförmedlingen (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 25-64 år (årsmedelvärde år T) som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader,\ndividerat med antal invånare 25-64 år den 31/12 år T-1."
  
  diagramtitel <- "Långtidsarbetslöshet 25-64 år i Dalarna"
  diagramfilnamn <- "langtidsarbetsloshet_kolada.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = långtidsarbetslöshet,
                               skickad_x_var = "ar",
                               skickad_y_var = "varde",
                               skickad_x_grupp = "kon",
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
                               manual_y_axis_title = "procent",
                               x_axis_lutning = 0,
                               manual_color= valda_farger,
                               lagg_pa_logga = FALSE,
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}
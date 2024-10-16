diag_kompetensbrist <- function(diagram_capt =  diagram_capt <- "Källa: Tillväxtverket: Företagens villkor och verklighet.\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel småföretag som anser att tillgång till lämplig arbetskraft är ett stort hinder för tillväxt", 
                                output_mapp_figur = NA,
                                skapa_fil = TRUE,
                                start_ar = "2020",# År som senaste år skall jämföras med. Finns 2011,2014,2017 och 2020
                                returnera_data = FALSE,
                                returnera_figur = TRUE
){
  
  # ========================================== Allmän info ============================================
  # Ett diagram som visar upplevd kompetensbrist i Sveriges regioner
  #
  # Data uppdaterades senast Sommaren 2023. Ingen ny data verkar ha kommit (JF 2024-02-12)
  # Skript uppdaterad 20240212
  # Förbättringspotential: Gör så att diagram 2 kan skapas som en facet med uppdelning inrikes/utrikes
  # ========================================== Inställningar ============================================
  
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge ggplot-objekt
  
  # ========================================== Läser in data ============================================
  
  # Data nedan laddas hem från: https://tillvaxtverket.se/tillvaxtverket/statistikochanalys/statistikomforetag/foretagande/hinderfortillvaxt.1718.html
  # Välj ladda ner Excel-fil strax under figuren
  kompetensbrist_df <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/Tillgång till lämplig arbetskraft.xlsx",sheet=4) %>%
    rename("Region" = Kolumn1) %>% 
    pivot_longer(2:length(names(.)),names_to = "År",values_to = "Andel") %>% 
    mutate(Region = skapa_kortnamn_lan(Region))
  
  spara_figur = skapa_fil
  
  if(is.na(output_mapp_figur)) spara_figur = FALSE
  
  if(returnera_data == TRUE){
    assign("kompetensbrist", kompetensbrist_df, envir = .GlobalEnv)
  }
  
  diagram_titel <- paste0("Upplevd kompetensbrist i Sveriges regioner")
  diagramfil <- "kompetensbrist.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = kompetensbrist_df %>% 
                                 filter(År %in% c(start_ar,max(År))) %>% 
                                 mutate(Region = ifelse(Region == "Totalt","Sverige",Region)),
                               skickad_x_var = "Region", 
                               skickad_y_var = "Andel",
                               skickad_x_grupp = "År",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("rus_sex"),
                               diagram_titel = diagram_titel,
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 2,
                               y_axis_100proc = TRUE,
                               diagram_capt = diagram_capt,
                               x_axis_lutning = 45,
                               manual_y_axis_title = "procent",
                               berakna_index = FALSE,
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfil,
                               skriv_till_diagramfil = spara_figur)
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- "kompetensbrist"
  if(returnera_figur == TRUE) return(gg_list)
}

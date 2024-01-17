#test_list=diag_50proc_lonesumma(spara_figur = TRUE,returnera_data = TRUE)
diag_50proc_lonesumma <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  spara_figur = TRUE,
                                  returnera_data = FALSE
                                  ){
  
  # ========================================== Allmän info ============================================
  # Hur många företag krävs för att lönesumman i en kommun skall uppgå till 50 % (kumulativt). 
  # För tillfället enbart för Dalarna. Vill man ha annat län måste data hämtas via MONA. Skript finns på:
  # P1079_Gem/Jon/Sårbarhetsanalys/Ftg_50procent_lonesumma_ny_variant
  # Data uppdaterades senast 1 november 2023
  # ========================================== Inställningar ============================================
  
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  # Text till diagram
  diagram_capt_kommun <- c("Källa: SCB.\nBearbetning: Samhällsanalys, Region Dalarna.")

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung NMS-databasen)
  kommun_df <- read.xlsx("G:/skript/projekt/data/sarbarhetsanalys/1_nov_23_50_procent_lonesumma.xlsx",sheet=1)
  
  if(returnera_data == TRUE){
    assign("lonesumma_andel_50proc", kommun_df, envir = .GlobalEnv)
  }
  
  diagramtitel <- paste0("Summan av antalet företag i respektive kommun som utgör 50 procent av lönesumman ",max(kommun_df$Ar))
  diagramtitel <- str_wrap(diagramtitel,55)
  diagramfilnamn <- "50proc_lonesumma.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = kommun_df,
                               skickad_x_var = "LAKommun_namn", 
                               skickad_y_var = "cnum", 
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("rus_sex")[1],
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt_kommun,
                               manual_y_axis_title = "Antal företag",
                               stodlinjer_avrunda_fem = TRUE,
                               dataetiketter = TRUE,
                               x_axis_lutning = 45,
                               x_axis_sort_value = TRUE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- "50_proc_lonesumma"
  return(gg_list)
}

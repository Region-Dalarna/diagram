diag_matchning_lan <- function(region_vekt = "20", # Region vi är intresserade av. Gäller diagrammet 
                               output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                               output_mapp_data = NA, # Här hamnar sparad data
                               filnamn_data = "matchning.xlsx",
                               spara_figur = TRUE, # Om true sparas figuren till output_mapp
                               kon_klartext = c("män", "kvinnor"), # Alternativet är "samtliga anställda" för att undvika könsuppdelning. Vill man ha alla tre i datafilen som returneras sätts denna till "*". Det blir enbart kvinnor och män i diagrammet
                               valda_farger = diagramfarger("kon"), # Vilka färger skall användas i diagram
                               diag_lan = TRUE, # Skapar ett diagram där alla län jämförs
                               diag_bakgrund = TRUE, # Skapar ett diagram där bakgrund i samma län jämförs
                               returnera_figur = TRUE, # Skall figur returneras (i en lista)
                               returnera_data = FALSE,# Skall data returneras (till R-studios globla miljö)
                               demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                               ) {
  
  # ========================================== Allmän info ============================================
  # 1: Skapar diagram för matchningsgraden på arbetsmarknaden, dels på länsnivå, dels i ett enskilt län uppdelat på bakgrund.
  # Enbart för senaste år
  #  
  # Senast uppdaterad: Jon 2024-11-01
  # ========================================== Inställningar ============================================
  # Nödvändiga bibliotek och funktioner

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/matchning_bakgrund.png",
"https://region-dalarna.github.io/utskrivna_diagram/matchning_jmf.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Det som står under diagrammet
  diagram_capt <- "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Branschens andel av totalt antal förvärvsarbetande"
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c() 
  
  vald_region = skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
  # =============================================== API-uttag ===============================================
  
  # Hämtar data
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_matchningsgrad_lan_FA.R")
  df = hamta_data_matchning_lan_fa(region = hamtaAllaLan(tamedriket = TRUE),
                                   tid = "9999",
                                   kon_klartext = kon_klartext,
                                   alder_fodelseland = c("Sverige","Norden/EU","Afrika","Asien","Övriga_världen","totalt"),
                                   returnera_data = TRUE)
  
  
  df <- df %>% 
    rename("fodelseland" = `ålder/födelselandgrupp`,
           matchningsgrad = `Matchningsgrad, procent `) %>% 
    mutate("fodelseland" = ifelse(fodelseland %in% c("födda i Europa utom Norden och EU samt Sydamerika, Nordamerika och Oceanien"),"övriga", fodelseland)) 
  
  if(returnera_data == TRUE){
    assign("matchning_df", df, envir = .GlobalEnv)
  }
  
  # Om det finns fler än två kön i datafilen så filtreras de bort. 
  if(length(unique(df$kön))>2) df <- df %>% filter(kön %in%c("män","kvinnor"))
  
  if(diag_lan==TRUE){
    
    diagram_titel <- paste0("Matchningsgrad på arbetsmarknaden år ",max(df$år))
    diagramfil <- "matchning_jmf.png"
    objektnamn <- c(objektnamn,"matchning_jmf")
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, regionala matchningsindikatorer.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Med matchning avses matchad förvärvsgrad\ndvs. andelen anställda som har ett yrke som helt matchar deras utbildning (enligt SCB)."
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>%
                                   filter(fodelseland == "totalt",
                                          år == max(år)),
                                 skickad_x_var = "region",
                                 skickad_y_var = "matchningsgrad",
                                 skickad_x_grupp = ifelse("samtliga anställda" %in% kon_klartext,NA,"kön"),
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = valda_farger,
                                 diagram_titel = diagram_titel,
                                 diagram_capt =  diagram_capt,
                                 y_axis_100proc = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = "procent",
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  
  if(diag_bakgrund==TRUE){
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas, regionala matchningsindikatorer.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Med matchning avses matchad förvärvsgrad\ndvs. andelen anställda som har ett yrke som helt matchar deras utbildning (enligt SCB)."
    diagram_titel <- paste0("Matchning på arbetsmarknaden i " ,vald_region, " år ",max(df$år))
    diagramfil <- paste0("matchning_bakgrund.png")
    objektnamn <- c(objektnamn,"matchning_bakgrund")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =df %>%
                                   filter(år == max(år),
                                          region == vald_region,
                                          fodelseland != "totalt"),
                                 skickad_x_var = "fodelseland",
                                 skickad_y_var = "matchningsgrad",
                                 skickad_x_grupp = ifelse("samtliga anställda" %in% kon_klartext,NA,"kön"),
                                 manual_color = valda_farger,
                                 diagram_titel = diagram_titel,
                                 diagram_capt =  diagram_capt,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning =0,
                                 y_axis_100proc = TRUE,
                                 diagram_liggande = TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
  
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(df,paste0(output_mapp_data,filnamn_data))
  }
  
}

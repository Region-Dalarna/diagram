diagram_andel_offentligt <- function(region_vekt = hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av kommuner
                                     alder_klartext = "15-74 år", # Ålder. Finns: "15-24 år", "25-54 år", "55-74 år", "15-74 år", "16-64 år", "20-64 år", "16-65 år", "20-65 år" Max 1 åt gången
                                     kon_klartext = "totalt", # Finns också c("kvinnor", "män") # Styr vilket typ av diagram som skrivs ut. Enbart antingen eller
                                     output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Vart hamnar figur om den skall sparas
                                     output_mapp_data = NA, # Vart hamnar data om den skall sparas. NA medför att data inte sparas
                                     filnamn_data = "andel_offentligt.xlsx", # Filnamn för sparad data
                                     vald_farg = "rus_sex", # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                     spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                     returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                     returnera_data = FALSE # True om användaren vill returnera data från funktionen
){
  
  # ===========================================================================================================
  #
  # Skript som skapar diagram för andelen som arbetar inom offentlig sektor. Funkar med och utan könsuppdelning men enbart för senaste år
  # Går även att använda olika åldersspann.
  # Uppdaterat så att data hämtas från BAS istället för RAMS. Jag lämnar kvar det gamla skriptet diagram_andel_offentligt.R ifall det används någonstans
  # Skapat: 2024-11-15 av Jon Frank
  # ===========================================================================================================
  if(length(kon_klartext)>2){
    stop("Går enbart att välja antingen totalt eller uppdelat på kvinnor och män (kon_klartext)")
  }
  
    if (!require("pacman")) install.packages("pacman")
    p_load(pxweb,
           tidyverse,
           openxlsx)
    
    gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
    #list_data <- list() # Skapar en tom lista som används för att spara data 
    objektnamn <- c() # Används för att namnge
    
    # Data som sourcas från Region Dalarna
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_forvarvsarbetande_sektor_SCB.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sysselsatta_region_arbetssektor_kon_alder_fodelseregion_tid_SektorSyssM_scb.R")
    options(dplyr.summarise.inform = FALSE)
    
    df <- hamta_sysselsatta_region_arbetssektor_kon_alder_fodelseregion_tid_scb(region = region_vekt,
                                                                                alder_klartext = alder_klartext,
                                                                                output_mapp =  output_mapp_data,
                                                                                arbetssektor_klartext = c("samtliga sektorer","offentlig förvaltning (staten, kommun, region)"),
                                                                                fodelseregion_klartext = "totalt",
                                                                                cont_klartext = "sysselsatta efter arbetsställets belägenhet",			
                                                                                kon_klartext = kon_klartext,
                                                                                returnera_df = TRUE,
                                                                                tid = "9999")
    
    
    # Pivot wider  and calculate share
    andel_df <- df %>%
      select(-arbetssektorkod) %>% 
      rename(varde = `sysselsatta efter arbetsställets belägenhet`) %>% 
      pivot_wider(names_from = `arbetsställets sektortillhörighet`, values_from = varde) %>% 
      mutate(`Offentlig sektor` = ((`offentlig förvaltning (staten, kommun, region)`/`samtliga sektorer`)*100),
             Övriga = 100-`Offentlig sektor`) %>%
      select (-`offentlig förvaltning (staten, kommun, region)`,-`samtliga sektorer`) %>% 
      pivot_longer(cols = c("Offentlig sektor","Övriga"), names_to = "sektor", values_to = "varde") %>% 
      mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE),
             varde = varde-0.01) %>% 
      separate(månad, into = c("år","månad"), sep = "M") %>% 
      mutate(månad_namn = case_when(
        månad == "01" ~ "januari",
        månad == "02" ~ "februari",
        månad == "03" ~ "mars",
        månad == "04" ~ "april",
        månad == "05" ~ "maj",
        månad == "06" ~ "juni",
        månad == "07" ~ "juli",
        månad == "08" ~ "augusti",
        månad == "09" ~ "september",
        månad == "10" ~ "oktober",
        månad == "11" ~ "november",
        månad == "12" ~ "december",
        TRUE ~ "okänd")) 
    
    
    if(length(unique(andel_df$kön))>1){
      andel_df <- andel_df %>%
        filter(sektor == "Offentlig sektor")
      vald_farg <- "kon"
    }
    
    if(returnera_data == TRUE){
      assign("andel_offentligt", andel_df, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Andel offentligt anställda (",alder_klartext,") i ",unique(andel_df$månad_namn)," ",unique(andel_df$år))
    diagramfilnamn <- "andel_offentligt.png"
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    objektnamn = c(objektnamn,"andel_off_totalt")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = andel_df %>% 
                                   mutate(sektor = factor(`sektor`, levels = c("Offentlig sektor","Övriga")[2:1])),
                                 skickad_x_var = "region", 
                                 skickad_y_var = "varde", 
                                 skickad_x_grupp = ifelse(length(unique(andel_df$kön))>1,"kön","sektor"),
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagramfarger(vald_farg),
                                 stodlinjer_avrunda_fem = TRUE,
                                 geom_position_stack = ifelse(length(unique(andel_df$kön))>1,FALSE,TRUE),
                                 legend_vand_ordning = ifelse(length(unique(andel_df$kön))>1,FALSE,TRUE),
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 1,
                                 manual_y_axis_title ="procent",
                                 vand_sortering = ifelse(length(unique(andel_df$kön))>1,TRUE,FALSE),
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      write.xlsx(andel_df,paste0(output_mapp_data,filnamn_data))
    }
    
    names(gg_list) <- c(objektnamn)
    if(returnera_figur == TRUE) return(gg_list)
  
}

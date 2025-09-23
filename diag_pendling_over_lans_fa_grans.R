diag_pendling_over_lans_fa_grans <- function(region_vekt = hamtaAllaLan(F),            # 
                                           valt_kon = "totalt",                # 
                                           valt_ar = "9999",                   # 
                                           visa_dataetiketter = FALSE,         # dataetiketter i diagrammet
                                           diag_absoluta_tal = TRUE,           # skriv ut diagram med absoluta tal
                                           diag_procent = TRUE,                # skriv ut diagram med procent
                                           skapa_fil = TRUE, # skapa en fil dig figuren sparas
                                           returnera_figur = TRUE, # Om TRUE returneras figur som ggplot-objekt
                                           enbart_in_ut = TRUE, # TRUE om man bara vill visa in och utpendling (ej bor och arbetar i samma kommun)
                                           diagramfarg_vektor = NA, # Valda färger
                                           diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                                           output_mapp_figur = "G:/Samhällsanalys/API/Fran_R/Utskrift/", # Hit sparas figuren
                                           output_mapp_data = NA, # Hit sparas data
                                           spara_data = FALSE, # Skall data sparas
                                           filnamn_data = "pendling.xlsx",
                                           returnera_data = TRUE) {# Filnamn för sparad data
  
  # ===========================================================================================================
  #
  # Skript för att skriva ut diagram (från RAMS, SCB) med andel och antal in- och utpendlare över läns- eller FA-gräns,
  # samt även de som bor och arbetar i samma län eller FA
  # Skapad av: Peter
  # 
  # Ändrat i hämtning av data så att det automatiskt blir i long-format (det funkade inte annars) Jon 2025-09-23
  # ===========================================================================================================
  

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         httr,
         tidyverse,
         dplyr,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_pendling_lan_fa_region_utbildngrupp_kon_tid_RegionInd19U2N1_RegionInd19U2_scb.R")  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
                              
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diagramfarg_vektor))) {
    if (exists("diagramfarger", mode = "function")) {
      diagramfarg_vektor <- diagramfarger("rus_sex")
    } else {
      diagramfarg_vektor <- hue_pal()(9)
    }
  }
  
  
  vald_kommun_txt <- hamtaregion_kod_namn(region_vekt)$region %>% list_komma_och()
  vald_kommun_txt <- ar_alla_kommuner_i_ett_lan(region_vekt, returnera_text = TRUE, returtext = vald_kommun_txt)
  vald_kommun_txt <- ar_alla_lan_i_sverige(region_vekt, returnera_text = TRUE, returtext = vald_kommun_txt)
  
  vald_region_filnamn <- if (ar_alla_kommuner_i_ett_lan(region_vekt) | ar_alla_lan_i_sverige(region_vekt)) vald_kommun_txt %>% tolower() %>% byt_ut_svenska_tecken() %>% str_replace(" ", "_") else region_vekt %>% paste0(collapse = "_")
  
  url_scb <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906B/RegionInd19U2N1",
               "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM9906/AM9906O/RegionInd19U2")
  
  px_meta_list <- map(url_scb, ~ pxweb_get(.x))
  
  px_meta_enkel_list <- extrahera_unika_varden_flera_scb_tabeller(px_meta_list)
  tabell_variabler <- pxvarlist(list(title = NULL, variables = px_meta_enkel_list))
  
  # om det finns år och månader så sorterar vi dessa i listan så det blir snyggare när de listas som möjliga värden i parameterlistan
  if ("tid" %in% tolower(tabell_variabler$koder)) {
    px_meta_enkel_list <- sortera_px_variabler(px_meta_enkel_list, sorterings_vars = "tid", sortera_pa_kod = TRUE)
  }
  px_meta <- list(title = px_meta_list[[1]]$title, variables = px_meta_enkel_list)
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  list_data <- list() # Skapar en tom lista som används för att spara data 
  
  visa_dataetik_txt <- ifelse(visa_dataetiketter, "_lbl_","")
  
  px_df <-  hamta_pendling_lan_fa_region_utbildngrupp_kon_tid_scb(region_vekt = region_vekt,
                                                                  utbildngrupp_klartext = "samtliga utbildningsgrupper",
                                                                  kon_klartext = valt_kon,
                                                                  wide_om_en_contvar = FALSE,
                                                                  tid_koder = valt_ar)

  # ============================== diagram med absoluta tal ==================================
  if (diag_absoluta_tal) {
    
    px_df_ut = px_df %>% 
      filter(!variabel %in% c("Dagbefolkning (förvärvsarbetande)", "Nattbefolkning (förvärvsarbetande)"))
    
    if(enbart_in_ut == TRUE) px_df_ut <- px_df_ut %>% filter(variabel != "Bor och arbetar i samma region")
    
    if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("antal_pendlare" = px_df_ut))
    }
    
    if(returnera_data == TRUE){
      assign("antal_pendlare_lan_df", px_df_ut, envir = .GlobalEnv)
    }
  
    diagram_titel <- paste0("Antal pendlare 20-64 år i ", vald_kommun_txt, " år ", unique(px_df$år))
    diagramfil <- paste0("in_utpendling_", vald_region_filnamn, "_", unique(px_df$år), visa_dataetik_txt, ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_ut,  
                       skickad_x_var = "region", 
                       skickad_y_var = "varde",
                       skickad_x_grupp = "variabel",
                       diagram_titel = diagram_titel,
                       diagram_capt = diagram_capt,
                       manual_x_axis_text_vjust = 1,
                       manual_x_axis_text_hjust = 1,
                       manual_color = diagramfarg_vektor,
                       manual_y_axis_title = "antal förvärvsarbetande",
                       stodlinjer_avrunda_fem = TRUE,
                       geom_position_stack = TRUE,
                       dataetiketter = visa_dataetiketter,
                       skriv_till_diagramfil = skapa_fil,
                       output_mapp = output_mapp_figur,
                       filnamn_diagram = diagramfil)
  
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- "In_och_utpendling_absoluta_tal"
    
  } # slut if-sats för diag_abosluta tal
  
  # ================================= diagram med procent ==================================
  if (diag_procent) {
    
    px_df_andel = px_df %>% 
      pivot_wider(names_from = variabel, values_from = varde) %>% 
        mutate("Andel utpendling" = (`Utpendlare över regiongräns`/ `Nattbefolkning (förvärvsarbetande)`) * 100,
               "Andel inpendling" = (`Inpendlare över regiongräns`/ `Dagbefolkning (förvärvsarbetande)`) * 100,
               "Andel som bor och arbetar i samma region" = (`Bor och arbetar i samma region`/(`Bor och arbetar i samma region`+abs(`Utpendlare över regiongräns`)))*100) %>% 
          select(-c(`Inpendlare över regiongräns`,`Utpendlare över regiongräns`,`Bor och arbetar i samma region`, `Dagbefolkning (förvärvsarbetande)`, `Nattbefolkning (förvärvsarbetande)`)) %>% 
            pivot_longer((length(names(.))-2):length(names(.)),names_to = "variabel",values_to = "andel")
    
    if(enbart_in_ut == TRUE) px_df_andel <- px_df_andel %>% filter(variabel != "Andel som bor och arbetar i samma region")
    
    if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("andel_pendlare" = px_df_andel))
    }
    
    if(returnera_data == TRUE){
      assign("andel_pendlare_lan_df", px_df_andel, envir = .GlobalEnv)
    }
    
    diagram_titel <- paste0("Andel pendlare av förvärvsarbetande 20-64 år i ", vald_kommun_txt, " år ", unique(px_df$år)) %>% 
    dela_upp_strang_radbryt(70)

    diagramfil <- paste0("in_utpendling_procent_", vald_region_filnamn, "_", unique(px_df$år), visa_dataetik_txt, ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_andel ,  
                       skickad_x_var = "region", 
                       skickad_y_var = "andel",
                       skickad_x_grupp = "variabel",
                       diagram_titel = diagram_titel,
                       diagram_capt = diagram_capt,
                       manual_y_axis_title = "procent",
                       manual_x_axis_text_vjust = 1,
                       manual_x_axis_text_hjust = 1,
                       manual_color = diagramfarg_vektor,
                       x_axis_sort_value = TRUE,
                       x_axis_sort_grp = 2,
                       vand_sortering = TRUE,
                       stodlinjer_avrunda_fem = TRUE,
                       dataetiketter = visa_dataetiketter,
                       skriv_till_diagramfil = skapa_fil,
                       output_mapp = output_mapp_figur,
                       filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- "In_och_utpendling_procent"
    
  } # slut if-sats diag_procent
  
  # Sparar data
  if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
    # assign("andel_off_tot_df", andel_totalt_utskrift, envir = .GlobalEnv)
  }
 if(returnera_figur == TRUE) return(gg_list)
} # slut funktion

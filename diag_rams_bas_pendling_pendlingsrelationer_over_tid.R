
diag_arbetspendling_over_tid <- function(
    region_vekt = "20", 
    skriv_ut_dataetiketter_diagram = FALSE,
    skriv_till_diagramfil = TRUE,
    output_mapp = NA,
    fargvekt_tre = NA,
    fargvekt_atta = NA,
    lagg_till_logga = TRUE,
    logga_sokvag = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    diag_in_ut_sammma = TRUE,
    diag_in_ut = TRUE,
    diag_nettopendling = TRUE,
    diag_storsta_inpendlrelationer = TRUE,
    diag_storsta_utpendlrelationer = TRUE
    #diag_pendling_over_grans = TRUE
  ) {

  # =======================================================================================================
  #
  # Fem diagram över pendlingen till och från kommuner.
  # 1. In- och utpendling samt de som bor och jobbar i samma kommun, per år och kommun från 1993 och framåt.
  # 2. Samma som ovan men utan de som bor och jobbar i samma kommun.
  # 3. Nettopendlingen per år och kommun från 1993 och framåt.
  # 4. Största inpendlingskommuner för den kommun man valt, per år och kommun från 1993 och framåt.
  # 5. Största utpendlingskommuner för den kommun man valt, per år och kommun från 1993 och framåt.
  #
  # =======================================================================================================
  library(tidyverse)
  library(pxweb)
  library(openxlsx)
  library(RColorBrewer)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_pendling_rams_bas_scb.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  
  options(dplyr.summarise.inform = FALSE)
  
  # om ingen output-mapp anges, används en standardmapp om den finns, annars stoppas skriptet
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(fargvekt_tre))) {
    if (exists("diagramfarger", mode = "function")) {
      fargvekt_tre <- diagramfarger("bla_gra_tre")
    } else {
      fargvekt_tre <- hue_pal()(9)
    }
  }
  
  # detsamma görs för fargvektor med åtta färger
  if (all(is.na(fargvekt_atta))) {
      fargvekt_atta <- brewer.pal(9, "Set3")[c(1,3:7)]
  }
  
  gg_list <- list()               # skapa lista för att lägga diagrammen i
  
  dataetiketter_txt <- ifelse(skriv_ut_dataetiketter_diagram, "_dataetiketter", "")
  
  px_df <- funktion_upprepa_forsok_om_fel( function() hamta_pendling_rams_bas_scb(region_vekt = region_vekt))
  
  skapa_diagram <- function(regionkod) {
    # förbered regionkoder för att också kunna aggregera till län (tabellerna innehåller bara kommuner)
    regiontyp <- ifelse(nchar(regionkod) == 2, "län", "kommun") 
    regiontyp_titel <- ifelse(nchar(regionkod) == 2, "län", "kommuner") 
    region_txt <- hamtaregion_kod_namn(regionkod)$region %>% skapa_kortnamn_lan() %>% list_komma_och()
    
    # förbered df för att skapa diagram
    pendling_df <- px_df %>% 
      filter(regionkod_bo %in% regionkod | regionkod_arb %in% regionkod) %>% 
      mutate(pendlingstyp = case_when(regionkod_bo == regionkod_arb ~ paste0("bor och arbetar i samma ", regiontyp),
                                      regionkod_bo %in% regionkod ~ "utpendlare",
                                      regionkod_arb %in% regionkod ~ "inpendlare")) %>% 
      rename(antal_pendlare = pendlare)
    
    # =================== diagram för in- och utpendlng över kommungräns samt även de som bor och arbetar i samma kommun =========================
    
    if (diag_in_ut_sammma){
      
      diagram_titel <- paste0("In- och utpendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("pendling_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = pendling_df, 
                         skickad_x_var = "år",
                         skickad_x_grupp = "pendlingstyp",
                         skickad_y_var = "antal_pendlare",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         manual_y_axis_title = "",
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         manual_color = fargvekt_tre,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         lagg_pa_logga = lagg_till_logga,
                         logga_path = logga_sokvag,
                         logga_scaling = 22,
                         output_mapp = output_mapp,
                         skriv_till_diagramfil = skriv_till_diagramfil,
                         filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_in_ut_sammma"
    
    } # slut if-sats för diag_in_ut_sammma
    
    
    if (diag_in_ut){
      diagram_titel <- paste0("In- och utpendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("pendling_bara_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = pendling_df %>% 
                           filter(pendlingstyp != paste0("bor och arbetar i samma ", regiontyp)), 
                         skickad_x_var = "år",
                         skickad_x_grupp = "pendlingstyp",
                         skickad_y_var = "antal_pendlare",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         manual_y_axis_title = "",
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         lagg_pa_logga = lagg_till_logga,
                         logga_path = logga_sokvag,
                         manual_color = fargvekt_tre[c(2,3)],
                         logga_scaling = 22,
                         skriv_till_diagramfil = skriv_till_diagramfil,
                         output_mapp = output_mapp,
                         filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_in_ut"
    
    } # slut if-sats för diag_in_ut
    
    
    if (diag_nettopendling){
    
      nettopendling_df <- pendling_df %>%
        group_by(år, kön, pendlingstyp) %>% 
        summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE)) %>% 
        ungroup() %>% 
        pivot_wider(names_from = pendlingstyp, 
                    values_from = antal_pendlare) %>% 
        mutate(nettopendling = inpendlare - utpendlare) %>% 
        rename(ar = år)
      
      diagram_titel <- paste0("Nettopendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("nettopendling_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = nettopendling_df, 
                         skickad_x_var = "ar",
                         skickad_y_var = "nettopendling",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         manual_color = fargvekt_tre[1],
                         lagg_pa_logga = lagg_till_logga,
                         logga_path = logga_sokvag,
                         logga_scaling = 22,
                         skriv_till_diagramfil = skriv_till_diagramfil,
                         output_mapp = output_mapp,
                         filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_in_ut_sammma"
    
    } # slut if-sats för diag_nettopendling
    
    # =========================================== diagram med pendlingsrelationsregioner =====================================
    
    if (diag_storsta_inpendlrelationer | diag_storsta_utpendlrelationer) {
    
    
      bara_pendlare_df <- pendling_df %>%
        filter(pendlingstyp != paste0("bor och arbetar i samma ", regiontyp))
      
      # ta ut de sex största inpendlingskommunerna och lägg i en vektor
      storsta_inpendl_regioner <-  bara_pendlare_df %>% 
        filter(pendlingstyp == "inpendlare") %>% 
        group_by(regionkod_bo) %>% 
        summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(desc(antal_pendlare)) %>% 
        slice(1:6) %>% 
        select(regionkod_bo) %>% 
        dplyr::pull()
      
      
      # ta ut de sex största utpendlingskommunerna och lägg i en vektor
      storsta_utpendl_regioner <-  bara_pendlare_df %>% 
        filter(pendlingstyp == "utpendlare") %>% 
        group_by(regionkod_arb) %>% 
        summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(desc(antal_pendlare)) %>% 
        slice(1:6) %>% 
        select(regionkod_arb) %>% 
        dplyr::pull()
      
      # ================= diagram - inpendling till de sex största inpendlingsrelationsregionerna över hela perioden =================
      
      if (diag_storsta_inpendlrelationer) {
        diagram_titel <- paste0("Största inpendlings", regiontyp_titel, " till ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
        diagramfil <- paste0("inpendlingsrelationer_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")
        
        gg_obj <- SkapaStapelDiagram(skickad_df = bara_pendlare_df %>% 
                             filter(pendlingstyp == "inpendlare",
                                    regionkod_bo %in% storsta_inpendl_regioner), 
                           skickad_x_var = "år",
                           skickad_x_grupp = "bostadsregion",
                           skickad_y_var = "antal_pendlare",
                           diagram_titel = diagram_titel,
                           diagram_capt = diagram_capt,
                           manual_y_axis_title = "antal pendlare",
                           manual_x_axis_text_vjust = 1,
                           manual_x_axis_text_hjust = 1,
                           stodlinjer_avrunda_fem = TRUE,
                           manual_color = fargvekt_atta,
                           lagg_pa_logga = lagg_till_logga,
                           logga_scaling = 22,
                           logga_path = logga_sokvag,
                           dataetiketter = skriv_ut_dataetiketter_diagram,
                           skriv_till_diagramfil = skriv_till_diagramfil,
                           output_mapp = output_mapp,
                           filnamn_diagram = diagramfil)
        
        gg_list <- c(gg_list, list(gg_obj))
        names(gg_list)[length(gg_list)] <- "diag_storsta_inpendlrelationer"
      
      } # slut if-sats om diagram med största inpendlingskommuner är vald
      
      # ================= diagram - utpendling till de sex största utpendlingsrelationsregionerna över hela perioden =================
      
      if (diag_storsta_utpendlrelationer) {
        diagram_titel <- paste0("Största utpendlings", regiontyp_titel, " i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
        diagramfil <- paste0("utpendlingsrelationer_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")
        
        gg_obj <- SkapaStapelDiagram(skickad_df = bara_pendlare_df %>% 
                             filter(pendlingstyp == "utpendlare",
                                    regionkod_arb %in% storsta_utpendl_regioner), 
                           skickad_x_var = "år",
                           skickad_x_grupp = "arbetsställeregion",
                           skickad_y_var = "antal_pendlare",
                           diagram_titel = diagram_titel,
                           diagram_capt = diagram_capt,
                           manual_y_axis_title = "antal pendlare",
                           manual_x_axis_text_vjust = 1,
                           manual_x_axis_text_hjust = 1,
                           stodlinjer_avrunda_fem = TRUE,
                           manual_color = fargvekt_atta,
                           logga_scaling = 22,
                           logga_path = logga_sokvag,
                           lagg_pa_logga = lagg_till_logga,
                           dataetiketter = skriv_ut_dataetiketter_diagram,
                           skriv_till_diagramfil = skriv_till_diagramfil,
                           output_mapp = output_mapp,
                           filnamn_diagram = diagramfil)
        
        gg_list <- c(gg_list, list(gg_obj))
        names(gg_list)[length(gg_list)] <- "diag_storsta_utpendlrelationer"
        
      } # slut if-sats om diagram med största utpendlingskommuner är vald
    
    } # if-sats om någon av största in- eller utpendlingsrelationskommuner är valda
    
    # # förbered dataset om man valt att skapa diagram över pendling över kommungräns eller länsgräns
    # if (diag_pendling_over_grans) {
    #   
    #   dagbef_df <- pendling_df %>% 
    #     filter(regionkod_arb %in% regionkod) %>% 
    #     group_by(år, kön, regionkod = regionkod_arb, region = arbetsställeregion) %>% 
    #     summarise(dagbef = sum(antal_pendlare, na.rm = TRUE), .groups = "drop")
    #   
    #   nattbef_df <- pendling_df %>% 
    #     filter(regionkod_bo %in% regionkod) %>% 
    #     group_by(år, kön, regionkod = regionkod_bo, region = bostadsregion) %>% 
    #     summarise(nattbef = sum(antal_pendlare, na.rm = TRUE), .groups = "drop")
    #   
    #   inpendlare_df <- pendling_df %>% 
    #     filter(regionkod_arb %in% regionkod,
    #            pendlingstyp == "inpendlare") %>% 
    #     group_by(år, kön, regionkod = regionkod_arb, region = arbetsställeregion) %>% 
    #     summarise(inpendlare = sum(antal_pendlare, na.rm = TRUE), .groups = "drop")
    #   
    #   utpendlare_df <- pendling_df %>% 
    #     filter(regionkod_bo %in% regionkod,
    #            pendlingstyp == "utpendlare") %>% 
    #     group_by(år, kön, regionkod = regionkod_bo, region = bostadsregion) %>% 
    #     summarise(utpendlare = sum(antal_pendlare, na.rm = TRUE), .groups = "drop")
    #   
    #   # Lägg ihop dataset ovan för att skapa ett över gränsöverskridande pendling
    #   pendlare_grans_df <- dagbef_df %>% 
    #     left_join(nattbef_df, by = c("år", "kön", "regionkod", "region")) %>%
    #     left_join(inpendlare_df, by = c("år", "kön", "regionkod", "region")) %>%
    #     left_join(utpendlare_df, by = c("år", "kön", "regionkod", "region")) %>% 
    #     mutate(andel_inpendlare = (inpendlare/dagbef)*100,
    #            andel_utpendlare = (utpendlare/nattbef)*100)
    #   
    # } # slut funktion för att skapa diagram för pendling över kommun- eller länsgräns
    
    
    return(gg_list)              # här returnerar vi listan med ggplot-diagram
    } # slut funktion för att skapa själva diagrammen
  
    retur_list <- map(region_vekt, ~ skapa_diagram(.x)) %>% purrr::flatten()
    return(retur_list)
} # slut funktion

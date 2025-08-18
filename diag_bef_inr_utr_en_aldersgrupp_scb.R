
diag_bef_inr_utr_en_aldersgrupp <- function(
    region_vekt = "20",                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region
    aldersintervall = c(20, 65),
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
        # om <prognos_ar> ligger med i diagram_capt så byts det ut mot det år prognosen gjordes
    output_mapp = NA,                                        # här sparas diagramet
    diagram_fargvekt = NA,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    returnera_dataframe_global_environment = FALSE,          
    ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
    skriv_till_diagramfil = TRUE,
    skriv_till_excelfil = FALSE
) {

  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue,
         writexl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_region_alder_kon_fodelseregion_tid_InrUtrFoddaRegAlKon_scb.R")
  
  gg_list <- list()
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diagram_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diagram_fargvekt <- diagramfarger("rus_sex")[c(1,2)]
    } else {
      diagram_fargvekt <- hue_pal()(9)
    }
  }
  
  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_till_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(utskriftsmapp())) {
        output_mapp <- utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }
  
  if (length(aldersintervall) != 2) stop("Parametern aldersintervall måste innehålla två värden. Det ena är från och med åldern, det andra till och med ålder. aldersintervall = c(20,65) blir alltså åldersgruppen 20-65 år.") 
  
  aldersgrupp_txt <- glue("{min(aldersintervall)}-{max(aldersintervall)} år")  
  
  bef_folkmangd <- funktion_upprepa_forsok_om_fel(function()
    hamta_bef_region_alder_kon_fodelseregion_tid_scb(region_vekt = region_vekt,
                                                     alder_koder = c(min(aldersintervall):max(aldersintervall)) %>% as.character()
                                                     ))
  diagram_df <- bef_folkmangd %>% 
      group_by(år, regionkod, region, bakgrund = födelseregion) %>% 
      summarise(antal = sum(Antal, na.rm = TRUE), .groups = "drop") %>% 
      mutate(aldersgrupp = aldersgrupp_txt,
             bakgrund = ifelse(bakgrund == "utrikes född", "Utrikes födda", "Inrikes födda"),
             bakgrund = factor(bakgrund, levels = c("Utrikes födda", "Inrikes födda")),
             region = region %>% skapa_kortnamn_lan())
  
  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("bef_inr_utr_en_aldersgrupp", diagram_df, envir = .GlobalEnv)
  }
  
  if (aldersgrupp_txt %in% c("20-64 år", "20-65 år")) {
    aldersgrupp_txt <- paste0("i arbetsför ålder (", aldersgrupp_txt, ")")
  }
  
  skapa_diagram <- function(skickad_regionkod) {
    
    diagram_region_df <- diagram_df %>%
      filter(regionkod %in% skickad_regionkod)
    
    region_txt <- unique(diagram_region_df$region) %>% skapa_kortnamn_lan()
    
    diagramtitel <- glue("Befolkning {aldersgrupp_txt} i {region_txt}")
    diagramfil <- glue("bef_inr_utr_{aldersgrupp_txt}_{region_txt}_ar{min(diagram_region_df$år)}-{max(diagram_region_df$år)}.png")
  
    gg_obj <- SkapaStapelDiagram(skickad_df = diagram_region_df,
                                 skickad_x_var = "år",
                                 skickad_y_var = "antal",
                                 skickad_x_grupp = "bakgrund",
                                 #x_axis_sort_value = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 utan_diagramtitel = ta_bort_diagramtitel,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 #x_axis_lutning = 0,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagram_fargvekt,
                                 output_mapp = output_mapp,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 dataetiketter = visa_dataetiketter,
                                 geom_position_stack = TRUE,
                                 legend_vand_ordning = TRUE,
                                 facet_legend_bottom = TRUE
    ) # slut skriv ggplot_objekt
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
    return(gg_list) 
    
  } # slut funktion som skapar diagrammet
  
  retur_list <- map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)) %>% purrr::flatten()
  
  if (skriv_till_excelfil) {
    region_xlsx <- unique(bef_folk_progn$region) %>% skapa_kortnamn_lan() %>% paste0(collapse = "_")
    startar_utfall <- bef_folk_progn %>% filter(typ == "utfall") %>% dplyr::pull(år) %>% min()
    slutar_prognos <- bef_folk_progn %>% filter(typ == "prognos") %>% dplyr::pull(år) %>% max() 
    excefilnamn <- glue("befolkning_utfall_progn_{region_xlsx}_ar{startar_utfall}-{slutar_prognos}.xlsx")
    write_xlsx(bef_folk_progn, paste0(output_mapp, excefilnamn))
  }
  
  return(retur_list)
  
} # slut funktion

diag_fodda_manad_scb <- function(
  region_vekt = "20",                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region
  bef_variabel = "födda",                                  # går att välja dessa: "folkmängd", "folkökning", "födda", "döda", "födelseöverskott", "samtliga inflyttningar", "samtliga utflyttningar", "samtliga inrikes inflyttningar", "inflyttningar från kommuner inom länet", "inflyttningar från övriga län", "invandringar", "samtliga inrikes utflyttningar", "utflyttningar till kommuner inom länet", "utflyttningar till övriga län", "utvandringar", "flyttningsöverskott totalt", "flyttningsöverskott inrikes totalt", "flyttningsöverskott eget län", "flyttningsöverskott övriga Sverige", "invandringsöverskott", "justeringspost"
  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
  output_mapp = NA,                                        # här sparas diagramet
  diagram_fargvekt = NA,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  returnera_dataframe_global_environment = FALSE,          
  ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
  visa_dataetiketter = FALSE,
  kortnamn_lan = TRUE,                                     # vid TRUE så tas inte "län" i länsnamnet med
  skriv_till_diagramfil = TRUE,
  skriv_till_excelfil = FALSE
) {


  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_forandringar_region_kon_manad_scb.R")
  
  gg_list <- list()
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diagram_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diagram_fargvekt <- c(rep(diagramfarger("rd_gra")[c(1,1,1,1,4)]), diagramfarger("rus_sex")[c(3)]) #diagramfarger("rus_sex")
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
  
  diagram_df <- funktion_upprepa_forsok_om_fel(function()
    hamta_befolkningsforandringar_manad(
        region_vekt = region_vekt,
        befforandr_klartext = bef_variabel,
        kon_klartext = "totalt",
        tid_koder = "*",
        long_format = TRUE)) %>% 
    mutate(
      fokus = as.integer(5 - (max(as.integer(as.character(år))) - as.integer(as.character(år)))),
      fokus = ifelse(fokus < 0, 0, fokus),  # Sätt alla äldre än 5 år från senaste året till 0
      ar_num = år %>% as.integer()
    ) %>% 
    filter(ar_num > max(ar_num)-6)
        
  if (kortnamn_lan) diagram_df <- diagram_df %>% mutate(region = region %>% skapa_kortnamn_lan())
  
  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("bef_forandringar_per_manad_df", diagram_df, envir = .GlobalEnv)
  }
  
  
  skapa_diagram <- function(skickad_regionkod) {
  
    diagram_region_df <- diagram_df %>%
      filter(regionkod %in% skickad_regionkod)
  
    region_txt <- unique(diagram_region_df$region)
    variabel_txt <- unique(diagram_region_df$förändringar)
  
    diagramtitel <- glue("{str_to_sentence(variabel_txt)} i {region_txt}")
    diagramfil <- glue("{variabel_txt}_{region_txt}_ar{first(diagram_region_df$år)}-{last(diagram_region_df$år)}.png")
  
    gg_obj <- SkapaLinjeDiagram(skickad_df = diagram_region_df,
                                 skickad_x_var = "månad",
                                 skickad_y_var = "Befolkning",
                                 skickad_x_grupp = "år",
                                 #x_axis_sort_value = TRUE,
                                 #x_var_fokus = "fokus",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 utan_diagramtitel = ta_bort_diagramtitel,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 #x_axis_lutning = 0,
                                 #manual_x_axis_text_vjust = 1,
                                 #manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 manual_color = diagram_fargvekt,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 legend_byrow = TRUE,
                                 #dataetiketter = visa_dataetiketter,
                                 #geom_position_stack = TRUE,
                                 #legend_vand_ordning = TRUE,
                                 #facet_legend_bottom = TRUE,
                                  output_mapp = output_mapp
    ) # slut skriv ggplot_objekt
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
    return(gg_list) 
    
  } # slut funktion som skapar diagrammet
  
  retur_list <- map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)) %>% purrr::flatten()
  
  if (skriv_till_excelfil) {
      excefilnamn <- glue("befolkning_utfall_progn_{region_xlsx}_ar{startar_utfall}-{slutar_prognos}.xlsx")
      write_xlsx(bef_folk_progn, paste0(output_mapp, excefilnamn))
  }
  
  return(retur_list)

} # slut funktion


diag_gymn_genomstromning_4ar_prg_skolverket <- function(
  region_vekt = "20",      # Val av region.
  tid_koder = "9999",       # "*" = alla år, finns från 2011 och framåt
  gymnasieprogram = "*",       #  # "*" = alla gymnasieprogram, annars anges programnamn, dessa finns: "Nationella program", "Högskoleförberedande program", "Yrkesprogram", "Introduktionsprogrammen", "Barn- och fritidsprogrammet", "Bygg- och anläggningsprogramme", "Ekonomiprogrammet", "El- och energiprogrammet", "Estetiska programmet", "Fordons- och transportprogramm", "Försäljnings- och serviceprogr", "Handels- och administrationspr", "Hantverksprogrammet", "Hotell- och turismprogrammet", "Humanistiska programmet", "Industritekniska programmet", "International Baccalaureate", "Introduktionsprogram, Individu", "Introduktionsprogram, Programi", "Introduktionsprogram, Språkint", "Introduktionsprogram, Yrkesint", "Naturbruksprogrammet", "Naturvetenskapsprogrammet", "Restaurang- och livsmedelsprog", "Riksrekryterande utbildningar", "Samhällsvetenskapsprogrammet", "Teknikprogrammet", "VVS- och fastighetsprogrammet", "Vård- och omsorgsprogrammet"
  diagram_capt = "Källa: Skolverket\nBearbetning: Samhällsanalys, Region Dalarna",
  x_variabel = "Gymnasieprogram",      # för x-axeln
  y_variabel = "andel",                # för y-axeln
  x_grupp = "region",                  # grupp för x-axeln
  facet_variabel = NA,                 # anges om man vill ha facetdiagram
  nytt_diagram_variabel = NA,          # anges om man vill ha ett diagram per värde, tex "region" ger ett diagram per region
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  output_mapp = NA,
  diagramrubrik_tabort = FALSE,
  skriv_diagramfil = TRUE,
  returnera_data_rmarkdown = FALSE,
  excelfil_mapp = NA,      # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  excel_filnamn = "gymnaseiet_elever.xlsx"      # filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  ) {
  
  # ==============================================================================================================================
  #
  # Skriver ut diagram med med elever per gymnasieprogram
  #  
  #
  # ==============================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_gymn_avg_genomstromning_4ar_prg_skolverket.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("rus_sex")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
 
  gg_list <- list()
  
  suppress_specific_warning(
  dataset_df <- hamta_gymn_avg_genomstromning_4ar_prg_skolverket(
    region_vekt = region_vekt,      # Val av region.
    gymnasieprogram = gymnasieprogram,
    huvudman = "Samtliga",
    konvertera_andel_till_numerisk = TRUE
  ))
  
  giltiga_ar <- unique(dataset_df$läsår)
  valda_ar <- tid_koder %>% str_replace("9999", max(giltiga_ar)) %>% str_replace("\\*", giltiga_ar) %>% unique() %>% .[. %in% giltiga_ar]
  
  # filtrera ut andel kvinnor, beräkna andel män utifrån andel kvinnor och lägg till i datasetet
  chart_df <- dataset_df %>% 
    mutate(region = region %>% skapa_kortnamn_lan(),
           region = factor(region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")),
           Gymnasieprogram = factor(Gymnasieprogram, levels = c("Yrkesprogram", "Högskoleförberedande program"))) %>% 
    filter(läsår %in% valda_ar)
    
  
  if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
    write.xlsx(chart_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
  }
  
  if(returnera_data_rmarkdown == TRUE){
    assign("gymn_genomstromning_4ar_prg_df", chart_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(skickad_df, filtrerings_kol = NA, filtrerings_varde = NA) {
  
    # om det ska filtreras på någon variabel
    if(!all(is.na(filtrerings_varde))){
      skickad_df <- skickad_df %>% 
        filter(!!sym(filtrerings_kol) %in% filtrerings_varde)
    }
    
    vald_region_txt <- skickad_df %>% 
      distinct(region) %>% 
      dplyr::pull() %>%
      list_komma_och() %>% 
      skapa_kortnamn_lan()
    
    lasar_txt <- unique(skickad_df$läsår)
  
    facet_txt <- if (length(unique(skickad_df)) > 1) "" else glue(" i {vald_region_txt}")
  
    diagramtitel <- glue("{chart_df$Genomströmning %>% unique()} - startläsår {lasar_txt}")
    diagramfil <- glue("gymn_genomstr_4ar_prg_{skickad_df$regionkod %>% paste0(collapse = '_')}_lasar{lasar_txt}.png") %>% str_replace_all("/", "_")
  
    gg_obj <- SkapaStapelDiagram(
      skickad_df = skickad_df,
      skickad_x_var = x_variabel,
      skickad_y_var = y_variabel,
      skickad_x_grupp = x_grupp,
      geom_position_stack = FALSE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = "procent",
      # manual_x_axis_text_vjust = 1,
      # manual_x_axis_text_hjust = 1,
      procent_0_100_10intervaller = TRUE,
      x_axis_lutning = 0,
      manual_color = diag_fargvekt,
      output_mapp = output_mapp,
      diagram_facet = !all(is.na(facet_variabel)),
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      utan_diagramtitel = diagramrubrik_tabort,
      facet_grp = facet_variabel,
      facet_scale = "fixed",
      facet_legend_bottom = TRUE,
      skriv_till_diagramfil = skriv_diagramfil
    )
  
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    return(gg_list)
  
  } # slut skapa_diagram-funktion
  
  if (!all(is.na(nytt_diagram_variabel))){
    retur_list <- map(unique(chart_df[[nytt_diagram_variabel]]), ~ skapa_diagram(skickad_df = chart_df, 
                                                                                 filtrerings_kol = nytt_diagram_variabel,
                                                                                 filtrerings_varde = .x)) %>% flatten()
  } else {
    retur_list <- skapa_diagram(skickad_df = chart_df)
  }
  
  return(retur_list)
} # slut diag-funktion

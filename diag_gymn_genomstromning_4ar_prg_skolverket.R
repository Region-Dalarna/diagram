diag_gymn_genomstromning_4ar_prg_skolverket <- function(
  region_vekt = "20",      # Val av region.
  gymnasieprogram = "Gymnasieskolan totalt",       # dessa finns: "Gymnasieskolan totalt", "Nationella program", "Högskoleförberedande program", "Yrkesprogram", "Introduktionsprogram", "Barn- och fritidsprogrammet", "Bygg- och anläggningsprogramme", "Ekonomiprogrammet", "El- och energiprogrammet", "Estetiska programmet", "Fordons- och transportprogramm", "Handels- och administrationspr", "Hantverksprogrammet", "Hotell- och turismprogrammet", "Humanistiska programmet", "Industritekniska programmet", "Naturbruksprogrammet", "Naturvetenskapsprogrammet", "Restaurang- och livsmedelsprog", "Samhällsvetenskapsprogrammet", "Teknikprogrammet", "VVS- och fastighetsprogrammet", "Vård- och omsorgsprogrammet"
  diagram_capt = "Källa: Skolverket\nBearbetning: Samhällsanalys, Region Dalarna",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  output_mapp = NA,
  diagramrubrik_tabort = FALSE,
  skriv_diagramfil = TRUE,
  returnera_data_rmarkdown = FALSE,           # TRUE = lägger dataframe i global environment, för användning i tex r-markdownrapporter
  ggobjektfilnamn_utan_tid = FALSE,           # TRUE = tar bort året ur filnamnet (som blir gg_plotobjektnamnet) så att det blir smidigare i markdownrapporter att använda samma objektsnamn varje år
  excelfil_mapp = NA,                         # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
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
  #source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
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
    gymn_df <- hamta_gymn_avg_genomstromning_4ar_prg_skolverket(
      region_vekt = region_vekt,      # Val av region.
      gymnasieprogram = gymnasieprogram,
      huvudman = "Samtliga",
      konvertera_andel_till_numerisk = TRUE
    )) %>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  if(returnera_data_rmarkdown == TRUE){
    assign("genomstromning_gymnasiet_df", chart_df, envir = .GlobalEnv)
  }
  
  kombinationer <- expand_grid(region = unique(gymn_df$region), 
                               gymnasieprogram = unique(gymn_df$Gymnasieprogram))
  
  gg_list <- pmap(kombinationer, function(region, gymnasieprogram) {
    
    chart_df <- gymn_df %>% 
      filter(region == .env$region, Gymnasieprogram == gymnasieprogram)
  
  prg_txt <- if_else(gymnasieprogram == "Gymnasieskolan totalt", "", paste0(" på ", tolower(gymnasieprogram)))
    
  diagram_titel <- paste0("Andel i ", region," med fullföljd gymnasieutbildning inom fyra år", prg_txt)
  diagramfilnamn <- glue("genomstromning_gymnasiet_{region}_{svenska_tecken_byt_ut(gymnasieprogram)}_ar_{substr(last(chart_df$läsår),1,4)}_{substr(first(chart_df$läsår),1,4)}.png")

  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df ,
                               skickad_x_var = "läsår",
                               skickad_y_var = "andel",
                               diagram_titel = if(diagramrubrik_tabort) NULL else diagram_titel,
                               lagg_pa_logga = ta_med_logga,
                               logga_path = logga_sokvag,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               manual_x_axis_text_hjust = 1,
                               manual_x_axis_text_vjust = 1,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_lutning = 45,
                               manual_y_axis_title = "procent",
                               manual_x_axis_title = "Läsår då gymnasieutbildningen påbörjades",
                               procent_0_100_10intervaller = TRUE,
                               manual_color = diag_fargvekt,
                               skriv_till_diagramfil = skriv_diagramfil)
    
    objektnamn <- diagramfilnamn %>% str_remove(".png")
    if (ggobjektfilnamn_utan_tid) objektnamn <- sub("_ar.*", "", objektnamn)
    setNames(list(gg_obj), objektnamn)
    }) %>% purrr::flatten()
  
  if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
    write.xlsx(gymn_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
  }
  return(gg_list)
  
} # slut diag-funktion
  

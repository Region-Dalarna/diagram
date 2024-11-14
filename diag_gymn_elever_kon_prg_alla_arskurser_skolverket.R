diag_gymn_elever_kon_prg_skolverket <- function(
  region_vekt = "20",      # Val av region.
  tid_koder = "9999",       # "*" = alla år, finns från 2011 och framåt
  gymnasieprogram = "*",       #  # "*" = alla gymnasieprogram, annars anges programnamn, dessa finns: "Nationella program", "Högskoleförberedande program", "Yrkesprogram", "Introduktionsprogrammen", "Barn- och fritidsprogrammet", "Bygg- och anläggningsprogramme", "Ekonomiprogrammet", "El- och energiprogrammet", "Estetiska programmet", "Fordons- och transportprogramm", "Försäljnings- och serviceprogr", "Handels- och administrationspr", "Hantverksprogrammet", "Hotell- och turismprogrammet", "Humanistiska programmet", "Industritekniska programmet", "International Baccalaureate", "Introduktionsprogram, Individu", "Introduktionsprogram, Programi", "Introduktionsprogram, Språkint", "Introduktionsprogram, Yrkesint", "Naturbruksprogrammet", "Naturvetenskapsprogrammet", "Restaurang- och livsmedelsprog", "Riksrekryterande utbildningar", "Samhällsvetenskapsprogrammet", "Teknikprogrammet", "VVS- och fastighetsprogrammet", "Vård- och omsorgsprogrammet"
  diagram_capt = "Källa: Skolverket\nBearbetning: Samhällsanalys, Region Dalarna",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  skapa_facet_diagram = TRUE,
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
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_gymn_elever_kon_bakgrund_arskurs_prg_skolverket.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("kon")
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
  dataset_df <- hamta_gymn_elever_kon_bakgrund_arskurs_prg_skolverket(
    region_vekt = region_vekt,      # Val av region.
    valda_ar = tid_koder,       # "*" = alla år eller månader, "9999" = senaste, finns: "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12"
    gymnasieprogram = gymnasieprogram,
    huvudman = "Samtliga",
    konvertera_andel_till_numerisk = TRUE
  ))
  
  # filtrera ut andel kvinnor, beräkna andel män utifrån andel kvinnor och lägg till i datasetet
  chart_df <- dataset_df %>% 
    mutate(variabel = ifelse(variabel == "Andel kvinnor (%)", "Kvinnor", variabel),
           region = region %>% skapa_kortnamn_lan(),
           region = factor(region, levels = c("Dalarna", "Gävleborg", "Värmland", "Riket")),
           gymnasieprogram = factor(gymnasieprogram, levels = c("Yrkesprogram", "Högskoleförberedande program", "Introduktionsprogrammen" ))) %>% 
    filter(variabel == "Kvinnor")
  
  chart_df <- chart_df %>%
    # Skapa en ny dataram för "Män"
    bind_rows(
      chart_df %>%
        mutate(
          variabel = "Män",       # Ändra variabeln
          varde = 100 - varde              # Beräkna andelen män
        )
    )
    
  
  if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
    write.xlsx(chart_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
  }
  
  if(returnera_data_rmarkdown == TRUE){
    assign("gymn_elever_kon_prg_df", chart_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(skickad_df, region_kod) {
  
    skickad_df <- skickad_df %>% 
      filter(regionkod %in% region_kod)
  
    vald_region_txt <- skickad_df %>% 
      distinct(region) %>% 
      dplyr::pull() %>%
      list_komma_och() %>% 
      skapa_kortnamn_lan()
    
    lasar_txt <- unique(skickad_df$lasar)
  
    facet_txt <- if (length(unique(skickad_df)) > 1) "" else glue(" i {vald_region_txt}")
  
    diagramtitel <- glue("Könsbalans per gymnasieprogram i alla årskurser läråret {lasar_txt}")
    diagramfil <- glue("gymn_elever_kon_prg_{region_kod %>% paste0(collapse = '_')}_lasar{lasar_txt}.png") %>% str_replace_all("/", "_")
  
    gg_obj <- SkapaStapelDiagram(
      skickad_df = skickad_df,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      geom_position_stack = FALSE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = "procent",
      # manual_x_axis_text_vjust = 1,
      # manual_x_axis_text_hjust = 1,
      x_axis_lutning = 0,
      manual_color = diag_fargvekt,
      output_mapp = output_mapp,
      diagram_facet = length(unique(skickad_df)) > 1,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      utan_diagramtitel = diagramrubrik_tabort,
      facet_grp = "gymnasieprogram",
      facet_scale = "fixed",
      facet_legend_bottom = TRUE,
      skriv_till_diagramfil = skriv_diagramfil
    )
  
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    return(gg_list)
  
  } # slut skapa_diagram-funktion
  
  if (skapa_facet_diagram) {
    retur_list <- skapa_diagram(skickad_df = chart_df, region_kod = region_vekt)
  
  } else {
  
    retur_list <- map(unique(region_vekt), ~ skapa_diagram(skickad_df = chart_df, region_kod = .x)) %>% flatten()
  }
} # slut diag-funktion

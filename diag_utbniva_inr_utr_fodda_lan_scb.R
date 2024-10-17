
diag_utbniva_inr_utr_fodda_kon_lan <- function(
    region_vekt = "20",                       # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
    gruppera_namn = NA,                       # NA = gör ingenting, annars anges namn på gruppering som medskickade regioner ska grupperas till
    valt_kon = c("män", "kvinnor"),           #  Finns: "män och kvinnor", "män", "kvinnor"
    tid_koder = "9999",			                  # "*" = alla år eller månader, "9999" = senaste, finns: "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
    bakgrund_klartext = c("andel med förgymnasial utbildning, procent", "andel med gymnasial utbildning, procent", "andel med eftergymnasial utbildning, procent"),			 #  Finns: "andel 0-19 år, procent", "andel 20-64 år, procent", "andel 65+ år, procent", "samtliga utbildningsnivåer, procent", "andel med förgymnasial utbildning, procent", "andel med gymnasial utbildning, procent", "andel med eftergymnasial utbildning, procent", "andel där uppgift saknas för utbildningsnivå, procent", "samtliga, procent"
    con_klartext = c("Födda i Sverige", "Utrikes födda"),			 #  Finns: "Födda i Sverige", "Utländsk bakgrund", "Utrikes födda", "Födda i Norden exkl. Sverige", "Födda i EU/EFTA exkl. Norden", "Födda i övriga världen"
    diagram_capt = "Källa: Tema registerbaserad integration, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    returnera_df_rmarkdown = FALSE,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = TRUE,
    diag_fargvekt = NA
) {

  # ======================================================================================================
  #
  # Diagram som visar utbildningsnivå bland inrikes och utrikes födda samt även kön. Tre utbildningsnivåer.
  # Skapat 17 okt 2024 av Peter.
  #
  # ======================================================================================================
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_integration_region_kon_bakgrund_tid_IntGr3LanKONS_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  gg_list <- list()
  
  # om ingen output_mapp är medskickad och funktionen utskriftsmapp finns, använd den, annars sätt skriv_diagramfil till FALSE
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp() 
    } else {
      skriv_diagramfil <- FALSE
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("rus_sex")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }

  integration_df <- hamta_integration_region_kon_bakgrund_tid_scb(
  			region_vekt = region_vekt,			   
  			kon_klartext = valt_kon,			 
  			bakgrund_klartext = bakgrund_klartext,
  			cont_klartext = cont_klartext,
  			tid_koder = tid_koder,
  			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  			wide_om_en_contvar = FALSE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			excel_filnamn = "integration.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  ) %>% 
    rename(utbildning = variabel) %>% 
    mutate(utbildning = utbildning %>% str_remove("andel med ") %>% str_remove(", procent"),
           utbildning = factor(utbildning, levels = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning")))
  
  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!is.na(gruppera_namn)) {
    integration_df <- integration_df %>% 
      group_by(across(-c(regionkod, region, varde))) %>% 
      summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") %>% 
      mutate(regionkod = "gg",
             region = gruppera_namn) %>% 
      relocate(region, .before = 1) %>% 
      relocate(regionkod, .before = region)
    
    region_vekt <- "gg"
  }
  
  if(returnera_df_rmarkdown == TRUE){
    assign("utbniva_bakgr_kon_df", integration_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(chart_df, skickad_regionkod) {
    
    chart_df <- chart_df %>% 
      filter(regionkod %in% skickad_regionkod)
    
    # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
    region_start <- unique(chart_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
    region_txt <- ar_alla_kommuner_i_ett_lan(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_start)
    region_txt <- ar_alla_lan_i_sverige(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_txt)
    regionfil_txt <- region_txt
    region_txt <- paste0(" i ", region_txt)
    regionkod_txt <- if (region_start == region_txt) unique(chart_df$regionkod) %>% paste0(collapse = "_") else region_txt
    
    ar_txt <- if (min(chart_df$år) == max(chart_df$år)) max(chart_df$år) else paste0(min(chart_df$år), "-", max(chart_df$år))
    
    diagramtitel <- glue("Utbildningsnivå invånare 20-64 år{region_txt} år {ar_txt}")
    diagramfil <- glue("utbniva_inr_utr_kon_{regionfil_txt}_ar{ar_txt}.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
    			 skickad_x_var = "bakgrund",
    			 skickad_y_var = "varde",
    			 skickad_x_grupp = "utbildning",
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_y_axis_title = "procent",
    			 x_axis_lutning = 0,
    			 manual_color = diag_fargvekt,
    			 output_mapp = output_mapp,
    			 skriv_till_diagramfil = skriv_diagramfil,
    			 diagram_facet = TRUE,
    			 facet_grp = "kön",
    			 facet_scale = "fixed",
    			 facet_legend_bottom = TRUE
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
    return(gg_list)
    
  } # slut skapa_diagram
  
  if (length(region_vekt) > 1) {
    retur_list <- map(unique(region_vekt), ~ skapa_diagram(chart_df = integration_df,
                                                           skickad_regionkod = .x)) %>% purrr::flatten()
  } else {
    retur_list <- skapa_diagram(chart_df = integration_df, 
                                skickad_regionkod = region_vekt)
  }
  return(retur_list)
} # slut funktion
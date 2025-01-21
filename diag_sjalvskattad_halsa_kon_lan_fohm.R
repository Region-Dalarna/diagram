

diag_sjalvskattad_halsa_kon_lan <- function(
    region_vekt = "20",
    svarsalterantiv_klartext = "Bra eller mycket bra hälsa",
    andel_konfinterv_klartext = "Andel",
    kon_klartext = c("Kvinnor", "Män"),
    tid_koder = "9999",         # "9999" = senaste år
    region_sort = FALSE,        # TRUE så sorteras regionerna enligt ordningen i region_vekt
    diagram_fargvekt = NA,
    output_mapp = NA,
    returnera_dataframe_global_environment = FALSE,
    diagram_capt = "Källa: Folkhälsomyndighetens öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    visa_dataetiketter = FALSE, 
    diagramtitel = TRUE                    # FALSE så skrivs ingen diagramtitel ut
  ) {

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_sjalvskattad_halsa_region_halsotillstand_andel_och_konfidensintervall_kon_ar_hlv1allmxreg_fohm.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  # om ingen output_mapp är vald. Kolla om standardmappen existerar, om inte ges ett felmeddelande
  if (all(is.na(output_mapp))) {
    if (!dir.exists(utskriftsmapp())) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }

  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diagram_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diagram_fargvekt <- diagramfarger("kon")
    } else {
      diagram_fargvekt <- hue_pal()(9)
    }
  }

  gg_list <- list()
  
  sjalvskattad_halsa_df <- hamta_sjalvskattad_halsa_region_halsotillstand_andel_och_konfidensintervall_kon_ar_fohm(
  			region_vekt = region_vekt,			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
  			halsotillstand_klartext = svarsalterantiv_klartext,			 #  Finns: "Bra eller mycket bra hälsa", "Dålig eller mycket dålig hälsa", "Långvarig sjukdom", "Nöjd med sexlivet"
  			andel_och_konfidensintervall_klartext = andel_konfinterv_klartext,			 #  Finns: "Andel", "Konfidensintervall nedre gräns", "Konfidensintervall övre gräns", "Antal svar"
  			kon_klartext =kon_klartext,			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Kvinnor", "Män"
  			tid_koder = tid_koder,			 # "*" = alla år eller månader, "9999" = senaste, finns: "2004-2007", "2005-2008", "2006-2009", "2007-2010", "2008-2011", "2009-2012", "2010-2013", "2011-2014", "2012-2015", "2013-2016", "2015-2018", "2017-2020", "2018-2021", "2019-2022", "2021-2024"
  			output_mapp = output_mapp,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			excel_filnamn = "test_fohm.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  
  )
  
  # om region_sort är TRUE sorteras regionerna enligt ordningen i region_vekt
  if (region_sort) {
    sjalvskattad_halsa_df <- sjalvskattad_halsa_df %>%
      mutate(region = factor(region, levels = unique(region[order(match(regionkod, region_vekt))])))
  } 
    
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(sjalvskattad_halsa_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(sjalvskattad_halsa_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(sjalvskattad_halsa_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) unique(sjalvskattad_halsa_df$regionkod) %>% paste0(collapse = "_") else region_txt
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("sjalvskattad_halsa_df", overrep_df, envir = .GlobalEnv)
  }
  
  
  diagramtitel <- glue("Allmän hälsa (självrapporterat) efter region, kön och år. Andel (procent).{region_txt} År {min(sjalvskattad_halsa_df$År)} - {max(sjalvskattad_halsa_df$År)}")
  diagramfil <- glue("test_fohm_{regionfil_txt}_ar{min(sjalvskattad_halsa_df$År)}_{max(sjalvskattad_halsa_df$År)}.png") %>% str_replace_all("__", "_")
  
  if ("variabel" %in% names(sjalvskattad_halsa_df)) {
     if (length(unique(sjalvskattad_halsa_df$variabel)) > 6) chart_df <- sjalvskattad_halsa_df %>% filter(variabel == unique(sjalvskattad_halsa_df$variabel)[1]) else chart_df <- sjalvskattad_halsa_df
  } else chart_df <- sjalvskattad_halsa_df
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
  			 skickad_x_var = "År",
  			 skickad_y_var = if ("varde" %in% names(chart_df)) "varde" else "names(sjalvskattad_halsa_df)[length(names(sjalvskattad_halsa_df))]",
  			 skickad_x_grupp = if ("variabel" %in% names(chart_df) & length(unique(chart_df$variabel)) > 1) "variabel" else NA,
  			 x_axis_sort_value = FALSE,
  			 diagram_titel = diagramtitel,
  			 diagram_capt = diagram_capt,
  			 stodlinjer_avrunda_fem = TRUE,
  			 filnamn_diagram = diagramfil,
  			 dataetiketter = visa_dataetiketter,
  			 manual_y_axis_title = "",
  			 manual_x_axis_text_vjust = 1,
  			 manual_x_axis_text_hjust = 1,
  			 manual_color = diagram_fargvekt,
  			 output_mapp = output_mapp,
  			 diagram_facet = FALSE,
  			 facet_grp = NA,
  			 facet_scale = "free",
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
}

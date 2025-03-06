diag_brp_per_inv_scb <- function(
  region_vekt = "20",      # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "RIKS1", "RIKS2", "RIKS3", "RIKS4", "RIKS5", "RIKS6", "RIKS7", "RIKS8", "90"
  cont_klartext = "*",       #  Finns: "BRP, löpande priser, mnkr", "BRP, volymutveckling i procent", "BRP per invånare, löpande priser, tkr", "BRP per sysselsatt, löpande priser, tkr", "Medelantal sysselsatta, personer i 1000-tal", "Egentlig lön, löpande priser, mnkr"
  tid_koder = "*",       # "*" = alla år eller månader, "9999" = senaste, finns: "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  #skapa_facet_diagram = TRUE,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  output_mapp = NA,
  skriv_diagramfil = TRUE,
  returnera_data_rmarkdown = FALSE,
  excelfil_mapp = NA,      # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  excel_filnamn = "brp_per_inv.xlsx",      # filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
  diag_lansjmfr_valt_ar = TRUE,
  diag_valt_lan_riket_tidsserie = TRUE
) {

# ==============================================================================================================================
#
# Skriver ut diagram med... . 
#  
#
# ==============================================================================================================================

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/brp_per_inv_jmfr_lan_ar2022.png",
"https://region-dalarna.github.io/utskrivna_diagram/brp_per_inv_tidsserie_20_ar2022.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       glue)

source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_brp_lan_region_tid_NR0105ENS2010T01A_scb.R")
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
  brp_lan_df <- hamta_brp_lan_region_tid_scb(
    region_vekt = hamtaAllaLan(T),			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "RIKS1", "RIKS2", "RIKS3", "RIKS4", "RIKS5", "RIKS6", "RIKS7", "RIKS8", "90"
    cont_klartext = "BRP per invånare, löpande priser, tkr",			 #  Finns: "BRP, löpande priser, mnkr", "BRP, volymutveckling i procent", "BRP per invånare, löpande priser, tkr", "BRP per sysselsatt, löpande priser, tkr", "Medelantal sysselsatta, personer i 1000-tal", "Egentlig lön, löpande priser, mnkr"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "brp_lan.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  ) %>% 
  mutate(region = region %>% skapa_kortnamn_lan(T)))


if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
  write.xlsx(brp_lan_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
}

if(returnera_data_rmarkdown == TRUE){
  assign("brp_lan_df", brp_lan_df, envir = .GlobalEnv)
}

vald_region_txt <- brp_lan_df %>% 
  distinct(region) %>% 
  dplyr::pull() %>%
  skapa_kortnamn_lan() %>% 
  list_komma_och()
  

  if (diag_lansjmfr_valt_ar) {
  
  diagramtitel <- glue("BRP per invånare per län år {max(brp_lan_df$år)}")
  diagramfil <- glue("brp_per_inv_jmfr_lan_ar{max(brp_lan_df$år)}.png")


  gg_obj <- SkapaStapelDiagram(
    skickad_df = brp_lan_df %>% 
      mutate(fokus = case_when(regionkod == region_vekt ~ 1,
                               regionkod == "00" ~ 2,
                               TRUE ~ 0)) %>% 
      filter(år == max(år)),
    skickad_x_var = "region",
    skickad_y_var = names(brp_lan_df)[ncol(brp_lan_df)],
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    x_axis_sort_value = TRUE,
    x_var_fokus = "fokus", 
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    dataetiketter = visa_dataetiketter,
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_color = diag_fargvekt,
    output_mapp = output_mapp,
    lagg_pa_logga = ta_med_logga,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_diagramfil
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
  } # slut if-sats om diag_lansjmfr_valt_ar

if (diag_valt_lan_riket_tidsserie) {
  
  diagramtitel <- glue("Förändring av BRP per invånare år {min(brp_lan_df$år)}-{max(brp_lan_df$år)}")
  diagramfil <- glue("brp_per_inv_tidsserie_{region_vekt}_ar{max(brp_lan_df$år)}.png")
  
  
  gg_obj <- SkapaLinjeDiagram(
    skickad_df = brp_lan_df %>% 
      mutate(fokus = case_when(regionkod == region_vekt ~ 1,
                               regionkod == "00" ~ 2,
                               TRUE ~ 0)) %>% 
      filter(regionkod %in% c(region_vekt, "00")),
    skickad_x_var = "år",
    skickad_y_var = names(brp_lan_df)[ncol(brp_lan_df)],
    skickad_x_grupp = "region",
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    berakna_index = TRUE,
    manual_color = diag_fargvekt,
    output_mapp = output_mapp,
    lagg_pa_logga = ta_med_logga,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_diagramfil
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
}


return(gg_list)
}

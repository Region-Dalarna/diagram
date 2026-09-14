diag_brp_per_inv_scb <- function(
  region_vekt = "20",      # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "RIKS1", "RIKS2", "RIKS3", "RIKS4", "RIKS5", "RIKS6", "RIKS7", "RIKS8", "90"
  cont_klartext = "BRP per invånare, löpande priser, tkr",       #  Finns: "BRP, löpande priser, mnkr", "BRP, volymutveckling i procent", "BRP per invånare, löpande priser, tkr", "BRP per sysselsatt, löpande priser, tkr", "Medelantal sysselsatta, personer i 1000-tal", "Egentlig lön, löpande priser, mnkr"
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
# Skriver ut diagram med BRP per invånare, dels som en länsjämförelse för
# senaste året, dels som en tidsserie för valt län jämfört med riket.
#
# ==============================================================================================================================

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <-
c("https://region-dalarna.github.io/utskrivna_diagram/brp_per_inv_jmfr_lan_ar2022.png",
"https://region-dalarna.github.io/utskrivna_diagram/brp_per_inv_tidsserie_20_ar2022.png")
  purrr::walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  rdverktyg::stop_tyst()
}

# Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
# p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
# stället för library().
if (!requireNamespace("rddiagram", quietly = TRUE)) {
  remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
}
if (!requireNamespace("rdverktyg", quietly = TRUE)) {
  remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
}
if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
# dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

# om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
if (all(is.na(diag_fargvekt))) {
  diag_fargvekt <- rddiagram::diagramfarger("rus_sex")
}

if (all(is.na(output_mapp))) {
  if (dir.exists(rdverktyg::utskriftsmapp())) {
    output_mapp <- rdverktyg::utskriftsmapp()
  } else {
    stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
  }
}

# hamta_data-repots hamta_brp_lan_region_tid_NR0105ENS2010T01A_scb.R (v1:
# NR/NR0105/NR0105A/NR0105ENS2010T01A) hämtas här direkt via
# v2-motsvarigheten TAB3138. Ingen CKM-uppdelning för den här tabellen
# (ekonomisk statistik, inte BE-området). OBS: originalet hårdkodade
# region_vekt = hamtaAllaLan(TRUE) och cont_klartext = "BRP per invånare,
# löpande priser, tkr" direkt i anropet till hamta_brp_lan_region_tid_scb()
# i stället för att koppla in funktionens egna cont_klartext/tid_koder-
# parametrar (region_vekt-parametern används däremot korrekt längre ner,
# för fokusering/filtrering) - kopplat in nu så att parametrarna faktiskt
# styr vad som hämtas.
brp_lan_df <- rdverktyg::suppress_specific_warning(
  pxweb2r::pxweb2_get_data(
    table = "TAB3138",
    query = list(
      Region = rdverktyg::hamtaAllaLan(TRUE),
      ContentsCode = cont_klartext,
      Tid = tid_koder
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod) |>
    dplyr::rename(!!cont_klartext := value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, TRUE))
)


if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
  openxlsx::write.xlsx(brp_lan_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
}

if(returnera_data_rmarkdown == TRUE){
  assign("brp_lan_df", brp_lan_df, envir = .GlobalEnv)
}

vald_region_txt <- brp_lan_df |>
  dplyr::distinct(region) |>
  dplyr::pull() |>
  rdverktyg::skapa_kortnamn_lan() |>
  rdverktyg::list_komma_och()


  if (diag_lansjmfr_valt_ar) {

  diagramtitel <- glue::glue("BRP per invånare per län år {max(brp_lan_df$år)}")
  diagramfil <- glue::glue("brp_per_inv_jmfr_lan_ar{max(brp_lan_df$år)}.png")


  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = brp_lan_df |>
      dplyr::mutate(fokus = dplyr::case_when(regionkod == region_vekt ~ 1,
                               regionkod == "00" ~ 2,
                               TRUE ~ 0)) |>
      dplyr::filter(år == max(år)),
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

  ett_diagram <- list(gg_obj)
  names(ett_diagram) <- stringr::str_remove(diagramfil, "\\.png")

  } else ett_diagram <- list()

if (diag_valt_lan_riket_tidsserie) {

  diagramtitel <- glue::glue("Förändring av BRP per invånare år {min(brp_lan_df$år)}-{max(brp_lan_df$år)}")
  diagramfil <- glue::glue("brp_per_inv_tidsserie_{region_vekt}_ar{max(brp_lan_df$år)}.png")


  gg_obj <- rddiagram::SkapaLinjeDiagram(
    skickad_df = brp_lan_df |>
      dplyr::mutate(fokus = dplyr::case_when(regionkod == region_vekt ~ 1,
                               regionkod == "00" ~ 2,
                               TRUE ~ 0)) |>
      dplyr::filter(regionkod %in% c(region_vekt, "00")),
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

  ett_diagram2 <- list(gg_obj)
  names(ett_diagram2) <- stringr::str_remove(diagramfil, "\\.png")

} else ett_diagram2 <- list()


return(c(ett_diagram, ett_diagram2))
}

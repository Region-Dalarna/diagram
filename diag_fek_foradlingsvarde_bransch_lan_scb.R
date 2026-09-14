diag_fek_foradlingsvarde_bransch_lan_scb <- function(
  region_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "SE0", "SE00", "SE1", "SE11", "SE110", "SE12", "SE121", "SE122", "SE123", "SE124", "SE125", "SE2", "SE21", "SE211", "SE212", "SE213", "SE214", "SE22", "SE221", "SE224", "SE23", "SE231", "SE232", "SE3", "SE31", "SE311", "SE312", "SE313", "SE32", "SE321", "SE322", "SE33", "SE331", "SE332"
  sni2007_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "A-SexklK-O samtliga näringsgrenar (exkl. K+O+T+U)", "A-01-03 jordbruk, skogsbruk och fiske", "B-05-09 utvinning av mineral", "C-10-33 tillverkning", "D-35 försörjning av el, gas, värme och kyla", "E-36-39 vattenförsörjning; avloppsrening, avfallshantering och sanering", "F-41-43 byggverksamhet", "G-45-47 handel; reparation av motorfordon och motorcyklar", "H-49-53 transport och magasinering", "I-55-56 hotell- och restaurangverksamhet", "J-58-63 informations- och kommunikationsverksamhet", "L-68 fastighetsverksamhet", "M-69-75 verksamhet inom juridik, ekonomi, vetenskap och teknik", "N-77-82 uthyrning, fastighetsservice, resetjänster och andra stödtjänster", "P-85 utbildning", "Q-86-88 vård och omsorg; sociala tjänster", "R-90-93 kultur, nöje och fritid", "S-94-96 annan serviceverksamhet"
  cont_klartext = "Förädlingsvärde, mnkr",			 #  Finns: "Antal arbetsställen (lokala verksamheter)", "Antal anställda", "Nettoomsättning exkl. merchantingkostnader, mnkr", "Produktionsvärde, mnkr", "Förädlingsvärde, mnkr", "Totala intäkter, mnkr", "Totala kostnader, mnkr"
  tid_koder = "*",			 # "*" = de tre senaste åren (grupperade per år i diagrammet), "9999" = enbart senaste året, eller ange enskilda år, finns: "2022", "2023", "2024"
  gruppera_namn = NA,              # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
  diagram_capt = "Källa: Företagens ekonomi i SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förädlingsvärde är den faktiska produktionen minus kostnader för köpta varor och tjänster, dock ej löner, sociala avgifter och kostnader för handelsvaror.",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  output_mapp = NA,
  skriv_diagramfil = TRUE,
  returnera_data_rmarkdown = FALSE,
  demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
  excelfil_mapp = NA,      # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  excel_filnamn = "helarsekvivalenter.xlsx"      # filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
) {

# ==============================================================================================================================
#
# Skriver ut diagram med förädlingsvärde per bransch.
#
#
# ==============================================================================================================================

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <-
c("https://region-dalarna.github.io/utskrivna_diagram/foradlingsvarde_bransch_20_ar2022.png")
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
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
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

gg_list <- list()

# hamta_data-repots hamta_fek_lve_region_sni2007_tid_NSEBasfaktaLVEngs07_
# scb.R (v1: NV/NV0109/NV0109P/NSEBasfaktaLVEngs07) hämtas här direkt via
# v2-motsvarigheten TAB6329. Tabellen (regionala basfakta för
# verksamhetsnivå enligt Företagens ekonomi) har bara funnits sedan 2022 -
# det finns ingen äldre/historisk tabell att slå ihop med (v1-skriptets
# egen kommentar listade också bara "2022" som giltigt år när det
# skrevs). "9999" (senaste år) är en v1-specifik sentinel som pxweb2r
# inte förstår - hanteras explicit nedan.
#
# Standardvärdet "*" gav tidigare alla tillgängliga år, vilket när
# tabellen bara hade ett år (2022) inte gjorde någon skillnad - men nu
# när tabellen har tre år (2022-2024) och diagrammet inte grupperade på
# år summerades alla årens värden osynligt ihop till en enda stapel per
# branschgrupp. "*" betyder därför nu i stället de tre senaste
# tillgängliga åren, som visas grupperade per år i diagrammet (se
# skickad_x_grupp nedan) - vill man ha ett enskilt år anges det
# (eller "9999" för enbart senaste året).
if (identical(tid_koder, "*")) {
  giltiga_ar <- pxweb2r::pxweb2_get_values("TAB6329", "Tid", quiet = TRUE)$code
  tid_koder <- utils::tail(sort(giltiga_ar), 3)
} else if (any(tid_koder == "9999")) {
  senaste_ar <- max(pxweb2r::pxweb2_get_values("TAB6329", "Tid", quiet = TRUE)$code)
  tid_koder <- ifelse(tid_koder == "9999", senaste_ar, tid_koder)
}

foradl_df <- rdverktyg::suppress_specific_warning(
  pxweb2r::pxweb2_get_data(
    table = "TAB6329",
    query = list(
      Region = region_vekt,
      SNI2007 = sni2007_klartext,
      ContentsCode = cont_klartext,
      Tid = tid_koder
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)
)

branschnyckel <- readxl::read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") |>
  dplyr::select(Avdelning, Grupp_kod, Branschgrupp) |>
  dplyr::distinct()

chart_df <- foradl_df |>
  dplyr::mutate(branschbokstav = stringr::str_sub(`näringsgren SNI 2007`, 1, 1)) |>
  dplyr::filter(!stringr::str_detect(`näringsgren SNI 2007`, "samtliga näringsgrenar")) |>
  dplyr::left_join(branschnyckel, by = c("branschbokstav" = "Avdelning")) |>
  dplyr::group_by(år, regionkod, region, Grupp_kod, Branschgrupp, variabel) |>
  dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")

# om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
# så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
if (!is.na(gruppera_namn)) {
  chart_df <- chart_df |>
    dplyr::group_by(dplyr::across(-c(regionkod, region, varde))) |>
    dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(regionkod = "gg",
           region = gruppera_namn) |>
    dplyr::relocate(region, .before = 1) |>
    dplyr::relocate(regionkod, .before = region)

region_vekt <- "gg"
}

if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
  openxlsx::write.xlsx(chart_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
}

if(returnera_data_rmarkdown == TRUE){
  assign("chart_df", chart_df, envir = .GlobalEnv)
}

vald_region_txt <- chart_df |>
  dplyr::distinct(region) |>
  dplyr::pull() |>
  rdverktyg::list_komma_och() |>
  rdverktyg::skapa_kortnamn_lan()

ar_txt <- chart_df |>
  dplyr::distinct(år) |>
  dplyr::pull() |>
  rdverktyg::list_komma_och()

flera_ar <- length(unique(chart_df$år)) > 1

diagramtitel <- glue::glue("Förädlingsvärde i {vald_region_txt} per bransch år {ar_txt}")
diagramfil <- glue::glue("foradlingsvarde_bransch_{paste0(region_vekt, collapse = '_')}_ar{ar_txt}.png")


gg_obj <- rddiagram::SkapaStapelDiagram(
  skickad_df = chart_df,
  skickad_x_var = "Branschgrupp",
  skickad_y_var = "varde",
  skickad_x_grupp = if (flera_ar) "år" else NULL,
  diagram_titel = diagramtitel,
  diagram_capt = diagram_capt,
  x_axis_sort_value = TRUE,
  stodlinjer_avrunda_fem = TRUE,
  filnamn_diagram = diagramfil,
  dataetiketter = visa_dataetiketter,
  manual_y_axis_title = cont_klartext,
  manual_x_axis_text_vjust = 1,
  manual_x_axis_text_hjust = 1,
  x_axis_lutning = 45,
  manual_color = if (flera_ar) diag_fargvekt[seq_len(length(unique(chart_df$år)))] else diag_fargvekt,
  output_mapp = output_mapp,
  lagg_pa_logga = ta_med_logga,
  logga_path = logga_sokvag,
  skriv_till_diagramfil = skriv_diagramfil
)

gg_list <- c(gg_list, list(gg_obj))
names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")

return(gg_list)

} # slut diag-funktion

diag_nettoinkomst_kon_aldersgrupp_scb <- function(
    region_vekt = "20",
    alder_koder = c("16-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84"),			 #  Finns: "20-64", "16", "16-19", "16+", "17", "18", "19", "20", "20+", "20-24", "21", "22", "23", "24", "25", "25-29", "26", "27", "28", "29", "30", "30-34", "31", "32", "33", "34", "35", "35-39", "36", "37", "38", "39", "40", "40-44", "41", "42", "43", "44", "45", "45-49", "46", "47", "48", "49", "50", "50-54", "51", "52", "53", "54", "55", "55-59", "56", "57", "58", "59", "60", "60-64", "61", "62", "63", "64", "65", "65-69", "65+", "66", "67", "68", "69", "70", "70-74", "71", "72", "73", "74", "75", "75-79", "76", "77", "78", "79", "80", "80-84", "81", "82", "83", "84", "85", "85+", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100+"
    cont_klartext = "Medianinkomst, tkr",			 #  Finns: "Medelinkomst, tkr", "Medianinkomst, tkr", "Totalsumma, mnkr", "Antal personer"
    output_mapp,
    logga_i_diagram = NA,                      # ange sökväg och filnamn till en logga om man vill ha en med i diagrammet
    diag_fargvekt = NA,                        # färgvektor som används i diagrammet, om inte anges används R:s standardfärger
    ar_fokus = "9999",                         # vilket år väljs i diag_valt_ar_kon_diff_aldersgrupper, "9999" = senaste år
    visa_dataetiketter = FALSE,                # om man vill ha med dataetiketter i diagrammet
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    skriv_diagramfil = TRUE,                   # skriv en diagrambildfil
    diag_tidsserie_kon_facet_aldersgrupper = TRUE,       # diagram med tidsserie över medianinkomst per kön där åldersgrupper ligger som facets
    diag_tidsserie_kon_diff_facet_aldersgrupper = TRUE,  # diagram med tidsserie över kvinnors andel av mäns inkomster där åldersgrupper ligger som facets
    demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    diag_valt_ar_kon_diff_aldersgrupper = TRUE           # diagram med diff i medianinkomst mellan män och kvinnor för valt år, åldersgrupper på x-axeln
    ){

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
  c("https://region-dalarna.github.io/utskrivna_diagram/diff_medianinkomst_Dalarna_2022.png",
  "https://region-dalarna.github.io/utskrivna_diagram/diff_medianinkomst_Dalarna_ar2000_2022.png",
  "https://region-dalarna.github.io/utskrivna_diagram/nettoinkomst_Dalarna_ar2000_2022.png")
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
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("kon")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("kon")
  }

  gg_list <- list()

  # hamta_data-repots hamta_nettoinkomst_region_kon_alder_tid_NetInk02_
  # scb.R (v1: HE/HE0110/HE0110A/NetInk02) hämtas här direkt via
  # v2-motsvarigheten TAB4840. Ingen CKM-uppdelning - tabellen täcker
  # hela 2000-2024 utan uppdelning. Åldersvärdena skickas som koder
  # (t.ex. "20-24"), inte klartext, så ingen gemenhetstecken-konvertering
  # behövs.
  nettoinkomst_df <- pxweb2r::pxweb2_get_data(
    table = "TAB4840",
    query = list(
      Region = region_vekt,
      Kon = c("män", "kvinnor"),
      Alder = alder_koder,
      ContentsCode = cont_klartext,
      Tid = "*"
    )) |>
    dplyr::rename(regionkod = region_kod) |>
    dplyr::rename(!!cont_klartext := value) |>
    dplyr::select(-tabellinnehåll)

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(nettoinkomst_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(nettoinkomst_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(nettoinkomst_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  # OBS: originalet skrev "region_txt <- paste0(' i ', region_txt)" här,
  # men alla tre diagramtitlarna nedan skriver redan ut "i {region_txt}"
  # själva - gav en synlig dubblerad preposition ("... boende i i
  # Dalarna"). Borttaget.

  # den beräknade diff-datan (kvinnors andel av mäns inkomst) används av
  # både diag_tidsserie_kon_diff_facet_aldersgrupper och
  # diag_valt_ar_kon_diff_aldersgrupper - beräknas en gång här om någon
  # av dem är vald, i stället för (som originalet gjorde) bara inuti det
  # första blocket. Det gjorde att diag_valt_ar_kon_diff_aldersgrupper
  # kraschade med "object 'chart_diff_df' not found" om man valde det
  # diagrammet utan att också välja diag_tidsserie_kon_diff_facet_
  # aldersgrupper.
  if (diag_tidsserie_kon_diff_facet_aldersgrupper || diag_valt_ar_kon_diff_aldersgrupper) {
    chart_diff_df <- nettoinkomst_df |>
      tidyr::pivot_wider(names_from = kön, values_from = !!cont_klartext) |>
      dplyr::mutate(diff = (kvinnor / män)*100,
             diff_over = ifelse(diff > 100, diff - 100, 0),
             diff_under = ifelse(diff < 100, diff, 100)) |>
      tidyr::pivot_longer(cols = c("diff_over", "diff_under"), names_to = "diff_typ", values_to = "diff_varde")
  }

  if (diag_tidsserie_kon_facet_aldersgrupper) {
    # diagram för medianinkomst med åldersgrupper som facets
    diagramtitel <- glue::glue("Nettoinkomst för boende i {region_txt} år {min(nettoinkomst_df$år)} - {max(nettoinkomst_df$år)}")
    diagramfil <- stringr::str_replace_all(glue::glue("nettoinkomst_{regionfil_txt}_ar{min(nettoinkomst_df$år)}_{max(nettoinkomst_df$år)}.png"), "__", "_")

    chart_df <- nettoinkomst_df

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_df,
    			 skickad_x_var = "år",
    			 skickad_y_var = cont_klartext,
    			 skickad_x_grupp = "kön",
    			 x_axis_sort_value = FALSE,
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_x_axis_text_vjust = 1,
    			 manual_x_axis_text_hjust = 1,
    			 manual_y_axis_title = NULL,
    			 manual_color = rddiagram::diagramfarger("kon"),
    			 output_mapp = output_mapp,
    			 facet_grp = "ålder",
    			 facet_scale = "fixed",
    			 facet_x_axis_storlek = 6,
    			 lagg_pa_logga = !is.na(logga_i_diagram),
    			 logga_path = if (is.na(logga_i_diagram)) NA else logga_i_diagram,
    			 skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")

  } # slut test om diag_tidsserie_kon_facet_aldersgrupper är TRUE

  if (diag_tidsserie_kon_diff_facet_aldersgrupper) {

    # diagram över diff i medianinkomst mellan män och kvinnor, åldersgrupper som facet
    diagramtitel <- glue::glue("Kvinnors andel av mäns nettoinkomst i {region_txt} år {min(nettoinkomst_df$år)} - {max(nettoinkomst_df$år)}")
    diagramfil <- stringr::str_replace_all(glue::glue("diff_medianinkomst_{regionfil_txt}_ar{min(nettoinkomst_df$år)}_{max(nettoinkomst_df$år)}.png"), "__", "_")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_diff_df,
      skickad_x_var = "år",
      skickad_y_var = "diff_varde",
      skickad_x_grupp = "diff_typ",
      geom_position_stack = TRUE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = rddiagram::diagramfarger("rus_sex"),
      manual_y_axis_title = "procent",
      output_mapp = output_mapp,
      facet_grp = "ålder",
      facet_scale = "fixed",
      facet_x_axis_storlek = 7,
      lagg_pa_logga = !is.na(logga_i_diagram),
      logga_path = if (is.na(logga_i_diagram)) NA else logga_i_diagram,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")

  } # slut test om diag_tidsserie_kon_diff_facet_aldersgrupper är TRUE


  if (diag_valt_ar_kon_diff_aldersgrupper){

    # "9999" (senaste år) är en v1-specifik sentinel - löses upp explicit
    vald_ar <- if (identical(ar_fokus, "9999")) max(chart_diff_df$år) else ar_fokus

    # diagram över diff i medianinkomst mellan män och kvinnor, bara för valt år, ej facetdiagram
    diagramtitel <- glue::glue("Kvinnors andel av mäns nettoinkomst i {region_txt} år {vald_ar}")
    diagramfil <- stringr::str_replace_all(glue::glue("diff_medianinkomst_{regionfil_txt}_{vald_ar}.png"), "__", "_")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(chart_diff_df, år == vald_ar),
      skickad_x_var = "ålder",
      skickad_y_var = "diff_varde",
      skickad_x_grupp = "diff_typ",
      geom_position_stack = TRUE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = rddiagram::diagramfarger("rus_sex")[c(2,1)],
      manual_y_axis_title = "procent",
      output_mapp = output_mapp,
      legend_tabort = TRUE,
      lagg_pa_logga = !is.na(logga_i_diagram),
      logga_path = if (is.na(logga_i_diagram)) NA else logga_i_diagram,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")

    } # slut test om diag_valt_ar_kon_diff_aldersgrupper är TRUE

  return(gg_list)
} # slut funktion

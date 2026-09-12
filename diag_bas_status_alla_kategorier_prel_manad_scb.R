# ===========================================================================================================
#
# Skript för att skriva ut stapeldiagram över de olika kategorierna i BAS per månad (från BAS, SCB), främst
# utan uppdelning på kön eller inrikes och utrikes födda, för de(n) region(er) man valt.
# Default är åldersgruppen 20-64 år men det finns fler åldersgrupper att välja på i tabellen.
# Gå in på url:en för att se vilka som finns tillgängliga.
#
# Följande diagram finns i funktionen diag_bas_alla_kat():
#             1. diag_senaste_ar - senaste året med samtliga kategorier eller de man väljer själv att visa
#             2. diag_tidsserie - en tidssere med samtliga månader som finns i BAS
#             3. diag_fokusvariabel - en eller flera variabler som man vill titta extra på i en tidsserie
#
# ===========================================================================================================

diag_bas_status_alla_kat_prel_manad_scb <- function(
    region_vekt = "20",                 # c("20", "17", "21"),
    alder_vekt = "20-64 år",            # finns också: 15-19 år, 16-19 år, 20-24 år, 25-29 år, 30-34 år, 35-39 år, 40-44 år, 45-49 år, 50-54 år, 55-59 år, 60-64 år, 65-69 år, 70-74 år, 15-74 år, 16-64 år, 16-65 år, 20-64 år, 20-65 år
    kon_vekt = "Totalt",                # man måste välja totalt, män ELLER kvinnor
    fodelseregion_vekt = "totalt",      # man måste välja totalt, inrikes födda ELLER utrikes födda
    cont_uttag = c("antal sysselsatta", "antal arbetslösa", "antal studerande", "antal pensionärer", "antal sjuka", "antal övriga"),
    gruppering_namn = NA,
    visa_andelar = FALSE,
    fokus_variabel = "antal övriga",
    titel_bredd = 80,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    diag_fargvekt = NA,
    skickad_logga_path = NA,               # NA om man vill köra med standard, bra att sätta en lokal om man är på resande fot
    ta_med_logga = TRUE,
    skriv_till_diagramfil = TRUE,
    diag_senaste_ar = FALSE,
    diag_tidsserie = TRUE,
    diag_tidsserie_facet_kat = TRUE,
    diag_fokusvariabel = TRUE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna (den
  # senare pekade dessutom mot en lokal fil på G: som aldrig fanns med i
  # GitHub-repot) och inget p_load(tidyverse). Anropas med fullt namespace
  # (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  # dplyr/tidyr/stringr/purrr följer med som beroenden till rddiagram/rdverktyg.

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/bas_alla_kat_facet_tid_20_64ar_20.png",
        "https://region-dalarna.github.io/utskrivna_diagram/bas_alla_kat_tid_20_64ar_20.png",
        "https://region-dalarna.github.io/utskrivna_diagram/bas_antal övriga_tid_20_64ar_20.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rev(rddiagram::diagramfarger("rus_sex"))
  }

  if (all(is.na(output_mapp))) {
    output_mapp <- rdverktyg::utskriftsmapp()
  }

  retur_list <- list()

  # SCB skriver åldersintervallen med tankstreck ("20–64 år", U+2013), inte
  # vanligt bindestreck. Normaliserar bindestreck mellan siffror till
  # tankstreck här, samma fix som i systerskripten för denna tabell.
  alder_vekt_fetch <- gsub("(?<=[0-9])-(?=[0-9])", "–", alder_vekt, perl = TRUE)

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # (samma tabell, TAB6260, som redan används i flera andra migrerade skript.)
  # Datan är redan i long-format via pxweb2r, så vi hoppar över den
  # wide->long-omvandling som den gamla hamta_data-funktionen (long_format =
  # FALSE, sedan pivot_longer() här i skriptet) behövde göra.
  bas_aggr <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = region_vekt,
      Kon = kon_vekt,
      Alder = alder_vekt_fetch,
      Fodelseregion = fodelseregion_vekt,
      ContentsCode = cont_uttag,
      Tid = "*"
    )) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value) |>
    rdverktyg::manader_bearbeta_scbtabeller() |>
    dplyr::mutate(variabel = factor(variabel, levels = rev(c("antal sysselsatta", "antal arbetslösa", "antal studerande", "antal pensionärer", "antal sjuka", "antal övriga" ))))

  vald_region <- if(is.na(gruppering_namn)) {
    rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region, byt_ut_riket_mot_sverige = TRUE))
  } else {
    vald_region <- gruppering_namn
  } # slut if-sats om gruppering_namn är medskickat eller inte


  if (!is.na(gruppering_namn)) {
    bas_aggr <- bas_aggr |>
      dplyr::group_by(kön, ålder, födelseregion, tid, år, månad, år_månad, månad_år, variabel) |>
      dplyr::summarise(varde := sum(varde, na.rm = TRUE), .groups = "drop_last") |>
      dplyr::mutate(andel = round((varde/sum(varde))*100,1)) |>
      dplyr::ungroup() |>
      dplyr::mutate(regionkod = "grp",
             region = gruppering_namn) |>
      dplyr::relocate(regionkod, .before = 1) |>
      dplyr::relocate(region, .after = regionkod)

  } else { # slut if-sats om man vill gruppera

    bas_aggr <- bas_aggr |>
      dplyr::group_by(regionkod, region, kön, ålder, födelseregion, tid, år, månad, år_månad, månad_år, variabel) |>
      dplyr::summarise(varde := sum(varde, na.rm = TRUE), .groups = "drop_last") |>
      dplyr::mutate(andel = round((varde/sum(varde))*100,1)) |>
      dplyr::ungroup()
  }

  # hantera åldrar
  alla_aldrar <- stringr::str_extract_all(alder_vekt, "\\d+")
  alder_min <- min(unlist(alla_aldrar))
  alder_max <- max(unlist(alla_aldrar))

  aldrar_min_till_max <- c(alder_min:alder_max)        # gör en vektor med alla nummer mellan min- och maxvärden
  alla_ar_i_grupp <- unlist(purrr::map(alla_aldrar, ~ (purrr::pluck(.x, 1)):(purrr::pluck(.x, 2))))       # gör sekvenser med alla åldersgrupper

  if (all(aldrar_min_till_max == alla_ar_i_grupp)) {
    # om sekvensen som går från min till max är samma som sekvenser för samtliga åldersgrupper så
    # är alla åldrar i ordning och finns för alla år, då lägger vi ihop dem till "en" åldersgrupp

      aldrar_titel <- paste0(min(alla_ar_i_grupp), "-", max(alla_ar_i_grupp), " år")
      aldrar_txt <- paste0(min(alla_ar_i_grupp), "_", max(alla_ar_i_grupp), "ar")
  } else {
    # om sekvensom som går från min till max INTE är samma som sekvenser för samtliga åldersgrupper
    # så har användaren valt åldersgrupper som inte ligger "intill" varandra, då behöver åldersgrupperna
    # redovisas var och en för sig
    aldrar_titel <- aldrar_titel
    aldrar_txt <- aldrar_txt

  }

  if (diag_senaste_ar) {

    diagram_titel <- paste0("Arbetsmarknadsstatus för befolkningen ", aldrar_titel, " i ", vald_region, " i ", unique(dplyr::pull(dplyr::select(dplyr::filter(bas_aggr, tid == max(tid)), månad_år))))
    diagramfil <- paste0("bas_alla_kat_", aldrar_txt, "_", paste0(region_vekt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(bas_aggr, tid == max(tid)),
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      x_axis_lutning = 0,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter = FALSE,
      dataetiketter_justering_hojdled = -1.7,
      manual_y_axis_title = "antal individer",
      manual_color = diag_fargvekt,
      logga_path = skickad_logga_path,
      lagg_pa_logga = ta_med_logga,
      skriv_till_diagramfil = skriv_till_diagramfil,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- stringr::str_remove(diagramfil, ".png")

  } # slut if-sats diag_senaste_ar


  if (diag_tidsserie) {

    diagram_titel <- paste0("Arbetsmarknadsstatus för befolkningen ", aldrar_titel, " i ", vald_region)
    diagramfil <- paste0("bas_alla_kat_tid_", aldrar_txt, "_", paste0(region_vekt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = bas_aggr,
      skickad_x_var = "månad_år",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_hjust = 1,
      manual_x_axis_text_vjust = 1,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter = FALSE,
      dataetiketter_justering_hojdled = -1.7,
      manual_y_axis_title = "antal individer",
      manual_color = diag_fargvekt,
      facet_grp = if (length(unique(bas_aggr$regionkod)) > 1) "region" else NULL,
      facet_x_axis_storlek = 5,
      facet_legend_bottom = TRUE,
      skriv_till_diagramfil = skriv_till_diagramfil,
      lagg_pa_logga = ta_med_logga,
      logga_path = skickad_logga_path,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- stringr::str_remove(diagramfil, ".png")

  } # slut if-sats diag_tidsserie

  if (diag_tidsserie_facet_kat) {

    diagram_titel <- paste0("Arbetsmarknadsstatus för befolkningen ", aldrar_titel, " i ", vald_region)
    diagramfil <- paste0("bas_alla_kat_facet_tid_", aldrar_txt, "_", paste0(region_vekt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = bas_aggr,
      skickad_x_var = "månad_år",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_hjust = 1,
      manual_x_axis_text_vjust = 1,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      x_axis_visa_var_xe_etikett = 2,
      dataetiketter = FALSE,
      dataetiketter_justering_hojdled = -1.7,
      manual_y_axis_title = "antal individer",
      manual_color = diag_fargvekt,
      facet_grp = "variabel",
      facet_x_axis_storlek = 5,
      facet_legend_bottom = FALSE,
      facet_scale = "free",
      skriv_till_diagramfil = skriv_till_diagramfil,
      lagg_pa_logga = ta_med_logga,
      logga_path = skickad_logga_path,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- stringr::str_remove(diagramfil, ".png")
  } # slut if-sats diag_tidsserie_facet_kat




  if (diag_fokusvariabel) {

    diagram_titel <- stringr::str_wrap(paste0("Arbetsmarknadsstatus för befolkningen ", aldrar_titel, " i ", vald_region, " i kategorin ", stringr::str_remove(fokus_variabel, "antal")), titel_bredd)
    diagramfil <- paste0("bas_", fokus_variabel, "_tid_", aldrar_txt, "_", paste0(region_vekt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(bas_aggr, variabel %in% fokus_variabel),
      skickad_x_var = "månad_år",
      skickad_y_var = "varde",
      skickad_x_grupp = if (length(fokus_variabel) > 1) "variabel" else NA,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_hjust = 1,
      manual_x_axis_text_vjust = 1,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter = FALSE,
      dataetiketter_justering_hojdled = -1.7,
      manual_y_axis_title = "antal individer",
      manual_color = if (length(fokus_variabel) > 1) diag_fargvekt else diag_fargvekt[1],
      facet_grp = if (length(unique(bas_aggr$regionkod)) > 1) "region" else NULL,
      facet_x_axis_storlek = 5,
      facet_legend_bottom = TRUE,
      facet_scale = "fixed",
      skriv_till_diagramfil = skriv_till_diagramfil,
      lagg_pa_logga = ta_med_logga,
      logga_path = skickad_logga_path,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- stringr::str_remove(diagramfil, ".png")

  } # slut if-sats diag_senaste_ar
  return(retur_list)
} # slut funktion

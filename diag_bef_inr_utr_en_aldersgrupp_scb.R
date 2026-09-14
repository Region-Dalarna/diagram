diag_bef_inr_utr_en_aldersgrupp <- function(
    region_vekt = "20",                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region
    aldersintervall = c(20, 65),
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
        # om <prognos_ar> ligger med i diagram_capt så byts det ut mot det år prognosen gjordes
    output_mapp = NA,                                        # här sparas diagramet
    diagram_fargvekt = NA,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    stodlinjer_avrunda_fem = TRUE,
    returnera_dataframe_global_environment = FALSE,
    ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
    skriv_till_diagramfil = TRUE,
    skriv_till_excelfil = FALSE
) {

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

  gg_list <- list()

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(diagram_fargvekt))) {
    diagram_fargvekt <- rddiagram::diagramfarger("rus_sex")[c(1,2)]
  }

  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_till_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(rdverktyg::utskriftsmapp())) {
        output_mapp <- rdverktyg::utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }

  if (length(aldersintervall) != 2) stop("Parametern aldersintervall måste innehålla två värden. Det ena är från och med åldern, det andra till och med ålder. aldersintervall = c(20,65) blir alltså åldersgruppen 20-65 år.")

  aldersgrupp_txt <- glue::glue("{min(aldersintervall)}-{max(aldersintervall)} år")

  # hamta_data-repots hamta_bef_region_alder_kon_fodelseregion_tid_
  # InrUtrFoddaRegAlKon_scb.R (v1: BE0101E/InrUtrFoddaRegAlKon) hämtas här
  # direkt via v2-motsvarigheterna TAB4823 (historik 2000-2024) och TAB6645
  # (CKM 2025-), samma tabellpar som redan verifierats i
  # diag_bef_inrikes_utrikes_antal_forandring_prognos_IntRap.R. Individuella
  # åldrar (inte "totalt") begärs alltid explicit eftersom historiktabellen
  # saknar en "totalt"-kod för Alder - ren digitkod (t.ex. "20") funkar för
  # båda tabellerna så länge intervallet inte når 100+ år (skulle det
  # hända krävs samma "hamta_individuella_aldrar()"-hjälpfunktion som i
  # systerskriptet).
  alder_koder_hamta <- as.character(min(aldersintervall):max(aldersintervall))

  bef_folkmangd_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB4823",
    query = list(
      Region = region_vekt,
      Alder = alder_koder_hamta,
      Kon = c("män","kvinnor"),
      Fodelseregion = c("född i Sverige","utrikes född"),
      ContentsCode = "Antal",
      Tid = "*"
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  bef_folkmangd_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6645",
    query = list(
      Region = region_vekt,
      Alder = alder_koder_hamta,
      Kon = c("män","kvinnor"),
      Fodelseregion = c("född i Sverige","utrikes född"),
      ContentsCode = "Antal",
      Tid = "*"
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  bef_folkmangd <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
    dplyr::bind_rows(bef_folkmangd_historik, bef_folkmangd_ckm) |>
      dplyr::rename(regionkod = region_kod, Antal = value) |>
      dplyr::select(-tabellinnehåll))

  diagram_df <- bef_folkmangd |>
      dplyr::group_by(år, regionkod, region, bakgrund = födelseregion) |>
      dplyr::summarise(antal = sum(Antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(aldersgrupp = aldersgrupp_txt,
             bakgrund = ifelse(bakgrund == "utrikes född", "Utrikes födda", "Inrikes födda"),
             bakgrund = factor(bakgrund, levels = c("Utrikes födda", "Inrikes födda")),
             region = rdverktyg::skapa_kortnamn_lan(region))

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("bef_inr_utr_en_aldersgrupp", diagram_df, envir = .GlobalEnv)
  }

  if (aldersgrupp_txt %in% c("20-64 år", "20-65 år")) {
    aldersgrupp_txt <- paste0("i arbetsför ålder (", aldersgrupp_txt, ")")
  }

  skapa_diagram <- function(skickad_regionkod) {

    diagram_region_df <- dplyr::filter(diagram_df, regionkod %in% skickad_regionkod)

    region_txt <- rdverktyg::skapa_kortnamn_lan(unique(diagram_region_df$region))

    diagramtitel <- glue::glue("Befolkning {aldersgrupp_txt} i {region_txt}")
    diagramfil <- glue::glue("bef_inr_utr_{aldersgrupp_txt}_{region_txt}_ar{min(diagram_region_df$år)}-{max(diagram_region_df$år)}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = diagram_region_df,
      skickad_x_var = "år",
      skickad_y_var = "antal",
      skickad_x_grupp = "bakgrund",
      diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      filnamn_diagram = diagramfil,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diagram_fargvekt,
      output_mapp = output_mapp,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      dataetiketter = visa_dataetiketter,
      geom_position_stack = TRUE,
      legend_vand_ordning = TRUE,
      facet_legend_bottom = TRUE
    ) # slut skriv ggplot_objekt

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

    return(gg_list)

  } # slut funktion som skapar diagrammet

  retur_list <- purrr::flatten(purrr::map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)))

  if (skriv_till_excelfil) {
    region_xlsx <- paste0(rdverktyg::skapa_kortnamn_lan(unique(diagram_df$region)), collapse = "_")
    excefilnamn <- glue::glue("bef_inr_utr_{aldersgrupp_txt}_{region_xlsx}_ar{min(diagram_df$år)}-{max(diagram_df$år)}.xlsx")
    openxlsx::write.xlsx(diagram_df, paste0(output_mapp, excefilnamn))
  }

  return(retur_list)

} # slut funktion

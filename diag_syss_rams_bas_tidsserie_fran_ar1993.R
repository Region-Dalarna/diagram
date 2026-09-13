
diag_syss_rams_bas_tidssserie_fran_ar1993 <- function(
  region_vekt = "20",
  inrikesutrikes_klartext = "*",      #  NA = tas inte med i uttaget,  Finns: "inrikes födda", "utrikes födda", "inrikes och utrikes födda"
  kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "kvinnor", "män", "kvinnor och män"
  tid_koder = "*",
  visa_dataetiketter = FALSE,
  diagram_capt = "Källa: RAMS och BAS i SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna\nBeskrivning: Det är ett tidsseriebrott år 2020 så jämförelser med år innan detta bör göras med viss försiktighet.",
  output_mapp = NA,
  skriv_diagramfil = TRUE,      # TRUE skrivs till fil (output_mapp måste finnas) annars returneras enbart ett ggplot-objekt
  excelfil_mapp = NA,
  excel_filnamn = NA
  ) {

  # ===========================================================================================================
  #
  # Migrerad till pxweb2r/rddiagram/rdverktyg. hamta_data-repots hamta_rams_bas_region_inrikesutrikes_kon_tid_scb()
  # slog ihop fyra v1-tabeller. Alla fyra finns kvar i SCB:s v1-API men motsvaras nu av fyra v2-tabeller:
  #   AM0207/AM0207B/RAMSForvInt03  (RAMS 1993-2003)               -> TAB4334
  #   AM0207/AM0207J/RAMSForvInt04  (RAMS 2004-2018)                -> TAB4356
  #   AM0207/AM0207Z/RamsForvInt04N (RAMS, ny tidsserie 2019-2021) -> TAB5117
  #   AM0210/AM0210D/ArRegArbStatus (BAS 2020-)                     -> TAB3200
  # Används bara av det här skriptet - logiken läggs därför in direkt här i stället för i rdverktyg.
  #
  # OBS: pxweb2r skiljer på att UTELÄMNA en variabel ur frågan helt (ger då alla enskilda värden,
  # ingen automatisk summering) och att skicka med NA som värde för en elimination = TRUE-variabel
  # (ger då SCB:s summerade "totalt" - bekräftat genom test). Där originalet tog bort en nyckel ur
  # varlista helt för att få en total (v1:s elimination-beteende vid utelämnad variabel) skickas här
  # i stället NA explicit som variabelns värde.
  # ===========================================================================================================

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

  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(rdverktyg::utskriftsmapp())) {
        output_mapp <- rdverktyg::utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }

  gg_list <- list()

  # =============================================== API-uttag ===============================================

  hamta_en_syss_tabell <- function(tabell_id, ar_bas_tabell) {

    giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid")$code
    tid_vekt <- if (identical(tid_koder, "*")) giltiga_ar else as.character(tid_koder)[as.character(tid_koder) %in% giltiga_ar]

    # RAMS ny tidsserie (2019-2021) och BAS (2020-) delar åren 2020-2021 - dessa tas alltid bort ur
    # RAMS-tabellen för att inte räknas dubbelt (samma hantering som i originalskriptet).
    if (tabell_id == "TAB5117") tid_vekt <- tid_vekt[!tid_vekt %in% c("2020", "2021")]
    if (length(tid_vekt) == 0) return(NULL)

    if (ar_bas_tabell) {
      # BAS-tabellen (TAB3200) har egna klartextvärden för födelseregion/kön ("inrikes född" i st.f.
      # "inrikes födda", "totalt" i st.f. "kvinnor och män"/"inrikes och utrikes födda").
      fodelseregion_vekt <- if (all(is.na(inrikesutrikes_klartext))) NA else dplyr::case_when(
        inrikesutrikes_klartext == "inrikes födda" ~ "inrikes född",
        inrikesutrikes_klartext == "utrikes födda" ~ "utrikes född",
        inrikesutrikes_klartext == "inrikes och utrikes födda" ~ "totalt",
        TRUE ~ inrikesutrikes_klartext
      )
      kon_vekt <- if (all(is.na(kon_klartext))) NA else ifelse(kon_klartext == "kvinnor och män", "totalt", kon_klartext)

      px <- pxweb2r::pxweb2_get_data(
        table = tabell_id,
        query = list(
          Region = region_vekt,
          Kon = kon_vekt,
          Alder = "20-64",
          Fodelseregion = fodelseregion_vekt,
          ContentsCode = "sysselsättningsgrad",
          Tid = tid_vekt
        ),
        on_all_values_invalid = "null")
      if (is.null(px)) return(NULL)

      px <- dplyr::rename(px, regionkod = region_kod, sysselsättningsgrad = value)
      if ("kön" %in% names(px)) px <- dplyr::mutate(px, kön = ifelse(kön == "totalt", "kvinnor och män", kön))
      if ("födelseregion" %in% names(px)) {
        px <- dplyr::mutate(px, födelseregion = dplyr::case_when(
          födelseregion == "inrikes född" ~ "inrikes födda",
          födelseregion == "utrikes född" ~ "utrikes födda",
          födelseregion == "totalt" ~ "inrikes och utrikes födda",
          TRUE ~ födelseregion
        ))
      }
      dplyr::mutate(px, ålder = "20-64 år") |>
        dplyr::select(dplyr::any_of(c("år", "regionkod", "region", "födelseregion", "kön", "ålder", "sysselsättningsgrad")))

    } else {
      # RAMS-tabellerna: klartexterna för InrikesUtrikes/Kon matchar redan skriptets egna
      # ("inrikes/utrikes födda", "kvinnor"/"män"/"kvinnor och män") - ingen översättning behövs.
      px <- pxweb2r::pxweb2_get_data(
        table = tabell_id,
        query = list(
          Region = region_vekt,
          InrikesUtrikes = if (all(is.na(inrikesutrikes_klartext))) NA else inrikesutrikes_klartext,
          Kon = if (all(is.na(kon_klartext))) NA else kon_klartext,
          ContentsCode = "*",
          Tid = tid_vekt
        ),
        on_all_values_invalid = "null")
      if (is.null(px)) return(NULL)

      px <- dplyr::rename(px, regionkod = region_kod, sysselsättningsgrad = value)
      if ("inrikes/utrikes född" %in% names(px)) px <- dplyr::rename(px, födelseregion = `inrikes/utrikes född`)
      dplyr::mutate(px, ålder = "20-64 år") |>
        dplyr::select(dplyr::any_of(c("år", "regionkod", "region", "födelseregion", "kön", "ålder", "sysselsättningsgrad")))
    }
  }

  rams_df <- purrr::map2(
    c("TAB4334", "TAB4356", "TAB5117", "TAB3200"),
    c(FALSE, FALSE, FALSE, TRUE),
    hamta_en_syss_tabell
  ) |>
    purrr::list_rbind()

  if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)) {
    openxlsx::write.xlsx(rams_df, paste0(excelfil_mapp, excel_filnamn))
  }

  chart_df <- rams_df |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))

  if ("kön" %in% names(chart_df)) chart_df <- dplyr::filter(chart_df, kön != "kvinnor och män")
  if ("födelseregion" %in% names(chart_df)) chart_df <- dplyr::filter(chart_df, födelseregion != "inrikes och utrikes födda")

  alder_txt <- unique(chart_df$ålder)
  # anpassa diagramtitel och diagramfilnamn utifrån om kön är med eller inte och om båda könen är med
  # OBS: originalet satte bara kon_titel/bakgr_titel i if-grenen men refererade dem sedan ovillkorligt
  # i glue()-strängen längre ner - kraschade ("object not found") så fort kon_klartext eller
  # inrikesutrikes_klartext var NA. Båda ges nu alltid ett värde (tom sträng i else-fallet).
  if (!all(is.na(kon_klartext))) {
    kon_filnamn <- "_kon_"
    kon_titel <- if (length(unique(chart_df$kön)) == 1) unique(chart_df$kön) else "invånare"
  } else {
    kon_filnamn <- ""
    kon_titel <- ""
  }

  if (!all(is.na(inrikesutrikes_klartext))) {
    bakgr_filnamn <- "_inr_utr_"
    bakgr_titel <- if (length(unique(chart_df$födelseregion)) == 1) paste0(unique(chart_df$födelseregion), " ") else ""
  } else {
    bakgr_filnamn <- ""
    bakgr_titel <- ""
  }

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(chart_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionkod_txt <- if (region_start == region_txt) paste0(unique(chart_df$regionkod), collapse = "_") else region_txt

  diagramtitel <- glue::glue("Sysselsättningsgrad {bakgr_titel}{kon_titel} {alder_txt} i {region_txt} år {min(chart_df$år)}-{max(chart_df$år)}")
  diagramfil <- stringr::str_replace_all(
    glue::glue("syss{bakgr_filnamn}{kon_filnamn}{regionkod_txt}_ar{min(chart_df$år)}_{max(chart_df$år)}.png"),
    "__", "_")

  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = chart_df,
  			 skickad_x_var = "år",
  			 skickad_y_var = "sysselsättningsgrad",
  			 skickad_x_grupp = dplyr::case_when(!all(is.na(kon_klartext)) ~ "kön",
  			                             !all(is.na(inrikesutrikes_klartext)) ~ "födelseregion",
  			                             TRUE ~ NA),
  			 x_axis_sort_value = FALSE,
  			 diagram_titel = diagramtitel,
  			 diagram_capt = diagram_capt,
  			 stodlinjer_avrunda_fem = TRUE,
  			 filnamn_diagram = diagramfil,
  			 dataetiketter = visa_dataetiketter,
  			 manual_y_axis_title = "procent",
  			 manual_x_axis_text_vjust = 1,
  			 manual_x_axis_text_hjust = 1,
  			 manual_color = if (!all(is.na(kon_klartext))) rddiagram::diagramfarger("kon") else rddiagram::diagramfarger("rus_sex"),
  			 output_mapp = output_mapp,
  			 facet_grp = if (!all(is.na(kon_klartext)) & !all(is.na(inrikesutrikes_klartext))) "födelseregion" else NULL,
  			 facet_scale = "fixed",
  			 skriv_till_diagramfil = skriv_diagramfil
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

  return(gg_list)

} # slut funktion

diag_gini_SCB <- function(region_vekt = rdverktyg::hamtaAllaLan(tamedriket = TRUE), # De regioner som skall jämföras i stapeldiagram.
                          diagram_fokus = "20", # Vilken region skall fokus ligga på i stapeldiagrammet. Måste vara en av de som finns ovan
                          region_vekt_linje = c("20","00"), # Vilka regioner skall jämföras i linjediagrammet. Måste vara två av de som finns ovan
                          output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                          spara_diagrambildfil = FALSE,
                          ggobjektfilnamn_utan_tid = TRUE,
                          diagram_capt = "Källa: SCB, bearbetning av Samhällsanalys, Region Dalarna\nDiagramförklaring: För att redovisa ojämnheten i inkomstfördelningen används gini-koefficienten.\nKoefficienten kan anta ett värde mellan 0 och 1. Ett högt värde på koefficienten visar på större ojämnhet än ett lågt värde",
                          diag_fargvekt_linje = NA,
                          diag_fargvekt_stapel = NA,
                          inkomsttyp_klartext = "disponibel inkomst per k.e. inkl. kapitalvinst",			 #  Finns: "faktorinkomst per k.e. inkl. kapitalvinst", "faktorinkomst per k.e. exkl. kapitalvinst", "disponibel inkomst per k.e. inkl. kapitalvinst", "disponibel inkomst per k.e. exkl. kapitalvinst",
                          diag_tidsserie = TRUE,
                          diag_jmfr_senastear = TRUE,
                          returnera_data = FALSE
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
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  # if (demo){
  #   demo_url <-
  #     c("https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_ar2007_2022.png",
  #       "https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_jmfr_riket_ar2007-2022.png")
  #   walk(demo_url, ~browseURL(.x))
  #   if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  #   stop_tyst()
  # }

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger()
  if (all(is.na(diag_fargvekt_linje))) {
    diag_fargvekt_linje <- rddiagram::diagramfarger("rus_sex")
  }

  if (all(is.na(diag_fargvekt_stapel))) {
    diag_fargvekt_stapel <- rddiagram::diagramfarger("rus_tva_fokus")
  }


  gg_list <- list()

  # hamta_data-repots hamta_inkomstfordelning_region_inkomsttyp_tid_
  # TabVX1DispInkN_HE0110_HE0110F_scb.R (v1: HE/HE0110/HE0110F/
  # TabVX1DispInkN) hämtas här direkt via v2-motsvarigheten TAB1121.
  gini_df <- pxweb2r::pxweb2_get_data(
    table = "TAB1121",
    query = list(
      Region = region_vekt,
      InkomstTyp = inkomsttyp_klartext,
      ContentsCode = "Gini-koefficient",
      Tid = "*"
    )) |>
    dplyr::rename(regionkod = region_kod, `Gini-koefficient` = value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))


  if (diag_tidsserie) {

    diagramtitel <- "Ginikoefficient"
    valda_regioner <- paste(region_vekt_linje,collapse="_")
    diagramfil <- glue::glue("gini_tidsserie_{valda_regioner}_ar_{min(gini_df$år)}_{max(gini_df$år)}.png")


    gini_tidsserie <- dplyr::filter(gini_df, regionkod %in% region_vekt_linje)

    if(returnera_data == TRUE & diag_tidsserie == TRUE & diag_jmfr_senastear == FALSE){
      assign("gini_df", gini_tidsserie, envir = .GlobalEnv)
    }

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = gini_tidsserie,
      skickad_x_var = "år",
      skickad_y_var = "Gini-koefficient",
      diagram_titel = diagramtitel,
      skickad_x_grupp = "region",
      stodlinjer_avrunda_fem = TRUE,
      diagram_capt = diagram_capt,
      manual_color = diag_fargvekt_linje,
      manual_y_axis_title = "Ginikoefficient",
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_diagrambildfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")

    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfil)
    }

  } # slut if-sats om diag_tidsserie

  if (diag_jmfr_senastear) {

    diagramtitel <- glue::glue("Ginikoefficient år {max(gini_df$år)}")
    diagramfil <- glue::glue("gini_jmf_ar_{max(gini_df$år)}.png")


    if(returnera_data == TRUE & diag_tidsserie == TRUE & diag_jmfr_senastear == TRUE){
      assign("gini_df", gini_df, envir = .GlobalEnv)
    }

    region_fokus <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(diagram_fokus)[[2]])

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(gini_df, år == max(år)) |>
                                   dplyr::mutate(fokus = ifelse(region == region_fokus,1,0)),
      skickad_x_var = "region",
      skickad_y_var = "Gini-koefficient",
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      manual_x_axis_text_hjust = 1,
      manual_x_axis_text_vjust = 1,
      x_var_fokus = "fokus",
      x_axis_sort_value = TRUE,
      manual_y_axis_title = "Ginikoefficient",
      manual_color = diag_fargvekt_stapel,
      output_mapp = output_mapp,
      skriv_till_diagramfil = spara_diagrambildfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")

    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfil)
    }
  } # slut if-sats om diag_tidsserie

  return(gg_list)

} # slut diag-funktion

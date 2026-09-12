diag_chefer<-function(region_vekt = "20", # Enbart på län, max 1 åt gången
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      diag_senaste_ar = TRUE,
                      diag_linje = TRUE,
                      returnera_data = FALSE,
                      spara_figur = TRUE){

  ## =================================================================================================================
  # Skript som skapar två diagram för andel chefer
  #
  # Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003X/IntGr1LanKonUtb/
  # Används primärt i kvinnor och män i Dalarna
  #
  # Uppdatering 2026-07-07. Uppdaterat med ny version av PXweb /Jon
  # =================================================================================================================
  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()

  # Data som inte uppdateras (fram till 2021 - RAMS)
  chefer_RAMS_df <- pxweb2r::pxweb2_get_data(
    table = "TAB389",
    query = list(
      Region = region_vekt,
      Kon = c("1","2"),
      UtbNiv = c("000","F","3","EU","US"),
      BakgrVar = c("tot20-64"),
      ContentsCode = c("0000001Y"),
      Tid = "*"
    )) |>
      dplyr::select(-region_kod,-tabellinnehåll) |>
      dplyr::rename(Andel = value)

  # Data som uppdateras (från 2022 - BAS)
  chefer_bas_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6384",
    query = list(
      Region = region_vekt,
      Kon = c("1","2"),
      UtbNiv = c("000","F","3","EU","US"),
      BakgrVar = "TOT",
      ContentsCode=c("000007KF"),
      Tid=c("*"))
    ) |>
      dplyr::select(-region_kod,-tabellinnehåll) |>
      dplyr::rename(Andel = value)

  # Binder ihop dataseten, tar bort NA och fixar utbildningsnivå
  chefer_df <- rbind(chefer_RAMS_df,chefer_bas_df) |>
    dplyr::filter(!is.na(Andel)) |>
      dplyr::mutate(utbildningsnivå = stringr::str_replace(utbildningsnivå, "^.*?:\\s*", ""))

  if(returnera_data == TRUE){
    assign("chefer_df", chefer_df, envir = .GlobalEnv)
  }

  if(diag_senaste_ar){

    diagram_capt <- c("Källa: SCB:s öppna statistikdatabas, BAS\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel chefer av sysselsatta med ett klassificerat yrke (20-65 år) uppdelat på utbildningsnivå")
    diagramtitel <- paste0("Andel chefer år ",max(chefer_df$år)," i ",rdverktyg::skapa_kortnamn_lan(unique(chefer_df$region)))
    diagramfilnamn <- paste0("andel_chefer_",rdverktyg::skapa_kortnamn_lan(unique(chefer_df$region)),".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chefer_df |>
        dplyr::filter(år == max(år),
                      utbildningsnivå != "uppgift saknas") |>
        dplyr::mutate(utbildningsnivå = factor(utbildningsnivå,
                                                levels = c("förgymnasial utbildning",
                                                           "gymnasial utbildning",
                                                           "eftergymnasial utbildning",
                                                           "samtliga utbildningsnivåer"))),
      skickad_x_var = "utbildningsnivå",
      skickad_y_var = "Andel",
      skickad_x_grupp = "kön",
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = rddiagram::diagramfarger("kon"),
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      x_axis_sort_value = FALSE,
      manual_y_axis_title="procent",
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  }

  if(diag_linje){
    diagram_capt <- c("Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förändring i andelen chefer av sysselsatta med ett klassificerat yrke för samtliga utbildningsnivåer.\nFram till och med 2021, 20-64 år och data från RAMS. Från 2022, 20-65 år och data från BAS.")
    diagramtitel <- paste0("Förändring i andel chefer i ",rdverktyg::skapa_kortnamn_lan(unique(chefer_df$region)))
    diagramfilnamn <- paste0("andel_chefer_linje_",rdverktyg::skapa_kortnamn_lan(unique(chefer_df$region)),".png")

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = chefer_df |>
        dplyr::filter(år >"2000",
                      utbildningsnivå == "samtliga utbildningsnivåer"),
      skickad_x_var = "år",
      skickad_y_var = "Andel",
      skickad_x_grupp = "kön",
      x_axis_lutning = 45,
      manual_color = rddiagram::diagramfarger("kon"),
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      berakna_index = TRUE,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  return(gg_list)

}

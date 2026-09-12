diag_foraldrapenning_vab <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                     diag_foraldrapenning_mottagare = TRUE,
                                     diag_foraldrapenning_andel_nettodagar = TRUE,
                                     diag_foraldrapenning_andel_senaste_ar_lanets_kommuner = TRUE,
                                     diag_foraldrapenning_antal_nettodagar = TRUE,
                                     diag_vab_antal_nettodagar = TRUE,
                                     diag_vab_forandring_nettodagar = TRUE,
                                     output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                                     spara_diagrambildfil = FALSE,
                                     spara_dataframe_till_global_environment = FALSE){

  ## =================================================================================================================
  # Skript som skapar tre diagram för föräldrapenning och två diagram för vård av barn (VAB) i valt län.
  # Används i första hand i rapporten "Kvinnor och män i Dalarna"
  # Skapad av Jon Frank 2025-07-04
  #
  # Reviderad av Peter Möller 2025-11-24.
  # Nu går det att hämta Riket, län och kommuner. Ersatt län/kommun (bara namn, inte kod) med
  # Regionkod och Region. Lagt till TRUE/FALSE för varje enskild diagram
  #
  # diagram_capt saknades i diagrammet foraldrapening_antal_nettodagar, så jag la till den. Jon 2025-11-26
  # Ändrat så att även data på kommunnivå returneras (foraldrapenning_lan_df) då detta behövs i markdown-rapporten Jon 2026-07-07
  # =============================================== Uttag ===============================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load().
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # Ingen SCB-hämtning här - datan kommer direkt från Försäkringskassans egna
  # öppna Excel-filer (inte PxWeb), så ingen pxweb2r/CKM-hantering behövs.
  # "here" togs bort - laddades men användes aldrig i skriptet.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/tidyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/fp-antal-mottagare-nettodagar-belopp/FPAntalDagarBeloppLanKommun.xlsx",
           "https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/tfp-vab-antal-mottagare-belopp/tfpVabAntalDagarBeloppLanKommun.xlsx")

  # Med Peters nya skript
  flik_lista = list()

  gg_list = list()

  # om något av föräldrapenningsdiagrammen är TRUE så hämtas data för föräldrapenning, annars inte
  if(any(
    diag_foraldrapenning_mottagare, diag_foraldrapenning_andel_nettodagar,
         diag_foraldrapenning_andel_senaste_ar_lanets_kommuner,
         diag_foraldrapenning_antal_nettodagar)){

    foraldrapenning_df = rdverktyg::hamta_excel_dataset_med_url(path[1], skippa_rader = 2) |>
      dplyr::rename(Region = Kommun) |>
      dplyr::mutate(Region = dplyr::if_else(Region == "Riket", "00 Riket", Region)) |>
      tidyr::separate(Region, into = c("Regionkod", "Region"), sep = " ", extra = "merge") |>
      dplyr::rename(Antal_mottagare = `Antal mottagare`,
             Andel = `Andel nettodagar per kön`) |>
      dplyr::select(-c(Län, kolumnnamn))

    lanskod <- stringr::str_sub(region_vekt, 1, 2)
    lan_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(lanskod)$region, TRUE)

    foraldrapenning_lan_df <- dplyr::filter(foraldrapenning_df, stringr::str_sub(Regionkod, 1, 2) == lanskod)

    foraldrapenning_df <- dplyr::filter(foraldrapenning_df, Regionkod %in% region_vekt)

    if(spara_dataframe_till_global_environment) {
      assign("foraldrapenning_df", foraldrapenning_df, envir = .GlobalEnv)
    }

    # Antal mottagare
    if (diag_foraldrapenning_mottagare) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Antal mottagare av föräldrapenning i ", rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(foraldrapenning_df$Region))))
      diagramfilnamn <- paste0("Foraldrapenning_antal_", paste0(rdverktyg::skapa_kortnamn_lan(unique(foraldrapenning_df$Region)), collapse = "_"),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::mutate(
          dplyr::filter(foraldrapenning_df, Kön != "Kvinnor och män"),
          Kön = tolower(Kön),
          Antal_mottagare = as.numeric(Antal_mottagare)
        ),
        skickad_x_var = "År",
        skickad_y_var = "Antal_mottagare",
        skickad_x_grupp = "Kön",
        x_axis_lutning = 45,
        manual_x_axis_text_vjust=1,
        manual_x_axis_text_hjust=1,
        manual_y_axis_title = "Antal mottagare",
        manual_color = rddiagram::diagramfarger("kon"),
        stodlinjer_avrunda_fem = TRUE,
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_diagrambildfil
      )

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
    } # slut if-sats för diagram

    # Andel nettodagar
    if (diag_foraldrapenning_andel_nettodagar) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Föräldrapenning, andel nettodagar per kön i ", rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(foraldrapenning_df$Region))))
      diagramfilnamn <- paste0("Foraldrapenning_andel_", rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(foraldrapenning_df$Region))),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::mutate(
          dplyr::filter(foraldrapenning_df, Kön != "Kvinnor och män"),
          Kön = tolower(Kön),
          Andel = as.numeric(Andel)
        ),
        skickad_x_var = "År",
        skickad_y_var = "Andel",
        skickad_x_grupp = "Kön",
        x_axis_lutning = 45,
        manual_x_axis_text_vjust=1,
        manual_x_axis_text_hjust=1,
        manual_color = rddiagram::diagramfarger("kon"),
        stodlinjer_avrunda_fem = TRUE,
        manual_y_axis_title = "procent",
        procent_0_100_10intervaller = TRUE,
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_diagrambildfil
      )

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
    } # slut if-sats för diagram

    # Andel per kommun i ett län
    if (diag_foraldrapenning_andel_senaste_ar_lanets_kommuner) {

      if(spara_dataframe_till_global_environment) {
        assign("foraldrapenning_lan_df", dplyr::filter(foraldrapenning_lan_df, År == max(År)), envir = .GlobalEnv)
      }

      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Föräldrapenning, andel nettodagar per kön i ", rdverktyg::list_komma_och(lan_txt), " år ", max(foraldrapenning_df$År))
      diagramfilnamn <- paste0("Foraldrapenning_andel_kommun_", rdverktyg::list_komma_och(lan_txt),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::mutate(
          dplyr::filter(foraldrapenning_lan_df, Kön != "Kvinnor och män", År == max(År)),
          Kön = tolower(Kön),
          Andel = as.numeric(Andel)
        ),
        skickad_x_var = "Region",
        skickad_y_var = "Andel",
        skickad_x_grupp = "Kön",
        x_axis_lutning = 45,
        manual_color = rddiagram::diagramfarger("kon"),
        manual_y_axis_title = "procent",
        manual_x_axis_text_vjust=1,
        manual_x_axis_text_hjust=1,
        x_axis_sort_value = TRUE,
        procent_0_100_10intervaller = TRUE,
        stodlinjer_avrunda_fem = TRUE,
        x_axis_sort_grp = 1,
        vand_sortering = FALSE,
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_diagrambildfil
      )

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
    } # slut if-sats för diagram

    # föräldrapenning antal nettodagar
    if (diag_foraldrapenning_antal_nettodagar) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Föräldrapenning, antal nettodagar per kön i ", rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(foraldrapenning_df$Region))))
      # OBS: hette tidigare "Foraldrapenning_antal_<region>.png" - samma
      # filnamn som "Antal mottagare"-diagrammet ovan (skrev över det på
      # disk för ett enda län, eftersom paste0(collapse="_") och
      # list_komma_och() råkar ge samma resultat då). Lade till "nettodagar"
      # för att skilja dem åt.
      diagramfilnamn <- paste0("Foraldrapenning_antal_nettodagar_", rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(foraldrapenning_df$Region))),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::mutate(
          dplyr::filter(foraldrapenning_df, Kön != "Kvinnor och män"),
          Kön = tolower(Kön),
          Nettodagar = as.numeric(Nettodagar)
        ),
        skickad_x_var = "År",
        skickad_y_var = "Nettodagar",
        skickad_x_grupp = "Kön",
        x_axis_lutning = 45,
        manual_x_axis_text_vjust=1,
        manual_x_axis_text_hjust=1,
        manual_color = rddiagram::diagramfarger("kon"),
        stodlinjer_avrunda_fem = TRUE,
        manual_y_axis_title = "Antal nettodagar",
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_diagrambildfil
      )

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
    } # slut if-sats för diagram
  } # slut if-sats om det finns några föräldrapenningsdiagram

  if(any(diag_vab_antal_nettodagar, diag_vab_forandring_nettodagar)) {

    vab_df = rdverktyg::hamta_excel_dataset_med_url(path[2], skippa_rader = 2) |>
      dplyr::rename(Region = Kommun) |>
      dplyr::mutate(Region = dplyr::if_else(Region == "Riket", "00 Riket", Region)) |>
      tidyr::separate(Region, into = c("Regionkod", "Region"), sep = " ", extra = "merge") |>
      dplyr::rename(Antal_mottagare = `Antal mottagare`,
             Antal_nettodagar = `Antal nettodagar`,
             Andel = `Andel nettodagar per kön`) |>
      dplyr::select(-c(Län, kolumnnamn)) |>
      dplyr::filter(Regionkod %in% region_vekt)

    if(spara_dataframe_till_global_environment) {
      assign("vab_df", vab_df, envir = .GlobalEnv)
    }

    # Antal uttagna nettodagar
    if (diag_vab_antal_nettodagar) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Vård av barn, antal uttagna nettodagar i ", rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(vab_df$Region))))
      diagramfilnamn <- paste0("vab_antal_", paste0(rdverktyg::skapa_kortnamn_lan(unique(vab_df$Region)), collapse = "_"),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::mutate(
          dplyr::filter(vab_df, Kön != "Kvinnor och män"),
          Kön = tolower(Kön),
          Antal_nettodagar = as.numeric(Antal_nettodagar)
        ),
        skickad_x_var = "År",
        skickad_y_var = "Antal_nettodagar",
        skickad_x_grupp = "Kön",
        x_axis_lutning = 45,
        manual_x_axis_text_vjust=1,
        manual_x_axis_text_hjust=1,
        manual_y_axis_title = "",
        manual_color = rddiagram::diagramfarger("kon"),
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        stodlinjer_avrunda_fem = TRUE,
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_diagrambildfil
      )

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
    } # slut if-sats för diagram

    # diagram över förändring av nettodagar vab
    if (diag_vab_forandring_nettodagar) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Vård av barn, förändring i antal uttagna nettodagar i ", rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(vab_df$Region))))
      diagramfilnamn <- paste0("vab_antal_linje_", paste0(rdverktyg::skapa_kortnamn_lan(unique(vab_df$Region)), collapse = "_"),".png")

      gg_obj <- rddiagram::SkapaLinjeDiagram(
        skickad_df = dplyr::mutate(
          dplyr::filter(vab_df, Kön != "Kvinnor och män"),
          Kön = tolower(Kön),
          `Antal nettodagar` = as.numeric(Antal_nettodagar),
          "år" = År
        ),
        skickad_x_var = "år",
        skickad_y_var = "Antal nettodagar",
        skickad_x_grupp = "Kön",
        manual_color = rddiagram::diagramfarger("kon"),
        berakna_index = TRUE,
        x_axis_lutning = 45,
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        stodlinjer_avrunda_fem = TRUE,
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_diagrambildfil
      )

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
    } # slut if-sats för diagram
  } # slut if-sats om det finns några vab-diagram

  return(gg_list)

}

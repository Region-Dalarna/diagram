diag_pendling_over_lans_fa_grans <- function(region_vekt = rdverktyg::hamtaAllaLan(FALSE),  # Länskoder, FA-koder ("FA00"-"FA60") eller "00" (riket)
                                           valt_kon = "totalt",                # Finns: "totalt", "män", "kvinnor"
                                           valt_ar = "9999",                   # "9999" = senaste år
                                           visa_dataetiketter = FALSE,         # dataetiketter i diagrammet
                                           diag_absoluta_tal = TRUE,           # skriv ut diagram med absoluta tal
                                           diag_procent = TRUE,                # skriv ut diagram med procent
                                           skapa_fil = TRUE, # skapa en fil dig figuren sparas
                                           returnera_figur = TRUE, # Om TRUE returneras figur som ggplot-objekt
                                           enbart_in_ut = TRUE, # TRUE om man bara vill visa in och utpendling (ej bor och arbetar i samma kommun)
                                           diagramfarg_vektor = NA, # Valda färger
                                           diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                                           output_mapp_figur = "G:/Samhällsanalys/API/Fran_R/Utskrift/", # Hit sparas figuren
                                           output_mapp_data = NA, # Hit sparas data
                                           spara_data = FALSE, # Skall data sparas
                                           filnamn_data = "pendling.xlsx",
                                           returnera_data = TRUE) {# Filnamn för sparad data

  # ===========================================================================================================
  #
  # Skript för att skriva ut diagram (från SCB:s regionala matchningsindikatorer) med andel och antal in- och
  # utpendlare över läns- eller FA-gräns, samt även de som bor och arbetar i samma län eller FA.
  # Skapad av: Peter
  #
  # Migrerad till pxweb2r/rddiagram/rdverktyg. hamta_data-repots
  # hamta_pendling_lan_fa_region_utbildngrupp_kon_tid_scb() hämtade och slog ihop två v1-tabeller
  # (AM9906B/RegionInd19U2N1 för "Bor och arbetar i samma region"/"In-/utpendlare över regiongräns" och
  # AM9906O/RegionInd19U2 för "Dag-/Nattbefolkning"). AM9906O/RegionInd19U2 är numera helt borttagen ur
  # SCB:s v1-API (ger 400 Bad Request) - den är ersatt av en enda ny v2-tabell, TAB6369, som täcker samtliga
  # fem tabellinnehåll i ett och samma uttag. Ingen ihopslagning av flera tabeller behövs längre.
  # ===========================================================================================================

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget p_load(tidyverse). Anropas med
  # fullt namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(diagramfarg_vektor))) {
    diagramfarg_vektor <- rddiagram::diagramfarger("rus_sex")
  }

  vald_kommun_txt <- rdverktyg::hamtaregion_kod_namn(region_vekt)$region |> rdverktyg::list_komma_och()
  vald_kommun_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(region_vekt, returnera_text = TRUE, returtext = vald_kommun_txt)
  vald_kommun_txt <- rdverktyg::ar_alla_lan_i_sverige(region_vekt, returnera_text = TRUE, returtext = vald_kommun_txt)

  vald_region_filnamn <- if (rdverktyg::ar_alla_kommuner_i_ett_lan(region_vekt) || rdverktyg::ar_alla_lan_i_sverige(region_vekt)) {
    vald_kommun_txt |> tolower() |> rdverktyg::byt_ut_svenska_tecken() |> stringr::str_replace(" ", "_")
  } else {
    paste0(region_vekt, collapse = "_")
  }

  visa_dataetik_txt <- ifelse(visa_dataetiketter, "_lbl_","")

  # =============================================== API-uttag ===============================================

  # "totalt" finns som en riktig kod i Kon-variabeln (utöver "1"=män, "2"=kvinnor) - ingen
  # wildcard/summering behövs för default-fallet.
  kon_hamta <- dplyr::case_when(
    valt_kon %in% c("män", "man") ~ "1",
    valt_kon == "kvinnor" ~ "2",
    TRUE ~ valt_kon  # "totalt" eller "*" skickas vidare oförändrat
  )

  px_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6369",
    query = list(
      Region = region_vekt,
      Utbildngrupp = "00S",  # samtliga utbildningsgrupper
      Kon = kon_hamta,
      ContentsCode = "*",
      Tid = valt_ar
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value) |>
    dplyr::select(-utbildning_kod, -utbildning)

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  list_data <- list() # Skapar en tom lista som används för att spara data

  # ============================== diagram med absoluta tal ==================================
  if (diag_absoluta_tal) {

    px_df_ut <- dplyr::filter(px_df, !variabel %in% c("Dagbefolkning (förvärvsarbetande)", "Nattbefolkning (förvärvsarbetande)"))

    if (enbart_in_ut == TRUE) px_df_ut <- dplyr::filter(px_df_ut, variabel != "Bor och arbetar i samma region")

    if (!is.na(output_mapp_figur) & !is.na(filnamn_data)) {
      list_data <- c(list_data, list("antal_pendlare" = px_df_ut))
    }

    if (returnera_data == TRUE) {
      assign("antal_pendlare_lan_df", px_df_ut, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Antal pendlare 20-64 år i ", vald_kommun_txt, " år ", unique(px_df$år))
    diagramfil <- paste0("in_utpendling_", vald_region_filnamn, "_", unique(px_df$år), visa_dataetik_txt, ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = px_df_ut,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diagramfarg_vektor,
      manual_y_axis_title = "antal förvärvsarbetande",
      stodlinjer_avrunda_fem = TRUE,
      geom_position_stack = TRUE,
      dataetiketter = visa_dataetiketter,
      skriv_till_diagramfil = skapa_fil,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- "In_och_utpendling_absoluta_tal"

  } # slut if-sats för diag_abosluta tal

  # ================================= diagram med procent ==================================
  if (diag_procent) {

    # OBS: originalet räknade ut vilka kolumner som skulle pivoteras tillbaka till long-format via
    # "(length(names(.))-2):length(names(.))" - ett magrittr-".":s-specifikt knep som inte fungerar med
    # |>. De tre nya andelskolumnerna namnges i stället explicit i pivot_longer().
    px_df_andel <- px_df |>
      tidyr::pivot_wider(names_from = variabel, values_from = varde) |>
      dplyr::mutate(
        "Andel utpendling" = (`Utpendlare över regiongräns` / `Nattbefolkning (förvärvsarbetande)`) * 100,
        "Andel inpendling" = (`Inpendlare över regiongräns` / `Dagbefolkning (förvärvsarbetande)`) * 100,
        "Andel som bor och arbetar i samma region" = (`Bor och arbetar i samma region` / (`Bor och arbetar i samma region` + abs(`Utpendlare över regiongräns`))) * 100
      ) |>
      dplyr::select(-c(`Inpendlare över regiongräns`, `Utpendlare över regiongräns`, `Bor och arbetar i samma region`,
                        `Dagbefolkning (förvärvsarbetande)`, `Nattbefolkning (förvärvsarbetande)`)) |>
      tidyr::pivot_longer(cols = c("Andel utpendling", "Andel inpendling", "Andel som bor och arbetar i samma region"), names_to = "variabel", values_to = "andel")

    if (enbart_in_ut == TRUE) px_df_andel <- dplyr::filter(px_df_andel, variabel != "Andel som bor och arbetar i samma region")

    if (!is.na(output_mapp_figur) & !is.na(filnamn_data)) {
      list_data <- c(list_data, list("andel_pendlare" = px_df_andel))
    }

    if (returnera_data == TRUE) {
      assign("andel_pendlare_lan_df", px_df_andel, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Andel pendlare av förvärvsarbetande 20-64 år i ", vald_kommun_txt, " år ", unique(px_df$år)) |>
      rdverktyg::dela_upp_strang_radbryt(70)

    diagramfil <- paste0("in_utpendling_procent_", vald_region_filnamn, "_", unique(px_df$år), visa_dataetik_txt, ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = px_df_andel,
      skickad_x_var = "region",
      skickad_y_var = "andel",
      skickad_x_grupp = "variabel",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_y_axis_title = "procent",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diagramfarg_vektor,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 2,
      vand_sortering = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter = visa_dataetiketter,
      skriv_till_diagramfil = skapa_fil,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- "In_och_utpendling_procent"

  } # slut if-sats diag_procent

  # Sparar data
  if (!is.na(output_mapp_figur) & !is.na(filnamn_data)) {
    openxlsx::write.xlsx(list_data, paste0(output_mapp_data, filnamn_data))
  }
  if (returnera_figur == TRUE) return(gg_list)
} # slut funktion

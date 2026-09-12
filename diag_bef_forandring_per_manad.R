diag_fodda_manad_scb <- function(
  region_vekt = "20",                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region
  bef_variabel = "födda",                                  # går att välja dessa: "folkmängd", "folkökning", "födda", "döda", "födelseöverskott", "samtliga inflyttningar", "samtliga utflyttningar", "samtliga inrikes inflyttningar", "inflyttningar från kommuner inom länet", "inflyttningar från övriga län", "invandringar", "samtliga inrikes utflyttningar", "utflyttningar till kommuner inom länet", "utflyttningar till övriga län", "utvandringar", "flyttningsöverskott totalt", "flyttningsöverskott inrikes totalt", "flyttningsöverskott eget län", "flyttningsöverskott övriga Sverige", "invandringsöverskott", "justeringspost"
  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
  output_mapp = NA,                                        # här sparas diagramet
  diagram_fargvekt = NA,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  returnera_dataframe_global_environment = FALSE,
  ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
  visa_dataetiketter = FALSE,
  kortnamn_lan = TRUE,                                     # vid TRUE så tas inte "län" i länsnamnet med
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
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger()
  if (all(is.na(diagram_fargvekt))) {
    diagram_fargvekt <- c(rep(rddiagram::diagramfarger("rd_gra")[c(1,1,1,1,4)]), rddiagram::diagramfarger("rus_sex")[c(3)])
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

  # hamta_data-repots hamta_bef_forandringar_region_kon_manad_scb.R (v1:
  # BE0101G/ManadBefStatRegion + MBefStatRegionCKM) hämtas här direkt via
  # v2-motsvarigheterna TAB1625 (historik 2000M01-2024M12) och TAB6473
  # (CKM 2025M01-). OBS: originalskriptet anropade en funktion,
  # hamta_befolkningsforandringar_manad(), som inte längre finns någonstans
  # i funktioner-/hamta_data-reporna (den faktiska funktionen i den
  # sourcade filen heter hamta_bef_forandringar_region_forandringar_kon_
  # tid_scb) - skriptet skulle alltså krascha direkt om det kördes.
  diagram_df_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB1625",
    query = list(
      Region = region_vekt,
      Forandringar = bef_variabel,
      Kon = "totalt",
      ContentsCode = "Befolkning",
      Tid = "*"
    ),
    on_all_values_invalid = "null")

  # CKM-tabellens klartext för "totalt" är "totalt, samtliga män och
  # kvinnor" (kod "TotSa") - inte bara "totalt" som i historiktabellen.
  diagram_df_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6473",
    query = list(
      Region = region_vekt,
      Forandringar = bef_variabel,
      Kon = "TotSa",
      ContentsCode = "Befolkning",
      Tid = "*"
    ),
    on_all_values_invalid = "null")

  # Skriptet refererade en "år"-kolumn som aldrig fanns i pxweb-uttaget
  # (bara den råa månadskoden, t.ex. "2020M01") - manader_bearbeta_
  # scbtabeller() (samma hjälpfunktion som används i flera andra migrerade
  # skript för den här sortens tabeller) härleder år/månad/etc. från den
  # kolumnen, vilket krävs för att resten av skriptet ska fungera alls.
  diagram_df <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
    dplyr::bind_rows(diagram_df_historik, diagram_df_ckm) |>
      dplyr::rename(regionkod = region_kod, Befolkning = value) |>
      dplyr::select(-tabellinnehåll) |>
      # CKM-tabellens etikett för totalt kön skiljer sig från historiktabellens
      # ("totalt, samtliga män och kvinnor" resp. "totalt") - samma
      # normalisering som redan görs i hamta_bef_forandringar_region_kon_
      # manad_scb.R.
      dplyr::mutate(kön = dplyr::if_else(stringr::str_detect(kön, "totalt"), "totalt", kön)) |>
      rdverktyg::manader_bearbeta_scbtabeller()) |>
    dplyr::mutate(
      fokus = as.integer(5 - (max(as.integer(as.character(år))) - as.integer(as.character(år)))),
      fokus = ifelse(fokus < 0, 0, fokus),  # Sätt alla äldre än 5 år från senaste året till 0
      ar_num = as.integer(år)
    ) |>
    dplyr::filter(ar_num > max(ar_num)-6)

  if (kortnamn_lan) diagram_df <- dplyr::mutate(diagram_df, region = rdverktyg::skapa_kortnamn_lan(region))

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("bef_forandringar_per_manad_df", diagram_df, envir = .GlobalEnv)
  }


  skapa_diagram <- function(skickad_regionkod) {

    diagram_region_df <- dplyr::filter(diagram_df, regionkod %in% skickad_regionkod)

    region_txt <- unique(diagram_region_df$region)
    variabel_txt <- unique(diagram_region_df$förändringar)

    diagramtitel <- glue::glue("{stringr::str_to_sentence(variabel_txt)} i {region_txt}")
    diagramfil <- glue::glue("{variabel_txt}_{region_txt}_ar{dplyr::first(diagram_region_df$år)}-{dplyr::last(diagram_region_df$år)}.png")

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = diagram_region_df,
      skickad_x_var = "månad",
      skickad_y_var = "Befolkning",
      skickad_x_grupp = "år",
      diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      manual_y_axis_title = "",
      manual_color = diagram_fargvekt,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      legend_byrow = TRUE,
      output_mapp = output_mapp
    ) # slut skriv ggplot_objekt

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

    return(gg_list)

  } # slut funktion som skapar diagrammet

  retur_list <- purrr::flatten(purrr::map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)))

  if (skriv_till_excelfil) {
      excefilnamn <- glue::glue("befolkning_utfall_progn_{region_xlsx}_ar{startar_utfall}-{slutar_prognos}.xlsx")
      openxlsx::write.xlsx(bef_folk_progn, paste0(output_mapp, excefilnamn))
  }

  return(retur_list)

} # slut funktion

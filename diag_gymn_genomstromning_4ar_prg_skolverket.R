diag_gymn_genomstromning_4ar_prg_skolverket <- function(
  region_vekt = "20",      # Val av region.
  gymnasieprogram = "Gymnasieskolan totalt",       # dessa finns: "Gymnasieskolan totalt", "Nationella program", "Högskoleförberedande program", "Yrkesprogram", "Introduktionsprogram", "Barn- och fritidsprogrammet", "Bygg- och anläggningsprogramme", "Ekonomiprogrammet", "El- och energiprogrammet", "Estetiska programmet", "Fordons- och transportprogramm", "Handels- och administrationspr", "Hantverksprogrammet", "Hotell- och turismprogrammet", "Humanistiska programmet", "Industritekniska programmet", "Naturbruksprogrammet", "Naturvetenskapsprogrammet", "Restaurang- och livsmedelsprog", "Samhällsvetenskapsprogrammet", "Teknikprogrammet", "VVS- och fastighetsprogrammet", "Vård- och omsorgsprogrammet"
  diagram_capt = "Källa: Skolverket\nBearbetning: Samhällsanalys, Region Dalarna",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  output_mapp = NA,
  diagramrubrik_tabort = FALSE,
  skriv_diagramfil = TRUE,
  returnera_data_rmarkdown = FALSE,           # TRUE = lägger dataframe i global environment, för användning i tex r-markdownrapporter
  ggobjektfilnamn_utan_tid = FALSE,           # TRUE = tar bort året ur filnamnet (som blir gg_plotobjektnamnet) så att det blir smidigare i markdownrapporter att använda samma objektsnamn varje år
  excelfil_mapp = NA,                         # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  excel_filnamn = "gymnaseiet_elever.xlsx"      # filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  ) {

  # ==============================================================================================================================
  #
  # Skriver ut diagram med med elever per gymnasieprogram
  #
  #
  # ==============================================================================================================================

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
  # stället för library(). Ingen SCB-hämtning här - datan hämtas från
  # Skolverkets publika SIRIS-exportapi, så hamta_data-repots
  # hamta_gymn_avg_genomstromning_4ar_prg_skolverket.R (som bara
  # användes av det här skriptet) är inlinead nedan i stället för att
  # source()as, i övrigt oförändrad. svenska_tecken_byt_ut() (från
  # funktioner-repots func_API.R) är också inlinead - används bara för
  # att skapa ett filnamnsvänligt gymnasieprogramnamn.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  if (!requireNamespace("stringi", quietly = TRUE)) install.packages("stringi")
  # dplyr/purrr/stringr/tidyr/httr följer med som beroenden till rddiagram/rdverktyg.

  svenska_tecken_byt_ut <- function(textstrang) stringi::stri_trans_general(textstrang, "Latin-ASCII")

  hamta_gymn_avg_genomstromning_4ar_prg_skolverket <- function(region_vekt = "20",                     # NA = riket, alla län och alla kommuner
                                                               gymnasieprogram = "*",                     # "*" = alla gymnasieprogram
                                                               huvudman = "Samtliga",                     # finns: "Samtliga", "Kommunal" och "Enskild", det går att välja flera
                                                               ta_bort_na = TRUE,                                 # TRUE = tar bort rader med NA i andel, annars behålls dessa
                                                               konvertera_andel_till_numerisk = TRUE      # TRUE = numerisk kolumn av andel, då försvinner prickar och liknande och blir NA. Vill man se vad som är prickar och hur många det är kan man sätta denna till FALSE
  ) {

    # ==================================================================================================================
    #
    # Skript för att hämta genomströmning för gymnasieavgångna från Skolverket per län, kommun eller för riket.
    # Detta skript hämtar alla kommuner i en excelfil för alla år. Det går således inte att snabba upp skriptet genom att
    # välja någon eller några få kommuner och man hämtar alltid alla år. Men man kan ändå filtrera ut de kommuner, län
    # eller riket som man vill ha för att få ett mindre dataset. Skriptet kollar att det har senaste år och använder detta.
    # Det variabler som ingår är läsår, regionkod, region, Gymnasieprogram, Typ av huvudman, Genomströmning samt andel.
    #
    # Absolut tal finns inte i datasetet.
    #
    # Skapat av: Peter Möller, Region Dalarna
    #
    # ==================================================================================================================

    # ta fram senaste årtalet för att hämta aktuell data
    artal_txt <- as.numeric(format(Sys.Date(), "%Y"))
    artal_txt <- as.character(artal_txt - 5)

    # url:er till samtliga geografiska nivåer (riket, län och kommuner)
    url_lista <- c(url_riket = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=402&pAr=2019&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=25CB9D24F0FA4D5DE06311BA650A8D29&pFlikar=1&pVerkform=21",
                   url_lan = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=403&pAr=2019&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=25CB9D24F0FA4D5DE06311BA650A8D29&pFlikar=1&pVerkform=21",
                   url_kommun = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=404&pAr=2019&pLan=&pKommun=&pHmantyp=&pUttag=null&pToken=05D2B2A8022E3496E06320BA650A6F89&pFlikar=1&pVerkform=21")

    # om vi fått ett annat år när vi extraherat senaste år ovan än vad som finns i url:erna ovan (år 2019) så används detta istället
    if (artal_txt != "2019") {
      url_lista <- purrr::map_chr(url_lista, ~ stringr::str_replace(.x, "&pAr=\\d{4}", paste0("&pAr=", artal_txt)))
    }

    df_list <- list()                   # vi sparar hämtad statistik till denna lista

    # om region_Vekt är NA så hämtas alla län, kommuner och riket
    if (all(is.na(region_vekt))) region_vekt <- rdverktyg::hamtaregtab()$regionkod

    las_in_excelfil <- function(fil_url) {

      httr::GET(fil_url, httr::write_disk(tf_excelfil <- tempfile(fileext = ".xlsx")))
      flikar <- readxl::excel_sheets(tf_excelfil)
      flikar <- flikar[!stringr::str_detect(flikar, "beskrivning")]
      if (!all(gymnasieprogram == "*")) flikar <- flikar[flikar %in% gymnasieprogram]

      genomstr_df <- purrr::map(flikar, ~ readxl::read_excel(tf_excelfil, sheet = .x, skip = 6, col_types = "text") |>
                           tidyr::pivot_longer(dplyr::starts_with("20"), values_to = "andel", names_to = "läsår") |>
                           dplyr::mutate(Gymnasieprogram = .x) |>
                           dplyr::filter(`Typ av huvudman` %in% huvudman)) |>
        purrr::list_rbind() |>
        dplyr::relocate(läsår, .before = 1) |>
        dplyr::relocate(Gymnasieprogram, .after = läsår)

      # om det är riket som hämtas
      if ("Riket" %in% names(genomstr_df)) {
        genomstr_df <- genomstr_df |>
          dplyr::mutate(regionkod = "00") |>
          dplyr::rename(region = Riket) |>
          dplyr::relocate(region, .before = 1) |>
          dplyr::relocate(regionkod, .before = 1) |>
          dplyr::relocate(läsår, .before = 1)
      }

      # om det är län som hämtas
      if ("Län" %in% names(genomstr_df) & !"Kommunkod" %in% names(genomstr_df)) {
        genomstr_df <- genomstr_df |>
          dplyr::rename(regionkod = Länskod,
                 region = Län) |>
          dplyr::relocate(region, .before = 1) |>
          dplyr::relocate(regionkod, .before = 1) |>
          dplyr::relocate(läsår, .before = 1)
      }

      # om det är kommuner som hämtas
      if ("Län" %in% names(genomstr_df) & "Kommunkod" %in% names(genomstr_df)) {
        genomstr_df <- genomstr_df |>
          dplyr::rename(regionkod = Kommunkod,
                 region = Kommun) |>
          dplyr::select(-c(Länskod, Län)) |>
          dplyr::relocate(region, .before = 1) |>
          dplyr::relocate(regionkod, .before = 1) |>
          dplyr::relocate(läsår, .before = 1)
      }

      # ta bara med de regioner som användaren valt
      if (!all(region_vekt == "*")) {
        genomstr_df <- dplyr::filter(genomstr_df, regionkod %in% region_vekt)
      }

      # om senaste läsår bara har "." som värden, backa ett år och hämta om
      senaste_lasar <- max(genomstr_df$läsår)
      if (all(genomstr_df$andel[genomstr_df$läsår == senaste_lasar] == ".", na.rm = TRUE)) {
        nytt_artal <- as.character(as.integer(artal_txt) - 1)
        ny_url <- stringr::str_replace(fil_url, "&pAr=\\d{4}", paste0("&pAr=", nytt_artal))
        return(las_in_excelfil(ny_url))
      }

      return(genomstr_df)
    } # slut läs in excelfil-funktion

    if (length(region_vekt[region_vekt == "00"]) > 0 | any(region_vekt == "*")) df_list[["riket"]] <- las_in_excelfil(url_lista[["url_riket"]])
    if ((length(region_vekt[nchar(region_vekt) == 2 & region_vekt != "00"]) > 0) | any(region_vekt == "*")) df_list[["lan"]] <- las_in_excelfil(url_lista[["url_lan"]])
    if (length(region_vekt[nchar(region_vekt) == 4]) > 0 | any(region_vekt == "*")) df_list[["kommun"]] <- las_in_excelfil(url_lista[["url_kommun"]])


    retur_df <- dplyr::bind_rows(df_list)
    # konvertera till numerisk om det är valt
    if (konvertera_andel_till_numerisk) retur_df <- suppressWarnings(dplyr::mutate(retur_df,
      andel = dplyr::na_if(andel, ".."),
      andel = readr::parse_number(andel))
    )

    # ta bort NA-värden om det är valt
    if (ta_bort_na) retur_df <- dplyr::filter(retur_df, !is.na(andel))

    return(retur_df)
  } # slut hamta_gymn_avg_genomstromning_4ar_prg_skolverket

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

  gymn_df <- rdverktyg::suppress_specific_warning(
    hamta_gymn_avg_genomstromning_4ar_prg_skolverket(
      region_vekt = region_vekt,      # Val av region.
      gymnasieprogram = gymnasieprogram,
      huvudman = "Samtliga",
      konvertera_andel_till_numerisk = TRUE
    )) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))

  if(returnera_data_rmarkdown == TRUE){
    # OBS: originalet skrev "chart_df" här, en variabel som inte finns
    # förrän inuti pmap()-loopen nedan (och då bara det senast beräknade
    # enskilda diagrammets data) - avsikten är uppenbarligen hela det
    # hämtade datasetet, gymn_df.
    assign("genomstromning_gymnasiet_df", gymn_df, envir = .GlobalEnv)
  }

  kombinationer <- tidyr::expand_grid(region = unique(gymn_df$region),
                               gymnasieprogram = unique(gymn_df$Gymnasieprogram))

  gg_list <- purrr::flatten(purrr::pmap(kombinationer, function(region, gymnasieprogram) {

    chart_df <- dplyr::filter(gymn_df, region == .env$region, Gymnasieprogram == gymnasieprogram)

  prg_txt <- dplyr::if_else(gymnasieprogram == "Gymnasieskolan totalt", "", paste0(" på ", tolower(gymnasieprogram)))

  diagram_titel <- paste0("Andel i ", region," med fullföljd gymnasieutbildning inom fyra år", prg_txt)
  diagramfilnamn <- glue::glue("genomstromning_gymnasiet_{region}_{svenska_tecken_byt_ut(gymnasieprogram)}_ar_{substr(dplyr::last(chart_df$läsår),1,4)}_{substr(dplyr::first(chart_df$läsår),1,4)}.png")

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "läsår",
    skickad_y_var = "andel",
    diagram_titel = if(diagramrubrik_tabort) NULL else diagram_titel,
    lagg_pa_logga = ta_med_logga,
    logga_path = logga_sokvag,
    output_mapp = output_mapp,
    filnamn_diagram = diagramfilnamn,
    manual_x_axis_text_hjust = 1,
    manual_x_axis_text_vjust = 1,
    diagram_capt = diagram_capt,
    stodlinjer_avrunda_fem = TRUE,
    x_axis_lutning = 45,
    manual_y_axis_title = "procent",
    manual_x_axis_title = "Läsår då gymnasieutbildningen påbörjades",
    procent_0_100_10intervaller = TRUE,
    manual_color = diag_fargvekt,
    skriv_till_diagramfil = skriv_diagramfil)

    objektnamn <- stringr::str_remove(diagramfilnamn, "\\.png")
    if (ggobjektfilnamn_utan_tid) objektnamn <- sub("_ar.*", "", objektnamn)
    stats::setNames(list(gg_obj), objektnamn)
    }))

  if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
    openxlsx::write.xlsx(gymn_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
  }
  return(gg_list)

} # slut diag-funktion

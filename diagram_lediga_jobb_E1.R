diagram_lediga_jobb_tid_sektor_E1 <- function(region_vekt = "20",			# Val av region. Finns: "00", "FA00", "FA01", "FA02", "FA03", "FA04", "FA05", "FA06", "FA07", "FA08", "FA09", "FA10", "0114", "0115", "0117", "FA11", "0120", "0123", "0125", "0126", "0127", "0128", "FA12", "0136", "0138", "0139", "FA13", "0140", "FA14", "FA15", "0160", "0162", "0163", "FA16", "FA17", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "FA18", "0191", "0192", "FA19", "FA20", "FA21", "FA22", "FA23", "FA24", "FA25", "FA26", "FA27", "FA28", "FA29", "0305", "FA30", "0319", "FA31", "FA32", "0330", "0331", "FA33", "FA34", "FA35", "0360", "FA36", "FA37", "0380", "0381", "0382", "FA38", "FA39", "FA40", "FA41", "0428", "FA42", "FA43", "FA44", "FA45", "0461", "FA46", "FA47", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "FA48", "FA49", "0509", "FA50", "0512", "0513", "FA51", "FA52", "FA53", "FA54", "FA55", "0560", "0561", "0562", "0563", "FA56", "FA57", "0580", "0581", "0582", "0583", "0584", "0586", "FA58", "FA59", "0604", "FA60", "0617", "01", "03", "0642", "0643", "04", "05", "0662", "0665", "06", "07", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "08", "09", "10", "12", "13", "14", "17", "18", "0760", "0761", "0763", "0764", "0765", "0767", "19", "20", "0780", "0781", "21", "22", "23", "24", "0821", "25", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "0980", "1060", "1080", "1081", "1082", "1083", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "1315", "1380", "1381", "1382", "1383", "1384", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"
                                              output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                              tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
                                              kon_klartext = "totalt", # Finns: "män", "kvinnor", "totalt"
                                              sektor_klartext = c("offentlig sektor", "privat sektor"),			 #  Finns: "offentlig sektor", "privat sektor", "totalt", "hela ekonomin", "näringslivet och hushållens icke-vinstdrivande organisationer", "offentlig förvaltning"
                                              cont_klartext = "Lediga jobb",            # Finns:
                                              kvartal_klartext = "9999",     # finns: 1, 2, 3, 4  "9999" senaste tillfängliga kvartal, NA = alla kvartal
                                              spara_figur = TRUE, # Skall diagrammet sparas
                                              returnera_data = FALSE, # Skall data returneras
                                              returnera_figur = TRUE){

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # "here", "httr", "jsonlite", "rjstat" och "rlang" togs bort - de behövdes
  # bara som stöd åt den gamla func_pxweb2.R/pxweb-hämtningen, som pxweb2r nu
  # sköter internt.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/stringr följer med som beroenden till rdverktyg/pxweb2r.

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  visa_dataetiketter <- FALSE
  gg_list <- list()

  # ====================================================================================================
  # Hämta data direkt via pxweb2r
  # TAB6424 = nya tabellen (kvartal 2024K2-), TAB4429 = gamla tabellen (historik 2006K1-2024K1, uppdateras ej).
  # De båda tabellerna har olika klartext-etiketter för sektor och innehåll (ContentsCode). Vi bygger
  # därför en frågevektor som täcker båda tabellernas varianter - on_all_values_invalid = "null" gör
  # att en tabell hoppas över (ger NULL) i stället för att hela uttaget stoppas, om inget av de begärda
  # värdena finns i just den tabellen. pxweb2r har (till skillnad från den gamla wrappern func_pxweb2.R)
  # inget inbyggt stöd för att hämta flera tabeller i ett anrop, så de hämtas var för sig här och slås
  # ihop med bind_rows() - samma mönster som används för CKM-uppdelade tabeller i andra skript.
  # ====================================================================================================

  sektor_varden <- sektor_klartext
  if (any(stringr::str_detect(tolower(sektor_klartext), "offentlig"))) {
    sektor_varden <- c(sektor_varden, "offentlig sektor", "offentlig förvaltning")
  }
  if (any(stringr::str_detect(tolower(sektor_klartext), "privat|näringslivet"))) {
    sektor_varden <- c(sektor_varden, "privat sektor", "näringslivet och hushållens icke-vinstdrivande organisationer")
  }
  if (any(stringr::str_detect(tolower(sektor_klartext), "totalt|hela ekonomin"))) {
    sektor_varden <- c(sektor_varden, "totalt", "hela ekonomin")
  }
  sektor_varden <- unique(sektor_varden)

  cont_varden <- cont_klartext
  if (any(stringr::str_detect(tolower(cont_klartext), "lediga jobb"))) {
    cont_varden <- c(cont_varden, "Lediga jobb", "Lediga jobb, totalt")
  }
  cont_varden <- unique(cont_varden)

  hamta_lediga_jobb_tabell <- function(tabell_id) {
    df <- pxweb2r::pxweb2_get_data(
      table = tabell_id,
      query = list(
        Region = region_vekt,
        Sektor = sektor_varden,
        ContentsCode = cont_varden,
        Tid = tid_koder
      ),
      on_all_values_invalid = "null")
    if (!is.null(df)) df$tabell_id <- tabell_id
    df
  }

  dataset_df <- dplyr::bind_rows(
    hamta_lediga_jobb_tabell("TAB6424"),
    hamta_lediga_jobb_tabell("TAB4429"))

  if (is.null(dataset_df) || nrow(dataset_df) == 0) {
    stop("Ingen data kunde hämtas för lediga jobb (E1) - kontrollera region/sektor/tid-urvalet.")
  }

  # --- normalisera kolumnnamn så att resten av skriptet är oberoende av SCB:s exakta etiketter -------

  # Tidskolumnen heter normalt "kvartal" i dessa tabeller, men vi känner av det defensivt eftersom
  # pxweb2r döper kolumnen efter SCB:s etikett för variabeln (skulle SCB byta etikett bryts annars koden).
  tid_kol <- intersect(c("kvartal", "tid"), names(dataset_df))[1]
  if (is.na(tid_kol)) {
    stop("Hittade ingen tidskolumn (kvartal/tid) i dataset_df. Kolumner: ", paste(names(dataset_df), collapse = ", "))
  }
  if (tid_kol != "kvartal") dataset_df <- dataset_df |> dplyr::rename(kvartal = !!tid_kol)

  # Innehållskolumnen (ContentsCode) är den kolumn som blir kvar utöver de kända kolumnerna.
  # OBS: om SCB döper ContentsCode-dimensionen till något annat än vi förväntar oss dyker den upp
  # här ändå eftersom vi letar efter "det som blir över" - men döp gärna av vid felsökning med
  # names(dataset_df) om något inte stämmer.
  kanda_kolumner <- c("tabell_id", "region_kod", "region", "sektor", "kvartal", "value")
  innehall_kol <- setdiff(names(dataset_df), kanda_kolumner)

  if (length(innehall_kol) == 1) {
    dataset_df <- dataset_df |>
      dplyr::rename(innehall = !!innehall_kol) |>
      dplyr::mutate(innehall = ifelse(innehall %in% c("Lediga jobb", "Lediga jobb, totalt"), "Lediga jobb", innehall)) |>
      dplyr::filter(innehall == "Lediga jobb") |>
      dplyr::select(-innehall)
  }

  # döp om värdekolumnen till "varde" (samma konvention som övriga diagramskript använder)
  dataset_df <- dataset_df |> dplyr::rename(varde = value)

  # döp om sektor så att det stämmer mellan de båda tabellerna
  sektor_namnvektor <- c(
    "offentlig förvaltning" = "offentlig sektor",
    "näringslivet och hushållens icke-vinstdrivande organisationer" = "privat sektor",
    "hela ekonomin" = "totalt"
  )
  dataset_df <- dataset_df |>
    dplyr::mutate(sektor = dplyr::recode(sektor, !!!sektor_namnvektor))

  # om samma kvartal skulle finnas i båda tabellerna: behåll raden från TAB6424 (den nyare tabellen)
  if ("tabell_id" %in% names(dataset_df)) {
    prioritetsordning <- c("TAB6424", "TAB4429")
    dataset_df <- dataset_df |>
      dplyr::mutate(.prioritet = match(tabell_id, prioritetsordning)) |>
      dplyr::arrange(.prioritet) |>
      dplyr::distinct(region_kod, sektor, kvartal, .keep_all = TRUE) |>
      dplyr::select(-.prioritet, -tabell_id)
  }

  lediga_jobb_df <- dataset_df |>
    dplyr::mutate(
      ar = stringr::str_sub(kvartal, 1, 4),
      kvartal_num = as.numeric(stringr::str_sub(kvartal, 6, 6)),
      kvartal_txt = paste0("Kvartal ", kvartal_num)
    ) |>
    dplyr::arrange(kvartal, sektor)

  # filtrera på specifikt/senaste kvartal om kvartal_klartext är satt (NA = behåll alla kvartal)
  if (!all(is.na(kvartal_klartext))) {
    kvartal_senaste <- if ("9999" %in% as.character(kvartal_klartext)) {
      lediga_jobb_df |> dplyr::filter(kvartal == max(kvartal)) |> dplyr::pull(kvartal_num) |> unique()
    } else numeric(0)

    kvartal_siffror <- as.character(kvartal_klartext)
    kvartal_siffror <- as.numeric(kvartal_siffror[kvartal_siffror %in% c("1", "2", "3", "4")])

    kvartal_valda <- unique(c(kvartal_senaste, kvartal_siffror))

    lediga_jobb_df <- lediga_jobb_df |> dplyr::filter(kvartal_num %in% kvartal_valda)
  }

  if(returnera_data == TRUE){
    assign("lediga_jobb_E1_df", lediga_jobb_df, envir = .GlobalEnv)
  }

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(lediga_jobb_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(lediga_jobb_df$region_kod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(lediga_jobb_df$region_kod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) paste0(unique(lediga_jobb_df$region_kod), collapse = "_") else region_txt

  kvartal_titel <- dplyr::case_when(length(unique(lediga_jobb_df$kvartal_num)) > 1 ~ paste0("kvartal ", rdverktyg::list_komma_och(unique(lediga_jobb_df$kvartal_num))),
                             unique(lediga_jobb_df$kvartal_num) == 1 ~ "första kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 2 ~ "andra kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 3 ~ "tredje kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 4 ~ "fjärde kvartalet",
                             TRUE ~ "")

  diagramtitel <- glue::glue("Lediga jobb{region_txt} {kvartal_titel} varje år")
  diagramfil <- stringr::str_replace_all(
    glue::glue("lediga_jobb_{regionfil_txt}_E1_{dplyr::first(lediga_jobb_df$ar)}_{dplyr::last(lediga_jobb_df$ar)}_kvartal{unique(lediga_jobb_df$kvartal_num)}.png"),
    "__", "_")

  chart_df <- lediga_jobb_df

  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = chart_df,
                               skickad_x_var = "ar" ,
                               skickad_y_var = "varde",
                               skickad_x_grupp = if ("sektor" %in% names(chart_df) & length(unique(chart_df$sektor)) > 1) "sektor" else NA,
                               x_axis_sort_value = FALSE,
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               dataetiketter = visa_dataetiketter,
                               geom_position_stack = TRUE,
                               manual_y_axis_title = "",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = if ("sektor" %in% names(chart_df) & length(unique(chart_df$sektor)) > 1) rddiagram::diagramfarger("rus_sex") else rddiagram::diagramfarger("rus_sex")[1],
                               output_mapp = output_mapp_figur,
                               skriv_till_diagramfil = spara_figur,
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")
  if (returnera_figur == TRUE) return(gg_list)
}

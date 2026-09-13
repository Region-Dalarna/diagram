diag_gymn_elever_kon_prg_skolverket <- function(
  region_vekt = "20",      # Val av region.
  tid_koder = "9999",       # "*" = alla år, finns från 2011 och framåt
  gymnasieprogram = "*",       #  # "*" = alla gymnasieprogram, annars anges programnamn, dessa finns: "Nationella program", "Högskoleförberedande program", "Yrkesprogram", "Introduktionsprogrammen", "Barn- och fritidsprogrammet", "Bygg- och anläggningsprogramme", "Ekonomiprogrammet", "El- och energiprogrammet", "Estetiska programmet", "Fordons- och transportprogramm", "Försäljnings- och serviceprogr", "Handels- och administrationspr", "Hantverksprogrammet", "Hotell- och turismprogrammet", "Humanistiska programmet", "Industritekniska programmet", "International Baccalaureate", "Introduktionsprogram, Individu", "Introduktionsprogram, Programi", "Introduktionsprogram, Språkint", "Introduktionsprogram, Yrkesint", "Naturbruksprogrammet", "Naturvetenskapsprogrammet", "Restaurang- och livsmedelsprog", "Riksrekryterande utbildningar", "Samhällsvetenskapsprogrammet", "Teknikprogrammet", "VVS- och fastighetsprogrammet", "Vård- och omsorgsprogrammet"
  diagram_capt = "Källa: Skolverket\nBearbetning: Samhällsanalys, Region Dalarna",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  skapa_facet_diagram = TRUE,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  output_mapp = NA,
  diagramrubrik_tabort = FALSE,
  skriv_diagramfil = TRUE,
  returnera_data_rmarkdown = FALSE,
  excelfil_mapp = NA,      # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
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
  # hamta_gymn_elever_kon_bakgrund_arskurs_prg_skolverket.R (som bara
  # användes av det här skriptet) är inlinead nedan i stället för att
  # source()as, i övrigt oförändrad.
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
  # dplyr/purrr/stringr/tidyr/httr följer med som beroenden till rddiagram/rdverktyg.

  hamta_gymn_elever_kon_bakgrund_arskurs_prg_skolverket <- function(region_vekt = "20",                     # NA = riket, alla län och alla kommuner
                                                                    valda_ar = "9999",                          # "9999" senaste år, "*" = alla år
                                                                    gymnasieprogram = "*",                     # "*" = alla gymnasieprogram
                                                                    huvudman = "Samtliga",                     # finns: "Samtliga", "Kommunal" och "Enskild", det går att välja flera
                                                                    ta_bort_na = TRUE,                         # TRUE = ta bort NA-värden i andel-kolumnen
                                                                    konvertera_andel_till_numerisk = TRUE      # TRUE = numerisk kolumn av andel, då försvinner prickar och liknande och blir NA. Vill man se vad som är prickar och hur många det är kan man sätta denna till FALSE
                                                                    ) {

    # ==================================================================================================================
    #
    # Skript för att hämta gymnasieelever per kön, bakgrund och årskurs från Skolverket per län, kommun eller för riket.
    # Detta skript hämtar alla kommuner i en excelfil för alla år. Det går således inte att snabba upp skriptet genom att
    # välja någon eller några få kommuner och man hämtar alltid alla år. Men man kan ändå filtrera ut de kommuner, län
    # eller riket som man vill ha för att få ett mindre dataset. Skriptet kollar att det har senaste år och använder detta.
    # Det variabler som ingår är läsår, regionkod, region, Gymnasieprogram, Typ av huvudman, Genomströmning samt andel.
    #
    # Kön, bakgrund och föräldras utbildning är i procent, antal elever i absoluta tal.
    #
    # Skapat av: Peter Möller, Region Dalarna
    #
    # ==================================================================================================================

    # funktion som används nedan för att hitta rätt senaste år
    hitta_senaste_ar <- function(min_size_kb = 70, visa_meddelanden = FALSE) {

      start_ar <- as.integer(format(Sys.Date(), "%Y"))

      for (ar in start_ar:2011) {

        tf <- tempfile(fileext = ".xlsx")

        url <- paste0(
          "https://siris.skolverket.se/siris/reports/export_api/runexport/?",
          "pFormat=xls",
          "&pExportID=458",
          "&pAr=", ar,
          "&pLan=&pKommun=&pHmantyp=&pUttag=&pSortering=&pToken=",
          "&pVerkform=21",
          "&pFlikar=1"
        )

        res <- tryCatch({
          httr::GET(url, httr::write_disk(tf, overwrite = TRUE))
        }, error = function(e) {
          message("Fel vid hämtning för år ", ar, ": ", e$message)
          return(NULL)
        })

        if (is.null(res)) {
          next
        }

        if (httr::status_code(res) != 200) {
          message("Statuskod ", httr::status_code(res), " för år ", ar)
          next
        }

        size_kb <- file.size(tf) / 1024

        if (visa_meddelanden) cat("År:", ar, "- storlek:", round(size_kb, 1), "kB\n")

        if (!is.na(size_kb) && size_kb > min_size_kb) {
          return(list(
            ar = ar,
            fil = tf,
            storlek_kb = size_kb
          ))
        }
      }

      stop("Hittade inget år med tillräcklig storlek (>= 70 kB)")
    }

    senaste_ar <- hitta_senaste_ar()$ar

    giltiga_ar <- as.character(2011:senaste_ar)

    valda_ar <- unique(stringr::str_replace(stringr::str_replace(valda_ar, "9999", as.character(senaste_ar)), "\\*", giltiga_ar))
    valda_ar <- valda_ar[valda_ar %in% giltiga_ar]

    if (length(valda_ar) > 0) {

        url_lista <- c(
          url_riket = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=458&pAr=2025&pLan=&pKommun=&pHmantyp=&pUttag=&pSortering=&pToken=&pVerkform=21&pFlikar=1",
          url_lan = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=460&pAr=2025&pLan=&pKommun=&pHmantyp=&pUttag=&pSortering=&pToken=&pVerkform=21&pFlikar=1",
          url_kommun = "https://siris.skolverket.se/siris/reports/export_api/runexport/?pFormat=xls&pExportID=461&pAr=2025&pLan=&pKommun=&pHmantyp=&pUttag=&pSortering=&pToken=&pVerkform=21&pFlikar=1"
        )

        # om vi fått ett annat år när vi extraherat senaste år ovan än vad som finns i url:erna ovan (år 2019) så används detta istället

        url_lista <- purrr::map_chr(url_lista, ~ stringr::str_replace(.x, "&pAr=\\d{4}", paste0("&pAr=", senaste_ar)))

        df_list <- list()                   # vi sparar hämtad statistik till denna lista

        # om region_Vekt är NA så hämtas alla län, kommuner och riket
        if (all(region_vekt == "*")) region_vekt <- rdverktyg::hamtaregtab()$regionkod

        las_in_excelfil <- function(fil_url) {

          httr::GET(fil_url, httr::write_disk(tf_excelfil <- tempfile(fileext = ".xlsx")))
          flikar <- readxl::excel_sheets(tf_excelfil)
          flikar <- flikar[!stringr::str_detect(flikar, "beskrivning")]
          if (!all(gymnasieprogram == "*")) flikar <- flikar[flikar %in% gymnasieprogram]

          dataset_df <- purrr::map(flikar, ~ readxl::read_excel(tf_excelfil, sheet = .x, skip = 6, col_types = "text") |>
                              tidyr::pivot_longer(dplyr::matches("^\\d{4}"), values_to = "varde", names_to = "lasar") |>
                              dplyr::mutate(gymnasieprogram = .x) |>
                              dplyr::filter(`Typ av huvudman` %in% huvudman,
                                     varde != ".")) |>
            purrr::list_rbind() |>
            dplyr::relocate(gymnasieprogram, .before = 1)

          # om det är riket som hämtas
          if ("Riket" %in% names(dataset_df)) {
            dataset_df <- dataset_df |>
              dplyr::mutate(regionkod = "00") |>
              dplyr::rename(region = Riket) |>
              dplyr::relocate(region, .before = 1) |>
              dplyr::relocate(regionkod, .before = 1)
          }

          # om det är län som hämtas
          if ("Länskod" %in% names(dataset_df) & !"Kommun-kod" %in% names(dataset_df)) {
            dataset_df <- dataset_df |>
              dplyr::rename(regionkod = `Länskod`,
                     region = Län) |>
              dplyr::relocate(region, .before = 1) |>
              dplyr::relocate(regionkod, .before = 1)
          }

          # om det är kommuner som hämtas
          if ("Län" %in% names(dataset_df) & "Kommun-kod" %in% names(dataset_df)) {
            dataset_df <- dataset_df |>
              dplyr::rename(regionkod = `Kommun-kod`,
                     region = Kommun) |>
              dplyr::select(-c(`Läns-kod`, Län)) |>
              dplyr::relocate(region, .before = 1) |>
              dplyr::relocate(regionkod, .before = 1)
          }

          dataset_df <- dataset_df |>
            dplyr::rename(huvudman = `Typ av huvudman`) |>
            dplyr::relocate(lasar, .before = 1)

          if (!all(region_vekt == "*")) {
            dataset_df <- dplyr::filter(dataset_df, regionkod %in% region_vekt)
          }

          return(dataset_df)
        } # slut läs in excelfil-funktion

        if (length(region_vekt[region_vekt == "00"]) > 0) df_list[["riket"]] <- las_in_excelfil(url_lista[["url_riket"]])
        if (length(region_vekt[nchar(region_vekt) == 2 & region_vekt != "00"]) > 0) df_list[["lan"]] <- las_in_excelfil(url_lista[["url_lan"]])
        if (length(region_vekt[nchar(region_vekt) == 4]) > 0) df_list[["kommun"]] <- las_in_excelfil(url_lista[["url_kommun"]])

        retur_df <- dplyr::bind_rows(df_list) |>
          dplyr::rename(variabel = Elever)

        if (konvertera_andel_till_numerisk) retur_df <- suppressWarnings(
          dplyr::mutate(retur_df,
            varde = dplyr::na_if(varde, ".."),
            varde = readr::parse_number(varde))
          )

        # ta bort NA-värden om det är valt
        if (ta_bort_na) retur_df <- dplyr::filter(retur_df, !is.na(varde))

        return(retur_df)

    } else { # slut if-sats för att testa om det finns giltiga år
      message(glue::glue("Inga giltiga år medskickade till funktionen. Följande år finns i tabellen: {rdverktyg::list_komma_och(giltiga_ar)}. Kontrollera valda år och försök igen."))
    } # slut test om det finns giltiga år

  } # slut hamta_gymn_elever_kon_bakgrund_arskurs_prg_skolverket

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("kon")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("kon")
  }

  if (all(is.na(output_mapp))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_mapp <- rdverktyg::utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }

  dataset_df <- rdverktyg::suppress_specific_warning(
  hamta_gymn_elever_kon_bakgrund_arskurs_prg_skolverket(
    region_vekt = region_vekt,      # Val av region.
    valda_ar = tid_koder,
    gymnasieprogram = gymnasieprogram,
    huvudman = "Samtliga",
    konvertera_andel_till_numerisk = TRUE
  ))

  # OBS: den nedladdade Skolverket-excelfilen innehåller flera läsår
  # (en kolumn per läsår, som pivot_longer() i hamta-funktionen ovan gör
  # om till en rad per läsår) - "tid_koder"/valda_ar styr bara vilket
  # exportår som laddas ner, inte vilka läsår som faktiskt finns i den
  # nedladdade filen. Utan denna filtrering innehåller dataset_df alla
  # läsår i filen samtidigt, vilket kraschar diagrammet nedan (som
  # förutsätter ett enda läsår per diagram, t.ex. i titel/filnamn).
  # Behåller bara det senaste läsåret, vilket matchar standardvärdet
  # tid_koder = "9999" (senaste år).
  dataset_df <- dplyr::filter(dataset_df, lasar == max(lasar))

  # filtrera ut andel kvinnor, beräkna andel män utifrån andel kvinnor och lägg till i datasetet
  chart_df <- dataset_df |>
    dplyr::mutate(variabel = ifelse(variabel == "Andel kvinnor (%)", "Kvinnor", variabel),
           region = rdverktyg::skapa_kortnamn_lan(region)) |>
    dplyr::filter(variabel == "Kvinnor")

  chart_df <- chart_df |>
    # Skapa en ny dataram för "Män"
    dplyr::bind_rows(
      dplyr::mutate(chart_df,
          variabel = "Män",       # Ändra variabeln
          varde = 100 - varde              # Beräkna andelen män
        )
    )


  if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
    openxlsx::write.xlsx(chart_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
  }

  if(returnera_data_rmarkdown == TRUE){
    assign("gymn_elever_kon_prg_df", chart_df, envir = .GlobalEnv)
  }

  skapa_diagram <- function(skickad_df, region_kod) {

    skickad_df <- dplyr::filter(skickad_df, regionkod %in% region_kod)

    lasar_txt <- unique(skickad_df$lasar)

    # OBS: originalet skrev "length(unique(skickad_df)) > 1" här och i
    # diagram_facet nedan - length() på en dataframe räknar ANTAL
    # KOLUMNER, inte rader, så villkoret var alltid sant (antalet
    # kolumner ändras inte av unique()) oavsett hur många rader som
    # faktiskt fanns i datat. I det här diagrammet var det ändå alltid
    # rätt resultat: en fasett per gymnasieprogram behövs alltid (annars
    # summeras alla program felaktigt ihop till samma stapel, verifierat
    # vid test - staplarna gick långt över 100 %). Skriver ut det
    # ovillkorat i stället för det trasiga villkoret. (vald_region_txt
    # beräknades tidigare för en "facet_txt" som aldrig användes någon
    # annanstans - borttagen.)

    diagramtitel <- glue::glue("Könsbalans per gymnasieprogram i alla årskurser läråret {lasar_txt}")
    diagramfil <- stringr::str_replace_all(glue::glue("gymn_elever_kon_prg_{paste0(region_kod, collapse = '_')}_lasar{lasar_txt}.png"), "/", "_")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = skickad_df,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      geom_position_stack = FALSE,
      diagram_titel = if (diagramrubrik_tabort) NULL else diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = "procent",
      # manual_x_axis_text_vjust = 1,
      # manual_x_axis_text_hjust = 1,
      x_axis_lutning = 0,
      manual_color = diag_fargvekt,
      output_mapp = output_mapp,
      facet_grp = "gymnasieprogram",
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      facet_scale = "fixed",
      facet_legend_bottom = TRUE,
      skriv_till_diagramfil = skriv_diagramfil
    )

    ett_diagram <- list(gg_obj)
    names(ett_diagram) <- stringr::str_remove(diagramfil, "\\.png")
    return(ett_diagram)

  } # slut skapa_diagram-funktion

  if (skapa_facet_diagram) {
    retur_list <- skapa_diagram(skickad_df = chart_df, region_kod = region_vekt)

  } else {

    retur_list <- purrr::flatten(purrr::map(unique(region_vekt), ~ skapa_diagram(skickad_df = chart_df, region_kod = .x)))
  }

  return(retur_list)

} # slut diag-funktion

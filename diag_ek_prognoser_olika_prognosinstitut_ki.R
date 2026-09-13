diag_ekonomiska_prognoser_olika_progn_institut_ki <- function(vald_variabel = "BNP",                  # finns: "BNP", "Hushållens konsumtion", "Offentlig konsumtion", "Fasta bruttoinvesteringar", "Lagerinvesteringar., förändr. i proc. av BNP föreg. år", "Export", "Import", "Antal sysselsatta, 15-74 år (AKU)", "Arbetslöshet, procent av arbetskraften, 15-74 år (AKU)", "Timlön, totalt (konjunkturlönestatistiken)", "Konsumentprisindex (KPI), årsgenomsnitt", "KPI med fast bostadsränta (KPIF), årsgenomsnitt", "Real disponibel inkomst (nationalräkenskaperna)", "Styrränta, vid årets slut, procent**", "Offentligt finansiellt sparande, procent av BNP", "Bytesbalans, procent av BNP (nationalräkenskaperna)", "Timlön, näringslivet (konjunkturlönestatistiken)"
                                                                valda_prognos_ar = "+1",                  # NA eller "*" = alla år, "+1" = aktuellt år + ett år, kan vara andra antal år
                                                                endast_mest_aktuell_prognos = TRUE,      # TRUE om man bara vill ha den mest aktuella prognosen varje år, annars kommer alla prognoser som institut har gjort för prognosåret med i datasetet
                                                                output_mapp = rdverktyg::utskriftsmapp(),
                                                                x_axis_lutning = 45, # Lutning på x-axelns text
                                                                manual_y_axis_title = "Prognosticerad tillväxt (%)", # Möjlighet att styra namn på y-axel. Tenderar att överlappa med x-axel med prognosticerad tillväxt.
                                                                diagram_capt = "Källa: Konjunkturinstitutet, bearbetning av Samhällsanalys, Region Dalarna",
                                                                skriv_diagramfil = TRUE
                                                                ) {

  # GG-list skapades inte. Lagt till nedan Jon 2025-10-06
  # Har dessutom lagt till så att man kan ändra diverse parametrar i figuren
  # Gjort så att prognosinstitut sorteras efter kortaste namn (hamnar utanför figuren annars)
  # Ändrat felaktighet som gjorde att ett namn blev NA. Jon 2026-05-18

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
  # stället för library(). Ingen SCB-hämtning här - datan skrapas fram från
  # Konjunkturinstitutets publika sida (prognosjämförelse-excelfilen), så
  # hamta_data-repots hamta_ek_prognoser_fran_prognosinstitut_ki.R (som bara
  # användes av det här skriptet) är inlinead nedan i stället för att
  # source()as, i övrigt oförändrad.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
  if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
  if (!requireNamespace("forcats", quietly = TRUE)) install.packages("forcats")
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  # dplyr/purrr/stringr/httr följer med som beroenden till rddiagram/rdverktyg.

  hamta_ek_prognoser_fran_prognosinstitut_ki <- function(
    prognos_ar = "*",
    bara_senaste_prognos = TRUE,           # tar bara med vara prognosinstituts senaste år
    ta_bort_gamla_ar = TRUE                # tar bort år som är innan nu, det år som är nu kommer med
  ) {

    # Laddar hem data som Konjunkturinstitutet sammanställt från olika banker och andra institut på hur de prognosticerar
    # den ekonomiska utvecklingen framåt
    #
    # Man kan ange prognos_ar = "*" för att hämta alla prognosår, eller enskilda år som tex c("2024", "2025", "2026"), ett eller flera
    # bara_senaste_prognos = TRUE om man enbart vill ha varje instituts senaste prognos, vid FALSE så får man alla prognoser de gjort under åren

    url_progn <- "https://www.konj.se/publikationer/prognosjamforelse"       # url till Konjunkturinstitutets webbsida med ekonomiska prognoser

    # ======================== hämta fil med nyckeltabell för prognosinstitut från Konjunkturinstitutet ========================
    # extrahera tabell med koder och klartext för prognosinstitut direkt från konjunkturinstitutets webbsida
    progn_html <- rvest::read_html(url_progn)

    prognos_xlsx_url <- progn_html |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")
    prognos_xlsx_url <- prognos_xlsx_url[stringr::str_detect(prognos_xlsx_url, "prognos") & stringr::str_detect(prognos_xlsx_url, "xlsx")]

    temp_xlsx <- tempfile(fileext = ".xlsx")

    # Ladda ner excelfilen till temp-filen
    httr::GET(prognos_xlsx_url, httr::write_disk(temp_xlsx, overwrite = TRUE))

    # Se till att tempfilen tas bort när R-sessionen avslutas
    on.exit(unlink(temp_xlsx), add = TRUE)

    # Läs in filen som en data.frame (tibble)

    progn_flikar <- readxl::excel_sheets(temp_xlsx)
    progn_flikar <- progn_flikar[!stringr::str_detect(tolower(progn_flikar), "utskrift")]

    institutnyckelflik <- progn_flikar[stringr::str_detect(tolower(progn_flikar), "inst")]

    progn_list <- purrr::map(progn_flikar, ~ openxlsx::read.xlsx(temp_xlsx, sheet = .x, colNames = FALSE))
    names(progn_list) <- progn_flikar

    institutionnyckel <- purrr::pluck(progn_list, institutnyckelflik) |>
      dplyr::rename(inst_kod = 1, Prognosinstitut = 2, Prognosinstitut_eng = 3) |>
      dplyr::mutate(inst_kod = toupper(inst_kod))

    las_in_flik <- function(inlast_flik, inlast_flik_namn) {

      fliknamn <- stringr::str_extract(inlast_flik_namn, "-?[0-9]+\\.?[0-9]*")

      inlast_flik <- inlast_flik |>
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::na_if(., ""))) |>
        dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.))) |>       # ta bort helt tomma rader
        dplyr::select(dplyr::where(~ any(!is.na(.))))                  # ta bort helt tomma kolumner
      inlast_flik <- dplyr::filter(inlast_flik, dplyr::if_any(2:ncol(inlast_flik), ~ !is.na(.)))

      bnp_rad <- inlast_flik[inlast_flik[[1]] == "BNP", ]

      varde_vektor <- unlist(bnp_rad[ , -1])
      sista_kolumn <- max(which(!is.na(varde_vektor))) + 1

      inlast_flik <- inlast_flik[, 1:sista_kolumn]

      inst_kod   <- unlist(inlast_flik[1, -1])
      publiceringsdatum <- unlist(inlast_flik[2, -1])
      publiceringsvecka <- unlist(inlast_flik[3, -1])

      data <- inlast_flik[-c(1:3), ]
      forsta_kolnamn <- names(data)[1]

      inlast_flik_long <- data |>
        tidyr::pivot_longer(
          cols = -1,
          names_to = "col",
          values_to = "varde"
        ) |>
        dplyr::mutate(
          variabel           = .data[[forsta_kolnamn]],
          inst_kod    = rep(inst_kod,   times = nrow(data)),
          publiceringsdatum  = as.Date(as.integer(rep(publiceringsdatum, times = nrow(data))), origin = "1899-12-30"),
          publiceringsvecka  = rep(publiceringsvecka, times = nrow(data))
        ) |>
        dplyr::select(inst_kod, publiceringsdatum, publiceringsvecka, variabel, varde) |>
        dplyr::mutate(publiceringsdatum = as.Date(publiceringsdatum, origin = "1899-12-30"),
               prognos_for_ar = fliknamn) |>
        dplyr::filter(!is.na(varde))

      return(inlast_flik_long)
    }

    # läs in alla flikar som inte är nyckel för prognosinstitut, koppla på nyckeln och ändra vissa
    # prognosinstituts namn till kortare versioner
    prognoser_df <- purrr::imap(progn_list[!stringr::str_detect(tolower(names(progn_list)), "inst")], ~ las_in_flik(.x, .y)) |>
      purrr::list_rbind() |>
      dplyr::mutate(inst_kod = stringr::str_remove(toupper(inst_kod), "¹")) |>
      dplyr::left_join(dplyr::select(institutionnyckel, -Prognosinstitut_eng), by = "inst_kod") |>
      dplyr::relocate(Prognosinstitut, .after = "inst_kod") |>
      dplyr::mutate(Prognosinstitut = dplyr::case_when(inst_kod == "OECD" ~ "OECD",
                                         inst_kod == "EU" ~ "EU (Kommissionen)",
                                         inst_kod == "LO" ~ "LO",
                                         inst_kod == "SEB" ~ "SEB",
                                         inst_kod == "SKR" ~ "SKR",
                                         inst_kod == "TFIA" ~ "Teknikföret./Industriarbetsg.",
                                         TRUE ~ Prognosinstitut),
             varde = readr::parse_number(varde))

    # ta bara med varje prognosinstituts senaste prognos för aktuellt år om bara_senaste_prognos == TRUE
    if (bara_senaste_prognos) {
      prognoser_df <- prognoser_df |>
        dplyr::group_by(inst_kod, Prognosinstitut, variabel, prognos_for_ar) |>
        dplyr::filter(publiceringsdatum == max(publiceringsdatum, na.rm = TRUE)) |>
        dplyr::ungroup()
    }

    # ta bort år som är före det år som är nu om ta_bort_gamla_ar == TRUE
    if (ta_bort_gamla_ar) {
      innevarande_ar <- format(Sys.Date(), "%Y")
      prognoser_df <- dplyr::filter(prognoser_df, prognos_for_ar >= innevarande_ar)
    }

    # om prognos_ar har skickats med, ta bara med dessa
    if (all(prognos_ar != "*")) {
      prognoser_df <- dplyr::filter(prognoser_df, prognos_for_ar %in% prognos_ar)
      if (nrow(prognoser_df) == 0) stop("Valt/valda prognosår finns inte i datasetet")
    }

    return(prognoser_df)

  } # slut funktion hamta_ek_prognoser_fran_prognosinstitut_ki

  if (all(is.na(valda_prognos_ar))) valda_prognos_ar <- "*"
  if (stringr::str_detect(valda_prognos_ar, "\\+")) {
    innevarande_ar <- as.integer(format(Sys.Date(), "%Y"))
    slut_ar <- max(as.integer(stringr::str_extract(valda_prognos_ar, "[0-9]+"))) + innevarande_ar
    valda_prognos_ar <- innevarande_ar:slut_ar
  }
  prognoser_df <- hamta_ek_prognoser_fran_prognosinstitut_ki(prognos_ar = as.character(valda_prognos_ar),
                                                             bara_senaste_prognos = endast_mest_aktuell_prognos)

  # För vissa insitut (ESV) saknas utskrivet namn. Ändrar här så att namnet då sätts till förkortningen (annars blir det NA) Jon 2026-05-18
  prognoser_df <- prognoser_df |>
    dplyr::mutate(Prognosinstitut = ifelse(is.na(Prognosinstitut), inst_kod, Prognosinstitut))
  # Skapa diagram över prognoser

  prognoser_variabel <- dplyr::filter(prognoser_df, variabel == vald_variabel)
  #if (any(valda_prognos_ar != "*"))  prognoser_variabel <- prognoser_variabel %>% filter(prognos_ar %in% valda_prognos_ar)

  prognoser_variabel_ar <- unique(prognoser_variabel$prognos_for_ar)

  diagram_titel <- paste0(vald_variabel, " - prognoser över utveckling")
  diagramfil <- paste0(stringr::str_remove_all(vald_variabel, ","), "_prognos_ar_", paste0(prognoser_variabel_ar, collapse = "_"), ".png")

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = prognoser_variabel |>
      dplyr::mutate(antal_tecken = nchar(Prognosinstitut),
        Prognosinstitut = forcats::fct_reorder(Prognosinstitut, antal_tecken, .desc = FALSE)
      ) |>
      dplyr::arrange(antal_tecken),
    skickad_x_var = "Prognosinstitut",
    skickad_y_var = "varde",
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    #x_axis_storlek = 8,
    #x_var_fokus = "prognos_ar",
    stodlinjer_avrunda_fem = TRUE,
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_y_axis_title = manual_y_axis_title,
    x_axis_lutning = x_axis_lutning,
    facet_grp = "prognos_for_ar",
    facet_scale = "free_x",
    manual_color = rddiagram::diagramfarger("rus_sex")[1],
    output_mapp = output_mapp,
    skriv_till_diagramfil = skriv_diagramfil,
    filnamn_diagram = diagramfil)

  gg_list <- list(gg_obj)
  names(gg_list) <- stringr::str_remove(diagramfil, "\\.png")

  return(gg_list)

}

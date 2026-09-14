


karta_befolkningsprognos <- function(
    region_vekt = "20",                # skickas länskoder med så hämtas alla kommuner i länet, kommunkoder används som de är
    start_ar = "9999",                 # "9999" är senaste tillgängliga år för folkmängd för helår, annars går det att skicka med ett enskilt år från år 1968
    till_ar = "+10",                   # +10 innebär att det är 10 år efter startåret, går att skicka med ett enskilt år
    returnera_ggobj = TRUE,            # returnerar kartan som ggplot-plotobjekt
    skriv_karta_png = TRUE,            # skriver en png-fil av ggplot-objektet
    skriv_karta_html = TRUE,           # skriver en leaflet-karta till en html-fil
    filnamns_prefix = NA,              # om man vill döpa filerna själv ges möjlighet här, NA = de döps automatiskt
    kommuner_egetnamn = NA,            # om man vill döpa en samling kommuner själv, annars sker det automatiskt
    output_mapp = NA,                  # om man sparar filer så behövs anges en sökväg här
    prognosskapare = "Region Dalarna", # ändras till SCB om deras prognos används
    karta_capt_gg = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna",          # <prognos_ar> byts ut till prognosåret om det finns ett sådant
    karta_capt_leaflet = "Källa: <prognosskapare>s befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna"
  ) {

  if (!returnera_ggobj & !skriv_karta_png & !skriv_karta_html) stop("Välj TRUE på något av parametrarna returnera_ggobj, skriv_karta_png eller skriv_karta_html, annars körs inte skriptet.")

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna (utom hamta_befprognos_scb_data.R,
  # se nedan) och inget library(tidyverse)/p_load(). Anropas med fullt namespace. func_API.R och
  # func_GIS.R behövs inte längre alls här - samtliga funktioner som hämtades därifrån
  # (hamtaregtab/hamtakommuner/ar_alla_kommuner_i_ett_lan/hamtaregion_kod_namn/utskriftsmapp/
  # skapa_aldersgrupper/hamta_karta) finns redan i rdverktyg/rdgis med identisk signatur.
  # hamta_bef_folkmangd_alder_kon_ar_scb.R (v1: BE0101A/BefolkningNy+BefolkningCKM) är ersatt med
  # direkta pxweb2r-anrop mot v2-motsvarigheterna TAB638/TAB5557, samma tabellpar/mönster som redan
  # används/verifierats i diag_befolkningsprognos_scb_api_profet.R och
  # diag_bef_utfall_prognos_per_aldersgrupp_scb.R/diag_bef_utfall_prognos_per_region_totalt_scb.R.
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("rdgis", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdgis")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
  if (!requireNamespace("htmltools", quietly = TRUE)) install.packages("htmltools")
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/purrr/stringr/tidyr följer med som beroenden till rdverktyg/rdgis.
  # (mapview laddades tidigare men användes aldrig i skriptet - borttaget.)

  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")

  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_karta_png | skriv_karta_html) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(rdverktyg::utskriftsmapp())) {
        output_mapp <- rdverktyg::utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }

  kommunnyckel <- rdverktyg::hamtaregtab() |>                     # hämta alla kommunkoder
    dplyr::filter(nchar(regionkod) == 4) |>
    dplyr::rename(Kommun = region,
           Kommunkod = regionkod)

  region_kommuner <- region_vekt[nchar(region_vekt) == 4]            # lägg alla kommunkoder i en vektor
  region_lan <- region_vekt[nchar(region_vekt) == 2]                 # lägg alla länskoder i en vektor
  kommunkoder <- rdverktyg::hamtakommuner(region_lan, F, F) |>       # hämta alla kommuner som finns i län med länskod
    c(region_kommuner)                                               # och lägg ihop med skickade kommunkoder

  kommunkoder <- kommunkoder[kommunkoder %in% kommunnyckel$Kommunkod]      # behåll bara kommunkoder som är giltiga

  filnamn_kommuner <- rdverktyg::ar_alla_kommuner_i_ett_lan(kommunkoder, returnera_text = TRUE, returtext = paste0(kommunkoder, collapse = "_")) |> stringr::str_replace_all(" ", "_")

  if (is.na(kommuner_egetnamn)) {
    retur_kommuner <- rdverktyg::hamtaregion_kod_namn(kommunkoder)$region |> rdverktyg::list_komma_och()
    kommuner_egetnamn <- rdverktyg::ar_alla_kommuner_i_ett_lan(kommunkoder, returnera_text = TRUE, returtext = retur_kommuner)
  }

  # Bugfix-konsekvens (samma "+N betyder nu prognosår+N i stället för prognosår-1+N" som i
  # hamta_befprognos_scb_data.R, se commit e35978c i hamta_data-repot): "+N" ska enligt
  # parameterdokumentationen ovan fortfarande betyda "N år efter startåret" - det kräver nu att vi ber
  # om "+/-(N-1)" i stället för "+/-N" för att landa på samma faktiska målår som innan hamta_data-fixet
  # (ett enskilt år, utan "+"/"-", skickas vidare oförändrat).
  till_ar_hamta <- if (stringr::str_detect(till_ar, "^[+-][0-9]+$")) {
    tecken <- stringr::str_sub(till_ar, 1, 1)
    paste0(tecken, as.numeric(stringr::str_sub(till_ar, 2)) - 1)
  } else till_ar

  if (any(stringr::str_sub(kommunkoder, 1, 2) != "20")) {
    befprogn <- hamta_befprognos_data(region_vekt = kommunkoder,
                                      url_prognos_vektor = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                      tid_vekt = till_ar_hamta)
  } else {
    befprogn <- hamta_befprognos_data(region_vekt = kommunkoder,
                                      tid_vekt = till_ar_hamta)
  }

  # Hjälpfunktion: kod för en given ålder i en given tabell - TAB638 och TAB5557 har olika koder för
  # samma ålder (t.ex. är öppna åldersklassen "100+" i TAB638 men "100+1" i TAB5557, och "totalt"-koden
  # heter "tot" respektive "TotSA") - samma mönster/motivering som i SkapaBefPrognosDiagram() i
  # diag_befolkningsprognos_scb_api_profet.R. aldrar = NA ger bara totalkoden, "*" ger alla individuella
  # åldrar, en karaktärsvektor med specifika åldrar ("0".."99"/"100+") ger just dem.
  hamta_alderskoder <- function(table_id, aldrar) {
    totalkod <- if (table_id == "TAB638") "tot" else "TotSA"
    if (all(is.na(aldrar))) return(totalkod)
    v <- suppressMessages(pxweb2r::pxweb2_get_values(table_id, "Alder"))
    v <- v[grepl("^[0-9]+\\+? år$", v$label), ]
    v <- v[!duplicated(v$label), ]
    if (identical(aldrar, "*")) return(v$code)
    label_sokt <- ifelse(aldrar == "100+", "100+ år", paste0(aldrar, " år"))
    v$code[match(label_sokt, v$label)]
  }

  # v2-motsvarigheten till hamta_bef_folkmangd_alder_kon_ar_scb.R. Civilstånd saknar en riktig
  # "totalt"-kod i TAB638 - alla fyra hämtas explicit och summeras ihop (samma mönster som
  # SkapaBefPrognosDiagram). "9999" (senaste tillgängliga år) hanteras här själva, eftersom det bara var
  # en konvention i den gamla v1-funktionen och inte förstås av pxweb2r.
  civilstand_hamta <- c("ogifta", "gifta", "skilda", "änkor/änklingar")
  hamta_bef_folkmangd_v2 <- function(region_vekt, tid, aldrar) {
    if (identical(tid, "9999")) {
      tid <- as.character(max(as.numeric(c(
        suppressMessages(pxweb2r::pxweb2_get_values("TAB638", "Tid"))$code,
        suppressMessages(pxweb2r::pxweb2_get_values("TAB5557", "Tid"))$code
      ))))
    }
    # suppressMessages() tystar bara pxweb2r:s helt ofarliga "include_aggregations = auto: hittade N
    # kodlistor"-info (en riktig R message()) - inte de cat()-baserade "ogiltiga värden borttagna"-
    # notiserna, som vi förväntar oss här (TAB638/TAB5557 täcker olika årsspann) men som inte går att
    # tysta lika enkelt (se anteckning i minnet/commit-historiken om en tänkt warn-parameter i pxweb2r).
    historik <- suppressMessages(pxweb2r::pxweb2_get_data(
      table = "TAB638",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB638", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = tid),
      on_all_values_invalid = "null"))
    ckm <- suppressMessages(pxweb2r::pxweb2_get_data(
      table = "TAB5557",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB5557", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = tid),
      on_all_values_invalid = "null"))

    dplyr::bind_rows(historik, ckm) |>
      dplyr::rename(regionkod = region_kod, Folkmängd = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::mutate(ålder = ifelse(ålder %in% c("totalt, samtliga åldrar", "tot", "TotSA"), "totalt ålder", ålder)) |>
      dplyr::group_by(dplyr::across(-c(civilstånd, Folkmängd))) |>
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")
  }

  befutfall <- hamta_bef_folkmangd_v2(region_vekt = kommunkoder, tid = start_ar, aldrar = "*") |>
    dplyr::mutate(prognos_ar = "utfall")

  bef_diff <- befutfall |>
    dplyr::bind_rows(befprogn) |>
    dplyr::group_by(år, regionkod, region, ålder, prognos_ar) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")

  min_ar <- min(bef_diff$år)
  max_ar <- max(bef_diff$år)
  prognos_ar <- unique(befprogn$prognos_ar)
  # Bugfix (confirmed genom kodgranskning): villkoret var vänt fel väg - byggde bara det automatiska
  # filnamnet när filnamns_prefix VAR angivet (dvs. rakt motsatt vad parameterdokumentationen ovan
  # lovar: "NA = de döps automatiskt") och satte annars filnamn_pre till NA rakt av, vilket gett ett
  # filnamn med bokstavligen "NA" i sig så fort filnamns_prefix lämnades på sitt default-värde.
  if (is.na(filnamns_prefix)) {
    filnamn_pre <- glue::glue("befprogn_karta_{filnamn_kommuner}_ar{min_ar}-{max_ar}")
  } else {
    filnamn_pre <- filnamns_prefix
  }


  bef_diff_tot <- bef_diff |>
    dplyr::filter(ålder != "totalt ålder") |>
    dplyr::group_by(år, regionkod, region) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(aldergrp = "totalt")

  bef_diff_aldergrp <- bef_diff |>
    dplyr::filter(ålder != "totalt ålder") |>
    dplyr::mutate(aldergrp = rdverktyg::skapa_aldersgrupper(ålder, c(0, 20, 66, 80))) |>
    dplyr::group_by(år, regionkod, region, aldergrp) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")

  bef_df <- bef_diff_tot |>
    dplyr::bind_rows(bef_diff_aldergrp) |>
    tidyr::pivot_wider(names_from = "år", values_from = "Folkmängd") |>
    dplyr::mutate(diff = .data[[max_ar]] - .data[[min_ar]],
           diff_proc = (.data[[max_ar]] - .data[[min_ar]]) / .data[[min_ar]] * 100) |>
    dplyr::mutate(beskrivning = glue::glue("Befolkningsförändring år {min_ar}-{max_ar}"))

  kommuner_sf <- rdgis::hamta_karta("kommuner", regionkoder = kommunkoder)

  bef_karta <- kommuner_sf |>
    dplyr::left_join(bef_df, by = c("knkod" = "regionkod")) |>
    dplyr::filter(aldergrp == "totalt")

  if (skriv_karta_png | returnera_ggobj) {
    # ================= karta gglot
    karta_capt_gg <- karta_capt_gg |>
      stringr::str_replace_all("<prognos_ar>", "{prognos_ar}") |>
      stringr::str_replace_all("<prognosskapare>", "{prognosskapare}") |>
      glue::glue()

    befprognos_gg <- ggplot2::ggplot(bef_karta, ggplot2::aes(fill = diff_proc)) +
      ggplot2::geom_sf() +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::scale_fill_gradient2(low = "#F15060", mid = "white", high = "#0e5a4c", midpoint = 0) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(1, 1, 1, 1),
            panel.background = ggplot2::element_rect(fill = "white", colour = 'white'),
            plot.caption = ggplot2::element_text(size = 7, hjust = 0),
            plot.title = ggplot2::element_text(hjust = 0.5),
            plot.subtitle = ggplot2::element_text(hjust = 0.5),
            plot.caption.position = "plot",
            legend.position = "bottom",
            legend.title = ggplot2::element_text(size = 8),
            legend.text = ggplot2::element_text(size = 8),
            legend.key.height = ggplot2::unit(0.5, "cm"),
            legend.key.width  = ggplot2::unit(0.6, "cm")
            ) +
      ggplot2::labs(title = paste0("Prognosticerad befolkningsutveckling i ", kommuner_egetnamn),
           subtitle = paste0("år ", min_ar, "-", max_ar),
           fill = "Förändring (%)",
           caption = karta_capt_gg)

    if (skriv_karta_png) {
      ggplot2::ggsave(paste0(output_mapp, filnamn_pre, ".png"),
             width = 6,
             height = 7)
    }
  } # slut test om man ska skapa en gg-karta

  # ========= leaflet-karta

  if (skriv_karta_html) {
    karta_capt_leaflet <- karta_capt_leaflet |>
      stringr::str_replace_all("<prognos_ar>", "{prognos_ar}") |>
      stringr::str_replace_all("<prognosskapare>", "{prognosskapare}") |>
      glue::glue()

    max_abs <- max(abs(bef_karta$diff_proc), na.rm = TRUE)

    fargskala <- leaflet::colorNumeric(
      palette = c("#F15060", "white", "#0e5a4c"),
      domain = c(-max_abs, max_abs)
    )

    bef_leaflet <- bef_karta |>
      sf::st_transform(crs = 4326)

    befprognos_leaflet <- leaflet::leaflet(bef_leaflet) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addPolygons(
        fillColor = ~fargskala(diff_proc),
        weight = 1,
        color = "black",
        fillOpacity = 0.8,
        label = ~glue::glue("{knnamn}: {round(diff_proc, 1)} % ({diff} invånare)"),
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto")
      ) |>
      leaflet::addLegend(
        pal = fargskala,
        values = bef_karta$diff_proc,
        title = htmltools::HTML(glue::glue("Förändring (%)<br/><span style='font-size:11px; font-weight:normal'>år {min_ar}-{max_ar} enligt<br>befolkningsprognos</span>")),
        position = "bottomright"
      ) |>
      leaflet::addControl(
        html = karta_capt_leaflet,
        position = "bottomleft"
      )

    htmlwidgets::saveWidget(befprognos_leaflet, paste0(output_mapp, filnamn_pre, ".html"), selfcontained = TRUE)
  }

  if (returnera_ggobj) return(befprognos_gg)
}

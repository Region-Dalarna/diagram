diag_antal_utbniva_alder_kon <- function(
    region_vekt = "20",                       # Val av region. Finns: kommunkoder, länskoder eller riket
    gruppera_namn = NA,                       # NA = gör ingenting, annars anges namn på gruppering som medskickade regioner ska grupperas till
    valt_kon = c("män", "kvinnor"),           #  Finns: "män och kvinnor", "män", "kvinnor"
    alder_koder =  c(as.character(20:64)),     # antingen "tot16-74" eller annat intervall, exempelvis c(as.character(25:64)), "*" ger alla år
    tid_koder = "9999",			                  # "*" = alla år eller månader, "9999" = senaste, finns: "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
    vald_utbniva = "förgymnasial utbildning",                       # "*" = alla. "förgymnasial utbildning"   "gymnasial utbildning"      "eftergymnasial utbildning" "utbildningsnivå saknas"
    alder_grupper = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),                       # NA = gör ingenting utan åldrar som de är, annars tex c(20, 25, 30, 35, 40, 45, 50, 55, 60), dvs. start i varje åldersgrupp
    diagram_capt = "Källa: Utbildningsregistret, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    excelfil_mapp = NA,
    excelfil_namn = "utbniva.xlsx",
    returnera_df_rmarkdown = FALSE,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = TRUE,
    diag_fargvekt = NA
) {

  # ======================================================================================================
  #
  # Diagram som visar utbildningsnivå bland inrikes och utrikes födda samt även kön. Tre utbildningsnivåer.
  # Skapat 17 okt 2024 av Peter.
  # Senaste ändring: SCB hade bytt variabelnamn från Befolkning till Antal. Rättat på rad 69 Jon 2025-09-24
  # ======================================================================================================

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

  # om ingen output_mapp är medskickad, använd rdverktyg::utskriftsmapp()
  if (all(is.na(output_mapp))) {
    output_mapp <- rdverktyg::utskriftsmapp()
  }

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("kon")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("kon")
  }

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506__UF0506B/Utbniva3/
  utbniva_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3981",
    query = list(
      Region = region_vekt,
      Kon = valt_kon,
      Alder = alder_koder,
      UtbildningsNiva = "*",
      ContentsCode = "UF0506A1",
      Tid = tid_koder
    )) |>
    dplyr::rename(regionkod = region_kod,
           utbildningsnivå_alla = utbildningsnivå,
           varde = value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(utbildningsnivå = dplyr::case_when(
      stringr::str_detect(utbildningsnivå_alla, "eftergymnasial|forskar") ~ "eftergymnasial utbildning",
      stringr::str_detect(utbildningsnivå_alla, "förgymnasial utbildning") ~ "förgymnasial utbildning",
      stringr::str_detect(utbildningsnivå_alla, "gymnasial utbildning") ~ "gymnasial utbildning",
      stringr::str_detect(utbildningsnivå_alla, "saknas") ~ "utbildningsnivå saknas",
      TRUE ~ utbildningsnivå_alla # behåller ursprungligt värde om ingen matchning
    )) |>
    dplyr::relocate(utbildningsnivå, .before = utbildningsnivå_alla)

  if (!"ålder" %in% names(utbniva_df)) utbniva_df <- dplyr::mutate(utbniva_df, ålder = "16-74 år")

  if (!all(is.na(alder_grupper))) {
    utbniva_df <- utbniva_df |>
      dplyr::mutate(ålder = rdverktyg::skapa_aldersgrupper(ålder, alder_grupper))
  }

  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!all(is.na(gruppera_namn))) {
    utbniva_df <- utbniva_df |>
      dplyr::group_by(dplyr::across(-c(regionkod, region, varde))) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "gg",
             region = gruppera_namn) |>
      dplyr::relocate(region, .before = 1) |>
      dplyr::relocate(regionkod, .before = region)

    region_vekt <- "gg"
  }

  if(returnera_df_rmarkdown == TRUE){
    assign("utbniva_kon_alder_df", utbniva_df, envir = .GlobalEnv)
  }

  if (vald_utbniva != "*") {
    utbniva_df <- dplyr::filter(utbniva_df, utbildningsnivå %in% vald_utbniva)
  }

  skapa_diagram <- function(chart_df, skickad_regionkod, valt_ar) {

    chart_df <- chart_df |>
      dplyr::filter(regionkod %in% skickad_regionkod,
             år %in% valt_ar)

    # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
    region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(chart_df$region)))
    region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_start)
    region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_txt)
    regionfil_txt <- region_txt
    region_txt <- paste0(" i ", region_txt)
    regionkod_txt <- if (region_start == region_txt) paste0(unique(chart_df$regionkod), collapse = "_") else region_txt

    ar_txt <- if (min(chart_df$år) == max(chart_df$år)) max(chart_df$år) else paste0(min(chart_df$år), "-", max(chart_df$år))
    utbniva_txt <- paste0(unique(chart_df$utbildningsnivå), collapse = "_")
    utbniva_titel <- if (length(unique(chart_df$utbildningsnivå)) == 1) paste0(" med ", unique(chart_df$utbildningsnivå)) else ""

    diagramtitel <- glue::glue("Invånare{region_txt}{utbniva_titel} år {ar_txt}")
    diagramfil <- stringr::str_replace_all(glue::glue("utbniva_{utbniva_txt}_kon_{regionfil_txt}_ar{ar_txt}.png"), " ", "_")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_df,
    			 skickad_x_var = "ålder",
    			 skickad_y_var = "varde",
    			 skickad_x_grupp = "kön",
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 diagram_liggande = TRUE,
    			 geom_position_stack = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_y_axis_title = "Antal personer",
    			 x_axis_lutning = 0,
    			 manual_color = diag_fargvekt,
    			 output_mapp = output_mapp,
    			 skriv_till_diagramfil = skriv_diagramfil,
    			 facet_legend_bottom = TRUE,
    			 facet_grp = if (length(unique(chart_df$utbildningsnivå)) == 1) NULL else "utbildningsnivå",
    			 facet_scale = "fixed",
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

    return(gg_list)

  } # slut skapa_diagram

    arglist <- list(reg = region_vekt, valt_ar = unique(utbniva_df$år))                               # skapa lista med de två variabler vi vill göra diagram med
    crossarg <- expand.grid(arglist)
    retur_list <- purrr::flatten(purrr::pmap(crossarg, ~skapa_diagram(chart_df = utbniva_df, skickad_regionkod = ..1, valt_ar = ..2)))

  return(retur_list)
} # slut funktion

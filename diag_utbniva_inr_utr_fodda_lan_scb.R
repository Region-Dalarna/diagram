
diag_utbniva_inr_utr_fodda_kon_lan <- function(
    region_vekt = "20",                       # Val av region. Finns: "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
    gruppera_namn = NA,                       # NA = gör ingenting, annars anges namn på gruppering som medskickade regioner ska grupperas till
    valt_kon = c("män", "kvinnor"),           #  Finns: "män och kvinnor", "män", "kvinnor"
    tid_koder = "9999",			                  # "*" = alla år eller månader, "9999" = senaste, finns: "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
    bakgrund_klartext = c("andel med förgymnasial utbildning, procent", "andel med gymnasial utbildning, procent", "andel med eftergymnasial utbildning, procent"),			 #  Finns: "andel 0-19 år, procent", "andel 20-64 år, procent", "andel 65+ år, procent", "samtliga utbildningsnivåer, procent", "andel med förgymnasial utbildning, procent", "andel med gymnasial utbildning, procent", "andel med eftergymnasial utbildning, procent", "andel där uppgift saknas för utbildningsnivå, procent", "samtliga, procent"
    cont_klartext = c("Födda i Sverige", "Utrikes födda"),			 #  Finns: "Födda i Sverige", "Utländsk bakgrund", "Utrikes födda", "Födda i Norden exkl. Sverige", "Födda i EU/EFTA exkl. Norden", "Födda i övriga världen"
    diagram_capt = "Källa: Tema registerbaserad integration, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    returnera_df_rmarkdown = FALSE,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = TRUE,
    diag_fargvekt = NA
) {

  # ======================================================================================================
  #
  # Diagram som visar utbildningsnivå bland inrikes och utrikes födda samt även kön. Tre utbildningsnivåer.
  # Skapat 17 okt 2024 av Peter.
  #
  # Ändrat variabel till bakgrundsvariabel på rad 66. / Jon 2026-04-14
  #
  # Migrerad till pxweb2r/rddiagram/rdverktyg. hamta_data-repots hamta_integration_region_bakgrund_tid_kon_scb()
  # slog ihop två v1-tabeller (AA0003E/IntGr3LanKONS för län, AA0003E/IntGr3RikKONS för riket). Båda finns
  # kvar i SCB:s v1-API men motsvaras nu av två v2-tabeller: IntGr3LanKONS -> TAB4648, IntGr3RikKONS ->
  # TAB4655. Används bara av det här skriptet - logiken läggs därför in direkt här i stället för i
  # rdverktyg.
  # ======================================================================================================

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

  # om ingen output_mapp är medskickad, använd rdverktyg::utskriftsmapp() om den finns, annars sätt skriv_diagramfil till FALSE
  if (all(is.na(output_mapp))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_mapp <- rdverktyg::utskriftsmapp()
    } else {
      skriv_diagramfil <- FALSE
    }
  }

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(diag_fargvekt))) diag_fargvekt <- rddiagram::diagramfarger("rus_sex")

  # =============================================== API-uttag ===============================================

  hamta_en_integrationstabell <- function(tabell_id, region_vekt, kon_klartext, bakgrund_klartext, cont_klartext, tid_koder) {

    giltiga_regioner <- pxweb2r::pxweb2_get_values(tabell_id, "Region", quiet = TRUE)$code
    region_giltig <- region_vekt[region_vekt %in% giltiga_regioner]
    if (length(region_giltig) == 0) return(NULL)

    giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code
    tid_vekt <- if (identical(tid_koder, "9999")) max(giltiga_ar) else if (identical(tid_koder, "*")) giltiga_ar else as.character(tid_koder)[as.character(tid_koder) %in% giltiga_ar]
    if (length(tid_vekt) == 0) return(NULL)

    px <- pxweb2r::pxweb2_get_data(
      table = tabell_id,
      query = list(
        Region = region_giltig,
        Bakgrund = bakgrund_klartext,
        ContentsCode = cont_klartext,
        Tid = tid_vekt,
        Kon = kon_klartext
      ),
      on_all_values_invalid = "null", quiet = TRUE)
    if (is.null(px)) return(NULL)

    # "tabellinnehåll" (ContentsCode, dvs. Födda i Sverige/Utrikes födda) döps om till "bakgrund" - samma
    # kolumnnamn som originalets frusna konvertera_till_long_for_contentscode_variabler()-hjälpfunktion gav.
    dplyr::rename(px, regionkod = region_kod, bakgrund = tabellinnehåll, varde = value)
  }

  integration_df <- purrr::map(
    c("TAB4648", "TAB4655"),  # län, riket
    ~ hamta_en_integrationstabell(.x, region_vekt, valt_kon, bakgrund_klartext, cont_klartext, tid_koder)
  ) |>
    purrr::list_rbind() |>
    dplyr::rename(utbildning = bakgrundsvariabel) |>
    dplyr::mutate(utbildning = utbildning |> stringr::str_remove("andel med ") |> stringr::str_remove(", procent"),
           utbildning = factor(utbildning, levels = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning")))

  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!is.na(gruppera_namn)) {
    integration_df <- integration_df |>
      dplyr::group_by(dplyr::across(-c(regionkod, region, varde))) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "gg",
             region = gruppera_namn) |>
      dplyr::relocate(region, .before = 1) |>
      dplyr::relocate(regionkod, .before = region)

    region_vekt <- "gg"
  }

  if(returnera_df_rmarkdown == TRUE){
    assign("utbniva_bakgr_kon_df", integration_df, envir = .GlobalEnv)
  }

  skapa_diagram <- function(chart_df, skickad_regionkod) {

    chart_df <- dplyr::filter(chart_df, regionkod %in% skickad_regionkod)

    # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
    region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(chart_df$region)))
    region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_start)
    region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_txt)
    regionfil_txt <- region_txt
    region_txt <- paste0(" i ", region_txt)
    regionkod_txt <- if (region_start == region_txt) paste0(unique(chart_df$regionkod), collapse = "_") else region_txt

    ar_txt <- if (min(chart_df$år) == max(chart_df$år)) max(chart_df$år) else paste0(min(chart_df$år), "-", max(chart_df$år))

    diagramtitel <- glue::glue("Utbildningsnivå invånare 20-64 år{region_txt} år {ar_txt}")
    diagramfil <- glue::glue("utbniva_inr_utr_kon_{regionfil_txt}_ar{ar_txt}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = chart_df,
    			 skickad_x_var = "bakgrund",
    			 skickad_y_var = "varde",
    			 skickad_x_grupp = "utbildning",
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_y_axis_title = "procent",
    			 x_axis_lutning = 0,
    			 manual_color = diag_fargvekt,
    			 output_mapp = output_mapp,
    			 skriv_till_diagramfil = skriv_diagramfil,
    			 facet_grp = "kön",
    			 facet_scale = "fixed",
    			 facet_legend_bottom = TRUE
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

    return(gg_list)

  } # slut skapa_diagram

  if (length(region_vekt) > 1) {
    retur_list <- purrr::flatten(purrr::map(unique(region_vekt), ~ skapa_diagram(chart_df = integration_df,
                                                           skickad_regionkod = .x)))
  } else {
    retur_list <- skapa_diagram(chart_df = integration_df,
                                skickad_regionkod = region_vekt)
  }
  return(retur_list)
} # slut funktion


diag_fek_lve_tidsserie <- function(
    region_vekt = "20",
    diagram_capt = "Källa: Företagens ekonomi (FEK), SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förädlingsvärde är den faktiska produktionen minus kostnader för köpta varor och tjänster, dock ej löner, sociala avgifter och kostnader för handelsvaror",
    output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
    visa_dataetiketter = FALSE,
    cont_klartext = "Förädlingsvärde, mnkr",
    diag_tidsserie = TRUE,
    diag_jmfr_riket = TRUE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <-
c("https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_ar2007_2022.png",
"https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_jmfr_riket_ar2007-2022.png")
  purrr::walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  rdverktyg::stop_tyst()
}

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
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()

  # Originalet sourcade en lokal, aldrig publicerad wip-fil
  # (g:/skript/peter/temp/hamta_fek_lve_region_sni2007_tid_
  # NSEBasfaktaLVEngs07_RegionalBasf07_scb.R) som - att döma av
  # filnamnet och den efterföljande dubbla nyckel-joinen nedan (både
  # "Kod" och "Avdelning") - slog ihop två SCB-produkter: den äldre
  # "Regional Basfakta" (finare SNI2007-indelning, år 2007-2021) och den
  # nyare NSEBasfaktaLVEngs07 (grövre bokstavsindelning, år 2022-) som
  # redan används i diag_fek_foradlingsvarde_bransch_lan_scb.R. Hämtas
  # här direkt via v2-tabellerna TAB3513 (2007-2021) + TAB6329 (2022-).
  fek_lve_df <- rdverktyg::suppress_specific_warning({
    historik <- pxweb2r::pxweb2_get_data(
      table = "TAB3513",
      query = list(
        Region = c("00", region_vekt),
        SNI2007 = "*",
        ContentsCode = cont_klartext,
        Tid = "*"
      )) |>
      dplyr::rename(regionkod = region_kod, sni2007kod = `näringsgren sni 2007_kod`, variabel = tabellinnehåll, varde = value)

    ny <- pxweb2r::pxweb2_get_data(
      table = "TAB6329",
      query = list(
        Region = c("00", region_vekt),
        SNI2007 = "*",
        ContentsCode = cont_klartext,
        Tid = "*"
      )) |>
      dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)

    dplyr::bind_rows(historik, ny)
  })

  # !is.na(Kod)/!is.na(Avdelning) filtreras bort explicit innan join:
  # dplyr::left_join() matchar annars NA mot NA, vilket ger en
  # many-to-many-relation (och därmed uppblåsta, felaktiga summor) så
  # fort nyckelfilen har fler än en rad utan Kod (typiskt raderna som
  # bara har Avdelning ifyllt, och vice versa).
  branschnyckel <- readxl::read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") |>
    dplyr::select(Kod, Grupp_kod, Branschgrupp) |>
    dplyr::filter(!is.na(Kod)) |>
    dplyr::distinct()

  bransch_bokstav <- readxl::read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") |>
    dplyr::select(Avdelning, Grupp_kod, Branschgrupp) |>
    dplyr::filter(!is.na(Avdelning)) |>
    dplyr::distinct()

  vald_ar <- c(min(fek_lve_df$år), max(fek_lve_df$år))

  if (diag_tidsserie) {
    tidsserie_df <- fek_lve_df |>
      # OBS: originalet filtrerade bort totalraden med
      # "sni2007kod != 'Total_A-SexklK-O'" - sni2007kod finns bara för
      # den äldre tabellen (TAB3513 exponerarråa SNI-koder, TAB6329 gör
      # det inte), så den jämförelsen blev NA (och filtrerade därmed
      # bort ALLA rader) för den nyare tabellens data. Filtrerar i
      # stället bort totalraden via klartexten, som finns för båda
      # tabellerna (samma lösning som i
      # diag_fek_foradlingsvarde_bransch_lan_scb.R).
      dplyr::filter(!stringr::str_detect(`näringsgren SNI 2007`, "samtliga näringsgrenar"),
             regionkod %in% region_vekt,
             år %in% vald_ar) |>
      dplyr::mutate(bransch_bokstav = stringr::str_sub(`näringsgren SNI 2007`, 1, 1)) |>
      dplyr::left_join(branschnyckel, by = c("sni2007kod" = "Kod")) |>
      dplyr::left_join(dplyr::rename(bransch_bokstav, gk = Grupp_kod, bg = Branschgrupp), by = c("bransch_bokstav" = "Avdelning")) |>
      dplyr::mutate(Grupp_kod = ifelse(is.na(Grupp_kod), gk, Grupp_kod),
             Branschgrupp = ifelse(is.na(Branschgrupp), bg, Branschgrupp)) |>
      dplyr::group_by(år, regionkod, region, Branschgrupp, variabel) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")

    region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(tidsserie_df$region)))
    cont_txt <- stringr::str_extract(cont_klartext, "^[^,]*")

    diagramtitel <- glue::glue("{cont_txt} i {region_txt}")
    diagramfil <- glue::glue("fek_{cont_txt}_{region_txt}_ar{min(fek_lve_df$år)}_{max(fek_lve_df$år)}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = tidsserie_df,
    			 skickad_x_var = "Branschgrupp",
    			 skickad_y_var = "varde",
    			 skickad_x_grupp = "år",
    			 x_axis_sort_value = TRUE,
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_y_axis_title = cont_klartext,
    			 manual_x_axis_text_vjust = 1,
    			 manual_x_axis_text_hjust = 1,
    			 manual_color = rddiagram::diagramfarger("rus_sex"),
    			 output_mapp = output_mapp
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")
  } # slut if-sats om diag_tidsserie

  if (diag_jmfr_riket) {
    jmfr_riket_df <- fek_lve_df |>
      dplyr::filter(!stringr::str_detect(`näringsgren SNI 2007`, "samtliga näringsgrenar")) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region)) |>
      dplyr::group_by(år, regionkod, region, variabel) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")

    region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(dplyr::pull(dplyr::filter(jmfr_riket_df, regionkod != "00"), region))))
    cont_txt <- stringr::str_extract(cont_klartext, "^[^,]*")

    diagramtitel <- glue::glue("Förändring av {tolower(cont_txt)} i {region_txt} jämfört med riket")
    diagramfil <- glue::glue("fek_{cont_txt}_{region_txt}_jmfr_riket_ar{min(fek_lve_df$år)}-{max(fek_lve_df$år)}.png")

    gg_obj <- rddiagram::SkapaLinjeDiagram(
      skickad_df = dplyr::rename(jmfr_riket_df, !!rlang::sym(cont_klartext) := varde),
                                 skickad_x_var = "år",
                                 skickad_y_var = cont_klartext,
                                 skickad_x_grupp = "region",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 berakna_index = TRUE,
                                 #manual_y_axis_title = cont_klartext,
                                 manual_color = rddiagram::diagramfarger("rus_sex"),
                                 output_mapp = output_mapp
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")
  } # slut if-sats om diag_jmfr_riket

  return(gg_list)

} # slut diag-funktion

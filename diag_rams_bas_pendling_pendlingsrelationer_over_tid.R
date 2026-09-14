
diag_arbetspendling_over_tid <- function(
    region_vekt = "20",
    skriv_ut_dataetiketter_diagram = FALSE,
    skriv_till_diagramfil = TRUE,
    skriv_till_excelfil = FALSE,
    output_mapp = NA,
    fargvekt_tre = NA,
    fargvekt_atta = NA,
    lagg_till_logga = TRUE,
    logga_sokvag = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, bearbetning av: Samhällsanalys, Region Dalarna\nPendlingsstatistiken har tidsseriebrott år 2003-2004, 2018-2019 och 2019-2020, så jämförelser mellan dessa perioder bör göras med viss försiktighet.",
    diag_in_ut_sammma = TRUE,
    diag_in_ut = TRUE,
    diag_nettopendling = TRUE,
    diag_storsta_inpendlrelationer = TRUE,
    diag_storsta_utpendlrelationer = TRUE
    #diag_pendling_over_grans = TRUE
  ) {

  # =======================================================================================================
  #
  # Fem diagram över pendlingen till och från kommuner.
  # 1. In- och utpendling samt de som bor och jobbar i samma kommun, per år och kommun från 1993 och framåt.
  # 2. Samma som ovan men utan de som bor och jobbar i samma kommun.
  # 3. Nettopendlingen per år och kommun från 1993 och framåt.
  # 4. Största inpendlingskommuner för den kommun man valt, per år och kommun från 1993 och framåt.
  # 5. Största utpendlingskommuner för den kommun man valt, per år och kommun från 1993 och framåt.
  #
  # Migrerad till pxweb2r/rddiagram/rdverktyg. hamta_data-repots
  # hamta_pendling_rams_bas_scb() slog ihop fyra v1-tabeller. Samtliga fyra
  # finns kvar i SCB:s v1-API men motsvaras nu av fyra v2-tabeller:
  #   AM0207/AM0207Z/AM0207PendlKomA04N (RAMS, ny tidsserie 2019-2021) -> TAB5850
  #   AM0207/AM0207L/AM0207PendlKomA04  (RAMS 2004-2018)               -> TAB333
  #   AM0207/AM0207L/AM0207PendlKomA9303 (RAMS 1993-2003)              -> TAB334
  #   AM0210/AM0210F/ArRegPend2 (BAS 2020-)                            -> TAB1830
  # hamta_pendling_rams_bas_scb() är bara använd av det här skriptet och av
  # karta_pendling_leaflet.R (en karta, inte en del av den här migreringen) -
  # logiken läggs därför in direkt här i stället för i rdverktyg.
  # =======================================================================================================

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget p_load(tidyverse)/library().
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  # dplyr/purrr/stringr/tidyr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen output-mapp anges, använd rdverktyg::utskriftsmapp()
  if (all(is.na(output_mapp))) output_mapp <- rdverktyg::utskriftsmapp()

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("bla_gra_tre")
  if (all(is.na(fargvekt_tre))) fargvekt_tre <- rddiagram::diagramfarger("bla_gra_tre")

  # detsamma görs för fargvektor med åtta färger
  if (all(is.na(fargvekt_atta))) fargvekt_atta <- RColorBrewer::brewer.pal(9, "Set3")[c(1,3:7)]

  gg_list <- list()               # skapa lista för att lägga diagrammen i
  flikar_list <- list()

  dataetiketter_txt <- ifelse(skriv_ut_dataetiketter_diagram, "_dataetiketter", "")

  # =============================================== API-uttag ===============================================

  # De fyra v2-tabellerna, i tidsordning. TAB5850 (RAMS, ny tidsserie) och TAB1830 (BAS) delar åren 2020-2021 -
  # dessa år tas bort ur TAB5850 nedan för att inte räknas dubbelt (samma hantering som i originalskriptet).
  hamta_en_pendlingstabell <- function(tabell_id, hamta_region_vekt, kommun_vekt, lan_vekt,
                                       kon_klartext_vekt, tid_vekt, regionnyckel) {

    giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code
    akt_tid_vekt <- if (all(tid_vekt == "*")) giltiga_ar else tid_vekt[tid_vekt %in% giltiga_ar]
    if (tabell_id == "TAB5850") akt_tid_vekt <- akt_tid_vekt[!akt_tid_vekt %in% c("2020", "2021")]
    if (length(akt_tid_vekt) == 0) return(NULL)

    # "totalt" (BAS) och "män och kvinnor" (RAMS) är samma sak fast med olika klartextlabel per tabell -
    # be om båda, pxweb2r plockar bort den etikett som inte finns i just den här tabellen.
    kon_hamta <- if (any(kon_klartext_vekt %in% c("totalt", "män och kvinnor"))) {
      unique(c(kon_klartext_vekt, "totalt", "män och kvinnor"))
    } else {
      kon_klartext_vekt
    }

    hamta_riktning <- function(bostad_vekt, arbete_vekt) {
      suppressMessages(pxweb2r::pxweb2_get_data(
        table = tabell_id,
        query = list(
          Bostadskommun = bostad_vekt,
          Arbetsstallekommun = arbete_vekt,
          Kon = kon_hamta,
          ContentsCode = "*",
          Tid = akt_tid_vekt
        ),
        on_all_values_invalid = "null",
      quiet = TRUE))
    }

    stada_riktning <- function(px) {
      if (is.null(px)) return(NULL)
      px |>
        dplyr::rename(regionkod_bo = bostadskommun_kod, bostadsregion = bostadskommun,
                       regionkod_arb = arbetsställekommun_kod, arbetsställeregion = arbetsställekommun,
                       pendlare = value) |>
        dplyr::mutate(bostadsregion = stringr::str_remove(bostadsregion, " \\(bostad\\)"),
                       arbetsställeregion = stringr::str_remove(arbetsställeregion, " \\(arbetsställe\\)")) |>
        dplyr::select(-tabellinnehåll)
    }

    px_in <- stada_riktning(hamta_riktning("*", hamta_region_vekt))
    px_ut <- stada_riktning(hamta_riktning(hamta_region_vekt, "*"))

    px_kommun_in <- if (length(kommun_vekt) > 0 && !is.null(px_in)) {
      dplyr::filter(px_in, regionkod_bo %in% kommun_vekt | regionkod_arb %in% kommun_vekt)
    } else NULL
    px_kommun_ut <- if (length(kommun_vekt) > 0 && !is.null(px_ut)) {
      dplyr::filter(px_ut, regionkod_bo %in% kommun_vekt | regionkod_arb %in% kommun_vekt)
    } else NULL

    aggregera_lan <- function(px) {
      if (is.null(px) || length(lan_vekt) == 0) return(NULL)
      px |>
        dplyr::mutate(bolan_kod = stringr::str_sub(regionkod_bo, 1, 2),
                       arblan_kod = stringr::str_sub(regionkod_arb, 1, 2)) |>
        dplyr::filter(bolan_kod %in% lan_vekt | arblan_kod %in% lan_vekt) |>
        dplyr::group_by(år, kön, regionkod_bo = bolan_kod, regionkod_arb = arblan_kod) |>
        dplyr::summarise(pendlare = sum(pendlare, na.rm = TRUE), .groups = "drop") |>
        dplyr::left_join(dplyr::rename(regionnyckel, regionkod_bo = regionkod, bostadsregion = region), by = "regionkod_bo") |>
        dplyr::left_join(dplyr::rename(regionnyckel, regionkod_arb = regionkod, arbetsställeregion = region), by = "regionkod_arb")
    }

    dplyr::bind_rows(px_kommun_in, px_kommun_ut, aggregera_lan(px_in), aggregera_lan(px_ut))
  }

  hamta_pendling_rams_bas <- function(region_vekt, kon_klartext_vekt = "män och kvinnor", tid_vekt = "*") {
    regionnyckel <- rdverktyg::hamtaregtab()
    kommun_vekt <- region_vekt[nchar(region_vekt) == 4]
    lan_vekt <- region_vekt[nchar(region_vekt) == 2]
    lan_kommuner_vekt <- rdverktyg::hamtakommuner(lan_vekt, tamedlan = FALSE, tamedriket = FALSE)
    hamta_region_vekt <- c(kommun_vekt, lan_kommuner_vekt)

    tabell_id_vekt <- c("TAB5850", "TAB333", "TAB334", "TAB1830")

    px_df <- purrr::map(tabell_id_vekt, ~ hamta_en_pendlingstabell(
      .x, hamta_region_vekt, kommun_vekt, lan_vekt, kon_klartext_vekt, tid_vekt, regionnyckel
    )) |>
      purrr::list_rbind() |>
      dplyr::filter(pendlare > 0) |>
      dplyr::distinct(.keep_all = TRUE) |>
      dplyr::relocate(pendlare, .after = dplyr::last_col())

    if ("kön" %in% names(px_df)) px_df <- dplyr::mutate(px_df, kön = ifelse(kön == "totalt", "män och kvinnor", kön))

    px_df
  }

  px_df <- rdverktyg::funktion_upprepa_forsok_om_fel(function() hamta_pendling_rams_bas(region_vekt = region_vekt))

  skapa_diagram <- function(regionkod) {
    # förbered regionkoder för att också kunna aggregera till län (tabellerna innehåller bara kommuner)
    regiontyp <- ifelse(nchar(regionkod) == 2, "län", "kommun")
    regiontyp_titel <- ifelse(nchar(regionkod) == 2, "län", "kommuner")
    region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(regionkod)$region))

    # förbered df för att skapa diagram
    pendling_df <- px_df |>
      dplyr::filter(regionkod_bo %in% regionkod | regionkod_arb %in% regionkod) |>
      dplyr::mutate(pendlingstyp = dplyr::case_when(
        regionkod_bo == regionkod_arb ~ paste0("bor och arbetar i samma ", regiontyp),
        regionkod_bo %in% regionkod ~ "utpendlare",
        regionkod_arb %in% regionkod ~ "inpendlare")) |>
      dplyr::rename(antal_pendlare = pendlare) |>
      dplyr::relocate(år, pendlingstyp, .before = 1)

    if (skriv_till_excelfil) {
      flikar_list <- c(flikar_list, list(pendling_df))
      names(flikar_list)[length(flikar_list)] <- "pendling_in_ut"
    }

    # =================== diagram för in- och utpendlng över kommungräns samt även de som bor och arbetar i samma kommun =========================

    if (diag_in_ut_sammma){

      diagram_titel <- paste0("In- och utpendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("pendling_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = pendling_df,
                         skickad_x_var = "år",
                         skickad_x_grupp = "pendlingstyp",
                         skickad_y_var = "antal_pendlare",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         manual_y_axis_title = "",
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         manual_color = fargvekt_tre,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         lagg_pa_logga = lagg_till_logga,
                         logga_path = logga_sokvag,
                         logga_scaling = 22,
                         output_mapp = output_mapp,
                         skriv_till_diagramfil = skriv_till_diagramfil,
                         filnamn_diagram = diagramfil)

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_in_ut_sammma"

    } # slut if-sats för diag_in_ut_sammma


    if (diag_in_ut){
      diagram_titel <- paste0("In- och utpendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("pendling_bara_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
                         skickad_df = dplyr::filter(pendling_df, pendlingstyp != paste0("bor och arbetar i samma ", regiontyp)),
                         skickad_x_var = "år",
                         skickad_x_grupp = "pendlingstyp",
                         skickad_y_var = "antal_pendlare",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         manual_y_axis_title = "",
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         lagg_pa_logga = lagg_till_logga,
                         logga_path = logga_sokvag,
                         manual_color = fargvekt_tre[c(2,3)],
                         logga_scaling = 22,
                         skriv_till_diagramfil = skriv_till_diagramfil,
                         output_mapp = output_mapp,
                         filnamn_diagram = diagramfil)

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_in_ut"

    } # slut if-sats för diag_in_ut


    if (diag_nettopendling){

      nettopendling_df <- pendling_df |>
        dplyr::mutate(regionkod = dplyr::if_else(pendlingstyp == "inpendlare", regionkod_arb, regionkod_bo),
               region = dplyr::if_else(pendlingstyp == "inpendlare", arbetsställeregion, bostadsregion)) |>
        dplyr::group_by(år, regionkod, region, kön, pendlingstyp) |>
        dplyr::summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(names_from = pendlingstyp,
                    values_from = antal_pendlare) |>
        dplyr::mutate(nettopendling = inpendlare - utpendlare) |>
        dplyr::rename(ar = år)

      # skriv till excelfil om man valt det
      if (skriv_till_excelfil) {
        flikar_list <- c(flikar_list, list(nettopendling_df))
        names(flikar_list)[length(flikar_list)] <- "nettopendling"
      }

      diagram_titel <- paste0("Nettopendling i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
      diagramfil <- paste0("nettopendling_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = nettopendling_df,
                         skickad_x_var = "ar",
                         skickad_y_var = "nettopendling",
                         diagram_titel = diagram_titel,
                         diagram_capt = diagram_capt,
                         dataetiketter = skriv_ut_dataetiketter_diagram,
                         manual_x_axis_text_vjust = 1,
                         manual_x_axis_text_hjust = 1,
                         stodlinjer_avrunda_fem = TRUE,
                         manual_color = fargvekt_tre[1],
                         lagg_pa_logga = lagg_till_logga,
                         logga_path = logga_sokvag,
                         logga_scaling = 22,
                         skriv_till_diagramfil = skriv_till_diagramfil,
                         output_mapp = output_mapp,
                         filnamn_diagram = diagramfil)

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[length(gg_list)] <- "diag_nettopendling"

    } # slut if-sats för diag_nettopendling

    # =========================================== diagram med pendlingsrelationsregioner =====================================

    if (diag_storsta_inpendlrelationer | diag_storsta_utpendlrelationer) {


      bara_pendlare_df <- dplyr::filter(pendling_df, pendlingstyp != paste0("bor och arbetar i samma ", regiontyp))

      # skriv till excelfil om man valt det
      if (skriv_till_excelfil) {
        flikar_list <- c(flikar_list, list(bara_pendlare_df))
        names(flikar_list)[length(flikar_list)] <- "pendling_relationer"
      }

      # ta ut de sex största inpendlingskommunerna och lägg i en vektor
      storsta_inpendl_regioner <- bara_pendlare_df |>
        dplyr::filter(pendlingstyp == "inpendlare") |>
        dplyr::group_by(regionkod_bo) |>
        dplyr::summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(antal_pendlare)) |>
        dplyr::slice(1:6) |>
        dplyr::pull(regionkod_bo)


      # ta ut de sex största utpendlingskommunerna och lägg i en vektor
      storsta_utpendl_regioner <- bara_pendlare_df |>
        dplyr::filter(pendlingstyp == "utpendlare") |>
        dplyr::group_by(regionkod_arb) |>
        dplyr::summarise(antal_pendlare = sum(antal_pendlare, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(antal_pendlare)) |>
        dplyr::slice(1:6) |>
        dplyr::pull(regionkod_arb)

      # ================= diagram - inpendling till de sex största inpendlingsrelationsregionerna över hela perioden =================

      if (diag_storsta_inpendlrelationer) {
        diagram_titel <- paste0("Största inpendlings", regiontyp_titel, " till ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
        diagramfil <- paste0("inpendlingsrelationer_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

        gg_obj <- rddiagram::SkapaStapelDiagram(
                             skickad_df = dplyr::filter(bara_pendlare_df, pendlingstyp == "inpendlare",
                                    regionkod_bo %in% storsta_inpendl_regioner),
                           skickad_x_var = "år",
                           skickad_x_grupp = "bostadsregion",
                           skickad_y_var = "antal_pendlare",
                           diagram_titel = diagram_titel,
                           diagram_capt = diagram_capt,
                           manual_y_axis_title = "antal pendlare",
                           manual_x_axis_text_vjust = 1,
                           manual_x_axis_text_hjust = 1,
                           stodlinjer_avrunda_fem = TRUE,
                           manual_color = fargvekt_atta,
                           lagg_pa_logga = lagg_till_logga,
                           logga_scaling = 22,
                           logga_path = logga_sokvag,
                           dataetiketter = skriv_ut_dataetiketter_diagram,
                           skriv_till_diagramfil = skriv_till_diagramfil,
                           output_mapp = output_mapp,
                           filnamn_diagram = diagramfil)

        gg_list <- c(gg_list, list(gg_obj))
        names(gg_list)[length(gg_list)] <- "diag_storsta_inpendlrelationer"

      } # slut if-sats om diagram med största inpendlingskommuner är vald

      # ================= diagram - utpendling till de sex största utpendlingsrelationsregionerna över hela perioden =================

      if (diag_storsta_utpendlrelationer) {
        diagram_titel <- paste0("Största utpendlings", regiontyp_titel, " i ", region_txt, " ", min(pendling_df$år), "-", max(pendling_df$år))
        diagramfil <- paste0("utpendlingsrelationer_", region_txt, "_", min(pendling_df$år), "-", max(pendling_df$år), dataetiketter_txt, ".png")

        gg_obj <- rddiagram::SkapaStapelDiagram(
                             skickad_df = dplyr::filter(bara_pendlare_df, pendlingstyp == "utpendlare",
                                    regionkod_arb %in% storsta_utpendl_regioner),
                           skickad_x_var = "år",
                           skickad_x_grupp = "arbetsställeregion",
                           skickad_y_var = "antal_pendlare",
                           diagram_titel = diagram_titel,
                           diagram_capt = diagram_capt,
                           manual_y_axis_title = "antal pendlare",
                           manual_x_axis_text_vjust = 1,
                           manual_x_axis_text_hjust = 1,
                           stodlinjer_avrunda_fem = TRUE,
                           manual_color = fargvekt_atta,
                           logga_scaling = 22,
                           logga_path = logga_sokvag,
                           lagg_pa_logga = lagg_till_logga,
                           dataetiketter = skriv_ut_dataetiketter_diagram,
                           skriv_till_diagramfil = skriv_till_diagramfil,
                           output_mapp = output_mapp,
                           filnamn_diagram = diagramfil)

        gg_list <- c(gg_list, list(gg_obj))
        names(gg_list)[length(gg_list)] <- "diag_storsta_utpendlrelationer"

      } # slut if-sats om diagram med största utpendlingskommuner är vald

    } # if-sats om någon av största in- eller utpendlingsrelationskommuner är valda

    if (skriv_till_excelfil) {
      start_ar <- min(pendling_df$år)
      slut_ar <- max(pendling_df$år)
      pendling_excelflikar <- purrr::compact(flikar_list)
      writexl::write_xlsx(pendling_excelflikar, paste0(output_mapp, "arbetspendling_", start_ar, "-", slut_ar, "_", regionkod, ".xlsx"))
    }

    return(gg_list)              # här returnerar vi listan med ggplot-diagram
    } # slut funktion för att skapa själva diagrammen

    retur_list <- purrr::flatten(purrr::map(region_vekt, ~ skapa_diagram(.x)))
    return(retur_list)
} # slut funktion

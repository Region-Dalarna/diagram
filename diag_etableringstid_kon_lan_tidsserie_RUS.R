diag_etablering_diverse_scb <- function(region = "20", # Enbart ett i taget.
                                        diag_alla_lan = TRUE, # Skapar ett diagram där länen jämförs för för vald vistelsetid
                                        vald_vistelsetid = "10- år", # Vistelsetid som ska visas i länsdiagrammet. Finns även: "0-1 år", "2-3 år", "4-9 år"
                                        diag_tidsserie = TRUE, # Skapar ett diagram
                                        diag_facet = TRUE,
                                        visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                        logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                        startar = 2012, # Startår för tidsserien. Bör inte vara tidigare än 12 år före slutåret då färgerna inte räcker till.
                                        diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                                        output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                        skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                        excel_mapp = NA,                                   # mapp där excelfil ska sparas, NA = sparas ingen fil
                                        returnera_data_rmarkdown = FALSE,
                                        demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Tre diagram kopplade till invandringsetablering som används i Rus-uppföljningen
  #
  #
  #
  # =======================================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
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
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # SCB:s PxWeb-etiketter för vistelsetidsintervall använder gemenhetstecken
  # (en-dash "–") i stället för bindestreck i v2-tabellerna nedan - samma
  # mönster som redan hittat/fixat i systerskriptet
  # diag_etableringstid_kon_lan_tidsserie_KvMa_IntRap.R.
  till_endash <- function(x) gsub("(?<=[0-9])-(?=[0-9 ])", "\u2013", x, perl = TRUE)

  if(diag_alla_lan) {
    region_fokus <- region
    region <- rdverktyg::hamtaAllaLan(tamedriket = FALSE)
  } else {
    region_fokus <- region
  }

  # hamta_data-repots hamta_etableringstid_mm_region_kon_utbniv_bakgrvar_
  # tid_IntGr1LanKonUtb_scb.R (v1: AA/AA0003/AA0003X/IntGr1LanKonUtb +
  # IntGr1KomKonUtb) och _IntGr1KomKonUtb_ny_BAS_scb.R (2022 och senare)
  # hämtas här direkt via samma v2-tabeller som redan verifierats i
  # systerskriptet diag_etableringstid_kon_lan_tidsserie_KvMa_IntRap.R:
  # TAB389 (län, till och med 2021), TAB4881 (kommun, till och med 2021),
  # TAB6384 (län, fr.o.m. 2022), TAB6383 (kommun, fr.o.m. 2022).
  lan_koder    <- region[nchar(region) == 2]
  kommun_koder <- region[nchar(region) == 4]

  bakgrundsvariabler <- till_endash(c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"))

  resultat <- list()

  if (length(lan_koder) > 0) {
    resultat$lan <- pxweb2r::pxweb2_get_data(
      table = "TAB389",
      query = list(
        Region = lan_koder,
        Kon = "*",
        UtbNiv = "samtliga utbildningsnivåer",
        BakgrVar = bakgrundsvariabler,
        ContentsCode = "Andel förvärvsarbetande (ny definition från och med 2019)",
        Tid = as.character(startar:2021)
      )) |>
      dplyr::rename(Andel_forvarvsarbetande = value)
  }

  if (length(kommun_koder) > 0) {
    resultat$kommun <- pxweb2r::pxweb2_get_data(
      table = "TAB4881",
      query = list(
        Region = kommun_koder,
        Kon = "*",
        UtbNiv = "samtliga utbildningsnivåer",
        BakgrVar = bakgrundsvariabler,
        ContentsCode = "Andel förvärvsarbetande (ny definition från och med 2019)",
        Tid = as.character(startar:2021)
      )) |>
      dplyr::rename(Andel_forvarvsarbetande = value)
  }

  if (length(lan_koder) > 0) {
    resultat$lan_22 <- pxweb2r::pxweb2_get_data(
      table = "TAB6384",
      query = list(
        Region = lan_koder,
        Kon = "*",
        UtbNiv = "samtliga utbildningsnivåer",
        BakgrVar = bakgrundsvariabler,
        ContentsCode = "Andel sysselsatta",
        Tid = "*"
      )) |>
      dplyr::rename(Andel_forvarvsarbetande = value)
  }

  if (length(kommun_koder) > 0) {
    resultat$kommun_22 <- pxweb2r::pxweb2_get_data(
      table = "TAB6383",
      query = list(
        Region = kommun_koder,
        Kon = "*",
        UtbNiv = "samtliga utbildningsnivåer",
        BakgrVar = bakgrundsvariabler,
        ContentsCode = "Andel sysselsatta",
        Tid = "*"
      )) |>
      dplyr::rename(Andel_forvarvsarbetande = value)
  }

  etablering <- dplyr::bind_rows(resultat) |>
    dplyr::rename(regionkod = region_kod) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))

  if(returnera_data_rmarkdown == TRUE){
    assign("etablering", etablering, envir = .GlobalEnv)
  }

  #diag_fargvektor <- if (all(is.na(diag_fargvektor)) & exists("diagramfarger")) diagramfarger("rus_sex") else c("darkred", "yellow", "darkgreen")

  # rus_gradient har 12 nyanser, men tidsserien (grupperad på år) kan
  # omfatta fler år än så (t.ex. startar = 2012 och senaste år 2025 =
  # 14 år) - antalet interpolerade nyanser anpassas därför efter det
  # faktiska antalet år i datat i stället för att hårdkoda paletten
  # direkt, annars kraschar SkapaStapelDiagram() med "Insufficient values
  # in manual scale".
  antal_ar_etablering <- length(unique(etablering$år))
  fargvekt_ar <- grDevices::colorRampPalette(rddiagram::diagramfarger("rus_gradient"))(antal_ar_etablering)

  gg_list <- list()

  if(diag_tidsserie == TRUE){

    diagramtitel <- glue::glue("Etablering på arbetsmarknaden efter vistelsetid för invandrade i {rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_fokus)$region)}")
    diagramfil <- glue::glue("etablering_vistelsetid_{rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_fokus)$region)}.png")


    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = etablering |>
                                   dplyr::filter(region == rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_fokus)$region),
                                          kön %in% c("män och kvinnor")) |>
                                   dplyr::mutate(år = as.character(år),
                                          bakgrundsvariabel = stringr::str_remove(bakgrundsvariabel, "vistelsetid ")),
      skickad_x_var = "bakgrundsvariabel",
      skickad_y_var = "Andel_forvarvsarbetande",
      skickad_x_grupp = "år",
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      x_axis_lutning = 0,
      manual_color = fargvekt_ar,
      manual_y_axis_title = "procent",
      procent_0_100_10intervaller = TRUE,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil,
      legend_rader = 2,
      legend_byrow = TRUE,
      x_axis_sort_value = TRUE,
      vand_sortering = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")
  }

  if(diag_facet == TRUE){
    diagramtitel <- glue::glue("Etablering på arbetsmarknaden efter vistelsetid för invandrade i {rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_fokus)$region)}")
    diagramfil <- glue::glue("etablering_vistelsetid_kon_{rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_fokus)$region)}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = etablering |>
                                   dplyr::filter(region == rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_fokus)$region),
                                          kön %in% c("män","kvinnor")) |>
                                   dplyr::mutate(år = as.character(år),
                                          bakgrundsvariabel = stringr::str_remove(bakgrundsvariabel, "vistelsetid "),
                                          kön = stringr::str_to_title(kön)),
      skickad_x_var = "bakgrundsvariabel",
      skickad_y_var = "Andel_forvarvsarbetande",
      skickad_x_grupp = "år",
      legend_rader = 2,
      legend_byrow = TRUE,
      facet_grp = "kön",
      facet_scale = "fixed",
      facet_legend_bottom = TRUE,
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      x_axis_lutning = 0,
      manual_color = fargvekt_ar,
      manual_y_axis_title = "procent",
      procent_0_100_10intervaller = TRUE,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil,
      x_axis_sort_value = TRUE,
      vand_sortering = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")
  }

  if(diag_alla_lan == TRUE){

    # vald_vistelsetid skickas med vanligt bindestreck (default "10- år") -
    # etablering$bakgrundsvariabel har redan konverterats till gemenhetstecken
    # via till_endash() vid hämtningen, så samma konvertering görs här innan
    # jämförelsen nedan.
    vald_vistelsetid_endash <- till_endash(vald_vistelsetid)

    diagram_titel = paste0("Arbetsmarknadsetablering med ",vald_vistelsetid, "s vistelsetid i Sverige år ",max(etablering$år))
    diagram_capt = paste0("Källa: SCB\n Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andelen etablerade på arbetsmarknaden efter  ",vald_vistelsetid, "s vistelsetid i landet")
    diagramfil <- ("etablering_vistelsetid_allalan.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = etablering |>
                                   dplyr::mutate( fokus = ifelse(region == rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_fokus)$region), "1", "0"),
                                           år = as.character(år),
                                           bakgrundsvariabel = stringr::str_remove(bakgrundsvariabel, "vistelsetid "),
                                           region = rdverktyg::skapa_kortnamn_lan(region)) |>
                                   dplyr::filter(år==max(år),
                                          kön== "män och kvinnor",
                                          bakgrundsvariabel == stringr::str_remove(vald_vistelsetid_endash, "vistelsetid ")),
      skickad_x_var = "region",
      skickad_y_var = "Andel_forvarvsarbetande",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      x_axis_lutning = 45,
      manual_color = rddiagram::diagramfarger("rus_tva_fokus"),
      manual_y_axis_title = "procent",
      procent_0_100_10intervaller = TRUE,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil,
      x_axis_sort_value = TRUE,
      x_var_fokus= "fokus",
      lagg_pa_logga = visa_logga_i_diagram,
      skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.png")
  }

  return(gg_list)

}

# ===========================================================================================================
#
# Skript för att skriva ut stapeldiagram över huvudsaklig inkomstkälla (HuvudFoT1m) per månad, från SCB:s
# tabell TAB1784 (samma statistikprodukt som BAS, AM0210, men uppdelat på inkomstkälla i stället för
# arbetsmarknadsstatus). Man väljer en grupp ur ContentsCode (default "antal totalt") och får ut diagram
# där de olika värdena i HuvudFoT1m (arbete, arbetslöshet, studier, pension, sjukdom m.m.) är kategorierna.
# Default är åldersgruppen 20-66 år men det finns fler åldersgrupper att välja på i tabellen.
# Gå in på url:en för att se vilka som finns tillgängliga.
#
# Följande diagram finns i funktionen diag_huvink_alla_kat_prel_manad_scb():
#             1. diag_senaste_ar - senaste månaden med samtliga inkomstkällor
#             2. diag_tidsserie - en tidsserie med samtliga månader som finns i tabellen
#             3. diag_tidsserie_facet_kat - en tidsserie med ett facet per inkomstkälla (HuvudFoT1m)
#             4. diag_fokusvariabel - en eller flera inkomstkällor som man vill titta extra på i en tidsserie
#
# ===========================================================================================================

diag_huvink_alla_kat_prel_manad_scb <- function(
    region_vekt = "20",                 # c("20", "17", "21"),
    alder_vekt = "20-66 år",            # finns också: 15-19 år, 20-24 år, 25-54 år, 55-64 år, 65-74 år, 15-74 år, 16-64 år, 16-65 år, 16-66 år, 20-64 år, 20-65 år
    kon_vekt = "Totalt",                # man måste välja totalt, män ELLER kvinnor
    fodelseregion_vekt = "totalt",      # man måste välja totalt, inrikes födda ELLER utrikes födda
    cont_uttag = "antal totalt",        # en grupp ur ContentsCode: antal totalt, antal sysselsatta, antal arbetslösa, antal studerande, antal pensionärer, antal sjuka, antal övriga
    gruppering_namn = NA,
    visa_andelar = FALSE,
    fokus_variabel = "ekonomiskt stöd",
    titel_bredd = 80,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS) - huvudsaklig inkomstkälla\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    diag_fargvekt = NA,
    skickad_logga_path = NA,               # NA om man vill köra med standard, bra att sätta en lokal om man är på resande fot
    ta_med_logga = TRUE,
    skriv_till_diagramfil = TRUE,
    diag_senaste_ar = FALSE,
    diag_tidsserie = TRUE,
    diag_tidsserie_facet_kat = TRUE,
    diag_fokusvariabel = TRUE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
  # stället för library(), samma mönster som diag_bas_status_alla_kategorier_prel_manad_scb.R.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  # dplyr/tidyr/stringr/purrr följer med som beroenden till rddiagram/rdverktyg.

  # om parametern demo är satt till TRUE: inga exempelbilder är uppladdade för det här skriptet ännu.
  if (demo){
    cat("Inga exempelbilder är uppladdade för det här skriptet ännu.\n")
    rdverktyg::stop_tyst()
  }

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rd_primar_nio") - nio färger,
  # en per kategori i HuvudFoT1m
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("rd_primar_nio")
  }

  if (all(is.na(output_mapp))) {
    output_mapp <- rdverktyg::utskriftsmapp()
  }

  retur_list <- list()

  # SCB skriver åldersintervallen med tankstreck ("20–66 år", U+2013), inte
  # vanligt bindestreck. Normaliserar bindestreck mellan siffror till
  # tankstreck här, samma fix som i systerskriptet för BAS.
  alder_vekt_fetch <- gsub("(?<=[0-9])-(?=[0-9])", "–", alder_vekt, perl = TRUE)

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/HuvudForsTyp1Man/
  # TAB1784, samma statistikprodukt (AM0210) som BAS men uppdelat på HuvudFoT1m (huvudsaklig
  # inkomstkälla) i stället för ArbStatus. ContentsCode väljs till EN grupp (default "antal totalt")
  # eftersom det är HuvudFoT1m som ska vara kategorivariabeln/facet-variabeln i diagrammen.
  huvink_aggr <- pxweb2r::pxweb2_get_data(
    table = "TAB1784",
    query = list(
      Region = region_vekt,
      HuvudFoT1m = "*",
      Kon = kon_vekt,
      Alder = alder_vekt_fetch,
      Fodelseregion = fodelseregion_vekt,
      ContentsCode = cont_uttag,
      Tid = "*"
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, variabel = `huvudsaklig inkomstkälla`, varde = value) |>
    dplyr::select(-tabellinnehåll) |>
    rdverktyg::manader_bearbeta_scbtabeller() |>
    # str_wrap() på kategorierna så att facet-rubrikerna radbryts i stället för att klippas av
    # (de två längsta, "ersättning vid långvarigt nedsatt arbetsförmåga" och "ersättning vid
    # föräldraledighet/närståendeomvårdnad", får annars inte plats i en smal facet-panel).
    dplyr::mutate(variabel = stringr::str_wrap(variabel, width = 25),
                  variabel = factor(variabel, levels = rev(stringr::str_wrap(c(
      "ersättning för arbete", "ersättning vid arbetslöshet", "ersättning för studier", "pension",
      "ersättning vid långvarigt nedsatt arbetsförmåga", "ersättning vid sjukdom",
      "ersättning vid föräldraledighet/närståendeomvårdnad", "ekonomiskt stöd", "saknar ersättningar"),
      width = 25))))

  vald_region <- if(is.na(gruppering_namn)) {
    rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region, byt_ut_riket_mot_sverige = TRUE))
  } else {
    vald_region <- gruppering_namn
  } # slut if-sats om gruppering_namn är medskickat eller inte


  if (!is.na(gruppering_namn)) {
    huvink_aggr <- huvink_aggr |>
      dplyr::group_by(kön, ålder, födelseregion, tid, år, månad, år_månad, månad_år, variabel) |>
      dplyr::summarise(varde := sum(varde, na.rm = TRUE), .groups = "drop_last") |>
      dplyr::mutate(andel = round((varde/sum(varde))*100,1)) |>
      dplyr::ungroup() |>
      dplyr::mutate(regionkod = "grp",
             region = gruppering_namn) |>
      dplyr::relocate(regionkod, .before = 1) |>
      dplyr::relocate(region, .after = regionkod)

  } else { # slut if-sats om man vill gruppera

    huvink_aggr <- huvink_aggr |>
      dplyr::group_by(regionkod, region, kön, ålder, födelseregion, tid, år, månad, år_månad, månad_år, variabel) |>
      dplyr::summarise(varde := sum(varde, na.rm = TRUE), .groups = "drop_last") |>
      dplyr::mutate(andel = round((varde/sum(varde))*100,1)) |>
      dplyr::ungroup()
  }

  # hantera åldrar
  alla_aldrar <- stringr::str_extract_all(alder_vekt, "\\d+")
  alder_min <- min(unlist(alla_aldrar))
  alder_max <- max(unlist(alla_aldrar))

  aldrar_min_till_max <- c(alder_min:alder_max)        # gör en vektor med alla nummer mellan min- och maxvärden
  alla_ar_i_grupp <- unlist(purrr::map(alla_aldrar, ~ (purrr::pluck(.x, 1)):(purrr::pluck(.x, 2))))       # gör sekvenser med alla åldersgrupper

  if (all(aldrar_min_till_max == alla_ar_i_grupp)) {
    # om sekvensen som går från min till max är samma som sekvenser för samtliga åldersgrupper så
    # är alla åldrar i ordning och finns för alla år, då lägger vi ihop dem till "en" åldersgrupp

      aldrar_titel <- paste0(min(alla_ar_i_grupp), "-", max(alla_ar_i_grupp), " år")
      aldrar_txt <- paste0(min(alla_ar_i_grupp), "_", max(alla_ar_i_grupp), "ar")
  } else {
    # om sekvensom som går från min till max INTE är samma som sekvenser för samtliga åldersgrupper
    # så har användaren valt åldersgrupper som inte ligger "intill" varandra, då behöver åldersgrupperna
    # redovisas var och en för sig
    aldrar_titel <- aldrar_titel
    aldrar_txt <- aldrar_txt

  }

  if (diag_senaste_ar) {

    diagram_titel <- paste0("Huvudsaklig inkomstkälla för befolkningen ", aldrar_titel, " i ", vald_region, " i ", unique(dplyr::pull(dplyr::select(dplyr::filter(huvink_aggr, tid == max(tid)), månad_år))))
    diagramfil <- paste0("huvink_alla_kat_", aldrar_txt, "_", paste0(region_vekt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(huvink_aggr, tid == max(tid)),
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      x_axis_lutning = 0,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter = FALSE,
      dataetiketter_justering_hojdled = -1.7,
      manual_y_axis_title = "antal individer",
      manual_color = diag_fargvekt,
      logga_path = skickad_logga_path,
      lagg_pa_logga = ta_med_logga,
      skriv_till_diagramfil = skriv_till_diagramfil,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- stringr::str_remove(diagramfil, ".png")

  } # slut if-sats diag_senaste_ar


  if (diag_tidsserie) {

    diagram_titel <- paste0("Huvudsaklig inkomstkälla för befolkningen ", aldrar_titel, " i ", vald_region)
    diagramfil <- paste0("huvink_alla_kat_tid_", aldrar_txt, "_", paste0(region_vekt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = huvink_aggr,
      skickad_x_var = "månad_år",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_hjust = 1,
      manual_x_axis_text_vjust = 1,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter = FALSE,
      dataetiketter_justering_hojdled = -1.7,
      manual_y_axis_title = "antal individer",
      manual_color = diag_fargvekt,
      facet_grp = if (length(unique(huvink_aggr$regionkod)) > 1) "region" else NULL,
      facet_x_axis_storlek = 5,
      facet_legend_bottom = TRUE,
      skriv_till_diagramfil = skriv_till_diagramfil,
      lagg_pa_logga = ta_med_logga,
      logga_path = skickad_logga_path,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- stringr::str_remove(diagramfil, ".png")

  } # slut if-sats diag_tidsserie

  if (diag_tidsserie_facet_kat) {

    diagram_titel <- paste0("Huvudsaklig inkomstkälla för befolkningen ", aldrar_titel, " i ", vald_region)
    diagramfil <- paste0("huvink_alla_kat_facet_tid_", aldrar_txt, "_", paste0(region_vekt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = huvink_aggr,
      skickad_x_var = "månad_år",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_hjust = 1,
      manual_x_axis_text_vjust = 1,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      x_axis_visa_var_xe_etikett = 2,
      dataetiketter = FALSE,
      dataetiketter_justering_hojdled = -1.7,
      manual_y_axis_title = "antal individer",
      manual_color = diag_fargvekt,
      facet_grp = "variabel",
      facet_x_axis_storlek = 5,
      facet_legend_bottom = FALSE,
      facet_scale = "free",
      skriv_till_diagramfil = skriv_till_diagramfil,
      lagg_pa_logga = ta_med_logga,
      logga_path = skickad_logga_path,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- stringr::str_remove(diagramfil, ".png")
  } # slut if-sats diag_tidsserie_facet_kat


  if (diag_fokusvariabel) {

    # variabel-kolumnen i huvink_aggr är str_wrap()-ad (se ovan), så fokus_variabel - som skickas in
    # oradbruten - måste wrap:as på samma sätt innan den matchas mot variabel. Titel/filnamn använder
    # dock det oradbrutna fokus_variabel så de inte får inbäddade radbrytningar.
    fokus_variabel_wrap <- stringr::str_wrap(fokus_variabel, width = 25)

    diagram_titel <- stringr::str_wrap(paste0("Huvudsaklig inkomstkälla för befolkningen ", aldrar_titel, " i ", vald_region, " - ", stringr::str_flatten_comma(fokus_variabel)), titel_bredd)
    diagramfil <- paste0("huvink_", stringr::str_flatten(fokus_variabel, collapse = "_"), "_tid_", aldrar_txt, "_", paste0(region_vekt, collapse = "_"), ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(huvink_aggr, variabel %in% fokus_variabel_wrap),
      skickad_x_var = "månad_år",
      skickad_y_var = "varde",
      skickad_x_grupp = if (length(fokus_variabel) > 1) "variabel" else NA,
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      manual_x_axis_text_hjust = 1,
      manual_x_axis_text_vjust = 1,
      legend_vand_ordning = TRUE,
      legend_byrow = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      dataetiketter = FALSE,
      dataetiketter_justering_hojdled = -1.7,
      manual_y_axis_title = "antal individer",
      manual_color = if (length(fokus_variabel) > 1) diag_fargvekt else diag_fargvekt[1],
      facet_grp = if (length(unique(huvink_aggr$regionkod)) > 1) "region" else NULL,
      facet_x_axis_storlek = 5,
      facet_legend_bottom = TRUE,
      facet_scale = "fixed",
      skriv_till_diagramfil = skriv_till_diagramfil,
      lagg_pa_logga = ta_med_logga,
      logga_path = skickad_logga_path,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- stringr::str_remove(diagramfil, ".png")

  } # slut if-sats diag_fokusvariabel
  return(retur_list)
} # slut funktion

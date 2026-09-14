diag_inr_flytt_in_ut_netto_tidsserie_per_alder <- function(region_vekt = "20",
                                     gruppera_namn = NA,
                                     ta_med_logga = TRUE,
                                     logga_path = NA,
                                     farg_vektor = NA,
                                     output_mapp = NA,
                                     kon_klartext = NA,
                                     visa_flyttnetto_linje = TRUE,
                                     skriv_diagramfil = TRUE,
                                     alder_koder = "*",
                                     alder_grp = NA,                                   # skicka med startåldern i de åldersgrupper man vill skapa, t.ex. om man hämtar alla åldrar och tar ut 20, 65, 80 så blir åldersgrupperna 0-19, 20-64, 65-79 samt 80+
                                     tid_koder = "*",
                                     relativt_flyttnetto = TRUE                                      # beräkna andel istället för antal
                                     ){

  # =======================================================================================================================
  #
  # Hämta hem data med funktionen hamta_bef_flyttningar_region_alder_kon_scb, skriv ut ett diagram. Defaultinställning är
  # att visa flyttnettolinje, relativt flyttnetto och att hämta alla år. Det finns möjlighet att skapa åldersgrupper direkt
  # i diagramskriptet genom att skicka med startålder för varje åldersgrupp i parametern "alder_grp".
  #
  # Länk till SCB-tabell där data hämtas: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101J/Flyttningar97/   (flyttningar)
  #                                       https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101A/BefolkningNy/    (folkmängd för beräkning av relativt flyttnetto)
  #
  # Skapat av: Peter Möller, Region Dalarna
  #
  # =======================================================================================================================

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
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  if(all(is.na(farg_vektor))) farg_vektor <- rddiagram::diagramfarger("rus_sex")[1]

  # SCB:s v2-tabeller dubblerar "100+ år"/"totalt"-etiketter över flera
  # koder i CKM-tabellerna (en per åldershierarki) - samma
  # hjälpfunktion som i diag_inr_flytt_in_ut_netto_per_alder.R filtrerar
  # bort dem.
  hamta_individuella_aldrar <- function(table_id) {
    pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE) |>
      dplyr::filter(grepl("^[0-9]+\\+? år$", label)) |>
      dplyr::filter(!duplicated(label)) |>
      dplyr::pull(code)
  }

  if (all(alder_koder == "*")) {
    alder_flytt_historik <- hamta_individuella_aldrar("TAB1212")
    alder_flytt_ckm <- hamta_individuella_aldrar("TAB6640")
    alder_bef_historik <- hamta_individuella_aldrar("TAB638")
    alder_bef_ckm <- hamta_individuella_aldrar("TAB5557")
  } else {
    alder_flytt_historik <- alder_flytt_ckm <- alder_bef_historik <- alder_bef_ckm <- as.character(alder_koder)
  }

  kon_hamta <- c("män", "kvinnor")

  # hamta_data-repots hamta_bef_flyttningar_region_alder_kon_scb.R (v1:
  # BE/BE0101/BE0101J/Flyttningar97 + Flyttningar97CKM) hämtas här
  # direkt via v2-motsvarigheterna TAB1212 (historik) + TAB6640 (CKM) -
  # samma tabellpar som i diag_inr_flytt_in_ut_netto_per_alder.R. Kön
  # saknar en "totalt"-kod i den tabellen - båda könen hämtas alltid
  # explicit och summeras/filtreras nedan.
  flytt_df_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB1212",
    query = list(
      Region = region_vekt,
      Kon = kon_hamta,
      Alder = alder_flytt_historik,
      ContentsCode = c("Inrikes inflyttningar", "Inrikes utflyttningar"),
      Tid = tid_koder
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  flytt_df_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6640",
    query = list(
      Region = region_vekt,
      Kon = kon_hamta,
      Alder = alder_flytt_ckm,
      ContentsCode = c("Inrikes inflyttningar", "Inrikes utflyttningar"),
      Tid = tid_koder
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  flytt_df <- dplyr::bind_rows(flytt_df_historik, flytt_df_ckm) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)

  if (all(is.na(kon_klartext))) {
    flytt_df <- flytt_df |>
      dplyr::group_by(dplyr::across(-c(kön, varde))) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")
  } else {
    flytt_df <- dplyr::filter(flytt_df, kön %in% kon_klartext)
  }

  # kolla om kön är med som variabel
  if (!all(is.na(kon_klartext))) {
    kon_txt <- paste0("_", paste0(unique(flytt_df$kön), collapse = "_"))             # fyll kon_txt med rätt textsträng
    farg_vektor <- rddiagram::diagramfarger("kon")                                         # fyll farg_vektor med rätt färger
  } else {
    kon_txt <- ""
  }

  kon_titel <- ""

  if ("kön" %in% names(flytt_df)) {
    if (length(unique(flytt_df$kön)) == 1) {
      farg_vektor <- if (unique(flytt_df$kön) == "kvinnor") farg_vektor[1] else farg_vektor[2]
      kon_titel <- paste0("för ", unique(flytt_df$kön), " ")
      kon_ett_varde <- TRUE
    } else kon_ett_varde <- FALSE
  } else kon_ett_varde <- FALSE

  # välj ut variabler till gruppering av netto-datasetet, dvs. med eller utan kön
  if (!all(is.na(kon_klartext))) {
    netto_grp_var <- c("år", "regionkod", "region", "alder_num", "kön", "ålder")
  } else {
    netto_grp_var <- c("år", "regionkod", "region", "alder_num", "ålder")
  }

  # skapa åldersgrupper om det har skickats med som parameter, annars gör ingenting
  if (!all(is.na(alder_grp))) {
    flytt_df <- flytt_df |>
      dplyr::filter(ålder != "totalt ålder") |>
      dplyr::mutate(ålder = rdverktyg::skapa_aldersgrupper(ålder, alder_grp))
  } else {
    flytt_df <- flytt_df |>
      dplyr::filter(ålder != "totalt ålder") |>
      dplyr::mutate(alder_num = readr::parse_number(ålder)) |>
      dplyr::arrange(alder_num) |>
      dplyr::mutate(ålder = factor(ålder, levels = unique(ålder)))
  }

  # grupperar datasetet om man angett värde för gruppera_namn
  if (!is.na(gruppera_namn)){
    flytt_df <- flytt_df |>
      dplyr::group_by(dplyr::across(dplyr::where(is.character))) |>
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
      dplyr::mutate(region = gruppera_namn,
             regionkod = "gg")
  }

  chart_df <- flytt_df |>
    dplyr::mutate(varde = ifelse(variabel == "Inrikes utflyttningar", varde*-1, varde)) |>
    dplyr::arrange(ålder)

  netto_df <- chart_df |>
    dplyr::group_by(dplyr::across(dplyr::any_of(netto_grp_var))) |>
    dplyr::summarise(varde = sum(varde[variabel == "Inrikes inflyttningar"]) + sum(varde[variabel == "Inrikes utflyttningar"]), .groups = "drop") |>
    dplyr::mutate(variabel = "Netto")

  if (relativt_flyttnetto) {

    hamta_ar <- unique(netto_df$år)

    # hamta_data-repots hamta_bef_folkmangd_alder_kon_ar_scb.R (v1:
    # BE/BE0101/BE0101A/BefolkningNy + BefolkningCKM) hämtas här direkt
    # via v2-motsvarigheterna TAB638 (historik) + TAB5557 (CKM) - samma
    # tabellpar som redan verifierats i diagram_befolkningsforandring.R.
    # Civilstånd saknar en "totalt"-kod i TAB638 (bara 4 individuella
    # civilstånd) - alla fyra hämtas alltid explicit och summeras ihop,
    # eftersom det här skriptet aldrig vill dela upp på civilstånd.
    civilstand_hamta <- c("ogifta", "gifta", "skilda", "änkor/änklingar")

    befolkning_df_historik <- pxweb2r::pxweb2_get_data(
      table = "TAB638",
      query = list(
        Region = region_vekt,
        Civilstand = civilstand_hamta,
        Alder = alder_bef_historik,
        Kon = kon_hamta,
        ContentsCode = "Folkmängd",
        Tid = hamta_ar
      ),
      on_all_values_invalid = "null", quiet = TRUE)

    befolkning_df_ckm <- pxweb2r::pxweb2_get_data(
      table = "TAB5557",
      query = list(
        Region = region_vekt,
        Civilstand = civilstand_hamta,
        Alder = alder_bef_ckm,
        Kon = kon_hamta,
        ContentsCode = "Folkmängd",
        Tid = hamta_ar
      ),
      on_all_values_invalid = "null", quiet = TRUE)

    befolkning_df <- dplyr::bind_rows(befolkning_df_historik, befolkning_df_ckm) |>
      dplyr::rename(regionkod = region_kod, Folkmängd = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::group_by(dplyr::across(-c(civilstånd, Folkmängd))) |>
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")

    if (all(is.na(kon_klartext))) {
      befolkning_df <- befolkning_df |>
        dplyr::group_by(dplyr::across(-c(kön, Folkmängd))) |>
        dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")
    } else {
      befolkning_df <- dplyr::filter(befolkning_df, kön %in% kon_klartext)
    }

    if (!all(is.na(alder_grp))) {
      befolkning_df <- befolkning_df |>
        dplyr::filter(ålder != "totalt ålder") |>
        dplyr::mutate(ålder = rdverktyg::skapa_aldersgrupper(ålder, alder_grp))
    } else {
      befolkning_df <- befolkning_df |>
        dplyr::filter(ålder != "totalt ålder") |>
        dplyr::mutate(alder_num = readr::parse_number(ålder)) |>
        dplyr::arrange(alder_num) |>
        dplyr::mutate(ålder = factor(ålder, levels = unique(ålder)))
    }

    # grupperar datasetet om man angett värde för gruppera_namn
    if (!is.na(gruppera_namn)){
      befolkning_df <- befolkning_df |>
        dplyr::group_by(dplyr::across(dplyr::where(is.character))) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
        dplyr::mutate(region = gruppera_namn,
               regionkod = "gg")
    }

    bef_grp <- befolkning_df |>
      dplyr::group_by(dplyr::across(dplyr::any_of(netto_grp_var))) |>
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")

    join_var <- netto_grp_var[netto_grp_var != "alder_num"]

    chart_df <- netto_df |>
      dplyr::left_join(bef_grp, by = join_var) |>
      dplyr::mutate(andel = (varde / Folkmängd) * 100)
  } else chart_df <- netto_df

  aldergrupper_txt <- if (dplyr::first(chart_df$ålder) != dplyr::last(chart_df$ålder)) paste0(dplyr::first(chart_df$ålder), "-", dplyr::last(chart_df$ålder)) else paste0(dplyr::first(chart_df$ålder))
  andel_txt <- if (relativt_flyttnetto) "rel_" else ""
  andel_titel <- if (relativt_flyttnetto) " relativt " else " "

  diagram_capt <- "Källa: Befolkningsstatistik, SCB:s öppna statistikdatabas\nBearbetning av Samhällsanalys, Region Dalarna"
  diagram_titel <- glue::glue("Skillnad mellan in- och utflyttning (inrikes{andel_titel}flyttnetto) {kon_titel}i {unique(chart_df$region)} år {dplyr::first(chart_df$år)}-{dplyr::last(chart_df$år)}")
  diagram_filnamn <- glue::glue("{andel_txt}flyttnetto_{unique(chart_df$region)}_alder_{aldergrupper_txt}_ar_{dplyr::first(chart_df$år)}-{dplyr::last(chart_df$år)}{kon_txt}.png")

  if (all(is.na(output_mapp))) output_mapp <- rdverktyg::utskriftsmapp()

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "år",
    skickad_y_var = if (relativt_flyttnetto) "andel" else "varde",
    skickad_x_grupp = if (!all(is.na(kon_klartext))) "kön" else NULL,
    #skickad_x_grupp = "variabel",
    y_axis_minus_plus_samma_axel = TRUE,
    #x_axis_visa_var_xe_etikett = 2,
    #diagram_liggande = TRUE,
    #geom_position_stack = TRUE,
    manual_color = farg_vektor,
    manual_y_axis_title = if (relativt_flyttnetto) "procent" else "Inrikes flyttnetto (skillnad mellan inrikes in- och utflyttade)",
    manual_x_axis_text_hjust = 1,
    manual_x_axis_text_vjust = 1,
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    facet_grp = "ålder",
    facet_scale = "fixed",
    #facet_legend_bottom = if (kon_ett_varde) FALSE else TRUE,
    facet_x_axis_storlek = 6,
    skriv_till_diagramfil = skriv_diagramfil,
    filnamn_diagram = diagram_filnamn,
    output_mapp = output_mapp
  )

  return(gg_obj)

}  # slut funktion

diag_inr_flytt_in_ut_netto_per_alder <- function(region_vekt = "20",
                                     gruppera_namn = NA,
                                     ta_med_logga = TRUE,
                                     logga_path = NA,
                                     farg_vektor = NA,
                                     output_mapp = NA,
                                     kon_klartext = NA,
                                     visa_flyttnetto_linje = TRUE,
                                     inrikes_utflytt_till_vanster = TRUE,         # FALSE så blir utflytt till höger istället och inflytt till vänster
                                     stodlinjer_avrunda_fem = TRUE,
                                     skriv_diagramfil = TRUE,
                                     demo = FALSE,                    # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                     alder_koder = "*",
                                     tid_koder = "9999"){

  # =======================================================================================================================
  #
  # Hämta hem data med funktionen hamta_bef_flyttningar_region_alder_kon_scb och skriv ut ett diagram. Defaultinställning är
  # att visa en flyttnettolinje. Diagrammet liknar en Det finns fler innehålls-
  # variabler, vilka primärt används för att beräkna dödsrisker och återstående medellivslängd.
  #
  # Länk till SCB-tabell där data hämtas: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0701/LivslUtbLan/
  #
  # =======================================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      "https://region-dalarna.github.io/utskrivna_diagram/in_utflyttning_Dalarnas%20l%C3%A4n_ar_2023.png"
    browseURL(demo_url)
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
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  if(all(is.na(farg_vektor))) farg_vektor <- rddiagram::diagramfarger("rus_sex")

  # hamta_data-repots hamta_bef_flyttningar_region_alder_kon_scb.R (v1:
  # BE/BE0101/BE0101J/Flyttningar97 + Flyttningar97CKM) hämtas här
  # direkt via v2-motsvarigheterna TAB1212 (historik 1997-2024) och
  # TAB6640 (CKM, 2025) - samma tabellpar som redan verifierats i
  # diagram_befolkningsforandring.R. TAB6640 dubblerar "100+ år" och
  # "totalt"-etiketter över flera koder (en per åldershierarki) - filtreras
  # bort nedan med samma hjälpfunktion som i tidigare migrerade skript.
  # Kön saknar en "totalt"-kod i tabellen (bara "män"/"kvinnor") - till
  # skillnad från v1-apiet (som eliminerar/summerar Kon när variabeln
  # utelämnas) hämtas här alltid båda könen explicit, och summeras ihop
  # själva om kon_klartext = NA (ingen könsuppdelning önskad).
  hamta_individuella_aldrar <- function(table_id) {
    pxweb2r::pxweb2_get_values(table_id, "Alder") |>
      dplyr::filter(grepl("^[0-9]+\\+? år$", label)) |>
      dplyr::filter(!duplicated(label)) |>
      dplyr::pull(code)
  }

  if (all(alder_koder == "*")) {
    alder_hamta_historik <- hamta_individuella_aldrar("TAB1212")
    alder_hamta_ckm <- hamta_individuella_aldrar("TAB6640")
  } else {
    alder_hamta_historik <- as.character(alder_koder)
    alder_hamta_ckm <- as.character(alder_koder)
  }

  kon_hamta <- c("män", "kvinnor")

  # "9999" (senaste år) löses av pxweb2r ut separat per tabell - eftersom
  # TAB6640 (CKM) har ett senare senaste år (2025) än TAB1212 (2024)
  # skulle vardera tabell annars ge sitt EGET senaste år, så att den
  # sammanslagna datan innehåller två olika år samtidigt (vilket
  # kraschar diagrammet, som förutsätter ett enda år). Slår i stället
  # upp det verkliga senaste året över båda tabellerna och använder det
  # explicit för båda uttagen.
  if (identical(tid_koder, "9999")) {
    tid_koder <- max(c(pxweb2r::pxweb2_get_values("TAB1212", "Tid")$code,
                        pxweb2r::pxweb2_get_values("TAB6640", "Tid")$code))
  }

  flytt_df_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB1212",
    query = list(
      Region = region_vekt,
      Kon = kon_hamta,
      Alder = alder_hamta_historik,
      ContentsCode = c("Inrikes inflyttningar", "Inrikes utflyttningar"),
      Tid = tid_koder
    ),
    on_all_values_invalid = "null")

  flytt_df_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6640",
    query = list(
      Region = region_vekt,
      Kon = kon_hamta,
      Alder = alder_hamta_ckm,
      ContentsCode = c("Inrikes inflyttningar", "Inrikes utflyttningar"),
      Tid = tid_koder
    ),
    on_all_values_invalid = "null")

  flytt_df <- dplyr::bind_rows(flytt_df_historik, flytt_df_ckm) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value) |>
    dplyr::select(-dplyr::any_of("ålder_kod"))

  # om ingen könsuppdelning önskas (kon_klartext = NA, standard) summeras
  # könen ihop - motsvarar v1-apiets elimination av Kon.
  if (all(is.na(kon_klartext))) {
    flytt_df <- flytt_df |>
      dplyr::group_by(dplyr::across(-c(kön, varde))) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")
  } else {
    flytt_df <- dplyr::filter(flytt_df, kön %in% kon_klartext)
  }

  # kolla om kön är med som variabel
  kon_txt <- if (!all(is.na(kon_klartext))) "_kon" else ""

  # välj ut variabler till gruppering av netto-datasetet, dvs. med eller utan kön
  if (!all(is.na(kon_klartext))) {
    netto_grp_var <- c("år", "regionkod", "region", "alder_num", "kön", "ålder")
  } else {
    netto_grp_var <- c("år", "regionkod", "region", "alder_num", "ålder")
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
    dplyr::filter(ålder != "totalt ålder") |>
    dplyr::mutate(
      varde = dplyr::case_when(
        inrikes_utflytt_till_vanster & variabel == "Inrikes utflyttningar" ~ varde * -1,
        !inrikes_utflytt_till_vanster & variabel == "Inrikes inflyttningar" ~ varde * -1,
        TRUE ~ varde
      ),
      alder_num = readr::parse_number(ålder)) |>
    dplyr::arrange(alder_num) |>
    dplyr::mutate(ålder = factor(ålder, levels = unique(ålder)))

  netto_df <- chart_df |>
    dplyr::group_by(dplyr::across(dplyr::any_of(netto_grp_var))) |>
    dplyr::summarise(varde = sum(varde[variabel == "Inrikes inflyttningar"]) + sum(varde[variabel == "Inrikes utflyttningar"]), .groups = "drop") |>
    dplyr::mutate(variabel = "Netto",
           varde = ifelse(!inrikes_utflytt_till_vanster, varde * -1, varde))

  total_df <- dplyr::bind_rows(chart_df, netto_df) |>
    tidyr::pivot_wider(names_from = variabel, values_from = varde) |>
    dplyr::mutate(
      `Inrikes inflyttningar` = dplyr::case_when(
        inrikes_utflytt_till_vanster & Netto > 0 ~ `Inrikes inflyttningar` - Netto,
        !inrikes_utflytt_till_vanster & Netto < 0 ~ `Inrikes inflyttningar` - Netto,
        TRUE ~ `Inrikes inflyttningar`
      ),
      `Inrikes utflyttningar` = dplyr::case_when(
        inrikes_utflytt_till_vanster & Netto < 0 ~ `Inrikes utflyttningar` - Netto,
        !inrikes_utflytt_till_vanster & Netto > 0 ~ `Inrikes utflyttningar` - Netto,
        TRUE ~ `Inrikes utflyttningar`
      )) |>
    tidyr::pivot_longer(cols = c(`Inrikes inflyttningar`, `Inrikes utflyttningar`, Netto), names_to = "variabel", values_to = "varde")

  diagram_capt <- "Källa: Befolkningsstatistik, SCB:s öppna statistikdatabas\nBearbetning av Samhällsanalys, Region Dalarna"
  diagram_titel <- glue::glue("In- och utflyttning i {unique(chart_df$region)} år {unique(chart_df$år)}")
  diagram_filnamn <- glue::glue("in_utflyttning_{unique(chart_df$region)}_ar_{unique(chart_df$år)}{kon_txt}.png")

  # OBS: originalet skickade "output_mapp = utskriftsmapp()" hårdkodat
  # här (och i skriv_till_diagramfil-anropet längst ner) i stället för
  # att koppla in den faktiska output_mapp-parametern.
  if (all(is.na(output_mapp))) output_mapp <- rdverktyg::utskriftsmapp()

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "ålder",
    skickad_y_var = "varde",
    skickad_x_grupp = "variabel",
    x_axis_lutning = 0,
    y_axis_storlek = 6,
    y_axis_minus_plus_samma_axel = TRUE,
    x_axis_visa_var_xe_etikett = 2,
    diagram_liggande = TRUE,
    geom_position_stack = TRUE,
    stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
    manual_color = rddiagram::diagramfarger("rus_sex"),
    manual_y_axis_title = "Antal in- och utflyttade",
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    facet_grp = if (!all(is.na(kon_klartext))) "kön" else NULL,
    facet_legend_bottom = TRUE,
    skriv_till_diagramfil = FALSE,
    filnamn_diagram = diagram_filnamn,
    output_mapp = output_mapp
  )


  # lägger till en flyttnetto-linje om det är valt
  if (visa_flyttnetto_linje) {
    netto_dubbel <- dplyr::bind_rows(netto_df, netto_df) |>
      dplyr::arrange(år, regionkod, region, ålder)

    gg_obj <- gg_obj +
      ggplot2::geom_line(ggplot2::aes(y = netto_dubbel$varde, group = 1), colour = "black") +
      ggplot2::geom_line(ggplot2::aes(color="line"))+
      ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto")+
      ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"),
            legend.box.just = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
        color = ggplot2::guide_legend(order = 2))
  }

  # skriver ut diagramfilen om det är valt
  if (skriv_diagramfil) rddiagram::skriv_till_diagramfil(gg_obj, output_mapp = output_mapp, filnamn_diagram = diagram_filnamn)

  return(gg_obj)

}  # slut funktion

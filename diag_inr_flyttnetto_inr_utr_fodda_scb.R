diag_inr_flyttnetto_inr_utr_fodda <- function(
    region_vekt = "20",
    alder_grp = "*",                                 # "*" för alla åldrar, annars skickas vektorn till skapa_aldersgrupper()
               # list(c(20,66)) för bara åldersgruppen 20-66 år, för flera åldersgrupper:
               # list(c(0,19), c(20,65), c(66,79), c(80, 999)) skapar åldersgrupperna 0-19, 20-65, 66-79 och 80+
    gruppera_namn = NA,                               # om NA skapas ett diagram per region, annars grupperas de ihop och får namnet som anges här
    relativt_flyttnetto = FALSE,                     # TRUE om vi vill ha relativt flyttnetto (mot bef i samma grupper året innan)
    facet_diagram = TRUE,                            # om TRUE skapas ett diagram för alla regioner, annars ett diagram för varje region
    farg_vekt = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nInrikes flyttnetto är skillnaden mellan de som flyttat in till och de som flyttat ut från en kommun/län, från och till andra kommuner/län",
    output_mapp = NA,
    skriv_diagram = TRUE,
    skriv_excel = FALSE,
    spara_som_svg = FALSE,                            # TRUE om vi vill spara diagrammet som svg
    visa_totalvarden = TRUE,                          # skriver ut ett streck för netto både inrikes och utrikes födda
    visa_totalvarden_dataetiketter = FALSE,           # skriver ut dataetiketter för totalvärdena
    totalvarden_dataetiketter_farg = "black",         # välj färg på totalstrecken
    totalvarden_dataetiketter_hjust = 20,             # justerar dataetiketter för totalvärden i höjdled
    totalvarden_dataetiketter_textstorlek = 2,        # justerar textstorlek för dataetiketter för totalvärden
    totalvarden_linjetjocklek = 4                     # tjocklek på totalstrecken i tiondels % av hela diffen i datasetet
    ) {

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
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()

  diagram_format <- if (spara_som_svg)  "svg" else "png"

  # om ingen output_mapp är medskickad och funktionen utskriftsmapp finns, använd den, annars sätt skriv_diagramfil till FALSE
  if (all(is.na(output_mapp))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_mapp <- rdverktyg::utskriftsmapp()
    } else {
      skriv_diagram <- FALSE
    }
  }

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(farg_vekt))) {
    farg_vekt <- rddiagram::diagramfarger("rus_sex")[c(2,1)]
  }


  # =====================================================================================================
  #
  # hamta_data-repot har ingen egen hamta-funktion för den här tabellen -
  # originalet gjorde pxweb-uttaget direkt i diagramskriptet. Hämtas här
  # direkt via v2-motsvarigheterna TAB4693 (historik 2002-2024) och
  # TAB6657 (CKM, 2025) till v1-tabellen BE/BE0101/BE0101J/FlyttFodReg.
  # Kön saknar en "totalt"-kod i TAB4693 (bara "män"/"kvinnor") - båda
  # hämtas alltid explicit och summeras ihop, eftersom skriptet aldrig
  # delar upp på kön. TAB6657 dubblerar "100+ år"/"totalt"-etiketter över
  # flera koder - samma hjälpfunktion som i tidigare migrerade skript
  # filtrerar bort dem.

  hamta_individuella_aldrar <- function(table_id) {
    pxweb2r::pxweb2_get_values(table_id, "Alder") |>
      dplyr::filter(grepl("^[0-9]+\\+? år$", label)) |>
      dplyr::filter(!duplicated(label)) |>
      dplyr::pull(code)
  }

  # CKM-tabellernas kod för "100+ år" är inte alltid det enkla "100+" -
  # slår upp den faktiska koden för respektive tabell när 999 (= högsta
  # ålder) skickas med i alder_grp.
  hamta_100plus_kod <- function(table_id) {
    varden <- pxweb2r::pxweb2_get_values(table_id, "Alder")
    varden$code[varden$label == "100+ år" & varden$type == "Variable"][1]
  }

  if (!all(alder_grp == "*")) {

    alder_grp <- purrr::map(alder_grp, as.character)        # konvertera till text
    alder_vekt <- alder_grp                          # skapa alder_vekt utifrån alder_grp

    # om 999 är medskickat (= högsta ålder) så byts det ut till 99 för att kunna skapa vektor
    if (any(purrr::map_lgl(alder_grp, ~ "999" %in% .x))) {
      alder_vekt <- purrr::map(alder_vekt, ~ replace(.x, .x == "999", "99"))
    }

    # skapa åldersgrupper för varje vektor i listan och lägg ihop i en vektor
    alder_vekt <- as.character(unlist(purrr::map(alder_vekt, ~ seq(.x[1], .x[2]))))

    # om 999 är medskickat så lägg till 100+ i vektorn för att få med alla åldrar
    if (any(purrr::map_lgl(alder_grp, ~ "999" %in% .x))) alder_vekt <- c(alder_vekt, "100+")

    # skapa en vektor som vi kan skicka med till skapa_aldersgrupper()
    alder_grp_vekt <- purrr::map_int(alder_grp, ~ readr::parse_number(.[1]))

    # om sista vektorn inte är 100+ så lägger vi till sista värdet i vektorn innan + 1
    if (alder_grp[[length(alder_grp)]][2] != "100+") {
      alder_grp_vekt <- append(
        alder_grp_vekt,
        readr::parse_number(alder_grp[[length(alder_grp)]][2]) + 1
      )
    }

    alder_hamta_historik <- alder_vekt[alder_vekt != "100+"]
    alder_hamta_ckm <- alder_hamta_historik
    if ("100+" %in% alder_vekt) {
      alder_hamta_historik <- c(alder_hamta_historik, "100+")
      alder_hamta_ckm <- c(alder_hamta_ckm, hamta_100plus_kod("TAB6657"))
    }

  } else {
    alder_vekt <- alder_grp
    alder_hamta_historik <- "totalt ålder"
    alder_hamta_ckm <- "TotSA"
  }

  kon_hamta <- c("män", "kvinnor")
  cont_hamta <- c("Flyttningsnetto, eget län", "Flyttningsnetto, övriga län")

  flytt_df_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB4693",
    query = list(
      Region = region_vekt,
      Fodelseregion = c("född i Sverige", "utrikes född"),
      Alder = alder_hamta_historik,
      Kon = kon_hamta,
      ContentsCode = cont_hamta,
      Tid = "*"
    ),
    on_all_values_invalid = "null")

  flytt_df_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6657",
    query = list(
      Region = region_vekt,
      Fodelseregion = c("född i Sverige", "utrikes född"),
      Alder = alder_hamta_ckm,
      Kon = kon_hamta,
      ContentsCode = cont_hamta,
      Tid = "*"
    ),
    on_all_values_invalid = "null")

  px_df <- dplyr::bind_rows(flytt_df_historik, flytt_df_ckm) |>
    dplyr::rename(regionkod = region_kod) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE)) |>
    # summera bort kön (motsvarar v1-apiets elimination av Kon när
    # variabeln utelämnas helt, vilket originalet gjorde)
    dplyr::group_by(dplyr::across(-c(kön, value))) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    # summera ihop de två delkomponenterna (eget län + övriga län) till
    # inrikes flyttningsnetto - motsvarar originalets ContentsCode-summa
    dplyr::group_by(dplyr::across(-c(tabellinnehåll, value))) |>
    dplyr::summarise(Inrikes_flyttnetto = sum(value, na.rm = TRUE), .groups = "drop")

  # här grupperar vi på åldersgrupper om man valt att skicka med åldersgrupper
  if (any(alder_vekt != "*")) {
    px_df <- px_df |>
      dplyr::mutate(alder_num = readr::parse_number(ålder),
             aldersgrupp = rdverktyg::skapa_aldersgrupper(alder_num, alder_grp_vekt)) |>
      dplyr::select(-c(alder_num, ålder)) |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  } else {
    px_df <- dplyr::select(px_df, -ålder)
  }


  if (!is.na(gruppera_namn)){
    px_df <- px_df |>
      dplyr::select(-c(regionkod, region)) |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(Inrikes_flyttnetto, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(region = gruppera_namn)

  } else {
    px_df <- px_df |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(Inrikes_flyttnetto, na.rm = TRUE), .groups = "drop")
  }

  if (relativt_flyttnetto) {

    bef_ar_vekt <- as.character(as.numeric(unique(px_df$år)) - 1)

    # hamta_data-repots hamta_bef_region_alder_kon_fodelseregion_tid_
    # InrUtrFoddaRegAlKon_scb.R (v1: BE/BE0101/BE0101E/InrUtrFoddaRegAlKon)
    # hämtas här direkt via v2-motsvarigheterna TAB4823 (historik) och
    # TAB6645 (CKM) - samma tabellpar som redan verifierats i
    # diag_bef_inr_utr_en_aldersgrupp_scb.R. TAB4823 saknar en
    # "totalt"-kod för Alder (till skillnad från flyttnetto-tabellerna
    # ovan) - när alder_grp = "*" (ingen åldersuppdelning önskas) hämtas
    # här i stället alla individuella åldrar och summeras ihop till en
    # enda "totalt"-rad, i stället för att (som originalet gjorde)
    # försöka gruppera dem med en åldersgrupps-vektor som aldrig
    # definierades när alder_grp = "*" - ett bekräftat fel i originalet
    # (kraschade med "object 'alder_grp_vekt' not found" vid test av
    # relativt_flyttnetto = TRUE med standardvärdet alder_grp = "*").
    if (any(alder_vekt != "*")) {
      alder_bef_hamta_historik <- alder_vekt[alder_vekt != "100+"]
      alder_bef_hamta_ckm <- alder_bef_hamta_historik
      if ("100+" %in% alder_vekt) {
        alder_bef_hamta_historik <- c(alder_bef_hamta_historik, "100+")
        alder_bef_hamta_ckm <- c(alder_bef_hamta_ckm, hamta_100plus_kod("TAB6645"))
      }
    } else {
      alder_bef_hamta_historik <- "*"
      alder_bef_hamta_ckm <- hamta_individuella_aldrar("TAB6645")
    }

    bef_df_historik <- pxweb2r::pxweb2_get_data(
      table = "TAB4823",
      query = list(
        Region = region_vekt,
        Alder = alder_bef_hamta_historik,
        Kon = kon_hamta,
        Fodelseregion = c("född i Sverige", "utrikes född"),
        ContentsCode = "Antal",
        Tid = bef_ar_vekt
      ),
      on_all_values_invalid = "null")

    bef_df_ckm <- pxweb2r::pxweb2_get_data(
      table = "TAB6645",
      query = list(
        Region = region_vekt,
        Alder = alder_bef_hamta_ckm,
        Kon = kon_hamta,
        Fodelseregion = c("född i Sverige", "utrikes född"),
        ContentsCode = "Antal",
        Tid = bef_ar_vekt
      ),
      on_all_values_invalid = "null")

    bef_df <- dplyr::bind_rows(bef_df_historik, bef_df_ckm) |>
      dplyr::rename(regionkod = region_kod, Antal = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region)) |>
      # summera bort kön (originalet skickade kon_klartext = NA till
      # v1-hamtafunktionen, som då aldrig tog med kön alls i uttaget)
      dplyr::group_by(dplyr::across(-c(kön, Antal))) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop")

    if (any(alder_vekt != "*")) {
      bef_df <- bef_df |>
        dplyr::mutate(alder_num = readr::parse_number(ålder),
               aldersgrupp = rdverktyg::skapa_aldersgrupper(alder_num, alder_grp_vekt)) |>
        dplyr::select(-c(alder_num, ålder))
    } else {
      bef_df <- dplyr::select(bef_df, -ålder)
    }

    bef_df <- bef_df |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
      dplyr::rename(bef = Antal) |>
      dplyr::mutate(år = as.character(as.numeric(år) + 1 ))           # för att koppla till rätt år

    kol_nycklar <- intersect(
      names(dplyr::select(bef_df, dplyr::where(~ is.character(.x) || is.factor(.x)))),
      names(dplyr::select(px_df, dplyr::where(~ is.character(.x) || is.factor(.x))))
    )

    # lägg ihop de båda dataframesen på region, år och födelseregion, skapa ett relativt flyttnetto
    px_df <- dplyr::left_join(px_df, bef_df, by = kol_nycklar) |>
      dplyr::mutate(rel_flyttnetto = Inrikes_flyttnetto/bef * 100) |>
      dplyr::rename(abs_flyttnetto = Inrikes_flyttnetto,
             Inrikes_flyttnetto = rel_flyttnetto)

    visa_totalvarden <- FALSE          # vid relativt flyttnetto så tar vi bort visa totalvärden
    relativt_txt <- "relativt "

  } else relativt_txt <- ""

  relativt_filnamn <- paste0(stringr::str_remove(relativt_txt, " "), "_")

  if (skriv_excel){
    reg_namn <- ifelse(!is.na(gruppera_namn), gruppera_namn, paste0(region_vekt, collapse = "_"))
    openxlsx::write.xlsx(px_df, paste0("Flyttnetto_", reg_namn, "_ar", min(px_df$år), "_", max(px_df$år), ".xlsx"), overwrite = TRUE)
  }
  # ============================================= Skapa diagram ==============================================

  skapa_diagram <- function(vald_regionkod) {  # skapa en funktion som skapar diagram för varje region

    chart_df <- dplyr::filter(px_df, regionkod %in% vald_regionkod)

    reg_txt <- rdverktyg::skapa_kortnamn_lan(unique(chart_df$region), byt_ut_riket_mot_sverige = TRUE)
    unika_ar <- unique(chart_df$år)
    unika_reg <- unique(vald_regionkod)
    unika_reg_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(unika_reg)$region)
    # =================================== visa totalvärden ========================================
    if (visa_totalvarden){

      diff <- max(chart_df$Inrikes_flyttnetto) - min(chart_df$Inrikes_flyttnetto) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- diff * (totalvarden_linjetjocklek/1000)      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()

      for (reg in 1:length(unika_reg)) {
        for (ar in 1:length(unika_ar)){
          arsvarde <- dplyr::pull(dplyr::select(dplyr::filter(chart_df, år == unika_ar[ar],
                   regionkod == unika_reg[reg]), Inrikes_flyttnetto))
          arsvarde <- sum(arsvarde, na.rm = TRUE)
          total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))
          if (visa_totalvarden_dataetiketter) {
            total_list <- c(total_list, list(list(geom = "text", y=arsvarde+totalvarden_dataetiketter_hjust, x = ar, size = totalvarden_dataetiketter_textstorlek, angle=0, fontface = "plain", label =arsvarde, color = totalvarden_dataetiketter_farg)))
          } # slut if-sats om man vill vissa dataetiketter
        } # slut for-loop unika_ar
      } # slut for_loop unika_reg
    } else total_list <- NA # slut if-sats visa_totalvärden

    # ======================= skapa ggplot-objekt =================

    diagtitel_txt <- if (facet_diagram) " i" else paste0(" i ", reg_txt)

    diagram_titel <- glue::glue("Inrikes {relativt_txt}flyttnetto {diagtitel_txt}")
    diagramfil <- stringr::str_replace_all(paste0("Flyttnetto_", relativt_filnamn, paste0(unika_reg_txt, collapse = "_"), "_ar", min(chart_df$år), "_", max(chart_df$år), ".png"), "__", "_")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_df,
      skickad_x_var = "år",
      skickad_y_var = "Inrikes_flyttnetto",
      skickad_x_grupp = "födelseregion",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      #x_axis_storlek = 8,
      stodlinjer_avrunda_fem = TRUE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = if (relativt_flyttnetto) "procent" else "",
      geom_position_stack = if (relativt_flyttnetto) FALSE else TRUE,
      fokusera_varden = total_list,
      facet_grp = if (facet_diagram) "region" else NULL,
      skriv_till_diagramfil = !visa_totalvarden,
      manual_color = farg_vekt,
      diagram_bildformat = diagram_format,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil)

    if (visa_totalvarden){
      suppressMessages(
      dia_med_utan_legend <- gg_obj +
        ggplot2::geom_line(ggplot2::aes(group = 1, color="line"), alpha = 0)+
        ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto totalt")+
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"),
              legend.box.just = "bottom")
      )
    } else dia_med_utan_legend <- gg_obj # slut if-sats visa_totalvarden

    retur_list <- list(dia_med_utan_legend)
    names(retur_list) <- stringr::str_remove(diagramfil, "\\.[^.]+$")

      if (skriv_diagram) {                           # skriv en diagramfil om så önskas
        suppressMessages(
        invisible(
        rddiagram::skriv_till_diagramfil(dia_med_utan_legend,
                            output_mapp = output_mapp,
                            filnamn_diagram = diagramfil)
        ))
      } # slut if-sats
    return(retur_list)
  } # slut skapa diagram-funktion för varje region

  if (length(region_vekt) == 1) facet_diagram <- FALSE

  if (facet_diagram) {
    gg_list <- skapa_diagram(region_vekt)
  } else {
    gg_list <- purrr::list_flatten(purrr::map(unique(region_vekt), ~skapa_diagram(.x)))
  }

  return(gg_list)
} # slut funktion

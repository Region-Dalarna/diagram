diagram_utrikes_fodda_tidsserie <-function(region_vekt = c("20"),# Max 1, län
                                           kon_klartext = NA, # #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
                                           diag_antal = TRUE, # Hela populationen
                                           diag_forandring_kommuner = TRUE, # Hela populationen
                                           diag_forandring_lan = TRUE, # Förändring för valt åldersspann
                                           diag_forandring_prognos = TRUE, # Prognos för valt åldersspann, enbart län och enbart båda könen
                                           diag_antal_uppdelat = TRUE,
                                           output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                           spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                           fodelseregion_klartext = "*", # NA = tas inte med i uttaget,  Finns: "Född i Sverige", "Utrikes född"
                                           tid_koder = "*", # Finns från 2000 och framåt
                                           visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                           logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet.
                                           ta_bort_diagramtitel = FALSE,
                                           ta_bort_caption = FALSE,
                                           x_axis_storlek = 10.5,
                                           alder_grupp = c(16,65), # Spann som skall användas i diagrammen diag_forandring_lan respektive _prognos. Vill man ha 16-64 år skriv c(16,65)
                                           prognos_ar = 2034, # Prognosår
                                           returnera_figur = TRUE, # Returnerar en figur
                                           valda_farger = rddiagram::diagramfarger("rus_sex"),
                                           returnera_data = FALSE) # Skall data returneras)
{

  ## =================================================================================================================
  # Funktion som skapar fem diagram, två för antal utrikes födda i valt län (enbart utrikes eller utrikes och inrikes), ett för förändring i utrikes födda mellan första och sista år i länets kommuner,
  # ett för förändring i utrikes födda i valt län och ett för förändring i utrikes födda i valt län för åldersgruppen 16-64 år.
  # =================================================================================================================
  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load. Anropas med fullt namespace (dplyr::filter() osv.) i stället
  # för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()
  region_namn <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region)
  # Används till diagrammet förändring län
  region_vekt_lan = region_vekt

  if(diag_forandring_kommuner) region_vekt = rdverktyg::hamtakommuner(region_vekt,tamedriket = FALSE)

  # Hjälpfunktion: välj en enda kod per verklig ettårsålder. CKM-tabellen
  # (och delvis även historiktabellen) har samma ålder representerad under
  # flera olika aggregeringshierarkier (5-års-/10-årsklasser) - samma
  # problem och lösning som i diagram_flytt_inrikes_aldersgrupper_SCB.R.
  hamta_individuella_aldrar <- function(table_id) {
    v <- pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE)
    v <- v[grepl("^[0-9]+\\+? år$", v$label), ]
    v <- v[!duplicated(v$label), ]
    v$code
  }

  # hamta_data-repots hamta_bef_region_alder_kon_fodelseregion_tid_
  # InrUtrFoddaRegAlKon_scb.R (v1: BE0101E/InrUtrFoddaRegAlKon) hämtas här
  # direkt via v2-motsvarigheterna TAB4823 (historik 2000-2024) och TAB6645
  # (CKM 2025-), verifierat via tabelltitel/variabelstruktur/ContentsCode.
  # Ingen av tabellerna har en egen "totalt"-kod för Kon, och historik-
  # tabellen saknar helt en "totalt"-kod för Alder - båda könen/samtliga
  # ettårsåldrar hämtas därför alltid explicit och summeras ihop själva i
  # stället för att förlita sig på att en utelämnad variabel ger en färdig
  # totalsumma (pxweb2r fyller annars bara på med "*", se fynden i
  # diagram_befolkningsforandring.R).
  hamta_inrikes_utrikes_fodda <- function(vald_region, full_aldersuppdelning = FALSE) {

    historik <- pxweb2r::pxweb2_get_data(
      table = "TAB4823",
      query = list(
        Region = vald_region,
        Alder = hamta_individuella_aldrar("TAB4823"),
        Kon = c("män","kvinnor"),
        Fodelseregion = c("född i Sverige","utrikes född"),
        ContentsCode = "Antal",
        Tid = tid_koder
      ),
      on_all_values_invalid = "null", quiet = TRUE)

    ckm <- pxweb2r::pxweb2_get_data(
      table = "TAB6645",
      query = list(
        Region = vald_region,
        Alder = if (full_aldersuppdelning) hamta_individuella_aldrar("TAB6645") else "TotSA",
        Kon = c("män","kvinnor"),
        Fodelseregion = c("född i Sverige","utrikes född"),
        ContentsCode = "Antal",
        Tid = tid_koder
      ),
      on_all_values_invalid = "null", quiet = TRUE)

    df <- dplyr::bind_rows(historik, ckm) |>
      dplyr::rename(regionkod = region_kod, Antal = value) |>
      dplyr::select(-tabellinnehåll)

    grp_kolumner <- c("regionkod","region","födelseregion","år")
    if (full_aldersuppdelning) grp_kolumner <- c(grp_kolumner, "ålder")
    if (!all(is.na(kon_klartext))) grp_kolumner <- c(grp_kolumner, "kön")

    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_kolumner))) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop")

    if (!all(is.na(kon_klartext))) df <- dplyr::filter(df, kön %in% kon_klartext)

    df
  }

  # Hämta data
  antal_inrikes_utrikes_df <- hamta_inrikes_utrikes_fodda(region_vekt) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))


  if(diag_antal){
    antal_utrikes_region_df <- dplyr::filter(antal_inrikes_utrikes_df, födelseregion == "utrikes född",region == region_namn)

    if(returnera_data == TRUE){
      assign("antal_utrikes_region_df", antal_utrikes_region_df, envir = .GlobalEnv)
    }

    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = paste0("Antal utrikes födda i ",region_namn)
    diagramfilnamn <- paste0("utrikes_fodda_antal_",region_namn,".png")

    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }

    if(ta_bort_caption){
      diagram_capt = ""
    }

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = antal_utrikes_region_df,
      skickad_x_var = "år",
      skickad_y_var = "Antal",
      manual_color = valda_farger,
      diagram_titel = diagram_titel,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      diagram_capt =  diagram_capt,
      output_mapp = output_mapp_figur,
      x_axis_storlek = x_axis_storlek,
      stodlinjer_avrunda_fem = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      manual_y_axis_title = "",
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  if(diag_antal_uppdelat){

    if(returnera_data == TRUE){
      assign("antal_inrikes_utrikes_df", antal_inrikes_utrikes_df, envir = .GlobalEnv)
    }

    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = paste0("Folkmängd i ",region_namn," ",min(antal_inrikes_utrikes_df$år),"-",max(antal_inrikes_utrikes_df$år))
    diagramfilnamn <- paste0("utrikes_inrikes_antal_",region_namn,".png")

    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }

    if(ta_bort_caption){
      diagram_capt = ""
    }

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = antal_inrikes_utrikes_df |>
        dplyr::filter(region == region_namn) |>
        dplyr::mutate(födelseregion = factor(födelseregion, c("utrikes född","född i Sverige"))),
      skickad_x_var = "år",
      skickad_y_var = "Antal",
      skickad_x_grupp = "födelseregion",
      geom_position_stack = TRUE,
      manual_color = valda_farger,
      diagram_titel = diagram_titel,
      legend_vand_ordning = TRUE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      diagram_capt =  diagram_capt,
      output_mapp = output_mapp_figur,
      x_axis_storlek = x_axis_storlek,
      stodlinjer_avrunda_fem = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      manual_y_axis_title = "",
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  if(diag_forandring_kommuner){

    first_year <- min(antal_inrikes_utrikes_df$år)
    last_year <- max(antal_inrikes_utrikes_df$år)

    # Beräknar förändring i antal
    antal_forandring_df <- antal_inrikes_utrikes_df |>
      dplyr::filter(år %in% c(min(år),max(år))) |>
      tidyr::pivot_wider(names_from = år, values_from = Antal) |>
      dplyr::mutate(forandring = get(last_year) - get(first_year))


    if(returnera_data == TRUE){
      assign("antal_forandring_df", antal_forandring_df, envir = .GlobalEnv)
    }

    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    diagram_titel = paste0("Befolkningsförändring ",first_year,"-",last_year," i ",region_namn)
    diagramfilnamn <- paste0("befolkningsforandring_",region_namn,".png")

    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }

    if(ta_bort_caption){
      diagram_capt = ""
    }

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(antal_forandring_df, region != region_namn),
      skickad_x_var = "region",
      skickad_y_var = "forandring",
      skickad_x_grupp = "födelseregion",
      manual_color = rev(valda_farger[1:2]),
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      x_axis_storlek = x_axis_storlek,
      diagram_capt =  diagram_capt,
      output_mapp = output_mapp_figur,
      stodlinjer_avrunda_fem = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      manual_y_axis_title = "",
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  if(diag_forandring_lan){

    antal_inrikes_utrikes_lan_df <- hamta_inrikes_utrikes_fodda(region_vekt_lan, full_aldersuppdelning = TRUE) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region),
             alder_grupper = rdverktyg::skapa_aldersgrupper(ålder,alder_grupp)) |>
      dplyr::group_by(region,år,födelseregion,alder_grupper) |>
      dplyr::summarise(Antal = sum(Antal), .groups = "drop") |>
      dplyr::filter(alder_grupper == unique(alder_grupper)[2]) # Väljer åldersgruppen i mitten av spannet, skriver man exempelvis c(16,65) blir det 16-65 år

    # Beräknar förändring i antal
    antal_forandring_lan_df <- antal_inrikes_utrikes_lan_df |>
      dplyr::group_by(födelseregion) |>
      dplyr::arrange(år,.by_group=TRUE) |>
      dplyr::mutate(forandring = Antal - dplyr::lag(Antal)) |>  # Compute difference from previous year
      dplyr::ungroup()

    antal_forandring_lan_kumulativ <- antal_forandring_lan_df |>
      dplyr::filter(år>min(år)) |>
      dplyr::group_by(födelseregion) |>
      dplyr::arrange(år,.by_group=TRUE) |>
      dplyr::mutate(kumulativ_summa = cumsum(forandring)) |>
      dplyr::ungroup()

    if(returnera_data == TRUE){
      assign("antal_forandring_lan_kumulativ", antal_forandring_lan_kumulativ, envir = .GlobalEnv)
    }

    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Kumulativ förändring, dvs förändringen summeras varje år"
    diagram_titel = paste0("Befolkningsförändring ",unique(antal_inrikes_utrikes_lan_df$alder_grupper), " i ",region_namn)
    diagramfilnamn <- paste0("befolkningsforandring_lan_",region_namn,".png")

    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }

    if(ta_bort_caption){
      diagram_capt = ""
    }

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = antal_forandring_lan_kumulativ,
      skickad_x_var = "år",
      skickad_y_var = "kumulativ_summa",
      skickad_x_grupp = "födelseregion",
      manual_color = rev(valda_farger[1:2]),
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      x_axis_storlek = x_axis_storlek,
      diagram_capt =  diagram_capt,
      output_mapp = output_mapp_figur,
      stodlinjer_avrunda_fem = TRUE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      manual_y_axis_title = "",
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  if(diag_forandring_prognos){

    # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0401__BE0401A/BefProgRegFakN/
    # v2-motsvarighet: TAB6008 (rullande aktuell befolkningsframskrivning),
    # verifierat via variabelstruktur (Region/InrikesUtrikes/Kon/Alder/
    # ContentsCode/Tid) och att etiketterna "inrikes födda"/"utrikes födda"/
    # ContentsCode "Antal" är identiska med v1-tabellen.
    befprognos_df <- pxweb2r::pxweb2_get_data(
      table = "TAB6008",
      query = list(
        Region = region_vekt_lan,
        InrikesUtrikes = c("inrikes födda", "utrikes födda"),
        Kon = "*",
        Alder = "*",
        ContentsCode = "Antal",
        Tid = "*"
      ), quiet = TRUE) |>
      dplyr::rename(regionkod = region_kod, Antal = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::rename(födelseregion = `inrikes/utrikes född`) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region),
             alder_grupper = rdverktyg::skapa_aldersgrupper(ålder,alder_grupp)) |>
      dplyr::group_by(region,år,födelseregion,alder_grupper) |>
      dplyr::summarise(Antal = sum(Antal), .groups = "drop")

    befprognos_df <- dplyr::filter(befprognos_df, alder_grupper == unique(befprognos_df$alder_grupper)[2])

    antal_forandring_prognos_df <- befprognos_df |>
      dplyr::group_by(födelseregion) |>
      dplyr::arrange(år,.by_group=TRUE) |>
      dplyr::mutate(forandring = Antal - dplyr::lag(Antal)) |>  # Compute difference from previous year
      dplyr::ungroup()

    antal_forandring_prognos_kumulativ <- antal_forandring_prognos_df |>
      dplyr::filter(år>min(år)) |>
      dplyr::group_by(födelseregion) |>
      dplyr::arrange(år,.by_group=TRUE) |>
      dplyr::mutate(kumulativ_summa = cumsum(forandring)) |>
      dplyr::ungroup()


    if(returnera_data == TRUE){
      assign("antal_forandring_prognos_kumulativ", antal_forandring_prognos_kumulativ, envir = .GlobalEnv)
    }

    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Kumulativ förändring, dvs förändringen summeras varje år"
    diagram_titel = paste0("Befolkningsprognos ",unique(befprognos_df$alder_grupper), " i ",unique(antal_forandring_prognos_kumulativ$region))
    diagramfilnamn <- paste0("befolkningsprognos_lan_",unique(antal_forandring_prognos_kumulativ$region),".png")

    if(ta_bort_diagramtitel){
      diagram_titel = ""
    }

    if(ta_bort_caption){
      diagram_capt = ""
    }

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(antal_forandring_prognos_kumulativ, år <= prognos_ar),
      skickad_x_var = "år",
      skickad_y_var = "kumulativ_summa",
      skickad_x_grupp = "födelseregion",
      manual_color = rev(valda_farger[1:2]),
      geom_position_stack = TRUE,
      diagram_titel = diagram_titel,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      x_axis_storlek = x_axis_storlek,
      diagram_capt =  diagram_capt,
      output_mapp = output_mapp_figur,
      stodlinjer_avrunda_fem = FALSE,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      manual_y_axis_title = "",
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  }



  if(returnera_figur == TRUE){
    return(gg_list)
  }

}

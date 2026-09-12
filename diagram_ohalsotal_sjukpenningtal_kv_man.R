diag_ohalsotal_sjukpenningtal <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                          diag_ohalsotal = TRUE,
                                          diag_sjukpenningtal = TRUE,
                                          output_mapp = NA,
                                          spara_diagrambildfil = FALSE,
                                          spara_dataframe_till_global_environment = FALSE){

  ## =================================================================================================================
  # Skript som skapar två diagram för ohälsotal och två diagram för sjukpenningtal i valt län.
  # Används i första hand i rapporten "Kvinnor och män i Dalarna"
  # Skapad av Jon Frank 2025-07-04
  # Reviderad av Peter Möller 2025-12-08
  # =============================================== Uttag ===============================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # "here" togs bort - laddades men användes aldrig. svenska_tecken_byt_ut()
  # (func_API.R) är en dubblett av rdverktyg::byt_ut_svenska_tecken() (samma
  # Latin-ASCII-translitterering) och används i stället.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("rdpostgres", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdpostgres")
  }
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  if (is.na(output_mapp) & spara_diagrambildfil){
    if (file.exists(rdverktyg::utskriftsmapp())) {
      output_mapp <- rdverktyg::utskriftsmapp()
    } else {
      stop("Parametern 'output_mapp' måste anges om en diagrambild ska sparas.")
    }

  }

  # # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/ohm-ohalsotal/SJPohttal.xlsx","https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/ohm-sjptal/SJPsjptal.xlsx")

  # # Med Peters nya skript
  flik_lista = list()
  gg_list = list()

  vald_region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region))

  vald_region_filnamn <- rdverktyg::byt_ut_svenska_tecken(tolower(vald_region_txt))

  lan_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(stringr::str_sub(region_vekt, 1, 2))$region))

  lan_filnamn <- rdverktyg::byt_ut_svenska_tecken(tolower(lan_txt))

  # skapa variabel med regionkoder för län + kommuner för vald region
  region_vekt_lan <- rdverktyg::hamtakommuner(lan = stringr::str_sub(region_vekt, 1, 2),
                                   tamedlan = TRUE,
                                   tamedriket = FALSE)

  if(diag_ohalsotal == TRUE){

    databas_finns <- tryCatch({
      # Din kodrad här
      ohalsotal_df <- rdpostgres::oppnadata_hamta("forsakringskassan", "ohalsotal",
                                               query = glue::glue("WHERE regionkod IN ({glue::glue_collapse(glue::glue(\"'{region_vekt_lan}'\"), sep = ', ')}) AND ålder = 'Samtliga 16-64 år' and kön != 'Kvinnor och män'"))
      TRUE  # Om det fungerar
    }, error = function(e) {
      FALSE # Om det blir fel, tex om det inte finns någon databas
    })


    if (!databas_finns) {

      manad_nyckel <- format(as.Date(paste0(1:12, "-01"), format = "%m-%d"), "%b")
      # Om datasetet inte finns i databasen oppna_data så hämtas det direkt från Försäkringskassan
      ohalsa_lista = rdverktyg::hamta_excel_dataset_med_url(path[1],skippa_rader = 2)
      ohalsotal_df <- dplyr::bind_rows(ohalsa_lista) |>
        dplyr::rename_with(~ tolower(.x)) |>
        dplyr::rename(ohalsotal = ohälsotalet) |>
        dplyr::rename(region = kommun) |>
        dplyr::mutate(region = dplyr::if_else(region == "Riket", "00 Riket", region)) |>
        tidyr::separate_wider_delim(region,
                             delim = " ",
                             names = c("regionkod", "region"),
                             too_many = "merge") |>
        dplyr::select(-c(län, kolumnnamn)) |>
        dplyr::filter(regionkod %in% region_vekt_lan,
               ålder == 'Samtliga 16-64 år',
               kön != 'Kvinnor och män') |>
        dplyr::mutate(månad_txt = manad_nyckel[as.integer(månad)]) |>
        dplyr::relocate(månad_txt, .after = månad)
    }

    # För att kunna skriva ut i caption hur många månader som det senaste året består av
    senaste_manad <- dplyr::first(unique(dplyr::pull(dplyr::mutate(ohalsotal_df, månad_namn = format(as.Date(paste0(år,"-", månad, "-01")), "%B")), månad_namn)))

    # skapa variabler med text som används i diagramtitlar och filnamn
    alder_txt <- stringr::str_remove(unique(ohalsotal_df$ålder), "Samtliga ")

    ar_txt <- max(ohalsotal_df$år)

    # Bearbetar data
    ohalsotal_df <- ohalsotal_df |>
            dplyr::group_by(år, regionkod, region, ålder, kön) |>
             dplyr::summarize(Ohälsotalet_medel = mean(ohalsotal), .groups = "drop")

    # Omvandla kolumnnamn

    if(spara_dataframe_till_global_environment) {
      assign("ohalsotal_df", ohalsotal_df, envir = .GlobalEnv)
    }

    # Ohälsotal tidsserie
    diagram_capt_ohälsa <- glue::glue("Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ohälsotalet: hur många dagar under en tolvmånadersperiod\nFörsäkringskassan betalar ut ersättning för nedsatt arbetsförmåga\ni förhållande till antalet försäkrade i åldrarna 16-64 år. Data för år {max(ohalsotal_df$år)} till och med {senaste_manad}.")

    diagramtitel <- paste0("Genomsnittligt ohälsotal (", alder_txt, ") per år i " , vald_region_txt)
    diagramfilnamn <- paste0("ohalsotal_", vald_region_filnamn,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(dplyr::mutate(ohalsotal_df, kön = tolower(kön)), regionkod %in% region_vekt),
      skickad_x_var = "år",
      skickad_y_var = "Ohälsotalet_medel",
      skickad_x_grupp = "kön",
      x_axis_lutning = 45,
      manual_color = rddiagram::diagramfarger("kon"),
      manual_y_axis_title = "",
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt_ohälsa,
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

    #Ohälsotal per kommun i samma län

    diagramtitel <- paste0("Genomsnittligt ohälsotal (16-64 år) i " , lan_txt," år ", ar_txt)
    diagramfilnamn <- paste0("ohalsotal_kommun_", lan_filnamn,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(dplyr::filter(ohalsotal_df, år == max(år)), kön = tolower(kön)),
      skickad_x_var = "region",
      skickad_y_var = "Ohälsotalet_medel",
      skickad_x_grupp = "kön",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      x_axis_lutning = 45,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 1,
      vand_sortering = TRUE,
      manual_color = rddiagram::diagramfarger("kon"),
      manual_y_axis_title = "",
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt_ohälsa,
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  }

  if(diag_sjukpenningtal == TRUE){


    databas_finns <- tryCatch({
      # Din kodrad här
      sjukpenningtal_df <- rdpostgres::oppnadata_hamta("forsakringskassan", "sjukpenningtal",
                                      query = glue::glue("WHERE regionkod IN ({glue::glue_collapse(glue::glue(\"'{region_vekt_lan}'\"), sep = ', ')}) AND ålder = 'Samtliga 16-64 år' and kön != 'Kvinnor och män'"))
      TRUE  # Om det fungerar
    }, error = function(e) {
      FALSE # Om det blir fel, tex om det inte finns någon databas
    })


    if (!databas_finns) {

      manad_nyckel <- format(as.Date(paste0(1:12, "-01"), format = "%m-%d"), "%b")
      # Om datasetet inte finns i databasen oppna_data så hämtas det direkt från Försäkringskassan
      sjp_lista = rdverktyg::hamta_excel_dataset_med_url(path[2],skippa_rader = 2)
      sjukpenningtal_df <- dplyr::bind_rows(sjp_lista) |>
        dplyr::rename_with(~ tolower(.x)) |>
        dplyr::rename(sjukpenningtal = `sjukpenningtal 1.0`) |>
        dplyr::rename(region = kommun) |>
        dplyr::mutate(region = dplyr::if_else(region == "Riket", "00 Riket", region)) |>
        tidyr::separate_wider_delim(region,
                             delim = " ",
                             names = c("regionkod", "region"),
                             too_many = "merge") |>
        dplyr::select(-c(län, kolumnnamn)) |>
        dplyr::filter(regionkod %in% region_vekt_lan,
               ålder == 'Samtliga 16-64 år',
               kön != 'Kvinnor och män') |>
        dplyr::mutate(månad_txt = manad_nyckel[as.integer(månad)]) |>
        dplyr::relocate(månad_txt, .after = månad)
    }

    # För att kunna skriva ut i caption hur många månader som det senaste året består av
    senaste_manad <- dplyr::first(unique(dplyr::pull(dplyr::mutate(sjukpenningtal_df, månad_namn = format(as.Date(paste0(år,"-", månad, "-01")), "%B")), månad_namn)))

    # Bearbetar data
    sjukpenningtal_df <- sjukpenningtal_df |>
      dplyr::group_by(år, regionkod, region, ålder, kön) |>
      dplyr::summarize(Sjukpenningtal_medel = mean(sjukpenningtal), .groups = "drop")


    if(spara_dataframe_till_global_environment) {
      assign("sjukpenningtal_df", sjukpenningtal_df, envir = .GlobalEnv)
    }

    # Sjukpenningtal tidsserie
    diagram_capt_sjukpenning <- glue::glue("Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring:Sjukpenningtalet är antalet dagar med sjukpenning och rehabiliteringspenning\nsom har betalats ut under en 12-månaders period. Den summan delas med antalet försäkrade i\nSverige som är i åldrarna 16–64 år. Data för år {max(sjukpenningtal_df$år)} till och med {senaste_manad}.")
    diagramtitel <- paste0("Genomsnittligt sjukpenningtal (16-64 år) per år i " , vald_region_txt)
    diagramfilnamn <- paste0("sjukpenningtal_", vald_region_filnamn,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(dplyr::filter(sjukpenningtal_df, regionkod %in% region_vekt), kön = tolower(kön)),
      skickad_x_var = "år",
      skickad_y_var = "Sjukpenningtal_medel",
      skickad_x_grupp = "kön",
      x_axis_lutning = 45,
      manual_color = rddiagram::diagramfarger("kon"),
      manual_y_axis_title = "",
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt_sjukpenning,
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

    # Sjukpenningtal för samtliga kommuner i länet för vald region
    diagramtitel <- paste0("Genomsnittligt sjukpenningtal (16-64 år) i " , lan_txt," år ", ar_txt)
    diagramfilnamn <- paste0("sjukpenningtal_kommun_", lan_filnamn,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(dplyr::filter(sjukpenningtal_df, år == max(år)), kön = tolower(kön)),
      skickad_x_var = "region",
      skickad_y_var = "Sjukpenningtal_medel",
      skickad_x_grupp = "kön",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      x_axis_lutning = 45,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 1,
      vand_sortering = TRUE,
      manual_color = rddiagram::diagramfarger("kon"),
      stodlinjer_avrunda_fem = TRUE,
      manual_y_axis_title = "",
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt_sjukpenning,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  }

  return(gg_list)

}

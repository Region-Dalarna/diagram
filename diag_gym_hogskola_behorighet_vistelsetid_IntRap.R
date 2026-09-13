diag_gymnasiebehorighet_mm <- function(region_vekt = "20", # Enbart ett i taget.
                                       diag_kon_gym= TRUE,
                                       diag_kon_hogskola = TRUE,
                                       diag_vistelsetid_gym = TRUE, # Ej könsuppdelat
                                       jmf_ar = 2018, # År som senaste år skall jämföras med vid könsuppdelat diagram                              # sökväg till logga som ska visas i diagrammet
                                       output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                       visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                       logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet.
                                       skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                       returnera_data_rmarkdown = FALSE,
                                       demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Två diagram för behörighet till gymnasiet och ett för behörighet till högskola som används i integrationsrapporten
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

  gg_list <- list()
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

  # hamta_data-repots hamta_integration_gymn_hogsk_behorighet_region_kon_
  # bakgrund_tid_IntGr8RikKON2_IntGr8LanKON2_scb.R (v1: AA/AA0003/
  # AA0003H/IntGr8RikKON2 + IntGr8LanKON2) hämtas här direkt via
  # v2-motsvarigheterna TAB1813 (riket) och TAB1807 (län) - vilken/vilka
  # tabeller som används styrs av regionkoden, samma mönster som i
  # diag_ek_standard_boende_valdeltagande_vistelsetid_IntRap.R.
  # SCB:s klartext för vistelsetidsintervall använder gemenhetstecken
  # (en-dash) - konverteras nedan.
  till_endash <- function(x) gsub("(?<=[0-9])-(?=[0-9 ])", "\u2013", x, perl = TRUE)

  hamta_behorighet <- function(region_vekt, kon_klartext, bakgrund_klartext, cont_klartext) {
    lan_koder <- region_vekt[region_vekt != "00"]
    resultat <- list()
    if (length(lan_koder) > 0) {
      resultat$lan <- pxweb2r::pxweb2_get_data(
        table = "TAB1807",
        query = list(
          Region = lan_koder,
          Kon = kon_klartext,
          Bakgrund = till_endash(bakgrund_klartext),
          ContentsCode = cont_klartext,
          Tid = "*"
        ))
    }
    if ("00" %in% region_vekt) {
      resultat$riket <- pxweb2r::pxweb2_get_data(
        table = "TAB1813",
        query = list(
          Kon = kon_klartext,
          Bakgrund = till_endash(bakgrund_klartext),
          ContentsCode = cont_klartext,
          Tid = "*"
        ))
    }
    dplyr::bind_rows(resultat) |>
      dplyr::rename(regionkod = region_kod, variabel = bakgrundsvariabel) |>
      dplyr::rename(!!cont_klartext := value) |>
      dplyr::select(-tabellinnehåll)
  }

  # Hämtar data
  behorighet_gym_df <- hamta_behorighet(
    region_vekt = region_vekt,
    kon_klartext = "*",
    bakgrund_klartext  = c("födelseregion: Sverige","samtliga utrikes födda","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"),
    cont_klartext = "Andel behöriga till gymnasium, procent") |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region),
           variabel = dplyr::case_when(
             variabel == "vistelsetid 0\u20131 år" ~ "0-1 år",
             variabel == "vistelsetid 2\u20133 år" ~ "2-3 år",
             variabel == "vistelsetid 4\u20139 år" ~ "4-9 år",
             variabel == "vistelsetid 10\u2013 år" ~ "10- år",
             variabel == "samtliga utrikes födda" ~ "Utrikes född",
             variabel == "födelseregion: Sverige" ~ "Inrikes född",
             TRUE ~ variabel
           ),
           kön = dplyr::case_when(
             kön == "kvinnor" ~ "flickor",
             kön == "män" ~ "pojkar",
             kön == "män och kvinnor" ~ "pojkar och flickor",
             TRUE ~ kön
           )) |>
    dplyr::rename(Andel_behoriga = `Andel behöriga till gymnasium, procent`)


  if(returnera_data_rmarkdown == TRUE){
    assign("behorighet_gym_df", behorighet_gym_df, envir = .GlobalEnv)
  }

  if(diag_kon_gym){

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    # syssgrad_df$bakgrundsvariabel <- factor(syssgrad_df$bakgrundsvariabel, levels = c("0-1 år","2-3 år",
    #                                                                                   "4-9 år","10- år",
    #                                                                                   "Inrikes född"))

    diagramtitel <- paste0("Andel behöriga till gymnasiet i ",unique(behorighet_gym_df$region))
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("behorighet_gymnasiet_jmf_ar.png")

    aldersgrupp_saknas <- dplyr::pull(dplyr::filter(behorighet_gym_df, kön != "pojkar och flickor",
                                                        år %in% c(jmf_ar,max(år)),
                                                        variabel %in% c("Utrikes född","Inrikes född"),
                                                        is.na(Andel_behoriga)), variabel)

    if(length(aldersgrupp_saknas)> 0){

      aldersgrupp_saknas = paste(aldersgrupp_saknas, collapse = ", ")

      diagram_capt <- glue::glue("Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Data saknas för vistelsetid {aldersgrupp_saknas}.")

    }else{
      diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    }

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(behorighet_gym_df,
                                          kön != "pojkar och flickor",
                                          variabel %in% c("Utrikes född","Inrikes född"),
                                          år %in% c(jmf_ar,max(år))),
      skickad_x_var = "år",
      skickad_y_var = "Andel_behoriga",
      skickad_x_grupp = "kön",
      manual_color = rddiagram::diagramfarger("kon"),
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      manual_y_axis_title = "procent",
      facet_grp = "variabel",
      facet_legend_bottom = TRUE,
      facet_scale = "fixed",
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      y_axis_100proc = TRUE,
      x_axis_lutning = 0,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")
  }

  if(diag_vistelsetid_gym){

    behorighet_gym_df <- dplyr::filter(behorighet_gym_df,
             variabel != "Utrikes född",
             region != "Riket")

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    behorighet_gym_df$variabel <- factor(behorighet_gym_df$variabel, levels = c("0-1 år","2-3 år",
                                                                                "4-9 år","10- år",
                                                                                "Inrikes född"))

    diagramtitel <- paste0("Andel behöriga till gymnasiet i Dalarna"," ",max(behorighet_gym_df$år)," uppdelat på vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("behorighet_gymnasiet_vistelsetid.png")

    aldersgrupp_saknas <- dplyr::pull(dplyr::filter(behorighet_gym_df, kön == "pojkar och flickor",
                                                       år == max(år),
                                                       is.na(Andel_behoriga)), variabel)

    if(length(aldersgrupp_saknas)> 0){


      aldersgrupp_saknas = paste(aldersgrupp_saknas, collapse = ", ")

      diagram_capt <- glue::glue("Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Data saknas för vistelsetid {aldersgrupp_saknas}.")

    } else{
      diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    }

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(behorighet_gym_df,
                                          kön == "pojkar och flickor",
                                          !(is.na(Andel_behoriga)),
                                          år == max(år)),
      skickad_x_var = "variabel",
      skickad_y_var = "Andel_behoriga",
      manual_color = rddiagram::diagramfarger("rus_sex"),
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      manual_y_axis_title = "procent",
      facet_scale = "fixed",
      manual_x_axis_title = "Vistelsetid i Sverige",
      y_axis_100proc = TRUE,
      x_axis_lutning = 0,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")
  }

  if(diag_kon_hogskola){

    behorighet_hogskola_df <- hamta_behorighet(
      region_vekt = region_vekt,
      kon_klartext = c("män","kvinnor"),
      bakgrund_klartext  = c("födelseregion: Sverige","samtliga utrikes födda"),
      cont_klartext = "Andel behöriga till högskola, procent") |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region),
             variabel = dplyr::case_when(variabel == "samtliga utrikes födda" ~ "Utrikes född",
                                  variabel == "födelseregion: Sverige" ~ "Inrikes född",
                                  TRUE ~ variabel
             )) |>
      dplyr::rename(Andel_behoriga = `Andel behöriga till högskola, procent`)


    if(returnera_data_rmarkdown == TRUE){
      assign("behorighet_hogskola_df", behorighet_hogskola_df, envir = .GlobalEnv)
    }

    aldersgrupp_saknas <- dplyr::pull(dplyr::filter(behorighet_hogskola_df, kön != "män och kvinnor",
                                                             variabel %in% c("Utrikes född","Inrikes född"),
                                                             år %in% c(jmf_ar,max(år)),
                                                             is.na(Andel_behoriga)), variabel)

    if(length(aldersgrupp_saknas)> 0){

      aldersgrupp_saknas = paste(aldersgrupp_saknas, collapse = ", ")

      diagram_capt <- glue::glue("Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Data saknas för vistelsetid {aldersgrupp_saknas}.")

    } else{
      diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    }

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    diagramtitel <- paste0("Andel behöriga till högskola i ",unique(behorighet_hogskola_df$region))
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- "behorighet_högksola_jmf_ar.png"

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(behorighet_hogskola_df,
                                          kön != "män och kvinnor",
                                          variabel %in% c("Utrikes född","Inrikes född"),
                                          år %in% c(jmf_ar,max(år))),
      skickad_x_var = "år",
      skickad_y_var = "Andel_behoriga",
      skickad_x_grupp = "kön",
      manual_color = rddiagram::diagramfarger("kon"),
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      manual_y_axis_title = "procent",
      facet_grp = "variabel",
      facet_legend_bottom = TRUE,
      facet_scale = "fixed",
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      y_axis_100proc = TRUE,
      x_axis_lutning = 0,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")
  }

  return(gg_list)

}

diagram_diverse_vistelsetid <-function(region_vekt = c("20"),# Max 1,
                                       diag_ek_standard = TRUE,
                                       alder_ek_standard = "20-64 år",			 #  Finns: "20- år", "20-64 år", "65- år", "20-29 år", "30-49 år", "50-64 år", "65-79 år", "80- år"
                                       jmf_ar = 2017, # År att jämföra senaste år med (för diagrammet med ekonomisk standard)
                                       diag_boendetyp = TRUE,
                                       diag_valdeltagande = TRUE,
                                       typ_av_val = "Valdeltagande i val till riksdag, procent",# Finns även: "Valdeltagande i val till region, procent", "Valdeltagande i val till kommun, procent",
                                       output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                       visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                       logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet.
                                       skriv_diagrambildfil = FALSE, # Sparar figuren till output_mapp_figur
                                       returnera_figur = TRUE, # Returnerar en figur
                                       diag_fargvekt = NA, # Gäller samtliga diagram
                                       returnera_dataframe_global_environment = FALSE) # Skall data returneras)
{

  ## =================================================================================================================
  # Diagram kopplade till tre figurer från integrationsrapporten
  # - Ekonomisk Ett standard: Facet-diagram (stapel) som jämför andel personer med låg ekonomisk standard i olika grupper
  # - Boendeform: Ett Stapeldiagram som visar boendeform baserat på vistelsetid (och inrikes födda)
  # - Valdeltagande: Upp till tre Stapeldiagram som visar valdeltagande i olika val (riksdag, region och kommun) baserat på vistelsetid (och inrikes födda)
  #
  # 2025-05-07: Felaktighet då variabeln val inte längre skapas. Ändrat i skriptet så nu funkar det (det sista diagrammet). /Jon
  # =================================================================================================================
  # Skript som skapar tre diagram kopplade till låg ekonomisk standard

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
  # dplyr/purrr/stringr/tidyr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("rus_sex")
  }

  # SCB:s PxWeb-klartexter för ålders-/vistelsetidsintervall använder
  # gemenhetstecken (en-dash "–") i stället för vanligt bindestreck i
  # v2-tabellerna nedan - annars matchar inte klartexten som skickas till
  # pxweb2r::pxweb2_get_data(, quiet = TRUE).
  till_endash <- function(x) gsub("(?<=[0-9])-(?=[0-9 ])", "\u2013", x, perl = TRUE)

  if(diag_ek_standard == TRUE){

    # hamta_data-repots hamta_ekonomisk_standard_region_alder_
    # sysselsattning_utlbakgrund_inkomsttyp_tid_HE0110__HE0110F_scb.R (v1:
    # HE/HE0110/HE0110F/TabVXDispI69) hämtas här direkt via
    # v2-motsvarigheten TAB1125. "9999" (senaste år) är en v1-specifik
    # sentinel som pxweb2r inte känner till - slås upp explicit i stället.
    senaste_ar_ekstd <- max(pxweb2r::pxweb2_get_values("TAB1125", "Tid", quiet = TRUE)$code)

    ekonomisk_standard_bakgrund_df <- pxweb2r::pxweb2_get_data(
      table = "TAB1125",
      query = list(
        Region = region_vekt,
        Alder = till_endash(alder_ek_standard),
        Sysselsattning = c("samtliga personer", "förvärvsarbetande", "icke förvärvsarbetande"),
        UtlBakgrund = c("utrikes födda","född i Sverige"),
        InkomstTyp = "disponibel inkomst per k.e. inkl. kapitalvinst",
        ContentsCode = "Inkomst < 60 procent",
        Tid = unique(c(as.character(jmf_ar), senaste_ar_ekstd))
      ), quiet = TRUE) |>
      dplyr::rename(regionkod = region_kod, `Inkomst < 60 procent` = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE),
             sysselsättning = paste0(toupper(substr(sysselsättning,1,1)),substr(sysselsättning,2,nchar(sysselsättning))),
             `utländsk/svensk bakgrund` = ifelse(`utländsk/svensk bakgrund` == "född i Sverige","inrikes födda",`utländsk/svensk bakgrund`)) |>
      dplyr::rename(bakgrund = `utländsk/svensk bakgrund`)

    if(returnera_dataframe_global_environment == TRUE){
      assign("lag_ek_standard_bakgrund_df", ekonomisk_standard_bakgrund_df, envir = .GlobalEnv)
    }

    ekonomisk_standard_bakgrund_df$sysselsättning <- factor(ekonomisk_standard_bakgrund_df$sysselsättning,
                                                            levels = c("Samtliga personer","Förvärvsarbetande","Icke förvärvsarbetande"))

    regioner <- paste(unique(ekonomisk_standard_bakgrund_df$region), collapse = "_")

    diagram_titel = paste0("Andel personer i hushåll med låg ekonomisk standard i ",unique(ekonomisk_standard_bakgrund_df$region))
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Med låg ekonomisk standard menas att\ninkomsten är lägre än 60 procent av medianen."
    diagramfilnamn <- paste0("diagram_lagekstandard_bakgrund_",regioner,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = ekonomisk_standard_bakgrund_df,
      skickad_x_var = "sysselsättning",
      skickad_y_var = "Inkomst < 60 procent",
      skickad_x_grupp = "år",
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfilnamn,
      diagram_capt = diagram_capt,
      diagram_titel = diagram_titel,
      facet_grp =  "bakgrund",
      x_axis_lutning = 0,
      facet_scale = "fixed",
      procent_0_100_10intervaller = TRUE,
      facet_legend_bottom = TRUE,
      manual_color = diag_fargvekt,
      manual_y_axis_title = "procent",
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      skriv_till_diagramfil = skriv_diagrambildfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")
  }

  if(diag_boendetyp == TRUE){

    # hamta_data-repots hamta_integration_boende_region_kon_bakgrund_tid_
    # IntGr6LanKon_IntGr6RikKon_scb.R (v1: AA/AA0003/AA0003D/IntGr6LanKon
    # + IntGr6RikKon) hämtas här direkt via v2-motsvarigheterna TAB1798
    # (län) och TAB1800 (riket) - vilken tabell som används styrs av
    # regionkoden (bara en region stöds, se parameterkommentaren ovan).
    # Originalets specialhantering som tog bort åren 1997-2023 ur
    # riks-uttaget är inte med här - båda v2-tabellerna täcker fortfarande
    # bara 1997-2023 (ingen nyare data finns alls i varken v1 eller v2),
    # så det filtret skulle ge noll rader varje gång.
    boende_tabell <- if (identical(region_vekt[1], "00")) "TAB1800" else "TAB1798"

    bakgrund_boende <- till_endash(c("födelseregion: Sverige","vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år"))
    cont_boende <- c("Andel boende i egna hem, procent", "Andel boende i hyresrätt, procent", "Andel boende i bostadsrätt, procent")

    hamta_boende <- function(tid_koder) {
      query <- list(
        Bakgrund = bakgrund_boende,
        Kon = "män och kvinnor",
        ContentsCode = cont_boende,
        Tid = tid_koder)
      if (boende_tabell == "TAB1798") query <- c(list(Region = region_vekt), query)
      pxweb2r::pxweb2_get_data(table = boende_tabell, query = query, quiet = TRUE)
    }

    senaste_ar_boende <- max(pxweb2r::pxweb2_get_values(boende_tabell, "Tid", quiet = TRUE)$code)

    # Av oklar anledning saknas mycket data för senaste år. Jag gör därför ett enklare uttag för senaste år och om det saknas data väljs året innan
    boende_test <- hamta_boende(senaste_ar_boende)

    # Check if there exists na:s in any of the variables in the datafram boende_test
    tid_koder_boende <- if (is.null(boende_test) || anyNA(boende_test)) as.character(as.integer(senaste_ar_boende) - 1) else senaste_ar_boende

    # Hämtar data
    boende_df <- hamta_boende(tid_koder_boende) |>
      dplyr::rename(regionkod = region_kod, varde = value) |>
      dplyr::rename(bakgrund_kod = tabellinnehåll) |>
      dplyr::rename(variabel = bakgrundsvariabel, bakgrund = bakgrund_kod) |>
      dplyr::mutate(variabel = dplyr::case_when(
        variabel == "vistelsetid 0\u20131 år" ~ "0-1 år",
        variabel == "vistelsetid 2\u20133 år" ~ "2-3 år",
        variabel == "vistelsetid 4\u20139 år" ~ "4-9 år",
        variabel == "vistelsetid 10\u2013 år" ~ "10- år",
        variabel == "födelseregion: Sverige" ~ "Inrikes född",
        TRUE ~ variabel
      ),
      bakgrund = dplyr::case_when(
        bakgrund == "Andel boende i egna hem, procent" ~ "Äganderätt",
        bakgrund == "Andel boende i hyresrätt, procent" ~ "Hyresrätt",
        bakgrund == "Andel boende i bostadsrätt, procent" ~ "Bostadsrätt",
        TRUE ~ bakgrund
      )
      ) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))

    boende_df <- tidyr::pivot_wider(boende_df, names_from=bakgrund, values_from=varde) |>
      dplyr::mutate(Okänd = 100 - Äganderätt - Hyresrätt - Bostadsrätt) |>
      tidyr::pivot_longer(cols=6:9,names_to = "bakgrund",values_to = "varde")



    if(returnera_dataframe_global_environment == TRUE){
      assign("boendtyp_df", boende_df, envir = .GlobalEnv)
    }



    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    boende_df$variabel <- factor(boende_df$variabel, levels = c("0-1 år","2-3 år",
                                                                "4-9 år","10- år",
                                                                "Inrikes född"))

    boende_df$bakgrund <- factor(boende_df$bakgrund, levels = c("Okänd","Hyresrätt","Bostadsrätt","Äganderätt"))

    diagramtitel <- paste0("Boende per upplåtelseform i ",unique(boende_df$region)," ",max(boende_df$år)," efter vistelsetid")
    #diagramtitel <- str_wrap(diagramtitel,60)
    diagramfilnamn <- paste0("boendetyp_vistelsetid_inrikes.png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(boende_df, kön != "totalt"),
      skickad_x_var = "variabel",
      skickad_y_var = "varde",
      skickad_x_grupp = "bakgrund",
      # manual_x_axis_text_vjust=0.9,
      manual_color = diag_fargvekt,
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      manual_y_axis_title = "procent",
      manual_x_axis_title = "Vistelsetid i Sverige",
      geom_position_stack = TRUE,
      legend_vand_ordning = TRUE,
      y_axis_100proc = TRUE,
      x_axis_lutning = 0,
      lagg_pa_logga = visa_logga_i_diagram,
      logga_path = logga_sokvag,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")
  }

  if(diag_valdeltagande == TRUE){

    # hamta_data-repots hamta_integration_valdeltagande_region_bakgrund_
    # kon_tid_IntGr11Riket1_IntGr11Lan1_IntGr11Kom1_scb.R (v1:
    # AA/AA0003/AA0003J/IntGr11Riket1 + IntGr11Lan1 + IntGr11Kom1) hämtas
    # här direkt via v2-motsvarigheterna TAB4255 (riket), TAB4268 (län) och
    # TAB5211 (kommun) - vilken tabell som används styrs av regionkodens
    # längd/värde.
    val_tabell <- if (identical(region_vekt[1], "00")) "TAB4255" else if (nchar(region_vekt[1]) == 4) "TAB5211" else "TAB4268"

    bakgrund_valdeltagande <- till_endash(c("födelseregion: Sverige","samtliga utrikes födda", "vistelsetid 10- år", "vistelsetid < 10 år"))
    bakgrund_valdeltagande <- stringr::str_replace(bakgrund_valdeltagande, "< 10", "<10")  # v2-tabellernas etikett saknar mellanslag efter "<"

    query_val <- list(
      Bakgrund = bakgrund_valdeltagande,
      Kon = "män och kvinnor",
      ContentsCode = "*",
      Tid = "*")
    if (val_tabell != "TAB4255") query_val <- c(list(Region = region_vekt), query_val)

    # Hämtar data
    valdeltagande_df <- pxweb2r::pxweb2_get_data(table = val_tabell, query = query_val, quiet = TRUE) |>
      dplyr::rename(regionkod = dplyr::any_of("region_kod"), variabel = bakgrundsvariabel, val = tabellinnehåll, varde = value) |>
      dplyr::mutate(regionkod = dplyr::if_else(is.na(regionkod), region_vekt[1], regionkod)) |>
      tidyr::pivot_wider(names_from = val, values_from = varde) |>
      dplyr::mutate(variabel = dplyr::case_when(
        variabel == "vistelsetid <10 år" ~ "< 10 år",
        variabel == "vistelsetid 10\u2013 år" ~ "10- år",
        variabel == "födelseregion: Sverige" ~ "Inrikes född",
        TRUE ~ variabel
      )) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))

    if(returnera_dataframe_global_environment == TRUE){
      assign("valdeltagande_df", valdeltagande_df, envir = .GlobalEnv)
    }

    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

    # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
    valdeltagande_df$variabel <- factor(valdeltagande_df$variabel, levels = c("< 10 år","10- år",
                                                                              "samtliga utrikes födda","Inrikes född"))

    skapa_diagram <- function(val_vilket){

      val_utan_procent <- stringr::str_extract(val_vilket, "^[^,]+")
      diagramtitel <- paste0(val_utan_procent," i ",unique(valdeltagande_df$region)," efter vistelsetid")
      #diagramtitel <- str_wrap(diagramtitel,60)
      diagramfilnamn <- paste0(gsub(" ", "_", val_utan_procent),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = valdeltagande_df,
        skickad_x_var = "variabel",
        skickad_y_var = val_vilket,
        skickad_x_grupp = "år",
        # manual_x_axis_text_vjust=0.9,
        manual_color = diag_fargvekt,
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        manual_y_axis_title = "procent",
        manual_x_axis_title = "Vistelsetid i Sverige",
        y_axis_100proc = TRUE,
        x_axis_lutning = 0,
        output_mapp = output_mapp_figur,
        filnamn_diagram = diagramfilnamn,
        lagg_pa_logga = visa_logga_i_diagram,
        logga_path = logga_sokvag,
        skriv_till_diagramfil = skriv_diagrambildfil)


      ett_diagram <- list(gg_obj)
      names(ett_diagram) <- stringr::str_remove(diagramfilnamn, "\\.png")
      return(ett_diagram)
    }

    retur_list <- purrr::flatten(purrr::map(typ_av_val, ~skapa_diagram(val_vilket = .x)))

    gg_list <- c(gg_list, retur_list)

  }


  if(returnera_figur == TRUE){
    return(gg_list)
  }

}

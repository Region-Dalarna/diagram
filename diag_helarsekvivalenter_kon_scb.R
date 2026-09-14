diag_helarsekvivalenter <- function(
    region_vekt = "20",			# Val av region.
    kon_klartext = "män och kvinnor totalt",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "män och kvinnor totalt"
    aldersgrupp_klartext = "20–64 år",			 #  Finns: "20–64 år", "20–65 år"
    cont_klartext = c("Sjukpenning", "Sjuk- och aktivitetsersersättning", "Arbetslöshetsersättning", "Arbetsmarknadsåtgärder", "Ekonomiskt bistånd", "Etableringsersättning"),			 #  Finns: "Sjukpenning", "Sjuk- och aktivitetsersersättning", "Arbetslöshetsersättning", "Arbetsmarknadsåtgärder", "Ekonomiskt bistånd", "Etableringsersättning", "Summa helårsekvivalenter", "Folkmängd", "Andel av befolkningen"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2014M01"-"2025M12"
    gruppera_namn = NA,              # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    visa_dataetiketter = FALSE,
    diagram_farger = NA,
    skapa_kortnamn_for_lan = TRUE,                # tar bort " län" från alla länsnamn
    skapa_facet_diagram = TRUE,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    output_mapp = NA,
    skriv_diagramfil = TRUE,
    ggobjektfilnamn_utan_tid = FALSE,    # om TRUE så tas inte tex året med i filnamnet, vilket passar bättre i vissa sammanhang när man vill använda objektsnamnet utan att ändra vid varje uppdatering
    excelfil_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "helarsekvivalenter.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {

  # ==============================================================================================================================
  #
  # Skriver ut diagram med tidsserier för helårsekvivalenter. Det är samtliga sociala ersättningar och bidrag som standardinställning.
  # Man kan ta ut län fast det egentligen inte finns, det grupperas ihop i skriptet. Man kan också sätta ihop alla medskickade
  # regioner till en egen aggregerad geografisk indelning (som tex. Norra Mellansverige)
  #
  # Man kan skriva ut flera regioner som ett facetdiagram eller var och en för sig, det styrs med skapa_facet_diagram som sätts till
  # TRUE eller FALSE
  #
  # ==============================================================================================================================


  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/helarsekvivalenter_Dalarna_ar2014M01_2024M06.png")
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

  if (all(is.na(diagram_farger))) {
    diagram_farger <- rddiagram::diagramfarger("rus_sex")
  }

  if (all(is.na(output_mapp))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_mapp <- rdverktyg::utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }

  regionnyckel <- rdverktyg::hamtaregtab()

  regionkoder_lan <- region_vekt[nchar(region_vekt) == 2 & region_vekt != "00"]
  regionkoder_kommun <- region_vekt[nchar(region_vekt) == 4 | region_vekt == "00"]

  alla_lans_kommunkoder <- if (length(regionkoder_lan) > 0) rdverktyg::hamtakommuner(lan = regionkoder_lan, tamedlan = FALSE, tamedriket = FALSE) else NULL

  regionkoder_alla <- unique(c(alla_lans_kommunkoder, regionkoder_kommun))

  # hamta_data-repots hamta_helarsekvivalenter_region_kon_aldersgrupp_
  # tid_HE0000T02N2_scb.R (v1: HE/HE0112/HE0000T02N2) hämtas här direkt
  # via v2-motsvarigheten TAB1386.
  helarsekvivalenter_df <- pxweb2r::pxweb2_get_data(
    table = "TAB1386",
    query = list(
      Region = regionkoder_alla,
      Kon = kon_klartext,
      Aldersgrupp = aldersgrupp_klartext,
      ContentsCode = cont_klartext,
      Tid = tid_koder
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)

  helarsekv_alla_df <- tibble::tibble()

  if (length(regionkoder_lan) > 0) {
    helarsekv_lan <- helarsekvivalenter_df |>
      dplyr::mutate(lanskod = stringr::str_sub(regionkod, 1, 2)) |>
      dplyr::group_by(dplyr::across(-c(regionkod, region, varde))) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::rename(regionkod = lanskod) |>
      dplyr::left_join(regionnyckel, by = "regionkod")
  } else helarsekv_lan <- NULL

  # Lägg ihop dataframe för kommuner/riket och län
  helarsekv_alla_df <- helarsekvivalenter_df |>
    dplyr::filter(regionkod %in% regionkoder_kommun) |>
    dplyr::bind_rows(helarsekv_lan) |>
    rdverktyg::manader_bearbeta_scbtabeller()

  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!is.na(gruppera_namn)) {
    helarsekv_alla_df <- helarsekv_alla_df |>
      dplyr::group_by(dplyr::across(-c(regionkod, region, varde))) |>
      dplyr::summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "gg",
             region = gruppera_namn) |>
      dplyr::relocate(region, .before = 1) |>
      dplyr::relocate(regionkod, .before = region)

    region_vekt <- "gg"
  }


  if (skapa_kortnamn_for_lan) helarsekv_alla_df <- dplyr::mutate(helarsekv_alla_df, region = rdverktyg::skapa_kortnamn_lan(region))

  aldersgrp <- unique(helarsekv_alla_df$åldersgrupp)


  skapa_diagram <- function(region_kod) {

    chart_df <- dplyr::filter(helarsekv_alla_df, regionkod %in% region_kod)

    vald_region <- rdverktyg::list_komma_och(dplyr::pull(dplyr::distinct(chart_df, region)))

    facet_txt <- if (length(unique(chart_df$region)) > 1) "" else glue::glue(" i {vald_region}")
    konsuppdelat <- length(unique(chart_df$kön)) > 1
    kon_txt <- if (konsuppdelat) "kon_" else ""

    diagramtitel <- glue::glue("Helårsekvivalenter för invånare {aldersgrp}{facet_txt}")
    diagramundertitel <- " - motsvarar individer som på heltid försörjs med sociala ersättningar och bidrag"
    diagramfil <- glue::glue("helarsekvivalenter_{kon_txt}{paste0(vald_region, collapse = '_')}_ar{dplyr::first(chart_df$tid)}_{dplyr::last(chart_df$tid)}.png")


    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = chart_df,
      skickad_x_var = "månad_år",
      skickad_y_var = "varde",
      skickad_x_grupp = "variabel",
      x_axis_visa_var_xe_etikett = 6,
      geom_position_stack = TRUE,
      legend_vand_ordning = TRUE,
      diagram_titel = diagramtitel,
      diagram_undertitel = diagramundertitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      dataetiketter = visa_dataetiketter,
      manual_y_axis_title = "helårsekvivalenter",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diagram_farger,
      output_mapp = output_mapp,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      facet_grp = if (konsuppdelat) "kön" else if (length(unique(chart_df$region)) > 1) "region" else NULL,
      facet_scale = if (konsuppdelat) "free_x" else "free",
      facet_legend_bottom = TRUE,
      skriv_till_diagramfil = skriv_diagramfil
    )

    gg_list <- list(gg_obj)
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.[^.]+$")

    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(names(gg_list)[[length(gg_list)]], "_ar.*$")
    }

    return(gg_list)

  } # slut skapa_diagram-funktion

  if (skapa_facet_diagram & length(unique(helarsekv_alla_df$kön)) == 1) {
    retur_list <- skapa_diagram(region_vekt)

  } else {
    retur_list <- purrr::flatten(purrr::map(unique(region_vekt), ~ skapa_diagram(.x)))

  }

  return(retur_list)

}

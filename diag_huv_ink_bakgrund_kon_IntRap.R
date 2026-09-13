diag_huv_ink_kalla_bakgrund_scb <- function(region = "20", # Enbart ett i taget.
                                            visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                            logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                            output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                            skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                            diag_totalt = TRUE,
                                            diag_kon = TRUE,
                                            alder_klartext = "20-64 år",			 #  Finns: "15-19 år", "20-24 år", "25-54 år", "55-64 år", "65-74 år", "15-74 år", "16-64 år", "16-65 år", "20-64 år", "20-65 år"
                                            returnera_data_rmarkdown = FALSE,
                                            demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Två diagram för huvudsaklig inkomstkälla (exkl förvärvsarbetande) kopplat till bakgrund och kön
  # Från integrationsrapporten (därav IntRap i namnet)
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

  # hamta_data-repots hamta_bas_huvink_region_huvudfot1m_kon_alder_
  # fodelseregion_tid_ArbStatFoT1_scb.R (v1: AM/AM0210/AM0210A/
  # ArbStatFoT1) hämtas här direkt via v2-motsvarigheten TAB1784 - samma
  # tabell som redan verifierats i diag_ek_stod_bakgrund.R. Ålders-
  # klartexten konverteras till gemenhetstecken ("20-64 år" ->
  # "20–64 år") för att matcha v2-tabellens etiketter.
  huvud_ink_df <- pxweb2r::pxweb2_get_data(
    table = "TAB1784",
    query = list(
      Region = region,
      HuvudFoT1m = "*",
      Kon = "*",
      Alder = gsub("(?<=[0-9])-(?=[0-9 ])", "\u2013", alder_klartext, perl = TRUE),
      Fodelseregion = "*",
      ContentsCode = "antal totalt",
      Tid = "9999"
    )) |>
    dplyr::rename(regionkod = region_kod, `antal totalt` = value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(`huvudsaklig inkomstkälla` = dplyr::case_when(
      grepl("arbete", `huvudsaklig inkomstkälla`) ~ "Arbete",
      grepl("arbetslöshet", `huvudsaklig inkomstkälla`) ~ "Arbetslöshet",
      grepl("studier", `huvudsaklig inkomstkälla`) ~ "Studier",
      grepl("^pension$", `huvudsaklig inkomstkälla`) ~ "Pension",
      grepl("långvarigt nedsatt arbetsförmåga", `huvudsaklig inkomstkälla`) ~ "Långvarigt nedsatt arbetsförmåga",
      grepl("sjukdom", `huvudsaklig inkomstkälla`) ~ "Ersättning vid sjukdom",
      grepl("föräldraledighet|närståendeomvårdnad", `huvudsaklig inkomstkälla`) ~ "Föräldraledighet m.m.",
      grepl("ekonomiskt stöd", `huvudsaklig inkomstkälla`) ~ "Ekonomiskt stöd",
      grepl("saknar ersättningar", `huvudsaklig inkomstkälla`) ~ "Saknar ersättningar",
      TRUE ~ NA_character_
    )) |>
    dplyr::filter(födelseregion != "totalt") |>
    dplyr::group_by(region, månad, ålder, kön, födelseregion,`huvudsaklig inkomstkälla`) |>
    dplyr::summarize(`antal totalt` = sum(`antal totalt`, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::mutate(andel = round((`antal totalt` / sum(`antal totalt`, na.rm = TRUE))*100,1)) |>
    dplyr::ungroup() |>
    dplyr::filter(`huvudsaklig inkomstkälla` != "Arbete") |>
    rdverktyg::manader_bearbeta_scbtabeller()

  if(returnera_data_rmarkdown == TRUE){
    assign("huv_ink_df", huvud_ink_df, envir = .GlobalEnv)
  }

  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  huvud_ink_df$`huvudsaklig inkomstkälla` <- factor(huvud_ink_df$`huvudsaklig inkomstkälla`, levels = c("Studier","Föräldraledighet m.m.",
                                                                                                        "Ersättning vid sjukdom","Arbetslöshet",
                                                                                                        "Långvarigt nedsatt arbetsförmåga","Ekonomiskt stöd",
                                                                                                        "Pension","Saknar ersättningar"))

  diagramtitel <- paste0("Huvudsaklig inkomstkälla (exkl förvärvsarbetande) i ",rdverktyg::skapa_kortnamn_lan(unique(huvud_ink_df$region))," i ",unique(huvud_ink_df$månad), " ", unique(huvud_ink_df$år), ", ",unique(huvud_ink_df$ålder))


  if(diag_totalt){
    diagramfilnamn <- paste0("huvud_ink_bakgrund_",rdverktyg::skapa_kortnamn_lan(unique(huvud_ink_df$region)),".png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(huvud_ink_df, kön == "totalt"),
      skickad_x_var = "huvudsaklig inkomstkälla",
      skickad_y_var = "andel",
      skickad_x_grupp = "födelseregion",
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = rev(rddiagram::diagramfarger("rus_sex")[1:2]),
      manual_y_axis_title = "procent",
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      y_axis_100proc = FALSE,
      x_axis_lutning = 45,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      lagg_pa_logga = visa_logga_i_diagram,
      skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")
  }

  if(diag_kon){

    diagramfilnamn <- paste0("huvud_ink_kon_bakgrund_",rdverktyg::skapa_kortnamn_lan(unique(huvud_ink_df$region)),".png")

    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(huvud_ink_df, kön != "totalt"),
      skickad_x_var = "huvudsaklig inkomstkälla",
      skickad_y_var = "andel",
      skickad_x_grupp = "kön",
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = rev(rddiagram::diagramfarger("kon")),
      manual_y_axis_title = "procent",
      diagram_titel = diagramtitel,
      diagram_capt =  diagram_capt,
      facet_grp = "födelseregion",
      facet_scale = "fixed",
      y_axis_100proc = FALSE,
      x_axis_lutning = 45,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfilnamn,
      lagg_pa_logga = visa_logga_i_diagram,
      skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")
  }

  return(gg_list)

}

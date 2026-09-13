diag_inkomst_bakgrund_scb <- function(region = "20", # Enbart ett i taget.
                                      visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                      logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                      output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                      inkomst_typ = "Medianinkomst, tkr", # Finns "Medianinkomst, tkr", "Medelinkomst, tkr". Max 1 åt gången
                                      skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                      alder_klartext = "20–64 år",			 #  Finns: "20+ år", "20–64 år", "20–65 år", "65+ år", "66+ år". Max 1 åt gången
                                      returnera_data_rmarkdown = FALSE,
                                      demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Ett diagram för förvärvsinkomst kopplad till bakgrund (vistelsetid)
  # Från integrationsrapporten (därav IntRap i namnet)
  #
  # Ändrat felaktighet med för kort - och uppdaterat till nya PXweb - Jon 2026-09-10
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

  # Tabellen TAB5278 (v2) har redan använts direkt (Tid = "9999" förstås
  # av pxweb2r::pxweb2_get_data() som "senaste period" via dess egen
  # latest_period_code-inställning), så inget bakomliggande hamta_data-
  # skript att ersätta här.
  forvarvsinkomst_df <- pxweb2r::pxweb2_get_data(
    table = "TAB5278",
    query = list(
      Region = region,
      Kon = "*",
      Fodelseregion = "*",
      VistelsetidUF = "*",
      Alder = alder_klartext,
      ContentsCode = inkomst_typ,
      Tid ="9999"
    )
  ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::rename(!!inkomst_typ := value,
                     regionkod = region_kod,
                     vistelsetid = `vistelsetid år`) |>
    dplyr::mutate(vistelsetid = ifelse(födelseregion == "födda i Sverige","Inrikes född",vistelsetid)) |>
    dplyr::filter(födelseregion %in% c("födda i Sverige","utrikes födda"),
                 vistelsetid != "samtliga") |>
    dplyr::mutate(vistelsetid = dplyr::case_when(
            vistelsetid == "1–2 år i Sverige" ~ "1-2 år",
            vistelsetid == "3–4 år i Sverige" ~ "3-4 år",
            vistelsetid == "5–9 år i Sverige" ~ "5-9 år",
            vistelsetid == "10–19 år i Sverige" ~ "10-19 år",
            vistelsetid == "20– år i Sverige" ~ "20- år",
            TRUE ~ vistelsetid
          ))


  if(returnera_data_rmarkdown == TRUE){
    assign("forvarvsinkomst_df", forvarvsinkomst_df, envir = .GlobalEnv)
  }

  gg_list <- list()


  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nSammanräknad förvärvsinkomst, dvs. alla skattepliktiga inkomster före skatt (dock ej kapitalinkomster)."

  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  forvarvsinkomst_df$vistelsetid <- factor(forvarvsinkomst_df$vistelsetid, levels = c("1-2 år","3-4 år",
                                                                                      "5-9 år","10-19 år",
                                                                                      "20- år","Inrikes född"))
  variabel <- sub(",.*", "", dplyr::last(names(forvarvsinkomst_df)))
  diagramtitel <- paste0(variabel," (", unique(forvarvsinkomst_df$ålder),") i Dalarna ",max(forvarvsinkomst_df$år)," efter vistelsetid")
  #diagramtitel <- str_wrap(diagramtitel,60)
  diagramfilnamn <- paste0(variabel,"_bakgrund.png")

  # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = dplyr::filter(forvarvsinkomst_df, kön != "totalt"),
    skickad_x_var = "vistelsetid",
    skickad_y_var = dplyr::last(names(forvarvsinkomst_df)),
    skickad_x_grupp = "kön",
    # manual_x_axis_text_vjust=0.9,
    manual_color = rddiagram::diagramfarger("kon"),
    diagram_titel = diagramtitel,
    diagram_capt =  diagram_capt,
    manual_x_axis_title = "Vistelsetid i Sverige",
    y_axis_100proc = FALSE,
    x_axis_lutning = 0,
    output_mapp = output_mapp,
    filnamn_diagram = diagramfilnamn,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_diagrambildfil)


  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")

  return(gg_list)

}

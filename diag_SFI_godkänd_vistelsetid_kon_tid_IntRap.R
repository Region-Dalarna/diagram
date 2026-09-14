diag_SFI_bakgrund <- function(region = "20", # Enbart ett i taget.
                              diag_vistelsetid_senaste_ar = TRUE,
                              diag_vistelsetid_tidsserie = TRUE,
                              visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                              logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                              valda_farger = NA,
                              output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                              skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                              returnera_data_rmarkdown = FALSE,
                              demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  #
  # Två diagram godkända i SFI efter vistelsetid i Sverige
  # Uppdaterat med ny version av PXweb - Jon 2026-09-08. Lagt till så att PXweb2 hämtas via paket 2026-09-09
  # Färdigmigrerad till pxweb2r/rddiagram/rdverktyg (fullt namespace, ingen source()/p_load()) - Claude 2026-09-13
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

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse). Anropas med fullt
  # namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  if (all(is.na(valda_farger))) valda_farger <- rddiagram::diagramfarger("rus_sex")

  gg_list <- list()

  valt_lan <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region)$region)

  # Nya PXweb
  SFI_df <- pxweb2r::pxweb2_get_data(
    table = "TAB1808",
    query = list(
      Region = region,
      Kon = "*",
      Bakgrund = c("utbildningsnivå: förgymnasial utbildning", "utbildningsnivå: gymnasial utbildning", "utbildningsnivå: eftergymnasial utbildning"),
      ContentsCode = "Vistelsetid för godkända i sfi, median i antal dagar",
      Tid = "*"
    ), quiet = TRUE) |>
    dplyr::rename(variabel = bakgrundsvariabel) |>
    dplyr::mutate(variabel = sub("utbildningsnivå: ", "", variabel),
           variabel = stringr::str_to_sentence(variabel)) |>
    dplyr::select(-tabellinnehåll)


  if(returnera_data_rmarkdown == TRUE){
    assign("SFI_df", SFI_df, envir = .GlobalEnv)
  }



  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

  #Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  SFI_df$variabel <- factor(SFI_df$variabel, levels = unique(SFI_df$variabel))
  #
  # # Namn på variabel som används i diagramtitel
  # variabel_namn <- sub("sfi", "SFI", sub(",.*", "", last(names(SFI_df))))

  if(diag_vistelsetid_senaste_ar){

    diagramtitel <- glue::glue("Vistelsetid för godkända i SFI i {valt_lan} år {max(SFI_df$år)}")
    diagramfilnamn <- paste0("sfi_vistelsetid_senastear_",valt_lan,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(SFI_df, kön != "män och kvinnor", år == max(år)),
                                 skickad_x_var = "variabel",
                                 skickad_y_var = dplyr::last(names(SFI_df)),
                                 skickad_x_grupp = "kön",
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 facet_scale = "fixed",
                                 x_axis_lutning = 0,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  }

  if(diag_vistelsetid_tidsserie){

    diagramtitel <- glue::glue("Vistelsetid för godkända i SFI i {valt_lan}")
    diagramfilnamn <- paste0("sfi_vistelsetid_tidsserie_",valt_lan,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(SFI_df, kön == "män och kvinnor"),
                                 skickad_x_var = "år",
                                 skickad_y_var = dplyr::last(names(SFI_df)),
                                 skickad_x_grupp = "variabel",
                                 manual_color = valda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_axis_lutning = 45,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 skriv_till_diagramfil = skriv_diagrambildfil)


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  return(gg_list)

}

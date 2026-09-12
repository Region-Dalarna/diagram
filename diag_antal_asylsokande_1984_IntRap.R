diagram_asylsokande_tidsserie <-function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                         spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                         visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                         logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet.
                                         start_ar = 1984, # Välj ett startår, s
                                         returnera_figur = TRUE, # Returnerar en figur
                                         valda_farger = rddiagram::diagramfarger("rus_sex"),
                                         returnera_data = FALSE) # Skall data returneras)
{

  ## =================================================================================================================
  # En tidsserie för antal asylsökande i Sverige från 1984 och framåt.
  # Data för de första åren finns inte via API utan hämtas från en Excelfil som ligger i en mapp på G:
  # Används i integrationsrapporten
  # =================================================================================================================
  # Bara paket, ingen source() mot funktioner-repot och inget p_load.
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/stringr följer med som beroenden till rddiagram.

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()

  input_mapp <- "G:/skript/projekt/data/integration"
  files <- list.files(input_mapp, pattern = "asyl", full.names = TRUE)

  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]

  asyl_1984 <- openxlsx::read.xlsx(latest_file,startRow = 2) |>
    dplyr::rename(år = "År",
           Antal = "Asylsökande")

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101P/AsylsokandeN/
  asylsokande <- pxweb2r::pxweb2_get_data(
    table = "TAB5183",
    query = list(
      Medborgarskapsland = "SAMTL",
      Kon = "1+2",
      ContentsCode = "000003WL",
      Period = "hel",
      Tid = "*"
    )) |>
    dplyr::rename(Antal = value) |>
    dplyr::select(år,Antal)

  asylsokande_df <- rbind(dplyr::filter(asyl_1984, !(år %in% unique(asylsokande$år))),asylsokande) |>
    dplyr::filter(is.na(Antal) == FALSE) |>
      dplyr::filter(år >= start_ar)

  if(returnera_data == TRUE){
    assign("asylsokande_df", asylsokande_df, envir = .GlobalEnv)
  }

  diagram_capt = "Källa: Migrationsverket, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  diagram_titel = "Antal asylsökande i Sverige"
  diagramfilnamn <- "asylsokande_antal.png"

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = asylsokande_df,
    skickad_x_var = "år",
    skickad_y_var = "Antal",
    manual_color = valda_farger,
    diagram_titel = diagram_titel,
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    diagram_capt =  diagram_capt,
    output_mapp = output_mapp_figur,
    stodlinjer_avrunda_fem = TRUE,
    x_axis_visa_var_xe_etikett = 2,
    lagg_pa_logga = visa_logga_i_diagram,
    logga_path = logga_sokvag,
    manual_y_axis_title = "",
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = spara_figur)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

  if(returnera_figur == TRUE){
    return(gg_list)
  }

}

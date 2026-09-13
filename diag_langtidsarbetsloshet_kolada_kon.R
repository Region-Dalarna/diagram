diagram_langtidsarbetslohet_tidsserie <-function(region_vekt = "20",
                                                 output_mapp_data = NA, # Outputmapp för data
                                                 output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                                 spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                                 returnera_figur = TRUE, # Returnerar en figur
                                                 valda_farger = NA,
                                                 returnera_data = FALSE) # Skall data returneras)
{

  ## =================================================================================================================

  # =================================================================================================================
  # Bara paket, ingen source() mot funktioner-repot och inget p_load().
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället.
  # Ingen SCB-hämtning här - datan hämtas från Kolada (via rdverktyg::
  # hamta_kolada_df(), som i sin tur kräver paketet rKolada).
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("rKolada", quietly = TRUE)) install.packages("rKolada")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  if (all(is.na(valda_farger))) valda_farger <- rddiagram::diagramfarger("kon")

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # Hämtar data för långtidsarbetslöshet
  långtidsarbetslöshet <- rdverktyg::hamta_kolada_df(kpi_id = c("N03926"),
                                          valda_kommuner = region_vekt,
                                          valda_ar = 2011:2100,
                                          konsuppdelat = TRUE) |>
    dplyr::mutate(kon = tolower(kon))

  if(returnera_data == TRUE){
    assign("långtidsarbetslöshet", långtidsarbetslöshet, envir = .GlobalEnv)
  }

  diagram_capt <- "Källa: Arbetsförmedlingen (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 25-64 år (årsmedelvärde år T) som varit öppet arbetslösa eller i program med aktivitetsstöd i minst sex månader,\ndividerat med antal invånare 25-64 år den 31/12 år T-1."

  diagramtitel <- "Långtidsarbetslöshet 25-64 år i Dalarna"
  diagramfilnamn <- "langtidsarbetsloshet_kolada.png"

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = långtidsarbetslöshet,
    skickad_x_var = "ar",
    skickad_y_var = "varde",
    skickad_x_grupp = "kon",
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    manual_y_axis_title = "procent",
    x_axis_lutning = 0,
    manual_color= valda_farger,
    lagg_pa_logga = FALSE,
    output_mapp = output_mapp_figur,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = spara_figur)

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, "\\.png")

  if(returnera_figur == TRUE){
    return(gg_list)
  }

}

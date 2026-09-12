diag_kompetensbrist <- function(diagram_capt =  diagram_capt <- "Källa: Tillväxtverket: Företagens villkor och verklighet.\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel småföretag som anser att tillgång till lämplig arbetskraft är ett stort hinder för tillväxt",
                                output_mapp_figur = NA,
                                skapa_fil = TRUE,
                                start_ar = "2020",# År som senaste år skall jämföras med. Finns 2011,2014,2017 och 2020
                                returnera_data = FALSE,
                                returnera_figur = TRUE
){

  # ========================================== Allmän info ============================================
  # Ett diagram som visar upplevd kompetensbrist i Sveriges regioner
  #
  # Data uppdaterades senast Sommaren 2023. Ingen ny data verkar ha kommit (JF 2024-02-12)
  # Skript uppdaterad 20240212
  # Ingen ny data verkar ha kommit (JF 2025-09-25)
  # Förbättringspotential: Gör så att diagram 2 kan skapas som en facet med uppdelning inrikes/utrikes
  # ========================================== Inställningar ============================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # Ingen SCB-hämtning här - datan kommer från en lokal Excel-fil (Tillväxtverkets
  # "Företagens villkor och verklighet", manuellt nedladdad).
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/tidyr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge ggplot-objekt

  # ========================================== Läser in data ============================================

  # Data nedan laddas hem från: https://tillvaxtverket.se/tillvaxtverket/statistikochanalys/statistikomforetag/foretagande/hinderfortillvaxt.1718.html
  # Välj ladda ner Excel-fil strax under figuren
  kompetensbrist_df <- openxlsx::read.xlsx("G:/skript/projekt/data/kompetensforsorjning/Tillgång till lämplig arbetskraft.xlsx", sheet = 4) |>
    dplyr::rename(Region = Kolumn1)

  kompetensbrist_df <- kompetensbrist_df |>
    tidyr::pivot_longer(2:length(names(kompetensbrist_df)), names_to = "År", values_to = "Andel") |>
    dplyr::mutate(Region = rdverktyg::skapa_kortnamn_lan(Region))

  spara_figur = skapa_fil

  if(is.na(output_mapp_figur)) spara_figur = FALSE

  if(returnera_data == TRUE){
    assign("kompetensbrist", kompetensbrist_df, envir = .GlobalEnv)
  }

  diagram_titel <- paste0("Upplevd kompetensbrist i Sveriges regioner")
  diagramfil <- "kompetensbrist.png"

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = kompetensbrist_df |>
      dplyr::filter(År %in% c(start_ar, max(År))) |>
      dplyr::mutate(Region = ifelse(Region == "Totalt", "Sverige", Region)),
    skickad_x_var = "Region",
    skickad_y_var = "Andel",
    skickad_x_grupp = "År",
    manual_x_axis_text_vjust=1,
    manual_x_axis_text_hjust=1,
    manual_color = rddiagram::diagramfarger("rus_sex"),
    diagram_titel = diagram_titel,
    x_axis_sort_value = TRUE,
    x_axis_sort_grp = 2,
    y_axis_100proc = TRUE,
    diagram_capt = diagram_capt,
    x_axis_lutning = 45,
    manual_y_axis_title = "procent",
    output_mapp = output_mapp_figur,
    filnamn_diagram = diagramfil,
    skriv_till_diagramfil = spara_figur)
  gg_list <- c(gg_list, list(gg_obj))

  names(gg_list) <- "kompetensbrist"
  if(returnera_figur == TRUE) return(gg_list)
}

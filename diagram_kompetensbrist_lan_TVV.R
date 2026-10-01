diag_kompetensbrist <- function(diagram_capt =  diagram_capt <- "Källa: Tillväxtverket: Företagens villkor och verklighet.\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel småföretag som anser att tillgång till lämplig arbetskraft är ett stort hinder för tillväxt",
                                output_mapp_figur = NA,
                                skapa_fil = TRUE,
                                start_ar = c("2020","2023"),# År som senaste år skall jämföras med. Finns 2011,2014,2017, 2020 och 2023
                                returnera_data = FALSE,
                                returnera_figur = TRUE
){
  
  # ========================================== Allmän info ============================================
  # Ett diagram som visar upplevd kompetensbrist i Sveriges regioner
  #
  # Data uppdaterades senast Sommaren 2023. Ingen ny data verkar ha kommit (JF 2024-02-12)
  # Skript uppdaterad 20240212
  # Ingen ny data verkar ha kommit (JF 2025-09-25)
  # Skript uppdaterad 20261001 - Excel-filen exporteras numera direkt från Tillväxtverkets
  # statistikverktyg i långt format (Formulär år / Nivå 1 / Viktad andel), med några
  # beskrivande rader överst innan själva tabellen börjar. Tidigare var filen ett brett
  # format (en kolumn per år) som pivoterades om här i skriptet - det behövs inte längre.
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
  
  # Data nedan laddas hem från: https://tillvaxtdata.tillvaxtverket.se/statistik#page=ed8c6ae0-6f98-46fb-bd3b-66b69839e14a
  # Välj ladda ner Excel-fil strax under den andra figuren (linjediagram med alla regioner)
  
  # Letar reda på den senast nedladdade filen med namnet "Tillgång till lämplig
  # arbetskraft" i datamappen, istället för att peka på ett hårdkodat filnamn.
  # Det gör att skriptet automatiskt plockar upp en ny nedladdning (t.ex. om filen
  # sparas med ett datum eller en siffersuffix i namnet) utan att sökvägen i koden
  # behöver ändras för hand varje gång.
  kompetensbrist_mapp <- "G:/skript/projekt/data/kompetensforsorjning/"
  
  kompetensbrist_filer <- list.files(kompetensbrist_mapp, full.names = TRUE, recursive = TRUE)
  kompetensbrist_filer <- kompetensbrist_filer[!file.info(kompetensbrist_filer)$isdir]
  
  kandidater <- kompetensbrist_filer[
    grepl("Tillgång till lämplig arbetskraft", basename(kompetensbrist_filer), ignore.case = TRUE) &
      grepl("\\.xlsx$", basename(kompetensbrist_filer), ignore.case = TRUE)
  ]
  
  if (!length(kandidater)) {
    stop("Hittade ingen fil med namnet 'Tillgång till lämplig arbetskraft' i ", kompetensbrist_mapp)
  }
  
  # Väljer den senast ändrade filen bland kandidaterna
  kompetensbrist_fil <- kandidater[which.max(file.info(kandidater)$mtime)]
  
  # OBS: filen har fyra beskrivande/filterrader överst (t.ex. "Hinder för tillväxt: ...",
  # "Valda filter: ...") innan den riktiga tabellrubriken ("Formulär år" / "Nivå 1" /
  # "Viktad andel") på rad 5 - därav startRow = 5. Kontrollera gärna radnumret om
  # filens layout ändras igen. check.names = FALSE så att kolumnnamnen behåller
  # mellanslag exakt som i Excel-filen.
  kompetensbrist_df <- openxlsx::read.xlsx(kompetensbrist_fil,
                                           startRow = 5,
                                           check.names = FALSE) |>
    dplyr::rename(År = `Formulär.år`,
                  Region = `Nivå.1`,
                  Andel = `Viktad.andel`) |>
    dplyr::mutate(
      År = as.character(År),
      Region = rdverktyg::skapa_kortnamn_lan(Region)
    )
  
  # Andelen kan komma in som andel (0.24) eller som procenttal (24) beroende på hur
  # cellerna är formaterade i Excel-filen - normaliserar till procent (0-100) här,
  # vilket är vad diagrammet nedan förväntar sig.
  if (max(kompetensbrist_df$Andel, na.rm = TRUE) <= 1) {
    kompetensbrist_df <- kompetensbrist_df |>
      dplyr::mutate(Andel = Andel * 100)
  }
  
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
      dplyr::mutate(Region = ifelse(toupper(Region) %in% c("TOTAL", "TOTALT"), "Sverige", Region)),
    skickad_x_var = "Region",
    skickad_y_var = "Andel",
    skickad_x_grupp = "År",
    manual_x_axis_text_vjust=1,
    manual_x_axis_text_hjust=1,
    manual_color = rddiagram::diagramfarger("rus_sex"),
    diagram_titel = diagram_titel,
    x_axis_sort_value = TRUE,
    x_axis_sort_grp = 3,
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

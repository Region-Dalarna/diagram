diag_utb_niva_bakgr_alder <- function(diagram_capt =  "Källa: NMS-databasen (SCB)\nBearbetning: Samhällsanalys, Region Dalarna",
                                      skapa_fil = TRUE,
                                      output_mapp_figur = NA,
                                      utbildningar = "förgymnasial utbildning", # Val av utbildningsnivå till diagram 2. Se nedan för alternativ
                                      diag_utb_bakgrund = TRUE,
                                      diag_utb_alder = TRUE,
                                      returnera_data = FALSE,
                                      returnera_figur = TRUE
){

  # ========================================== Allmän info ============================================
  # Två figurer: den första är ett facet-diagram som visar utbildningsnivå uppdelat på bakgrund (utrikes/inrikes) och utbildningsnivå (andel eller antal).
  #              den andra visar andel/antal i olika åldersgrupper som har en viss utbildningsnivå (som användaren får välja)
  # För tillfället enbart för Dalarna. Vill man ha annat län måste data hämtas via MONA. Skript finns på:
  # P1079_Gem/Jon/Kompetensförsörjning/utbildningsnivå_befolkning_bakgrund_2024
  #
  # Utbildningar (alternativ för variabeln): "förgymnasial utbildning" , "gymnasial utbildning",
  # "eftergymnasial utbildning kortare än 3 år", "eftergymnasial utbildning 3 år eller längre"
  #
  # Data uppdaterades senast 9 feb 2024
  # Skript uppdaterad 20240212
  # Förbättringspotential: Gör så att diagram 2 kan skapas som en facet med uppdelning inrikes/utrikes
  # ========================================== Inställningar ============================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # Ingen SCB-hämtning här - datan kommer från en lokal, manuellt nedladdad
  # Excel-fil (NMS-databasen).
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr följer med som beroende till rddiagram.

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge ggplot-objekt

  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung NMS-databasen)
  df <- openxlsx::read.xlsx("G:/skript/projekt/data/kompetensforsorjning/9_feb_24_utdata_utbildningsniva_befolkning_bakgrund.xlsx",sheet=1)

  spara_figur = skapa_fil

  if(is.na(output_mapp_figur)) spara_figur = FALSE

  if(diag_utb_bakgrund == TRUE){

    utb_niva_bakgrund <- df |>
      dplyr::group_by(Ar, Kon_namn,bakgrund, SUN_6kat) |>
      dplyr::summarize(antal = sum(antal), .groups = "drop_last") |>
      dplyr::mutate(andel = round((antal/sum(antal))*100,2))

    if(returnera_data == TRUE){
      assign("utb_niva_bakgrund", utb_niva_bakgrund, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Befolkningens (20-64 år) utbildningsnivå i Dalarnas län ", unique(utb_niva_bakgrund$Ar))
    diagramfil <- "fodelseregion_utbildning.png"
    objektnamn = c(objektnamn,"utb_niva_bakgrund")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = utb_niva_bakgrund |>
        dplyr::filter(SUN_6kat != "Okänd") |>
        dplyr::mutate(SUN_6kat = factor(SUN_6kat,levels = c("Eftergymnasial utbildning 3 år eller längre","Eftergymnasial utbildning kortare än 3 år",
                                                                                "Gymnsial utbildning 3 år","Gymnasial utbildning, högst 2-årig",
                                                                                "Förgymnasial utbildning 9 år","Förgymnasial utbildning kortare än 9 år")[6:1])),
      skickad_x_var = "bakgrund",
      skickad_y_var = "andel",
      skickad_x_grupp = "SUN_6kat",
      manual_color = rddiagram::diagramfarger("rus_sex"),
      diagram_titel = diagram_titel,
      x_axis_sort_value = TRUE,
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      diagram_capt = diagram_capt,
      facet_legend_bottom = TRUE,
      facet_grp = "Kon_namn",
      stodlinjer_avrunda_fem = TRUE,
      facet_scale = "fixed",
      manual_y_axis_title = "procent",
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))

  }

  if(diag_utb_alder == TRUE){

    # dplyr::case_when() med skalära villkor (som utbildningar == "...") är numera
    # deprecated i dplyr - ger en varning som rekommenderar en if/else-kedja i stället
    # (samma resultat, ingen beteendeändring).
    valda_utbildningar <- if (utbildningar == "förgymnasial utbildning") {
      c("Förgymnasial utbildning kortare än 9 år","Förgymnasial utbildning 9 år")
    } else if (utbildningar == "gymnasial utbildning") {
      c("Gymnasial utbildning, högst 2-årig","Gymnsial utbildning 3 år")
    } else if (utbildningar == "eftergymnasial utbildning kortare än 3 år") {
      c("Eftergymnasial utbildning kortare än 3 år")
    } else if (utbildningar == "eftergymnasial utbildning 3 år eller längre") {
      c("Eftergymnasial utbildning 3 år eller längre")
    }

    utb_niva_alder <- df |>
      dplyr::filter(SUN_6kat %in% valda_utbildningar) |>
      dplyr::group_by(Ar,Kon_namn,alder_grp) |>
      dplyr::summarize(antal=sum(antal), .groups = "drop_last") |>
      dplyr::mutate(andel=round((antal/sum(antal))*100,2))

    if(returnera_data == TRUE){
      assign("utb_niva_alder", utb_niva_alder, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Invånare (20-64 år) med ",utbildningar, " i Dalarna ", unique(df$Ar))
    diagramfil <- paste0(utbildningar,"_alder_kon.png")
    objektnamn = c(objektnamn,"utb_niva_alder")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = utb_niva_alder |>
        dplyr::mutate(Kon_namn = factor(Kon_namn,levels =c("man","kvinna"))),
      skickad_x_var = "alder_grp",
      skickad_y_var = "antal",
      skickad_x_grupp = "Kon_namn",
      manual_color = rddiagram::diagramfarger("kon")[2:1],
      diagram_titel = diagram_titel,
      x_axis_lutning = 0,
      diagram_capt = diagram_capt,
      diagram_liggande = TRUE,
      legend_vand_ordning = TRUE,
      geom_position_stack = TRUE,
      stodlinjer_avrunda_fem = TRUE,
      manual_y_axis_title = "Antal personer",
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))

  }

  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)
}

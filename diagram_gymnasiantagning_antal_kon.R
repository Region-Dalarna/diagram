diag_gymnasieantagna_antal <- function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                       output_mapp_data = NA, # Här hamnar sparad data
                                       filnamn_data = "gymnasieantagning.xlsx",
                                       valda_farger = rddiagram::diagramfarger("rus_sex"), # Vilka färger skall användas i diagram
                                       spara_figur = TRUE, # Om true sparas figuren till output_mapp
                                       caption = "Källa: Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Antagna i början av september",
                                       caption_fleraar = "Källa: Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Antagna i början av september\nEnbart program som existerat samtliga år",
                                       diag_antal = TRUE, # Skapar ett diagram för senaste år
                                       diag_antal_fleraar = TRUE, # Skapar ett diagram för flera år
                                       valda_ar = c("99",2017,"9999"), # "99" ger första året, "9999" ger senaste året
                                       konsuppdelat = FALSE, # Skall diagrammet vara uppdelat på kön. Funkar enbart för diag_antal
                                       diag_lan_antal = FALSE, # Antal för länet, uppdelat på kvinnor och män
                                       returnera_figur = TRUE, # Skall figur returneras (i en lista)
                                       returnera_data = FALSE){ # Skall data returneras (till R-studios globla miljö)

  # ========================================== Allmän info ============================================

  # 1: Skapar diagram för antagna på gymnasiet, antingen för senaste år (uppdelat på kön eller inte) eller för valda år
  # Skapat av Jon Frank 2024-03-07
  # ========================================== Inställningar ============================================
  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # Ingen SCB-hämtning här - datan kommer från Dalarnas kommunförbunds egen
  # lokala Excel-export (G:/skript/hamta_data/func_gymnasieantagningen.R,
  # kvar som den var - inte en del av funktioner-/hamta_data-reporna på
  # GitHub som migreras här). "here" togs bort - laddades men användes
  # aldrig.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/tidyr följer med som beroenden till rddiagram.

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c()

  #vald_region = rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn("20")$region)

  # =============================================== API-uttag ===============================================

  # Hämtar data
  source("G:/skript/hamta_data/func_gymnasieantagningen.R", encoding = "utf-8", echo = FALSE)
  df <- las_in_data_gymnasieantagningen() |>
    dplyr::group_by(ar,program) |>
    dplyr::summarize(Män=sum(Ant_Män),
              Kvinnor=sum(Ant_Kv), .groups = "drop") |>
    tidyr::pivot_longer(3:4, names_to="Kon", values_to = "Antagna")

  # Slår ihop flera mindre program till en större kategori
  df[df == "Flygteknikutbildningen, Marinteknikutbildningen, Sjöfartsutbildningen, Tågteknikutbildningen, Utbildningen samiska näringar eller Yrkesdansarutbildningen"] <- "Flygteknikutbildningen mfl."


  if(returnera_data == TRUE){
    assign("gymnasieantagning_df", df, envir = .GlobalEnv)
  }

  # =============================================== Diagram ===============================================

  if(diag_antal == TRUE){

    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna ", max(df$ar)," per program")
    diagramfil <- "gymnasieantagning_tot.png"
    objektnamn <- if(konsuppdelat == FALSE) c(objektnamn,"gymnasieantagning_tot") else c(objektnamn,"gymnasieantagning_kon_tot")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(df, ar==max(ar)),
      skickad_x_var = "program",
      skickad_y_var = "Antagna",
      skickad_x_grupp = ifelse(konsuppdelat == FALSE,NA,"Kon"),
      manual_x_axis_text_vjust=0,
      manual_x_axis_text_hjust=0.6,
      manual_color = if(konsuppdelat == FALSE) valda_farger[1] else rddiagram::diagramfarger("kon"),
      diagram_titel = diagram_titel,
      x_axis_sort_value = TRUE,
      diagram_capt = caption,
      x_axis_lutning = 0,
      diagram_liggande = TRUE,
      manual_y_axis_title = "Antagna elever",
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur
    )

    gg_list <- c(gg_list, list(gg_obj))

  }

  if(diag_antal_fleraar==TRUE){

    if("99" %in% valda_ar) valda_ar <- replace(valda_ar,valda_ar=="99",min(df$ar))
    if("9999" %in% valda_ar) valda_ar <- replace(valda_ar,valda_ar=="9999",max(df$ar))

    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna per program")
    diagramfil <- "gymnasieantagning_fleraar.png"
    objektnamn <- c(objektnamn,"gymnasieantagning_fleraar")

    program_forsta_ar <- unique(dplyr::pull(dplyr::filter(df, ar == dplyr::first(valda_ar)), program))
    program_sista_ar  <- unique(dplyr::pull(dplyr::filter(df, ar == dplyr::last(valda_ar)), program))

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(
        dplyr::filter(df, program %in% program_forsta_ar, program %in% program_sista_ar, ar %in% valda_ar),
        ar = factor(ar, levels = valda_ar)
      ),
      skickad_x_var = "program",
      skickad_y_var = "Antagna",
      skickad_x_grupp = "ar",
      manual_x_axis_text_vjust=0,
      manual_x_axis_text_hjust=0.6,
      manual_color = rddiagram::diagramfarger("rus_sex"),
      diagram_titel = diagram_titel,
      x_axis_sort_value = TRUE,
      diagram_capt = caption_fleraar,
      #procent_0_100_10intervaller = TRUE,
      x_axis_lutning = 0,
      legend_vand_ordning = FALSE,
      diagram_liggande = TRUE,
      manual_y_axis_title = "Antagna elever",
      #geom_position_stack = TRUE,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur
    )

    gg_list <- c(gg_list, list(gg_obj))
  }

  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)

  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(df,paste0(output_mapp_data,filnamn_data))
  }

}

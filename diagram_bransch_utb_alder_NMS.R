diag_bransch_utb_alder <- function(output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                   output_mapp_figur= "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                                   diag_utbildningsniva = TRUE, # Jämför antingen kommuner eller län (beroende på val under jmf_omrade nedan)
                                   diag_alder = TRUE, # Jämför bransch i valt län/kommun
                                   spara_figur = TRUE, # Skall figur sparas
                                   andel = TRUE, # Skall figurer visas som andel av alla förvärvsarbetande eller antal förvärvsarbetande
                                   filnamn_data = "bransch_utbildning_alder.xlsx", # Filnamn på sparad data
                                   vald_farg = rddiagram::diagramfarger("rus_sex"), # Val av diagramfärger
                                   returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                                   returnera_data = FALSE){ # Skall data returneras till R-studios globala miljö

  # =================================================================================================================
  # Diagram som jämför utbildningsnivå och ålder för förvärvsarbetande inom olika branscher i Dalarna
  # Data hämtas från NMS
  # Skapad av Jon 2024-01-18
  # Senast uppdaterad:2024-10-21, justerat åldersgrupperna och lagt skriptet i en annan mapp på Mona (korrigerat nedan)
  #                               + uppdaterat lite allmänt till mer läsbar och optimerad kod, Peter
  # Data uppdaterat senast: 2024-01-18
  # Fil för att hämta data finns på MONA/NMS: P1079_GEM/Dalarna/sysselsattning/utbildningsniva_bransch_kon_alder_bakgrund.R
  # =================================================================================================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # Ingen SCB-hämtning i det här skriptet (datan kommer från en Excel-fil som
  # tas fram separat på SCB:s säkra MONA-miljö, se filsökvägen ovan) - därför
  # ingen pxweb2r/CKM-hantering här.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  indatamapp <- "G:/skript/projekt/data/kompetensforsorjning/"

  # Skapar listor
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge objekt i lista
  list_data <- list() # Skapa tom lista som används för att spara till Excel.

  datafil_senaste <- list.files(indatamapp, pattern = "syss_utbniva_bakgr_kon_alder_bransch.*\\.xlsx$",
                                full.names = TRUE) |>
    (\(f) f[which.max(file.info(f)$mtime)])()

  # Läser in data
  bransch_utb_alder_df <- openxlsx::read.xlsx(datafil_senaste) |>
    dplyr::mutate(lan = rdverktyg::skapa_kortnamn_lan(lan),
                  utbildningsniva_3kat = tolower(utbildningsniva_3kat))

  names(bransch_utb_alder_df) <- tolower(names(bransch_utb_alder_df))

  diagram_capt <- "Källa: NMS-databasen (SCB), databasen Stativ\nBearbetning: Samhällsanalys, Region Dalarna"

  if(diag_utbildningsniva == TRUE){

    # Variabler att gruppera på
    variabellista = c("ar","lan","bransch","utbildningsniva_3kat")

    # Grupperar på år, län, bransch och utbildning
    #  Drar bort en obefintlig summa för att diagrammet skall gå till 100.
    bransch_utb_df_sum <- bransch_utb_alder_df |>
      dplyr::group_by(dplyr::across(dplyr::any_of(variabellista))) |>
      dplyr::summarize(antal = sum(antal)) |>
      dplyr::mutate(andel = (antal / sum(antal) * 100) - 0.001) |>
      dplyr::ungroup() |>
      dplyr::filter(bransch != "Okänt") |>
      dplyr::mutate(utbildningsniva_3kat = factor(utbildningsniva_3kat, levels = c("okänd","eftergymnasial","gymnasial","förgymnasial")))

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data, list("Utbildningsnivå" = bransch_utb_df_sum))
    }

    if(returnera_data == TRUE){
      assign("bransch_utbildningsniva", bransch_utb_df_sum, envir = .GlobalEnv)
    }

    # Diagrammets titel
    diagram_titel <- paste0("Utbildningsnivå för förvärvsarbetande 16-74 år per bransch i ",unique(bransch_utb_df_sum$lan)," ",unique(bransch_utb_alder_df$ar))
    # Skapar en separat färgkod, så att okänd blir ljusgrå
    farger_gra <- c(grDevices::rgb(211,211,211, maxColorValue = 255), rddiagram::diagramfarger("rus_sex"))
    if(andel == TRUE){
      objektnamn <- paste0("utbildningsniva_bransch_andel_",unique(bransch_utb_df_sum$lan))
      diagramfil <-  paste0(objektnamn,".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = bransch_utb_df_sum,
                                            skickad_x_var = "bransch",
                                            skickad_y_var = "andel",
                                            skickad_x_grupp = "utbildningsniva_3kat",
                                            manual_color = farger_gra,
                                            x_axis_lutning = 0,
                                            diagram_titel = diagram_titel,
                                            x_axis_sort_value = TRUE,
                                            x_axis_sort_grp = 4,
                                            diagram_capt = diagram_capt,
                                            stodlinjer_avrunda_fem = TRUE,
                                            legend_vand_ordning = TRUE,
                                            diagram_liggande = TRUE,
                                            geom_position_stack = TRUE,
                                            manual_y_axis_title = "procent",
                                            output_mapp = output_mapp_figur,
                                            filnamn_diagram = diagramfil,
                                            skriv_till_diagramfil = spara_figur)

      gg_list <- c(gg_list, list(gg_obj))
    } else{
      objektnamn <- paste0("utbildningsniva_bransch_antal_",unique(bransch_utb_df_sum$lan))
      diagramfil <-  paste0(objektnamn,".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = bransch_utb_df_sum,
                                   skickad_x_var = "bransch",
                                   skickad_y_var = "antal",
                                   skickad_x_grupp = "utbildningsniva_3kat",
                                   manual_color = farger_gra,
                                   x_axis_lutning = 45,
                                   diagram_titel = diagram_titel,
                                   x_axis_sort_value = TRUE,
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_y_axis_title = "antal sysselsatta",
                                   diagram_capt = diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   legend_vand_ordning = TRUE,
                                   geom_position_stack = TRUE,
                                   output_mapp = output_mapp_figur,
                                   filnamn_diagram = diagramfil,
                                   skriv_till_diagramfil = spara_figur)

      gg_list <- c(gg_list, list(gg_obj))
    }
  }


  if(diag_alder == TRUE){

    variabellista = c("ar","lan","bransch","alder")

    # Grupperar på år, län, bransch och utbildning
    #  Drar bort en obefintlig summa för att diagrammet skall gå till 100.
    bransch_alder_df_sum <- bransch_utb_alder_df |>
      dplyr::group_by(dplyr::across(dplyr::any_of(variabellista))) |>
      dplyr::summarize(antal = sum(antal)) |>
      dplyr::mutate(andel = (antal/sum(antal) * 100) - 0.001) |>
      dplyr::ungroup() |>
      dplyr::filter(bransch != "Okänt") |>
      dplyr::mutate(alder = factor(alder, levels = c("70-74 år","65-69 år","60-64 år","35-59 år","20-34 år","16-19 år")))

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data, list("Ålder" = bransch_alder_df_sum))
    }

    if(returnera_data == TRUE){
      assign("bransch_alder", bransch_alder_df_sum, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Åldersfördelning för förvärvsarbetande 16-74 år per bransch i ", unique(bransch_alder_df_sum$lan), " år ", unique(bransch_alder_df_sum$ar))

    # unique(bransch_utb_df_sum$bransch_alder_df_sum)

    if(andel == TRUE){
      objektnamn <- c(objektnamn,paste0("alder_bransch_andel_",unique(bransch_alder_df_sum$lan)))
      diagramfil <- paste0("alder_bransch_andel_",unique(bransch_alder_df_sum$lan),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = bransch_alder_df_sum,
                                              skickad_x_var = "bransch",
                                              skickad_y_var = "andel",
                                              skickad_x_grupp = "alder",
                                              manual_color = rddiagram::diagramfarger("rus_sex"),
                                              diagram_titel = diagram_titel,
                                              x_axis_sort_value = TRUE,
                                              x_axis_lutning = 0,
                                              x_axis_sort_grp = 6,
                                              diagram_capt = diagram_capt,
                                              stodlinjer_avrunda_fem = TRUE,
                                              diagram_liggande = TRUE,
                                              legend_vand_ordning = TRUE,
                                              legend_byrow = TRUE,
                                              geom_position_stack = TRUE,
                                              manual_y_axis_title = "procent",
                                              output_mapp = output_mapp_figur,
                                              filnamn_diagram = diagramfil,
                                              skriv_till_diagramfil = spara_figur)

      gg_list <- c(gg_list, list(gg_obj))
    } else{
      objektnamn <- c(objektnamn,paste0("alder_bransch_antal_",unique(bransch_alder_df_sum$lan)))
      diagramfil <- paste0("alder_bransch_antal_",unique(bransch_alder_df_sum$lan),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = bransch_alder_df_sum,
                                   skickad_x_var = "bransch",
                                   skickad_y_var = "antal",
                                   skickad_x_grupp = "alder",
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_color = rddiagram::diagramfarger("rus_sex"),
                                   manual_y_axis_title = "antal sysselsatta",
                                   diagram_titel = diagram_titel,
                                   x_axis_sort_value = TRUE,
                                   x_axis_lutning = 45,
                                   diagram_capt = diagram_capt,
                                   diagram_liggande = FALSE,
                                   legend_vand_ordning = TRUE,
                                   legend_byrow = TRUE,
                                   geom_position_stack = TRUE,
                                   output_mapp = output_mapp_figur,
                                   filnamn_diagram = diagramfil,
                                   skriv_till_diagramfil = spara_figur)

      gg_list <- c(gg_list, list(gg_obj))
    }
  }

  names(gg_list) <- c(objektnamn)

  if(returnera_figur == TRUE) return(gg_list)

  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }

}

diagram_andel_offentligt <- function(region_vekt = rdverktyg::hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av kommuner
                                     alder_klartext = "*", # Ålder. Andra val: 16-19 år, 20-24 år, 25-34 år, 35-44 år, 45-54 år, 55-59 år, 60-64 år, 65+ år. Max 1 åt gången
                                     output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Vart hamnar figur om den skall sparas
                                     output_mapp_data = NA, # Vart hamnar data om den skall sparas. NA medför att data inte sparas
                                     filnamn_data = "andel_offentligt.xlsx", # Filnamn för sparad data
                                     vald_farg = rddiagram::diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                     spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                     returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                     returnera_data = FALSE, # True om användaren vill returnera data från funktionen
                                     diag_totalt = TRUE, # Skriver ut diagram för kön totalt
                                     diag_kon = TRUE # Skriver ut diagram uppdelat på kön
){

  # ===========================================================================================================
  #
  # Skript som skapar diagram för andelen som arbetar inom offentlig sektor. Funkar med och utan könsuppdelning men enbart för senaste år
  # Går även att använda olika åldersspann
  #
  # OBS!! Förlitar sig på gammal data från RAMS. Använd istället diagram_andel_offentligt_BAS.R OBS!!
  # ===========================================================================================================

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
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr följer med som beroende till rddiagram/rdverktyg.

  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  list_data <- list() # Skapar en tom lista som används för att spara data
  objektnamn <- c() # Används för att namnge

  # hamta_data-repots hamta_forvarvsarbetande_sektor_SCB.R hämtade denna RAMS-tabell
  # (v1: AM0207Z/DagSektAldKN) - v2-motsvarigheten är TAB5838 (samma ContentsCode
  # "00000545"), fryst till 2019-2021 precis som originaltabellen (RAMS ersattes av
  # BAS - se OBS-varningen ovan).
  andel_df <- pxweb2r::pxweb2_get_data(
    table = "TAB5838",
    query = list(
      Region = region_vekt,
      ArbetsSektor = "*",
      Alder = alder_klartext,
      Kon = c("män","kvinnor"),
      ContentsCode = "00000545",
      Tid = "9999"
    )) |>
    dplyr::rename(regionkod = region_kod,
                  `Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)` = value) |>
    dplyr::select(-tabellinnehåll)

  if(alder_klartext == "*") alder_klartext <- ("16-74 år")

  if(diag_totalt == TRUE){

    andel_totalt <- andel_df |>
      dplyr::mutate(`arbetsställets sektortillhörighet` =
                                 ifelse(`arbetsställets sektortillhörighet` %in%
                                          c("statlig förvaltning","statliga affärsverk","primärkommunal förvaltning","regioner","övriga offentliga institutioner"),"Offentlig sektor","Övriga")) |>
        dplyr::group_by(regionkod, region,`arbetsställets sektortillhörighet`,år) |>
         dplyr::summarize("Förvärvsarbetande" = sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`), .groups = "drop") |>
           dplyr::ungroup()


    # Beräknar andelar
    andel_totalt_utskrift <- andel_totalt |>
      dplyr::group_by(region,år) |>
        dplyr::mutate(Andel_forv = (Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01,
             region = rdverktyg::skapa_kortnamn_lan(region)) |>
         dplyr::rename(sektor = `arbetsställets sektortillhörighet`) |>
          dplyr::ungroup()

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Totalt" = andel_totalt_utskrift))
    }

    if(returnera_data == TRUE){
      assign("andel_offentligt", andel_totalt_utskrift, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Andel offentligt anställda (",alder_klartext,")" ," år ",unique(andel_totalt_utskrift$år))
    diagramfilnamn <- "andel_offentligt_totalt.png"
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    objektnamn = c(objektnamn,"andel_off_totalt")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = andel_totalt_utskrift |>
        dplyr::mutate(sektor = factor(`sektor`, levels = c("Offentlig sektor","Övriga")[2:1]),
                      region = ifelse(region == "Riket","Sverige",region)),
      skickad_x_var = "region",
      skickad_y_var = "Andel_forv",
      skickad_x_grupp = "sektor",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = vald_farg,
      stodlinjer_avrunda_fem = TRUE,
      geom_position_stack = TRUE,
      legend_vand_ordning = TRUE,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 1,
      manual_y_axis_title =" procent",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))

  }

  if(diag_kon == TRUE){

    andel_kon <- andel_df |>
      dplyr::mutate(`arbetsställets sektortillhörighet` =
               ifelse(`arbetsställets sektortillhörighet` %in%
                        c("statlig förvaltning","statliga affärsverk","primärkommunal förvaltning","regioner","övriga offentliga institutioner"),"Offentlig sektor","Övriga")) |>
      dplyr::group_by(regionkod, region,kön,`arbetsställets sektortillhörighet`,år) |>
      dplyr::summarize("Förvärvsarbetande" = sum(`Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`), .groups = "drop") |>
      dplyr::ungroup()


    # Beräknar andelar
    andel_kon_utskrift <- andel_kon |>
      dplyr::group_by(region,år,kön) |>
        dplyr::mutate(Andel_forv = (Förvärvsarbetande/sum(Förvärvsarbetande)*100)-0.01,
               region = rdverktyg::skapa_kortnamn_lan(region)) |>
          dplyr::rename(sektor = `arbetsställets sektortillhörighet`) |>
            dplyr::ungroup()

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Kön" = andel_kon_utskrift))
    }

    if(returnera_data == TRUE){
      assign("andel_offentligt_kon", andel_kon_utskrift, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Andel offentligt anställda (",alder_klartext,")" ," år ",unique(andel_kon_utskrift$år))
    diagramfilnamn <- "andel_offentligt_kon.png"
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    objektnamn = c(objektnamn,"andel_off_kon")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = andel_kon_utskrift |>
        dplyr::filter(sektor == "Offentlig sektor") |>
        dplyr::mutate(region = ifelse(region == "Riket","Sverige",region)),
      skickad_x_var = "region",
      skickad_y_var = "Andel_forv",
      skickad_x_grupp = "kön",
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = rddiagram::diagramfarger("kon"),
      stodlinjer_avrunda_fem = TRUE,
      legend_vand_ordning = TRUE,
      vand_sortering = TRUE,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 1,
      manual_y_axis_title =" procent",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfilnamn,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))

  }

  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }

  names(gg_list) <- c(objektnamn)
  if(returnera_figur == TRUE) return(gg_list)

}

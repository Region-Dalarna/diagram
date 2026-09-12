diag_demografi <-function(region = rdverktyg::hamtakommuner("20",tamedlan = TRUE,tamedriket = TRUE), # Val av region.
                          outputmapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                          output_mapp_data = NA, # Ändra om data skall sparas
                          filnamn_data = "medelalder_demo.xlsx",
                          diag_forsorjningskvot = TRUE, # Demografisk försörjningskvot
                          diag_medelalder = TRUE, # Medelålder
                          konsuppdelat = FALSE, # Om TRUE jämförs kön för senaste år, om FALSE jämförs första och sista år i sample
                          valda_farger = rddiagram::diagramfarger("rus_sex"),
                          stodlinjer_avrunda_fem = TRUE,
                          tid = 2007:2100, # Tid. Ett högt sistavärde ger senaste år
                          spara_figur = TRUE, # Skall figuren sparas
                          returnera_figur = TRUE, # Skall figur returneras från funktion
                          returnera_data = FALSE, # Om TRUE returneras data till R-Studios globala miljö
                          demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {

  # ===========================================================================================================  #
  # Skript som skriver ut diagram för demografisk försörjningskvot och medelålder. Går att välja med eller utan uppdelning på kön
  # Data för län som helhet finns enbart från 2007
  # Skapad av Jon Frank
  # Senast ändrad: 2023-12-08
  # Ändrat så att man kan välja bort stodlinjer_avrunda_fem /Jon 2025-12-09
  # Har uppdaterat skripet så att data hämtas direkt via Peters funktion, snarare än via ett skript
  # Funktionen fungerade tidigare inte, så det gjordes ett tillfälligt återfall till ett direktanrop av
  # rKolada::get_values() (se git-historik). Vid migreringen till paket testades rdverktyg::hamta_kolada_df()
  # på nytt mot skarp data och fungerar nu utmärkt - den returnerar dessutom redan städade regionnamn
  # ("Dalarna", "Falun", "Riket") och köns-klartext ("Kvinnor"/"Män"/"Båda könen"), så de tidigare
  # omvägarna via byt_namn_lan_kolada()/manuell kön-omkodning (som fanns i JonFrank81:s privata
  # func_API_alternativ.R-fork) behövs inte längre.
  # ===========================================================================================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/purrr följer med som beroenden till rddiagram/rdverktyg. rKolada
  # installeras automatiskt av rdverktyg::hamta_kolada_df() vid behov.

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/demo_fors.png",
        "https://region-dalarna.github.io/utskrivna_diagram/medelalder.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge
  list_data <- list() # Skapa tom lista som används för att spara till Excel.

  # =============================================== API-uttag ===============================================

  if(diag_forsorjningskvot == TRUE){

    demo_df <- rdverktyg::hamta_kolada_df(
      kpi_id = "N00927",
      valda_kommuner = region,
      valda_ar = tid,
      konsuppdelat = konsuppdelat
    ) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE),
                    varde = varde * 100)

    if(returnera_data == TRUE){
      assign("demografisk_forsorjningskvot", demo_df, envir = .GlobalEnv)
    }

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Demografisk försörjningskvot" = demo_df))
    }

    if(konsuppdelat == FALSE){
      demo_df <- demo_df |>
        dplyr::filter(ar %in% c(min(ar),max(ar)))
      diag_farger = valda_farger
      diagram_titel <- "Demografisk försörjningskvot"


    } else{
      demo_df <- demo_df |> dplyr::filter(ar == max(ar))
      diag_farger = rddiagram::diagramfarger("kon")
      diagram_titel <- paste0("Demografisk försörjningskvot år ",unique(demo_df$ar))
    }


    diagram_typ <- "demo_fors"
    diagramfil <- "demo_fors.png"
    objektnamn <- c(objektnamn,diagram_typ)

    diagram_capt <- "Källa: SCB (via RKA/Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Den demografiska försörjningskvoten beräknas som summan av antal personer 0-19 år och\nantal personer 65 år och äldre dividerat med antal personer 20-64 år. Ett värde över 100 innebär att gruppen\näldre och yngre är större än den i arbetsför ålder."

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = demo_df,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = ifelse(konsuppdelat == FALSE,"ar","kon"),
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = diag_farger,
      x_axis_sort_value = TRUE,
      x_axis_sort_grp = 2,
      vand_sortering = TRUE,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      manual_y_axis_title= "",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = outputmapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
  }


  if(diag_medelalder == TRUE){

    medelalder_df <- rdverktyg::hamta_kolada_df(
      kpi_id = "N00959",
      valda_kommuner = region,
      valda_ar = tid,
      konsuppdelat = konsuppdelat
    ) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

    # Då data inte finns för alla år i alla län/riket, så måste jag hitta det senaste år där alla regioner har data, och sedan bara visa från det året och framåt.
    regions_2024 <- medelalder_df |>
      dplyr::filter(ar == max(ar)) |>
      dplyr::distinct(region) |>
      dplyr::pull(region)

    common_start <- medelalder_df |>
      dplyr::filter(region %in% regions_2024) |>
      dplyr::group_by(ar) |>
      dplyr::summarise(n_present = dplyr::n_distinct(region), .groups = "drop") |>
      dplyr::filter(n_present == length(regions_2024)) |>
      dplyr::summarise(common_start = min(ar)) |>
      dplyr::pull(common_start)

    medelalder_df <- medelalder_df |>
      dplyr::filter(region %in% regions_2024,
                    ar >= common_start)

    if(returnera_data == TRUE){
      assign("medelalder", medelalder_df, envir = .GlobalEnv)
    }

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Medelålder" = medelalder_df))
    }

    if(konsuppdelat == FALSE){
      medelalder_df <- medelalder_df |>
        dplyr::filter(ar%in%c(min(ar),max(ar)))
      diag_farger = valda_farger
      diagram_titel <- "Medelålder"


    } else{
      medelalder_df <- medelalder_df |> dplyr::filter(ar == max(ar))
      diag_farger = rddiagram::diagramfarger("kon")
      diagram_titel <- paste0("Medelålder år ",unique(medelalder_df$ar))
    }

    diagram_typ <- "medelalder"
    diagramfil <- "medelalder.png"
    objektnamn <-  c(objektnamn,diagram_typ)
    diagram_capt <- "Källa: SCB (via RKA/Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\n"

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = medelalder_df,
      skickad_x_var = "region",
      skickad_y_var = "varde",
      skickad_x_grupp = ifelse(konsuppdelat == FALSE,"ar","kon"),
      manual_x_axis_text_vjust=1,
      manual_x_axis_text_hjust=1,
      manual_color = diag_farger,
      x_axis_sort_value = TRUE,
      dataetiketter = FALSE,
      stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
      dataetiketter_antal_dec = 1,
      manual_y_axis_title="Medelålder",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      output_mapp = outputmapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
  }

  names(gg_list) <- c(objektnamn)

  if(returnera_figur == TRUE) return(gg_list)

  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }

}

diag_pendling_over_kommungrans <- function(vald_kommun = "20", # Länsnamn ger samtliga kommuner i länet
                                           hela_lanet = TRUE,           # Ändra inte
                                           valt_kon = "män och kvinnor", # Enda valet
                                           valt_ar = NA, # Enbart senaste år. Ändra inte
                                           visa_dataetiketter = FALSE,         # dataetiketter i diagrammet
                                           diag_absoluta_tal = TRUE,           # skriv ut diagram med absoluta tal
                                           diag_procent = TRUE,                # skriv ut diagram med procent
                                           skapa_fil = TRUE, # skapa en fil dig figuren sparas
                                           returnera_figur = TRUE, # Om TRUE returneras figur som ggplot-objekt
                                           enbart_in_ut = FALSE, # TRUE om man bara vill visa in och utpendling (ej bor och arbetar i samma kommun)
                                           diagramfarg_vektor = NA, # Valda färger
                                           diagram_capt = "Källa: SCB:s öppna statistikdatabas (RAMS tom 2019, därefter BAS), bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Pendlingsdata kommer från RAMS tom år 2019 och inkluderar då åldrarna 16-74 år. Därefter kommer pendlingsdata från BAS och inkluderar åldrarna 15-74 år, från och med år 2020.",
                                           output_mapp_figur = "G:/Samhällsanalys/API/Fran_R/Utskrift/", # Hit sparas figuren
                                           output_mapp_data = NA, # Hit sparas data
                                           spara_data = FALSE, # Skall data sparas
                                           demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                           filnamn_data = "pendling.xlsx",
                                           returnera_data = TRUE) {# Filnamn för sparad data

  # ===========================================================================================================
  #
  # Skript för att skriva ut diagram (från RAMS, SCB) med andel och antal in- och utpendlare över kommungräns,
  # samt även de som bor och arbetar i samma kommun.
  # Skapad av: Peter
  # Senast uppdaterad: Peter, 2024-10-30
  #
  # Ändrat i hämtning av data så att det automatiskt blir i long-format (det funkade inte annars) Jon 2025-09-23
  # ===========================================================================================================

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <-
c("https://region-dalarna.github.io/utskrivna_diagram/in_utpendling_Dalarna2021.png",
"https://region-dalarna.github.io/utskrivna_diagram/in_utpendling_procent_Dalarna2021.png")
  purrr::walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  rdverktyg::stop_tyst()
}

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
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(diagramfarg_vektor))) {
    diagramfarg_vektor <- rddiagram::diagramfarger("rus_sex")
  }

  # hamta_data-repots hamta_pendling_over_grans_region_kon_tid_
  # ArRegPend1_PendlingKN_PendlingK_PendlingK9303_scb.R slår ihop fyra
  # v1-tabeller (BAS fr.o.m. 2020 + tre efterföljande RAMS-tabeller
  # tillbaka till 1993) för att kunna hämta en lång historisk tidsserie.
  # Det här diagrammet hämtar dock (enligt parameterkommentarerna,
  # "Ändra inte") alltid bara det senaste tillgängliga året - hämtas
  # här direkt via v2-motsvarigheten till BAS-tabellen (AM/AM0210/
  # AM0210F/ArRegPend1), TAB1828. Om ett äldre år uttryckligen begärs
  # (utanför TAB1828:s intervall 2020-2024) stoppas körningen med ett
  # tydligt felmeddelande i stället för att i det tysta hämta fel data -
  # det historiska RAMS-flödet (pre-2020) är inte återskapat, eftersom
  # skriptets egen kommentar avråder från att någonsin ändra valt_ar.
  giltiga_ar <- pxweb2r::pxweb2_get_values("TAB1828", "Tid", quiet = TRUE)$code
  valt_ar <- if (all(is.na(valt_ar)) || identical(valt_ar, "9999")) max(giltiga_ar) else as.character(valt_ar)
  if (!valt_ar %in% giltiga_ar) {
    stop(glue::glue("valt_ar = {valt_ar} finns inte i TAB1828 (giltiga år: {paste(giltiga_ar, collapse = ', ')}). Migreringen av det här skriptet återskapar bara BAS-tabellen (fr.o.m. 2020) - äldre RAMS-år stöds inte."))
  }

  visa_dataetik_txt <- ifelse(visa_dataetiketter, "_lbl_","")

  if (hela_lanet) {
    vald_kommun_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(stringr::str_sub(vald_kommun,1,2))$region))

    vald_kommun_long_txt <- rdverktyg::hamtaregion_kod_namn(stringr::str_sub(vald_kommun,1,2))$region

    vald_kommun_filnamn <- vald_kommun_txt

    titel_tillag <- "kommuner "         # ändra filnamnet om man tar ut hela länet eller inte
  } else {
    vald_kommun_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(vald_kommun)$region))

    if (length(vald_kommun) > 5) vald_kommun_filnamn <- c(vald_kommun[1:3], "mfl") else vald_kommun_filnamn <- vald_kommun
    vald_kommun_filnamn <- paste0(vald_kommun_filnamn, collapse = "_")

    titel_tillag <- ""                   # ändra filnamnet om man tar ut hela länet eller inte
  }

  # hämta rätt kod för kommun - OBS: originalet refererade en odefinierad
  # variabel (url_uttag) i den här grenen, vilket hade kraschat om man
  # skickade med ett regionnamn i stället för en regionkod. Slår i
  # stället upp koden via regiontabellen.
  if (is.na(as.numeric(vald_kommun))) {
    regdf <- rdverktyg::hamtaregtab()
    vald_kommun_kod <- regdf$regionkod[regdf$region %in% vald_kommun]
  } else {
    if (hela_lanet) {
      vald_kommun_kod <- rdverktyg::hamtakommuner(stringr::str_sub(vald_kommun,1,2), tamedlan = FALSE, tamedriket = FALSE)
    } else {
      vald_kommun_kod <- vald_kommun
    }
  }

  # =============================================== API-uttag ===============================================

  kon_hamta <- if (valt_kon %in% c("män och kvinnor", "totalt")) "totalt" else valt_kon

  px_df <- pxweb2r::pxweb2_get_data(
    table = "TAB1828",
    query = list(
      Kommun = vald_kommun_kod,
      Kon = kon_hamta,
      ContentsCode = "*",
      Tid = valt_ar
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = kommun_kod, region = kommun, variabel = tabellinnehåll, varde = value) |>
    dplyr::mutate(variabel = dplyr::case_when(
      variabel == "bostad utanför kommunen men arbetsställe i kommunen" ~ "Inpendlare över kommungräns",
      variabel == "bostad i kommunen men arbetsställe utanför kommunen" ~ "Utpendlare över kommungräns",
      variabel == "bostad och arbetsställe i kommunen" ~ "Bor och arbetar i kommunen",
      TRUE ~ variabel
    ),
    kön = ifelse(kön == "totalt", "män och kvinnor", kön))

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  list_data <- list() # Skapar en tom lista som används för att spara data

  # ============================== diagram med absoluta tal ==================================
  if (diag_absoluta_tal) {

    px_df_ut = px_df

    if(enbart_in_ut == TRUE) px_df_ut <- dplyr::filter(px_df_ut, variabel != "Bor och arbetar i kommunen")

    if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("antal_pendlare" = px_df_ut))
    }

    if(returnera_data == TRUE){
      assign("antal_pendlare_kommun_df", px_df_ut, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Antal pendlare i ", titel_tillag, "i ", vald_kommun_txt, " år ", unique(px_df$år))
    diagramfil <- paste0("in_utpendling_", vald_kommun_filnamn, unique(px_df$år), visa_dataetik_txt, ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = px_df_ut,
                       skickad_x_var = "region",
                       skickad_y_var = "varde",
                       skickad_x_grupp = "variabel",
                       diagram_titel = diagram_titel,
                       diagram_capt = diagram_capt,
                       manual_x_axis_text_vjust = 1,
                       manual_x_axis_text_hjust = 1,
                       manual_color = diagramfarg_vektor,
                       manual_y_axis_title = "antal förvärvsarbetande",
                       stodlinjer_avrunda_fem = TRUE,
                       geom_position_stack = TRUE,
                       dataetiketter = visa_dataetiketter,
                       skriv_till_diagramfil = skapa_fil,
                       output_mapp = output_mapp_figur,
                       filnamn_diagram = diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- "In_och_utpendling_absoluta_tal"

  } # slut if-sats för diag_abosluta tal

  # ================================= diagram med procent ==================================
  if (diag_procent) {

    # OBS: originalet räknade ut vilka kolumner som skulle pivoteras
    # tillbaka till long-format via "(length(names(.))-2):length(names(.))"
    # - ett magrittr-".":s-specifika knep som inte fungerar med |>. De tre
    # nya andelskolumnerna namnges i stället explicit i pivot_longer().
    px_df_andel <- px_df |>
      tidyr::pivot_wider(names_from = variabel, values_from = varde) |>
        dplyr::mutate("Andel utpendling" = (abs(`Utpendlare över kommungräns`)/(`Bor och arbetar i kommunen`+abs(`Utpendlare över kommungräns`)))*100,
               "Andel inpendling" = (`Inpendlare över kommungräns`/(`Bor och arbetar i kommunen`+`Inpendlare över kommungräns`))*100,
               "Bor och arbetar i samma kommun" = (`Bor och arbetar i kommunen`/(`Bor och arbetar i kommunen`+abs(`Utpendlare över kommungräns`)))*100) |>
          dplyr::select(-c(`Inpendlare över kommungräns`,`Utpendlare över kommungräns`,`Bor och arbetar i kommunen`)) |>
            tidyr::pivot_longer(cols = c("Andel utpendling", "Andel inpendling", "Bor och arbetar i samma kommun"), names_to = "variabel", values_to = "andel")

    if(enbart_in_ut == TRUE) px_df_andel <- dplyr::filter(px_df_andel, variabel != "Bor och arbetar i samma kommun")

    if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("andel_pendlare" = px_df_andel))
    }

    if(returnera_data == TRUE){
      assign("andel_pendlare_kommun_df", px_df_andel, envir = .GlobalEnv)
    }


    diagram_titel <- rdverktyg::dela_upp_strang_radbryt(paste0("Andel pendling i ", vald_kommun_long_txt, " år ", unique(px_df$år)), 70)

    diagramfil <- paste0("in_utpendling_procent_", vald_kommun_filnamn, unique(px_df$år), visa_dataetik_txt, ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = px_df_andel ,
                       skickad_x_var = "region",
                       skickad_y_var = "andel",
                       skickad_x_grupp = "variabel",
                       diagram_titel = diagram_titel,
                       diagram_capt = diagram_capt,
                       manual_y_axis_title = "procent",
                       manual_x_axis_text_vjust = 1,
                       manual_x_axis_text_hjust = 1,
                       manual_color = diagramfarg_vektor,
                       x_axis_sort_value = TRUE,
                       x_axis_sort_grp = 2,
                       vand_sortering = TRUE,
                       stodlinjer_avrunda_fem = TRUE,
                       dataetiketter = visa_dataetiketter,
                       skriv_till_diagramfil = skapa_fil,
                       output_mapp = output_mapp_figur,
                       filnamn_diagram = diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- "In_och_utpendling_procent"

  } # slut if-sats diag_procent

  # Sparar data
  if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }
 if(returnera_figur == TRUE) return(gg_list)
} # slut funktion

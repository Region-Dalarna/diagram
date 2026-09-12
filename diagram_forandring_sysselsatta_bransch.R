#test = diag_sysselsatta_forandring_bransch(region_vekt = hamtakommuner(lan="20",tamedriket = FALSE),spara_figur = FALSE,output_mapp_figur = "G:/skript/jon/Figurer/",diag_facet=FALSE)
diag_sysselsatta_forandring_bransch <- function(region_vekt = "20", # Region vi är intresserade av.
                                                output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                                filnamn_data = "andel_forvarvsarbetande.xlsx",
                                                diag_facet = FALSE,
                                                valda_farger = rddiagram::diagramfarger("rus_sex"), # Vilka färger skall användas i diagram
                                                spara_figur = TRUE, # Om true sparas figuren till output_mapp
                                                caption = "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Sysselsatta efter arbetsställets belägenhet (preliminär statistik).",
                                                returnera_figur = TRUE) { # Skall figur returneras (i en lista))

  # ========================================== Allmän info ============================================
  # Skapar diagram där antalet förvärvsarbetande i olika banscher jämförs mellan två år, senast och näst senaste år. Jämförelsen görs
  # baserat på den senast månaden för vilken det finns statistik. Diagrammet kan skapas per vald region/kommun eller som ett facet-diagram för alla regioner/kommuner.
  # Skapad av: Jon 2024-05-14
  # Notera att felmeddelandet Caused by error in `curl::curl_fetch_memory() Recv failure: Connection was reset ofta uppstår
  # Detta beror på för många androp på SCB:s API. Vänta en stund och försök igen.
  # ========================================== Inställningar ============================================
  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # "here" togs bort - laddades men användes aldrig i skriptet.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/purrr/stringr/readr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c()

  # =============================================== API-uttag ===============================================
  # Hämtar samma data som tidigare kom via
  # hamta_data/hamta_sysselsatta_region_kon_sni2007_fodelseregion_tid_ArbStDoNMNN_scb.R,
  # men direkt mot SCB:s PxWeb-API v2 med pxweb2r. TAB3784 = "Sysselsatta
  # 15-74 år efter region, kön, näringsgren (SNI 2007) och födelseregion.
  # Preliminär statistik" (månadsvis) - samma tabell (AM0210B/ArbStDoNMNN),
  # nytt id. Ingen CKM-tabell för den här (BAS-registret, inte
  # befolkningsstatistik, verkar inte omfattas av CKM).
  # pxweb2r ger näringsgrenskoden som en egen kolumn automatiskt (Region-
  # mönstret) - branschkoderna (SNI2007) binds sedan ihop med Region
  # Dalarnas egen bransch-/färgnyckel, precis som i originalet.
  branschtabell <- utils::read.csv("G:/skript/nycklar/Bransch_Gxx_farger.csv", sep = ";", encoding = "latin1")

  df <- pxweb2r::pxweb2_get_data(
      "TAB3784",
      query = list(Region = region_vekt, Kon = "totalt", SNI2007 = "*", Fodelseregion = "totalt",
                   ContentsCode = "sysselsatta efter arbetsställets belägenhet", Tid = "*")
    ) |>
    dplyr::rename(regionkod = region_kod, branschkod = `näringsgren sni 2007_kod`,
                  `sysselsatta efter arbetsställets belägenhet` = value, tid = månad) |>
    dplyr::mutate(branschkod = ifelse(branschkod == "US", "00", branschkod)) |>
    dplyr::filter(branschkod != "A-U+US") |>
    dplyr::left_join(
      dplyr::select(branschtabell, Br15kod, BrKod, HexCode, bransch = Bransch),
      by = c("branschkod" = "Br15kod")
    ) |>
    dplyr::mutate(år = as.integer(stringr::str_sub(tid, 1, 4)),
           månad_nr = readr::parse_integer(stringr::str_sub(tid, 6,7)),
           månad = format(as.Date(paste(år, stringr::str_sub(tid, 6,7), "1", sep = "-")), "%B"),
           år_månad = paste0(år, " - ", månad),
           månad_år = paste0(månad, " ", år))


  # År att jämföra med (dvs. året innan senaste år)
  jmf_ar = max(df$år)-1
  # Senaste år. Behövs för att döpa kolumner
  senaste_ar = max(df$år)
  # Senaste månad. Behövs för att filtrera data
  senaste_manad = unique(dplyr::pull(dplyr::filter(df, år==max(år), månad == dplyr::last(månad)), månad))

  # Summerar på region och sektor. Lägger till en kolumn senaste och näst senaste, för att kunna beräkna förändring
  df_sum <- df |>
    dplyr::filter(år >= jmf_ar, månad==senaste_manad) |>
    dplyr::group_by(år,regionkod, region, bransch) |>
    dplyr::summarize("Antal" = sum(`sysselsatta efter arbetsställets belägenhet`), .groups = "drop") |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE),
           år = dplyr::case_when(år == max(år) ~ "Senaste",
                                     TRUE ~ "nast_senaste"))


  # Beräknar förändring
  df_sum <- df_sum |>
    tidyr::pivot_wider(names_from = år, values_from = Antal) |>
      dplyr::mutate(forandring = (`Senaste`-`nast_senaste`)/`nast_senaste`*100) |>
        tidyr::pivot_longer(cols = c(`nast_senaste`,`Senaste`), names_to = "år", values_to = "Antal") |>
          dplyr::mutate(år = dplyr::case_when(år == "nast_senaste" ~ jmf_ar,
                                TRUE ~ senaste_ar))

  # Map-funktion som skapar diagrammen
  skapa_diagram = function(df,vald_region){

    df <- dplyr::filter(df, regionkod %in% vald_region)


    if(length(unique(df$region)) > 1){
      diagram_titel <- paste0("Föränding i antal förvärvsarbetande (16-74 år) per bransch")
      diagramfil <- paste0("förändring_bransch_facet_",dplyr::first(rdverktyg::hamtaregion_kod_namn(vald_region)[2]),".png")

    }else{
      diagram_titel <- paste0("Föränding i antal förvärvsarbetande (16-74 år) per bransch i ",rdverktyg::hamtaregion_kod_namn(vald_region)[2])
      diagramfil <- paste0("förändring_bransch_",rdverktyg::hamtaregion_kod_namn(vald_region)[2],".png")
    }

    #diagram_titel <- paste0("Föränding i antal förvärvsarbetande (16-74 år) per bransch i ",hamtaregion_kod_namn(vald_region)[2])
    #diagram_titel <- str_wrap(diagram_titel,40)
    diagram_undertitel <- paste0("Mellan ",senaste_manad," ",min(df$år)," och ",senaste_manad," ",max(df$år))
    #diagramfil <- paste0("förändring_bransch_",first(hamtaregion_kod_namn(vald_region)[2]),".png")
    objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(
        dplyr::filter(df, bransch != "Okänt", år==max(år)),
        bransch = stringr::str_wrap(bransch,40)
      ),
      skickad_x_var = "bransch",
      skickad_y_var = "forandring",
      manual_x_axis_text_vjust=1,
      #manual_x_axis_text_hjust=1,
      manual_color = valda_farger[1],
      x_axis_sort_value = TRUE,
      diagram_liggande = TRUE,
      manual_y_axis_title = "procent",
      x_axis_lutning = 0,
      stodlinjer_avrunda_fem = FALSE,
      y_axis_minus_plus_samma_axel = TRUE,
      facet_y_axis_storlek = 6,
      facet_sort = TRUE,
      facet_grp = if (length(unique(df$region)) > 1) "region" else NULL,
      facet_scale = "free",
      facet_legend_bottom = TRUE,
      diagram_titel = diagram_titel,
      diagram_undertitel = diagram_undertitel,
      diagram_capt = caption,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil,
      skriv_till_diagramfil = spara_figur
    )

    #gg_obj <- gg_obj + scale_y_continuous(expand = c(0,0), breaks = every_nth(n = 2, sista_vardet = TRUE))

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn
    return(gg_list)
  }

  # Vektor som används för att skapa figurer för samtliga kommuner (mha en loop)
  #kommun_vektor=hamtaregion_kod_namn(hamtakommuner(region_vekt,tamedlan=FALSE,tamedriket=FALSE))[2]

  # diag = map(region_vekt,~skapa_diagram(df_sum,.x)) %>% flatten()

  if (diag_facet) {
    diag <- skapa_diagram(df_sum,region_vekt)

  } else {
    diag = purrr::flatten(purrr::map(region_vekt, ~skapa_diagram(df_sum,.x)))
  }
  return(diag)
}

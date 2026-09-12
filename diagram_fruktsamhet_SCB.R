diagram_fruktsamhet <- function(region_vekt = rdverktyg::hamtakommuner("20"), # Vilka kommuner skall väljas. Default är alla kommuner i Dalarna. Bör var minst ett par stycken för att diag_jmf_lan och diag_forandring skall se vettiga ut
                                fokus_region = "20", # Vilken region skall fokuseras på. Måste vara ett som finns i region_vekt
                                output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                vald_farg = rddiagram::diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                diag_capt = "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna\nSummerad fruktsamhet per kvinna är ett mått på hur många barn som en kvinna i genomsnitt skulle föda under\n sin fruktsamma period utifrån, den vid tidpunkten för beräkningen, gällande fruktsamheten.",
                                vald_period = c(2000:9999), # Vilka år skall väljas. 9999 ger sista år och "*" ger alla år
                                visa_var_xte = 4, # Hur många år som skall visas på x-axeln. Automatiskt var 8:e vid facet-diagram.
                                ta_bort_nast_sista = FALSE, # Ta bort näst sista värdet på x-axeln (för att det inte skall krocka med sista årtalet)
                                spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                diag_fokus_tid = TRUE, # Skapa diagram för alla valda år i valda regioner
                                diag_facet = FALSE, # diag_fokus_tid som facet-diagram istället för ett per region
                                facet_skala = "free", # Finns free (varje diagram får en egen y-axel) eller fixed (alla diagram delar y-axel)
                                diag_jmf_lan = TRUE, # Skapa diagram för jämförelse mellan valda regioner
                                diag_forandring = TRUE, # Skapa diagram för förändring mellan första och sista år för valda regioner
                                demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                returnera_data = FALSE){ # True om användaren vill returnera data från funktionen


  # ===========================================================================================================
  #
  # Skript som skapar diagram över fruktsamhet i vald region. Finns för valda regioner över tid (både ett diagram per region och ett facet-diagram för alla regioner),
  # senaste år för alla valda regioner och förändring mellan första och sista året för samtliga valda regioner.
  # Skapad av Jon 2023-04-05 genom att ha kombinerat två av Peters skript (analys_befutv_berakna_summerad_fruktsamhet.R och diagram_summerad_fruktsamhet.R)
  # Revidering : har lagt till en map-funktion för att skapa diagram för alla valda regioner över tid./Jon
  # Lagt till så att man kan ta bort näst sista årtalet i facet-diagrammet som jämför regioner /Jon 2026-02-25
  # ===========================================================================================================


  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
  c("https://region-dalarna.github.io/utskrivna_diagram/forandring_summerad_fruktsamhet_Dalarna.png",
  "https://region-dalarna.github.io/utskrivna_diagram/jmf_summerad_fruktsamhet_Dalarna.png",
  "https://region-dalarna.github.io/utskrivna_diagram/summerad_fruktsamhet_Riket_facet.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # Skapar två listor, en de diagram jämför regionerna och en för de diagram som tar ut en region i taget
  gg_list <- list()
  gg_list_map <- list()
  objektnamn <- c()
  objektnamn_map <- c()

  # Hämta födelsetal för kvinnor
  # Samma två tabellpar som i diagram_fodelsenetto_region_SCB.R och
  # diagram_befolkningsforandring_region_kon_ar_SCB.R (TAB1264/TAB6401 för
  # födda, TAB638/TAB5557 för folkmängd), men här hämtas varje enskild ålder
  # 15-48 år (inte totalsumman) för att kunna räkna ut åldersspecifika
  # födelsetal. Individuella åldrar har - till skillnad från "100+"/
  # totalkoderna - inga dubbletter i CKM-tabellerna, så "*"-problematiken
  # från tidigare skript gäller inte här.
  alder_str <- as.character(15:48)
  # vald_period defaultar till c(2000:9999) - "9999" som genväg för senaste
  # år, men skrivet som en hel sekvens ger det också 8000 orimliga årtal
  # (2025-9998) som pxweb2r annars skulle varna om ett i taget. Samma
  # gallring som originalskriptets tid_koder[tid_koder %in% giltiga_ar]
  # gjorde tyst - trimmar bort allt som varken är "9999" eller ett rimligt
  # kalenderår innan frågan skickas.
  ar_nu <- as.integer(format(Sys.Date(), "%Y"))
  tid_str <- as.character(vald_period)
  tid_str <- unique(tid_str[tid_str %in% c("*", "9999") | (suppressWarnings(as.integer(tid_str)) <= ar_nu + 1)])

  fodda_hist <- pxweb2r::pxweb2_get_data("TAB1264", query = list(Region = region_vekt, Kon = NA, AlderModer = alder_str, Tid = tid_str), on_all_values_invalid = "null")
  fodda_ckm  <- pxweb2r::pxweb2_get_data("TAB6401", query = list(Region = region_vekt, Kon = NA, AlderModer = alder_str, Tid = tid_str), on_all_values_invalid = "null")

  fodda_df <- dplyr::bind_rows(fodda_hist, fodda_ckm) |>
    dplyr::rename(regionkod = region_kod, födda = value) |>
    dplyr::select(-dplyr::any_of("tabellinnehåll"))

  bef_hist <- pxweb2r::pxweb2_get_data("TAB638", query = list(Region = region_vekt, Kon = "kvinnor", Civilstand = NA, Alder = alder_str,
                                                                ContentsCode = "Folkmängd", Tid = tid_str), on_all_values_invalid = "null")
  # Civilstånd är inte elimineringsbart i TAB5557 (CKM) - måste anges
  # explicit som totalkoden "SC", vilket också gör den till en egen kolumn
  # som plockas bort igen innan bind_rows().
  bef_ckm <- pxweb2r::pxweb2_get_data(
    "TAB5557",
    query = list(Region = region_vekt, Kon = "kvinnor", Civilstand = "SC", Alder = alder_str,
                 ContentsCode = "Folkmängd", Tid = tid_str),
    on_all_values_invalid = "null"
  )
  if (!is.null(bef_ckm)) bef_ckm <- dplyr::select(bef_ckm, -dplyr::any_of("civilstånd"))

  har_ckm_data <- (!is.null(fodda_ckm) && nrow(fodda_ckm) > 0) || (!is.null(bef_ckm) && nrow(bef_ckm) > 0)
  ckm_fran_ar <- if (har_ckm_data) min(as.integer(c(fodda_ckm$år, bef_ckm$år))) else NULL
  diag_capt <- rddiagram::lagg_till_ckm_notering(diag_capt, har_ckm_data, fran_ar = ckm_fran_ar)

  bef_df <- dplyr::bind_rows(bef_hist, bef_ckm) |>
    dplyr::rename(regionkod = region_kod, Folkmängd = value) |>
    dplyr::select(-dplyr::any_of("tabellinnehåll"))


  # Beräknar födelsetal och fruktsamhet
  fodelsetal_df <- bef_df |>
    dplyr::left_join(fodda_df, by = c("år", "regionkod", "region","ålder" = "moderns ålder")) |>
    dplyr::mutate(födelsetal = födda / Folkmängd)

  sum_frukts_ar <- fodelsetal_df |>
    dplyr::group_by(år, regionkod, region) |>
    dplyr::summarise(sum_frukts_ar = round(sum(födelsetal, na.rm = TRUE), 2), .groups = "drop") |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

  # Returnera data om användaren vill det. Läggs i R-studios globala miljö så att data kan användas i markdown-rapporterna
  if(returnera_data == TRUE){
    assign("fruktsamhet_df", sum_frukts_ar, envir = .GlobalEnv)
  }

  # Region att fokusera på
  fokus_region_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(fokus_region)$region)

  # spara diagram
  #if (length(unique(sum_frukts_ar$regionkod)) > 1) facet_diagram <- TRUE else facet_diagram <- FALSE

  # Diagram som visar jämförelse i länet
  if(diag_jmf_lan == TRUE){

    diagram_titel <- paste0("Summerad fruktsamhet per kvinna år ", unique(max(sum_frukts_ar$år)))
    diagramfil <- paste0("jmf_summerad_fruktsamhet_", fokus_region_txt, ".png")
    objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(
        dplyr::filter(sum_frukts_ar, år == max(år)),
        fokus = dplyr::case_when(regionkod %in% fokus_region ~ 1,
                                 regionkod == "00"~ 2,
                                 TRUE ~ 0)
      ),
      skickad_x_var = "region",
      skickad_y_var = "sum_frukts_ar",
      diagram_titel = diagram_titel,
      diagram_capt = diag_capt,
      x_axis_sort_value = TRUE,
      x_var_fokus = "fokus",
      #x_axis_storlek = 8,
      stodlinjer_avrunda_fem = TRUE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = "",
      skriv_till_diagramfil = spara_figur,
      manual_color = vald_farg,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil
    )


    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn

  }
  # Diagram som visar förändring i samtliga valda regioner
  if(diag_forandring == TRUE){

    diagram_titel <- paste0("Förändring av summerad fruktsamhet per kvinna mellan år ", min(sum_frukts_ar$år), " och ", max(sum_frukts_ar$år))
    diagram_titel <- stringr::str_wrap(diagram_titel, width = 50)
    diagramfil <- paste0("forandring_summerad_fruktsamhet_", fokus_region_txt, ".png")
    objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::mutate(
        dplyr::summarise(
          dplyr::group_by(sum_frukts_ar, regionkod, region),
          forandring = (sum(`sum_frukts_ar`[år == max(år)]) - sum(`sum_frukts_ar`[år == min(år)])) /
                      sum(`sum_frukts_ar`[år == min(år)]), .groups = "drop"
        ),
        forandring = forandring * 100,
        fokus = dplyr::case_when(regionkod %in% fokus_region ~ 1,
                                 regionkod == "00"~ 2,
                                 TRUE ~ 0)
      ),
      skickad_x_var = "region",
      skickad_y_var = "forandring",
      diagram_titel = diagram_titel,
      diagram_capt = diag_capt,
      x_axis_sort_value = TRUE,
      x_var_fokus = "fokus",
      #x_axis_storlek = 8,
      stodlinjer_avrunda_fem = TRUE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = "procent",
      skriv_till_diagramfil = spara_figur,
      manual_color = vald_farg,
      output_mapp = output_mapp_figur,
      filnamn_diagram = diagramfil
    )

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn

  }

  # diagram med bara valda regioner över tid (ett för varje). Finns som enskilda och facet
  if(diag_fokus_tid == TRUE){


    skapa_diagram <- function(df,vald_region){
      vald_region_txt <- (rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(vald_region)$region))[1]

      if(length(vald_region) > 1){
        vald_region_txt <- paste0(vald_region_txt,"_facet")
        diagram_titel <- paste0("Summerad fruktsamhet per kvinna")
      }else{
        diagram_titel <- paste0("Summerad fruktsamhet per kvinna i ", vald_region_txt)
      }

      diagramfil <- paste0("summerad_fruktsamhet_", vald_region_txt,".png")
      objektnamn_map <- c(objektnamn_map, stringr::str_remove(diagramfil, ".png"))

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::filter(df, regionkod == vald_region),
        skickad_x_var = "år",
        skickad_y_var = "sum_frukts_ar",
        diagram_titel = diagram_titel,
        diagram_capt = diag_capt,
        stodlinjer_avrunda_fem = TRUE,
        x_axis_visa_var_xe_etikett = ifelse(length(vald_region) > 1,8,visa_var_xte),
        x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = ta_bort_nast_sista,
        manual_x_axis_text_vjust = 1,
        manual_x_axis_text_hjust = 1,
        manual_y_axis_title = "",
        facet_grp = if (length(vald_region) > 1) "region" else NULL,
        facet_scale = facet_skala,
        facet_legend_bottom = TRUE,
        skriv_till_diagramfil = spara_figur,
        manual_color = vald_farg[1],
        output_mapp = output_mapp_figur,
        filnamn_diagram = diagramfil
      )

      # Sparar och namnger diagram
      gg_list_map <- c(gg_list_map, list(gg_obj))
      names(gg_list_map) <- objektnamn_map
      return(gg_list_map)
    } # Slut funktion skapa_diagram

    # Funktionen körs enbart med hjälp av map när vi inte har facet-diagram
    if (diag_facet) {
      diag <- skapa_diagram(sum_frukts_ar,region_vekt)

    } else {
      diag <- purrr::flatten(purrr::map(region_vekt, ~ skapa_diagram(sum_frukts_ar, .x)))

    }

    gg_list <- c(gg_list, diag)
  }

  # Om användaren vill returnera en figur, gör detta
  if(returnera_figur==TRUE){
    return(gg_list)
  }

}


diag_bef_utfall_prognos_per_region_totalt <- function(
    region_vekt = c("20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085"),                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) för alla regioner
    diagram_capt = "auto",                    # diagram_capt skapa automatiskt och blir olika beroende på vilken tabell som används
        # om <prognos_ar> ligger med i diagram_capt så byts det ut mot det år prognosen gjordes
    output_mapp = NA,                                        # här sparas diagramet
    diagram_fargvekt = NA,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    jmfr_tid = 10,                                           # hur många år framåt från befolkningsprognosen vi ska ta med
    aldersgrupper = NA,                                      # NA = alla åldrar, annars list(c(20,65)), eller list(c(0,19), c(20,65)) om man vill ha fler
    returnera_dataframe_global_environment = FALSE,
    ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
    antal_istallet_for_andel = FALSE,                        # default är andel då regioner jämförs (mest rimligt) men man kan få antal om man sätter denna parameter till TRUE
    x_axis_visa_var_xe_etikett = 3,                          # var x:e etikett visas enbart
    x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = FALSE, # man kan ta bort näst sista värdet om det hamnar på varandra
    url_befprognos_tabell = NA,                              # om NA så väljs standardtabell, annars kan man skicka med vilken tabell man vill använda (SCB eller sökväg till egna datafiler), SCB:s senaste: "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
    filformat = "png",                                       # format på diagrammet
    skriv_till_diagramfil = TRUE,
    skriv_till_excelfil = FALSE
) {

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse). Anropas med fullt
  # namespace (dplyr::filter() osv.) i stället för library(). hamta_befprognos_scb_data.R sourcas
  # fortfarande direkt - samma beslut/skäl som i diag_befolkningsprognos_scb_api_profet.R (DB-migrering
  # av den tas vid ett senare tillfälle). hamta_bef_folkmangd_alder_kon_ar_scb.R (v1:
  # BE0101A/BefolkningNy+BefolkningCKM) är däremot ersatt med direkta pxweb2r-anrop mot
  # v2-motsvarigheterna TAB638/TAB5557 - samma tabellpar som redan används/verifierats i
  # SkapaBefPrognosDiagram() i diag_befolkningsprognos_scb_api_profet.R och i den snarlika
  # diag_bef_utfall_prognos_per_aldersgrupp_scb.R.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  # dplyr/purrr/stringr/readr följer med som beroenden till rddiagram/rdverktyg.

  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")

  options(dplyr.summarise.inform = FALSE)

  if (stringr::str_sub(filformat, 1, 1) != ".") filformat <- paste0(".", filformat)

  gg_list <- list()

  # Om url_befprognos_tabell är NA och förinställd mapp inte finns på datorn används SCB:s tabell för det senaste året som finns
  if (is.na(url_befprognos_tabell)) {
    url_befprognos_tabell <- if (!dir.exists("G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/")) {
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
    } else {
      "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/"
    }
  }

  # om ingen färgvektor är medskickad används två nyanser ur rus_sex-paletten
  if (all(is.na(diagram_fargvekt))) diagram_fargvekt <- rev(rddiagram::diagramfarger("rus_sex")[c(1, 2)])

  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_till_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(rdverktyg::utskriftsmapp())) {
        output_mapp <- rdverktyg::utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output_mapp ett värde.")
      }
    }
  }

  # Hjälpfunktion: kod för en given ålder i en given tabell - TAB638 och TAB5557 har olika koder för
  # samma ålder (t.ex. är öppna åldersklassen "100+" i TAB638 men "100+1" i TAB5557, och "totalt"-koden
  # heter "tot" respektive "TotSA") - samma mönster/motivering som i SkapaBefPrognosDiagram() i
  # diag_befolkningsprognos_scb_api_profet.R. aldrar = NA ger bara totalkoden, "*" ger alla individuella
  # åldrar, en karaktärsvektor med specifika åldrar ("0".."99"/"100+") ger just dem.
  hamta_alderskoder <- function(table_id, aldrar) {
    totalkod <- if (table_id == "TAB638") "tot" else "TotSA"
    if (all(is.na(aldrar))) return(totalkod)
    v <- pxweb2r::pxweb2_get_values(table_id, "Alder")
    v <- v[grepl("^[0-9]+\\+? år$", v$label), ]
    v <- v[!duplicated(v$label), ]
    if (identical(aldrar, "*")) return(v$code)
    label_sokt <- ifelse(aldrar == "100+", "100+ år", paste0(aldrar, " år"))
    v$code[match(label_sokt, v$label)]
  }

  # Hämta hela den historiska befolkningsserien (samtliga år) - v2-motsvarigheten till
  # hamta_bef_folkmangd_alder_kon_ar_scb.R. Civilstånd saknar en riktig "totalt"-kod i TAB638 - alla
  # fyra hämtas explicit och summeras ihop (samma mönster som SkapaBefPrognosDiagram).
  civilstand_hamta <- c("ogifta", "gifta", "skilda", "änkor/änklingar")
  hamta_bef_folkmangd_v2 <- function(aldrar) {
    historik <- pxweb2r::pxweb2_get_data(
      table = "TAB638",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB638", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = "*"),
      on_all_values_invalid = "null")
    ckm <- pxweb2r::pxweb2_get_data(
      table = "TAB5557",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB5557", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = "*"),
      on_all_values_invalid = "null")

    dplyr::bind_rows(historik, ckm) |>
      dplyr::rename(regionkod = region_kod, Antal = value) |>
      dplyr::select(-tabellinnehåll) |>
      dplyr::mutate(ålder = ifelse(ålder %in% c("totalt, samtliga åldrar", "tot", "TotSA"), "totalt ålder", ålder),
                    # Bugfix (samma som i SkapaBefPrognosDiagram): SCB:s klartext för regionkod "00" är
                    # "Riket", men hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb() (används för
                    # prognosdelen, se nedan) hårdkodar "Sverige" för samma regionkod - normaliseras här
                    # så att start- och slutårsdata för riket hamnar i samma grupp längre ned.
                    region = ifelse(regionkod == "00", "Sverige", region)) |>
      dplyr::group_by(dplyr::across(-c(civilstånd, Antal))) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop")
  }

  # om man har valt åldersgrupper
  if (!any(is.na(aldersgrupper))) {
    aldrar_hamta <- purrr::map(aldersgrupper, ~ seq(.x[1], .x[2])) |> unlist() |> unique() |>
      as.character() |> stringr::str_replace("100", "100+")
    aldervekt_max <- purrr::map(aldersgrupper, ~ .x[2]) |> unlist() |> unique() |> max() + 1
    till_aldervekt <- purrr::map(aldersgrupper, ~ .x[1]) |> unlist() |> unique() |> c(aldervekt_max)

    bef_folkmangd <- rdverktyg::funktion_upprepa_forsok_om_fel(function() hamta_bef_folkmangd_v2(aldrar = aldrar_hamta)) |>
      dplyr::mutate(alder_grp = rdverktyg::skapa_aldersgrupper(ålder, till_aldervekt),
             alder_grp = dplyr::if_else(stringr::str_detect(alder_grp, "-100 år"), stringr::str_replace(alder_grp, "-100 år", "+ år"), as.character(alder_grp)))

  } else {

    bef_folkmangd <- rdverktyg::funktion_upprepa_forsok_om_fel(function() hamta_bef_folkmangd_v2(aldrar = NA)) |>
      dplyr::mutate(alder_grp = "alla åldrar")
    till_aldervekt <- NA
  }

  # välj år för prognosen utifrån senaste år för befolkning
  start_ar <- as.character(as.numeric(max(bef_folkmangd$år)) + 1)
  slut_ar <- as.character(as.numeric(start_ar) + jmfr_tid)

  hamta_region <- region_vekt[region_vekt != "00"]
  hamta_riket <- region_vekt[region_vekt == "00"]

  if (length(hamta_region) > 0) {
    bef_prognos <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
      hamta_befprognos_data(region_vekt = hamta_region,
                          tid_vekt = c(start_ar:slut_ar),
                          url_prognos_vektor = url_befprognos_tabell
                          ), max_forsok = 4
    )
  } else bef_prognos <- NULL

  if (!any(is.na(aldersgrupper))) {
    bef_prognos <- bef_prognos |>
      dplyr::filter(ålder %in% paste0(aldrar_hamta, " år")) |>
      dplyr::mutate(alder_grp = rdverktyg::skapa_aldersgrupper(ålder, till_aldervekt),
             alder_grp = dplyr::if_else(stringr::str_detect(alder_grp, "-100 år"), stringr::str_replace(alder_grp, "-100 år", "+ år"), as.character(alder_grp)))
  } else {
    bef_prognos <- dplyr::mutate(bef_prognos, alder_grp = "alla åldrar")
  }
  # special för att hantera förändring av namn på innehållsvariable från Folkmängd till Antal
  if ("Folkmängd" %in% names(bef_prognos)) bef_prognos <- dplyr::rename(bef_prognos, Antal = Folkmängd)

  if (length(hamta_riket) > 0) {
    # Riket-uttaget (BefolkprognRevNb) är inte migrerat till pxweb2r ännu - samma motivering/lösning som
    # i SkapaBefPrognosDiagram() i diag_befolkningsprognos_scb_api_profet.R. func_API.R behövs av
    # hamta_giltiga_varden_fran_tabell() inuti den sourcade filen, och sourcas bara här (dvs. bara om
    # riket faktiskt efterfrågas).
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_BefolkprognRevNb_scb.R")
    if (!any(is.na(aldersgrupper))) {
      # med åldersgrupper
      bef_prognos_riket <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
        hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb(tid_koder = c(start_ar:slut_ar)
        ), max_forsok = 4
      ) |>
        dplyr::mutate(regionkod = "00",
               region = "Sverige",
               alder_koder = list(aldrar_hamta),
               prognos_ar = as.character(as.numeric(start_ar) - 1)) |>
        dplyr::group_by(regionkod, region, kön, år, prognos_ar) |>
        dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(alder_grp = rdverktyg::skapa_aldersgrupper(ålder, till_aldervekt))

    } else {
      # utan åldersgrupper
      bef_prognos_riket <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
        hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb(tid_koder = c(start_ar:slut_ar)
        ), max_forsok = 4
      ) |>
        dplyr::mutate(regionkod = "00",
               region = "Sverige",
               prognos_ar = as.character(as.numeric(start_ar) - 1)) |>
        dplyr::group_by(regionkod, region, kön, år, prognos_ar) |>
        dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(alder_grp = "alla åldrar")

    } # slut if-sats om vi har med åldersgrupper

  } else bef_prognos_riket <- NULL

  # lägg ihop regioner och riket, ta bara bort om en df = NULL,
  # finns två dataframes (både region(er) och riket) så läggs de
  # ihop med list_rbind() så resultatet blir alltid en df
  bef_prognos <- purrr::compact(list(bef_prognos, bef_prognos_riket)) |>
    purrr::list_rbind()

  bef_prognos2 <- bef_prognos |>
    dplyr::mutate(typ = "prognos") |>
    dplyr::group_by(regionkod, region, typ, alder_grp, år) |>
    dplyr::summarise(Antal = round(sum(Antal, na.rm = TRUE)), .groups = "drop")


  bef_folk_progn <- bef_folkmangd |>
    dplyr::mutate(typ = "utfall") |>
    dplyr::group_by(regionkod, region, typ, alder_grp, år) |>
    dplyr::summarise(Antal = round(sum(Antal, na.rm = TRUE)), .groups = "drop") |>
    dplyr::bind_rows(bef_prognos2) |>
    dplyr::arrange(regionkod, region, alder_grp, år) |>
    dplyr::mutate(Befolkningsökning = Antal - dplyr::lag(Antal, 1),
           bef_okning_rel = Befolkningsökning / dplyr::lag(Antal) * 100) |>
    dplyr::filter(år != min(år))

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if (returnera_dataframe_global_environment) {
    assign("bef_utfall_prognos_per_region_tot", bef_folk_progn, envir = .GlobalEnv)
  }

  # hantera diagram_capt
  if (identical(diagram_capt, "auto")) {
    diagram_capt <- dplyr::case_when(
      stringr::str_detect(url_befprognos_tabell, "api.scb.se") & stringr::str_detect(url_befprognos_tabell, "Profet/datafiler") ~
        "Källa: SCB:s befolkningsprognos och Region Dalarnas egna befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna\nI Region Dalarnas befolkningsprognos baseras prognosen för Ludvika kommun på ett scenario som i allt väsentligt liknar det som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt.",
      stringr::str_detect(url_befprognos_tabell, "api.scb.se") ~
        "Källa: SCB:s befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna",
      stringr::str_detect(url_befprognos_tabell, "Profet/datafiler") & any(region_vekt %in% c("20", "2085")) ~
        "Källa: Region Dalarnas egna befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna\nPrognosen för Ludvika kommun baseras på ett scenario som i allt väsentligt liknar den som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt.",
      stringr::str_detect(url_befprognos_tabell, "Profet/datafiler") ~
        "Källa: Region Dalarnas egna befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna"
    )
  }

  # byt ut <prognos_ar> mot året som prognosen är från om det finns i textsträngen
  diagram_capt <- diagram_capt |>
    stringr::str_replace_all("<prognos_ar>", unique(bef_prognos$prognos_ar))


  skapa_diagram <- function(skickad_regionkod, skickad_alder_grp) {

    diagram_df <- bef_folk_progn |>
      dplyr::filter(regionkod %in% skickad_regionkod,
             alder_grp %in% skickad_alder_grp)

    if (any(c("03", "19") %in% diagram_df$regionkod)) {
      diagram_df <- diagram_df |>
        dplyr::mutate(bef_okning_rel = dplyr::if_else(regionkod %in% c("03", "19") & år == "2006", 0, dplyr::lag(bef_okning_rel)))
    }

    regionkoder <- unique(diagram_df$regionkod)
    region_filnamn <- paste0(regionkoder, collapse = "_")
    region_filnamn <- rdverktyg::ar_alla_kommuner_i_ett_lan(regionkoder, returnera_text = TRUE, returtext = region_filnamn)
    startar_utfall <- diagram_df |> dplyr::filter(typ == "utfall") |> dplyr::pull(år) |> min()
    slutar_utfall <- diagram_df |> dplyr::filter(typ == "utfall") |> dplyr::pull(år) |> max()
    startar_prognos <- diagram_df |> dplyr::filter(typ == "prognos") |> dplyr::pull(år) |> min()
    slutar_prognos <- diagram_df |> dplyr::filter(typ == "prognos") |> dplyr::pull(år) |> max()
    alder_grp_filnamn <- if (skickad_alder_grp == "alla åldrar") "" else paste0("_", skickad_alder_grp)
    alder_grp_txt <- if (skickad_alder_grp == "alla åldrar") "" else paste0(" för invånare ", skickad_alder_grp)
    enhet_txt <- if (antal_istallet_for_andel) "Befolkningsförändring" else "Relativ befolkningsförändring"
    enhet_filnamn <- if (antal_istallet_for_andel) "" else "rel_"

    diagramtitel <- glue::glue("{enhet_txt} år {startar_utfall}-{slutar_utfall} samt befolkningsprognos {startar_prognos}-{slutar_prognos}{alder_grp_txt}")
    diagramfil <- glue::glue("{enhet_filnamn}befforandring_utfall_progn_{region_filnamn}_ar{startar_utfall}-{slutar_prognos}{alder_grp_filnamn}{filformat}")


    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_df,
                                 skickad_x_var = "år",
                                 skickad_y_var = if (antal_istallet_for_andel) "Befolkningsökning" else "bef_okning_rel",
                                 skickad_x_grupp = "typ",
                                 diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = if (antal_istallet_for_andel) "förändring antal invånare" else "procent",
                                 manual_color = diagram_fargvekt,
                                 output_mapp = output_mapp,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 dataetiketter = visa_dataetiketter,
                                 legend_vand_ordning = TRUE,
                                 facet_grp = "region",
                                 facet_scale = "free_x",
                                 x_axis_storlek = 7,
                                 facet_x_axis_storlek = 5,
                                 x_axis_visa_var_xe_etikett = x_axis_visa_var_xe_etikett,
                                 x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = x_axis_var_xe_etikett_ta_bort_nast_sista_vardet,
                                 facet_legend_bottom = TRUE,
                                 diagram_bildformat = stringr::str_sub(filformat, -3),
                                 skriv_till_diagramfil = skriv_till_diagramfil
    ) # slut skriv ggplot_objekt

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(as.character(diagramfil), "\\.[^.]+$")

    return(gg_list)

  } # slut funktion som skapar diagrammet

  retur_list <- purrr::map(unique(bef_folk_progn$alder_grp), ~ skapa_diagram(
    skickad_regionkod = region_vekt,
    skickad_alder_grp = .x)) |> purrr::flatten()

  if (skriv_till_excelfil) {
    regionkoder <- unique(bef_folk_progn$regionkod)
    region_filnamn <- paste0(regionkoder, collapse = "_")
    region_filnamn <- rdverktyg::ar_alla_kommuner_i_ett_lan(regionkoder, returnera_text = TRUE, returtext = region_filnamn)
    startar_utfall <- bef_folk_progn |> dplyr::filter(typ == "utfall") |> dplyr::pull(år) |> min()
    slutar_prognos <- bef_folk_progn |> dplyr::filter(typ == "prognos") |> dplyr::pull(år) |> max()
    excefilnamn <- glue::glue("befolkning_utfall_progn_{region_filnamn}_ar{startar_utfall}-{slutar_prognos}.xlsx")
    writexl::write_xlsx(bef_folk_progn, paste0(output_mapp, excefilnamn))
  }

  return(retur_list)

} # slut funktion

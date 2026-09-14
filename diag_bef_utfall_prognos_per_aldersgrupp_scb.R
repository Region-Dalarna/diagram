
diag_bef_utfall_prognos_per_aldersgrupp <- function(
    region_vekt = "20",                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region
    gruppera_namn = NA,                               # om NA skapas ett diagram per region, annars grupperas de ihop och får namnet som anges här
    aldersindelning = c(0, 1, 6, 16, 20, 66, 80),
    filtrera_alder = NA,                  # lägg in åldrar här som siffror så filtreras dessa ut efter att data hämtats ut, det går alltså inte snabbare att göra så
    diagram_capt = "auto",           # diagram_capt skapa automatiskt och blir olika beroende på vilken tabell som används
        # om <prognos_ar> ligger med i diagram_capt så byts det ut mot det år prognosen gjordes
    output_mapp = NA,                                        # här sparas diagramet
    diagram_fargvekt = NA,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    jmfr_tid = 10,                                           # hur många år framåt från befolkningsprognosen vi ska ta med
    x_axis_visa_var_xe_etikett = 3,
    x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = TRUE,  # ta bort var x:te etikett för det näst sista värdet på x-axeln, för att undvika överlappning med sista etiketten
    returnera_dataframe_global_environment = FALSE,
    ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
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
  # SkapaBefPrognosDiagram() i diag_befolkningsprognos_scb_api_profet.R.
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
  # åldrar.
  hamta_alderskoder <- function(table_id, aldrar) {
    totalkod <- if (table_id == "TAB638") "tot" else "TotSA"
    if (all(is.na(aldrar))) return(totalkod)
    v <- pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE)
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
      on_all_values_invalid = "null", quiet = TRUE)
    ckm <- pxweb2r::pxweb2_get_data(
      table = "TAB5557",
      query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                   Alder = hamta_alderskoder("TAB5557", aldrar), Kon = c("män", "kvinnor"),
                   ContentsCode = "Folkmängd", Tid = "*"),
      on_all_values_invalid = "null", quiet = TRUE)

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

  bef_folkmangd <- rdverktyg::funktion_upprepa_forsok_om_fel(function() hamta_bef_folkmangd_v2(aldrar = "*"))

  # gruppera
  if (!is.na(gruppera_namn)) {
    bef_folkmangd <- bef_folkmangd |>
      dplyr::select(-c(regionkod, region)) |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(region = gruppera_namn, regionkod = "grupp")
  }

  # filtrera åldrar om användaren skickat med filtrera_alder
  if (any(!is.na(filtrera_alder))) {
    alder_vektor <- paste0(filtrera_alder, " år")
    bef_folkmangd <- dplyr::filter(bef_folkmangd, ålder %in% alder_vektor)
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

  # special för att hantera förändring av namn på innehållsvariable från Folkmängd till Antal
  if ("Folkmängd" %in% names(bef_prognos)) bef_prognos <- dplyr::rename(bef_prognos, Antal = Folkmängd)

  if (length(hamta_riket) > 0) {
    # Riket-uttaget (BefolkprognRevNb) är inte migrerat till pxweb2r ännu - samma motivering/lösning som
    # i SkapaBefPrognosDiagram() i diag_befolkningsprognos_scb_api_profet.R. func_API.R behövs av
    # hamta_giltiga_varden_fran_tabell() inuti den sourcade filen, och sourcas bara här (dvs. bara om
    # riket faktiskt efterfrågas).
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_BefolkprognRevNb_scb.R")
    bef_prognos_riket <- rdverktyg::funktion_upprepa_forsok_om_fel(function()
      hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb(tid_koder = c(start_ar:slut_ar)
      ), max_forsok = 4
    ) |>
      dplyr::mutate(regionkod = "00",
             region = "Sverige",
             prognos_ar = as.character(as.numeric(start_ar) - 1),
             alder_num = readr::parse_number(ålder),
             ålder = dplyr::if_else(alder_num > 99, "100+ år", ålder)) |>
      dplyr::group_by(regionkod, region, kön, ålder, år, prognos_ar) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop")

  } else bef_prognos_riket <- NULL

  # lägg ihop regioner och riket, ta bara bort om en df = NULL,
  # finns två dataframes (både region(er) och riket) så läggs de
  # ihop med list_rbind() så resultatet blir alltid en df
  bef_prognos <- purrr::compact(list(bef_prognos, bef_prognos_riket)) |>
    purrr::list_rbind()

  # gruppera
  if (!is.na(gruppera_namn)) {
    bef_prognos <- bef_prognos |>
      dplyr::select(-c(regionkod, region)) |>
      dplyr::group_by(dplyr::across(dplyr::where(~ is.character(.x) || is.factor(.x)))) |>
      dplyr::summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(region = gruppera_namn, regionkod = "grupp")
  }

  # filtrera åldrar om användaren skickat med filtrera_alder
  if (any(!is.na(filtrera_alder))) {
    alder_vektor <- paste0(filtrera_alder, " år")
    bef_prognos <- dplyr::filter(bef_prognos, ålder %in% alder_vektor)
  }

  bef_prognos2 <- bef_prognos |>
    dplyr::mutate(aldergrp = rdverktyg::skapa_aldersgrupper(ålder, aldersindelning),
           typ = "prognos") |>
    dplyr::group_by(regionkod, region, typ, aldergrp, år) |>
    dplyr::summarise(Antal = round(sum(Antal, na.rm = TRUE)), .groups = "drop")


  bef_folk_progn <- bef_folkmangd |>
    dplyr::filter(ålder != "totalt ålder") |>
    dplyr::mutate(aldergrp = rdverktyg::skapa_aldersgrupper(ålder, aldersindelning),
           typ = "utfall") |>
    dplyr::group_by(regionkod, region, typ, aldergrp, år) |>
    dplyr::summarise(Antal = round(sum(Antal, na.rm = TRUE)), .groups = "drop") |>
    dplyr::bind_rows(bef_prognos2) |>
    dplyr::arrange(regionkod, region, aldergrp, år) |>
    dplyr::mutate(Befolkningsökning = Antal - dplyr::lag(Antal, 1)) |>
    dplyr::filter(år != min(år))

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if (returnera_dataframe_global_environment) {
    assign("bef_utfall_prognos_per_aldersgrupp", bef_folk_progn, envir = .GlobalEnv)
  }

  # hantera diagram_capt
  if (identical(diagram_capt, "auto")) {
    diagram_capt <- dplyr::case_when(
      stringr::str_detect(url_befprognos_tabell, "api.scb.se") & stringr::str_detect(url_befprognos_tabell, "Profet/datafiler") ~
        "Källa: SCB:s befolkningsprognos och Region Dalarnas egna befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna\nI Region Dalarnas befolkningsprognos baseras prognosen för Ludvika kommun på ett scenario som i allt väsentligt liknar det som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt.",
      stringr::str_detect(url_befprognos_tabell, "api.scb.se") ~
        "Källa: SCB:s befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna",
      any(stringr::str_detect(url_befprognos_tabell, "Profet/datafiler") & region_vekt %in% c("20", "2085")) ~
        "Källa: Region Dalarnas egna befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna\nPrognosen för Ludvika kommun baseras på ett scenario som i allt väsentligt liknar den som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt.",
      stringr::str_detect(url_befprognos_tabell, "Profet/datafiler") ~
        "Källa: Region Dalarnas egna befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna"
    )
  }

  # byt ut <prognos_ar> mot året som prognosen är från om det finns i textsträngen
  diagram_capt <- diagram_capt |>
    stringr::str_replace_all("<prognos_ar>", unique(bef_prognos$prognos_ar)) |>
    rdverktyg::list_komma_och()


  skapa_diagram <- function(skickad_regionkod) {

    diagram_df <- if (!is.na(gruppera_namn)) {
      bef_folk_progn
    } else {
      dplyr::filter(bef_folk_progn, regionkod %in% skickad_regionkod)
    }

    region_txt <- if (!is.na(gruppera_namn)) gruppera_namn else rdverktyg::skapa_kortnamn_lan(unique(diagram_df$region))
    region_filnamn <- if (!is.na(gruppera_namn)) stringr::str_replace_all(gruppera_namn, " ", "_") else region_txt
    startar_utfall <- diagram_df |> dplyr::filter(typ == "utfall") |> dplyr::pull(år) |> min()
    slutar_utfall <- diagram_df |> dplyr::filter(typ == "utfall") |> dplyr::pull(år) |> max()
    startar_prognos <- diagram_df |> dplyr::filter(typ == "prognos") |> dplyr::pull(år) |> min()
    slutar_prognos <- diagram_df |> dplyr::filter(typ == "prognos") |> dplyr::pull(år) |> max()

    diagramtitel <- glue::glue("Befolkning i {region_txt} år {startar_utfall}-{slutar_utfall} samt befolkningsprognos {startar_prognos}-{slutar_prognos}")
    diagramfil <- glue::glue("befolkning_utfall_progn_{stringr::str_replace_all(region_filnamn, ',', '_')}_ar{startar_utfall}-{slutar_prognos}{filformat}")


    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_df,
                                 skickad_x_var = "år",
                                 skickad_y_var = "Antal",
                                 skickad_x_grupp = "typ",
                                 diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagram_fargvekt,
                                 manual_y_axis_title = "antal invånare",
                                 output_mapp = output_mapp,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 dataetiketter = visa_dataetiketter,
                                 legend_vand_ordning = TRUE,
                                 facet_grp = "aldergrp",
                                 facet_scale = "free",
                                 x_axis_storlek = 7,
                                 facet_x_axis_storlek = 6,
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

  if (!is.na(gruppera_namn)) {
    retur_list <- skapa_diagram(skickad_regionkod = unique(bef_folk_progn$regionkod))
  } else {
    retur_list <- purrr::map(region_vekt, ~ skapa_diagram(skickad_regionkod = .x)) |> purrr::flatten()
  }

  if (skriv_till_excelfil) {
    region_xlsx <- unique(bef_folk_progn$region) |> rdverktyg::skapa_kortnamn_lan() |> paste0(collapse = "_")
    startar_utfall <- bef_folk_progn |> dplyr::filter(typ == "utfall") |> dplyr::pull(år) |> min()
    slutar_prognos <- bef_folk_progn |> dplyr::filter(typ == "prognos") |> dplyr::pull(år) |> max()
    excefilnamn <- glue::glue("befolkning_utfall_progn_{region_xlsx}_ar{startar_utfall}-{slutar_prognos}.xlsx")
    writexl::write_xlsx(bef_folk_progn, paste0(output_mapp, excefilnamn))
  }

  return(retur_list)

} # slut funktion

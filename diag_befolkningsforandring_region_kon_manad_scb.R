diag_befolkningsforandring_manad_scb <- function(region_vekt = "20",
                                                 region_fokus = NA,                     # om man vill lyfta fram någon eller några regioner (görs med regionkoder)
                                                 facet_region = TRUE,
                                                 befforandr_klartext = c("folkmängd", "folkökning"),
                                                 kon_klartext = "totalt",
                                                 tid_koder = "*",
                                                 nth_etikett = 3,
                                                 skriv_diagramfil = TRUE,
                                                 utmapp = rdverktyg::utskriftsmapp(),
                                                 diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
                                                 ) {

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
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # hamta_data-repots hamta_bef_forandringar_region_kon_manad_scb.R (v1:
  # BE0101G/ManadBefStatRegion + MBefStatRegionCKM) hämtas här direkt via
  # v2-motsvarigheterna TAB1625 (historik) + TAB6473 (CKM), samma tabellpar
  # som redan verifierats i diag_bef_forandring_per_manad.R. OBS: precis
  # som i det skriptet anropade originalet en obefintlig funktion,
  # hamta_befolkningsforandringar_manad() - den faktiska funktionen heter
  # hamta_bef_forandringar_region_forandringar_kon_tid_scb().
  #
  # CKM-tabellens klartext för totalt kön är "totalt, samtliga män och
  # kvinnor" (kod "TotSa"), inte bara "totalt" som i historiktabellen -
  # byt bara ut just den etiketten, lämna eventuella "män"/"kvinnor"
  # oförändrade (samma kod/etikett i båda tabellerna).
  kon_klartext_ckm <- ifelse(tolower(kon_klartext) == "totalt", "TotSa", kon_klartext)

  beffor_df_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB1625",
    query = list(
      Region = region_vekt,
      Forandringar = befforandr_klartext,
      Kon = kon_klartext,
      ContentsCode = "Befolkning",
      Tid = tid_koder
    ),
    on_all_values_invalid = "null")

  beffor_df_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6473",
    query = list(
      Region = region_vekt,
      Forandringar = befforandr_klartext,
      Kon = kon_klartext_ckm,
      ContentsCode = "Befolkning",
      Tid = tid_koder
    ),
    on_all_values_invalid = "null")

  beffor_df <- dplyr::bind_rows(beffor_df_historik, beffor_df_ckm) |>
    dplyr::rename(regionkod = region_kod, Befolkning = value) |>
    dplyr::select(-tabellinnehåll) |>
    # Samma normalisering som i hamta_bef_forandringar_region_kon_manad_scb.R:
    # CKM-tabellens etikett för totalt kön skiljer sig från historiktabellens.
    dplyr::mutate(kön = dplyr::if_else(stringr::str_detect(kön, "totalt"), "totalt", kön)) |>
    # Splittar månadskoden (t.ex. "2021M07") till år/månad/år_månad/månad_år -
    # detta var utkommenterat i originalet, vilket gjorde att alla senare
    # referenser till kolumnen månad_år (bl.a. diagramtiteln) aldrig kunde
    # fungera.
    rdverktyg::manader_bearbeta_scbtabeller() |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region))

  konsuppdelat <- ifelse(length(unique(beffor_df$kön)) > 1, TRUE, FALSE)
  kon_txt <- ifelse(konsuppdelat, "_kon", "")          # skapa textsträng för om diagrammet är könsuppdelat eller inte

  # skapa sortering av regioner där vissa kan läggas först i facet-diagrammen
  if (!all(is.na(region_fokus))) {
    reg_ej_fokus <- sort(unique(beffor_df$region[!beffor_df$regionkod %in% region_fokus]))
    reg_fokus <- unique(beffor_df$region[beffor_df$regionkod %in% region_fokus])
    region_sort <- c(reg_fokus, reg_ej_fokus)

    beffor_df <- beffor_df |>
      dplyr::mutate(region = factor(region, levels = region_sort),
             # OBS: originalet refererade en odefinierad variabel,
             # region_annan_farg - avsikten är uppenbarligen densamma som
             # region_fokus-parametern ovan (att fokusera just de angivna
             # regionerna).
             fokus = ifelse(regionkod %in% region_fokus, 2,1))
  } else beffor_df <- dplyr::mutate(beffor_df, fokus = 1)

  # bestäm vilken färgvektor som ska användas
  color_vekt <- if(length(unique(beffor_df$fokus)) > 1) rddiagram::diagramfarger("rd_gron")[c(1,4)] else rddiagram::diagramfarger("rd_gron")[1]
  if (konsuppdelat) color_vekt <- rddiagram::diagramfarger("kon")

  skapa_diagram <- function(beffor_val, region_val) {
    # skapa en textvariabel för befolkningsförändringsvariabeln som har stor bokstav i början
    beffor_val_txt <- paste0(toupper(stringr::str_sub(beffor_val, 1, 1)), stringr::str_sub(beffor_val, 2, nchar(beffor_val)))
    region_namn <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_val)$region))
    # OBS: originalet skickade regionkoden direkt till skapa_kortnamn_lan()
    # utan att först slå upp regionnamnet (till skillnad från region_namn
    # ovan) - gav filnamn med rå regionkod ("20") i stället för läsbart
    # namn ("Dalarna"). Fixat till samma uppslagning som region_namn.
    region_filnamn <- paste0(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_val)$region), collapse = "_")

    facet_txt <- if(facet_region) "" else paste0(" i ", region_namn)
    facet_filnamn <- if(facet_region) "_facet_" else ""

    diagram_titel <- paste0(beffor_val_txt, facet_txt, " ", levels(beffor_df$månad_år)[1], " - ", levels(beffor_df$månad_år)[length(levels(beffor_df$månad_år))])
    diagramfil <- paste0(beffor_val, "_", region_filnamn, facet_filnamn, kon_txt, ".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(beffor_df, förändringar == beffor_val, regionkod %in% region_val),
      skickad_x_var = "månad_år",
      skickad_y_var = "Befolkning",
      skickad_x_grupp = ifelse(konsuppdelat, "kön", NA),
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      facet_x_axis_storlek = 3,
      x_var_fokus = ifelse(konsuppdelat, NA, "fokus"),
      x_axis_visa_var_xe_etikett = if (facet_region) 12 else nth_etikett,
      manual_y_axis_title = beffor_val,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      stodlinjer_avrunda_fem = TRUE,
      manual_color = color_vekt,
      facet_grp = if (facet_region) "region" else NULL,
      facet_legend_bottom = if (konsuppdelat) TRUE else FALSE,
      logga_scaling = 20,
      skriv_till_diagramfil = skriv_diagramfil,
      output_mapp = utmapp,
      filnamn_diagram = diagramfil)

    # OBS: originalet skrev "gg_list <- c(gg_list, list(gg_obj))" här, men
    # eftersom "gg_list" bara tilldelas (inte <<-) skapar det en LOKAL
    # variabel inuti skapa_diagram() som skuggar den yttre gg_list - den
    # yttre listan uppdaterades alltså aldrig, och funktionens
    # return(gg_list) i slutet returnerade följaktligen alltid en tom
    # lista (bildfilerna skrevs fortfarande till disk, men inget
    # ggplot-objekt kom tillbaka i R). Bygger nu i stället en egen liten
    # namngiven lista per anrop och returnerar den.
    ett_diagram <- list(gg_obj)
    names(ett_diagram) <- stringr::str_remove(diagramfil, ".png")
    return(ett_diagram)
  } # slut skapa_diagram


  arglist <- list(beftyp = as.character(unique(beffor_df$förändringar)), reg = as.character(unique(beffor_df$regionkod)))                               # skapa lista med de två variabler vi vill göra diagram med
  crossarg <- expand.grid(arglist, stringsAsFactors = FALSE)
  retur_list <- purrr::flatten(purrr::map2(crossarg$beftyp, crossarg$reg, ~ skapa_diagram(beffor_val = .x, region_val = .y)))

  return(retur_list)

} # slut funktion

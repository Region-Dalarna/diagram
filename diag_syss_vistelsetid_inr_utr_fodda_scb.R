# ====================================================================================================
#
# Skapar diagram för sysselsättningsgrad uppdelat på inrikes födda samt utrikes födda uppdelat på
# vistelsetid. Man skriver ut ett diagram per region och år. Varje år man skickar med blir ett
# eget diagram. Vill man jämföra flera regioner i ett facetdiagram så kör man facet_diagram = TRUE,
# annars blir det ett diagram per region (och år).
#
# På SCB är riket, län och kommuner egna tabeller men skriptet lägger ihop dessa om man vill.
#
# Skapat av: Peter Möller, Region Dalarna
# November 2023
#
# Migrerad till pxweb2r/rddiagram/rdverktyg. hamta_data-repots hamta_syss_vistelsetid_inr_utr_fodda_scb()
# slog ihop sex v1-tabeller (tre preliminära AA0003X-tabeller för riket/län/kommun samt tre
# "BAS"-tabeller AA0003B för samma tre nivåer). De tre preliminära AA0003X-tabellerna är numera helt
# borttagna ur SCB:s v1-API (400 Bad Request) - bara AA0003B-tabellerna (riket/län/kommun) finns kvar,
# och de motsvaras nu av tre v2-tabeller: AA0003B/IntGr1RikUtbBAS -> TAB6379, AA0003B/IntGr1LanUtbBAS
# -> TAB6384, AA0003B/IntGr1KomUtbBAS -> TAB6383. Används bara av det här skriptet - logiken läggs
# därför in direkt här i stället för i rdverktyg.
# ====================================================================================================

diag_syss_vistelsetid_inr_utr_fodda_ett_per_ar_scb <- function(
    region_vekt = "20",
    diagram_capt = "Källa: Registerdata för integration, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    cont_klartext = "Andel förvärvsarbetande (ny definition från och med 2019)",
    tid_vekt = "9999",                  # 9999 = senaste år, NA = alla år
    konsuppdelat = FALSE,
    facet_diagram = TRUE,
    skriv_ut_logga = TRUE,     # TRUE fö ratt lägga till logga, FALSE för att inte ha med det
    logga_path = NA,
    output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/"
    ) {

  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # ================ Hämta tabell från SCB =============================================

  hamta_en_tabell <- function(tabell_id, region_vekt, kon_klartext, cont_klartext_kod, bakgr_kod, tid_vekt) {

    giltiga_regioner <- pxweb2r::pxweb2_get_values(tabell_id, "Region")$code
    region_var <- region_vekt[region_vekt %in% giltiga_regioner]
    if (length(region_var) == 0) return(NULL)

    giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid")$code
    tid_var <- if (identical(tid_vekt, "9999")) max(giltiga_ar) else if (all(is.na(tid_vekt))) giltiga_ar else as.character(tid_vekt)
    tid_var <- tid_var[tid_var %in% giltiga_ar]
    if (length(tid_var) == 0) return(NULL)

    px <- pxweb2r::pxweb2_get_data(
      table = tabell_id,
      query = list(
        Region = region_var,
        Kon = kon_klartext,
        UtbNiv = "samtliga utbildningsnivåer",
        BakgrVar = bakgr_kod,
        ContentsCode = cont_klartext_kod,
        Tid = tid_var
      ),
      on_all_values_invalid = "null")
    if (is.null(px)) return(NULL)

    # pxweb2r ger alltid en generisk "value"-kolumn, oavsett vilket tabellinnehåll som begärts - döps
    # om till samma kolumnnamn som originalskriptets hamta_data-funktion producerade.
    dplyr::rename(px, `Andel sysselsatta` = value) |>
      dplyr::select(-tabellinnehåll, -utbildningsnivå)
  }

  # BakgrVar-koderna är stabila oavsett region-nivå: vistelsetid 0-1/2-3/4-9/10- år samt
  # "födelseregion: Sverige" (koder i stället för klartext - etiketterna har bytt från vanligt
  # bindestreck till en-dash i v2, t.ex. "vistelsetid 0–1 år").
  bakgr_kod <- c("INT010", "INT020", "INT030", "INT040", "SE")

  # ContentsCode-etiketten har bytt namn i v2 - "Andel förvärvsarbetande (ny definition ...)" heter nu
  # bara "Andel sysselsatta". Samma fallback-omskrivning som i original-hamta_data-funktionen.
  cont_klartext_kod <- cont_klartext |>
    stringr::str_remove(" \\(.*$") |>
    stringr::str_replace("förvärvsarbetande", "sysselsatta")

  kon_klartext <- if (konsuppdelat) c("män", "kvinnor") else "män och kvinnor"

  px_df <- purrr::map(
    c("TAB6379", "TAB6384", "TAB6383"),  # riket, län, kommun
    ~ hamta_en_tabell(.x, region_vekt, kon_klartext, cont_klartext_kod, bakgr_kod, tid_vekt)
  ) |>
    purrr::list_rbind() |>
    dplyr::rename(regionkod = region_kod)

    plot_df <- px_df |>
      dplyr::mutate(bakgrundsvariabel = ifelse(bakgrundsvariabel == "födelseregion: Sverige", "inrikes födda", bakgrundsvariabel),
             bakgrundsvariabel = stringr::str_remove(bakgrundsvariabel, "vistelsetid "),
             bakgrundsvariabel = factor(bakgrundsvariabel, levels = c("0\u20131 år", "2\u20133 år", "4\u20139 år", "10\u2013 år", "inrikes födda")))


    skapa_diagram <- function(vald_region, valt_ar) {

      diagram_df <- dplyr::filter(plot_df, regionkod %in% vald_region, år %in% valt_ar)

      antal_kon <- length(unique(diagram_df$kön))
      region_txt <- if (length(vald_region) > 1) " " else glue::glue(" i {rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(vald_region)$region)} ")
      tabell_ar <- unique(diagram_df$år)
      ar_txt <- ifelse(length(tabell_ar) == 1, tabell_ar, paste0(min(tabell_ar), "-", max(tabell_ar)))

      kon_txt <- if (antal_kon > 1) "_kon" else ""

      diagram_fil <- glue::glue("syss_vistelsetid{kon_txt}_{paste0(region_vekt, collapse = '_')}_ar_{ar_txt}.png")
      diagramtitel <- glue::glue("Andel förvärvsarbetande{region_txt}år {unique(diagram_df$år)}")

      gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_df,
                         skickad_x_var = "bakgrundsvariabel",
                         skickad_y_var = "Andel sysselsatta",
                         skickad_x_grupp = if (antal_kon > 1) "kön" else NA,
                         diagram_titel = diagramtitel,
                         diagram_capt = diagram_capt,
                         output_mapp = output_mapp,
                         filnamn_diagram = diagram_fil,
                         manual_color = if (antal_kon > 1) rddiagram::diagramfarger("kon") else rddiagram::diagramfarger("rus_sex"),
                         x_axis_lutning = 0,
                         procent_0_100_10intervaller = TRUE,
                         #manual_x_axis_text_hjust = 1,
                         #manual_x_axis_text_vjust = 1,
                         manual_y_axis_title = "procent",
                         manual_x_axis_title = "vistelsetid i Sverige",
                         facet_grp = if (length(vald_region) > 1) "region" else NULL,
                         facet_scale = "fixed",
                         facet_legend_bottom = if (antal_kon > 1) TRUE else FALSE,
                         y_axis_100proc = TRUE,
                         lagg_pa_logga = skriv_ut_logga,
                         logga_path = logga_path,
                         dataetiketter = FALSE)

      gg_list <- list(gg_obj)
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagram_fil, "\\.[^.]+$")

      return(gg_list)

    } # slut funktion för att skapa diagram

    dia_lista <- list()
    if (facet_diagram) {
      dia_lista <- skapa_diagram(vald_region = region_vekt, valt_ar = unique(plot_df$år))
      #names(dia_lista) <- paste0("diff_syss_", vald_region_txt, facet_val_txt, etikett_txt)
    } else {
      arglist <- list(reg = region_vekt, valt_ar = unique(plot_df$år))                               # skapa lista med de två variabler vi vill göra diagram med
      crossarg <- expand.grid(arglist)
      # dia_lista <- map2(crossarg$reg, crossarg$bakgr, crossarg$valt_kon, ~skapa_diagram(vald_reg = .x, vald_bakgrund = .y, valt_kon = .z)) %>% flatten()
      dia_lista <- purrr::pmap(crossarg, ~skapa_diagram(vald_region = ..1, valt_ar = ..2)) |>
        unlist(recursive = FALSE)
    }

    dia_lista

} # slut funktion

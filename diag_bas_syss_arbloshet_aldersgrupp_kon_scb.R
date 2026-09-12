diag_bas_syss_arbloshet_aldersgrupp_kon_scb <- function(
    vald_region = "20",
    vald_alder = c("16-19 år", "20-24 år", "25-29 år", "30-34 år", "35-39 år", "40-44 år",
                   "45-49 år", "50-54 år", "55-59 år", "60-64 år", "65-69 år", "70-74 år"),
    vald_cont = c("arbetslöshet", "sysselsättningsgrad"),
    skriv_diagramfil = TRUE,
    diagram_capt = "Källa: Befolkningens arbetsmarknadsstatus (BAS), SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    logga_i_diagram = NA,
    diag_fargvekt = NA,
    demo = FALSE,           # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    utmapp
    ) {

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/Inrikes_flyttnetto_alder_Dalarna.png",
        "https://region-dalarna.github.io/utskrivna_diagram/Inrikes_flyttnetto_alder_20-29_%C3%A5r_Dalarna.png")
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
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("kon")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("kon")
  }

  # SCB skriver åldersintervallen med tankstreck ("16–19 år", U+2013), inte
  # vanligt bindestreck. Normaliserar bindestreck mellan siffror till
  # tankstreck här, samma fix som i systerskripten för denna tabell.
  vald_alder_fetch <- gsub("(?<=[0-9])-(?=[0-9])", "–", vald_alder, perl = TRUE)

  # hämta data från SCB via API. Länk till tabell:
  # https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  syss_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = vald_region,
      Kon = c("Män", "Kvinnor"),
      Alder = vald_alder_fetch,
      Fodelseregion = "*",
      ContentsCode = vald_cont,
      Tid = "*"
    )) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)

  # ta bort rader med NA och fixa till månadsvariabeln
  chart_df <- syss_df |>
    dplyr::filter(!is.na(varde)) |>
    rdverktyg::manader_bearbeta_scbtabeller()

  # skapa en funktion för att göra själva diagrammet, som sedan används för varje unik kombination
  skriv_diagram <- function(reg, cont_var) {

    vald_region_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(vald_region)$region, T)

    diagramtitel <- glue::glue("{stringr::str_to_sentence(cont_var)} per åldersgrupp och kön i {vald_region_txt} i {unique(dplyr::last(chart_df$månad_år))}")
    diagramfil <- glue::glue("{cont_var}_alder_kon_{vald_region_txt}_{dplyr::last(chart_df$tid)}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(chart_df,
                                  regionkod == reg,
                                  variabel == cont_var,
                                  födelseregion != "totalt",
                                  tid == max(tid)),
      skickad_x_var = "ålder",
      skickad_y_var = "varde",
      skickad_x_grupp = "kön",
      diagram_titel = diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      procent_0_100_10intervaller = ifelse(stringr::str_detect(cont_var, "antal"), FALSE, TRUE),
      filnamn_diagram = diagramfil,
      dataetiketter = FALSE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diag_fargvekt,
      manual_y_axis_title = ifelse(stringr::str_detect(cont_var, "antal"), "", "procent"),
      output_mapp = utmapp,
      skriv_till_diagramfil = skriv_diagramfil,
      logga_path = logga_i_diagram,
      facet_grp = "födelseregion",
      facet_scale = "fixed"
    )
  } # slut skriv_diagram-funktion

  # gör ett diagram för varje unik kombination av region och innehållsvariabel
  arglist <- list(reg = vald_region, cont_var = unique(chart_df$variabel))                               # skapa lista med de två variabler vi vill göra diagram med
  crossarg <- expand.grid(arglist)
  # skriv_diagram() returnerar ett enda ggplot-objekt (inte en lista), så
  # map2() ger redan en platt lista - purrr::flatten() på en lista av
  # ggplot-objekt kraschar ("must be a vector, not a <ggplot2::ggplot>
  # object") mot dagens purrr, precis som i
  # diag_bas_arbmstatus_forandr_senmanad_arsvis_scb.R. Samma bugg fanns i
  # originalet redan innan migreringen. flatten()-anropet togs bort.
  dia_lista <- purrr::map2(crossarg$reg, crossarg$cont_var, ~skriv_diagram(reg = .x, cont_var = .y))

  return(dia_lista)

} # slut diag-funktion

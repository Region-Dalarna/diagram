diag_bas_arbloshet_manad_jmfr_1ar_tillbaka_scb <- function(
    vald_region = NA,     # NA = jämförelse mellan länen, annars jämförs kommuner i de regioner vars länskoder man skickar med
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
    tid_koder = "9999",
    dela_upp_utrikes = TRUE, # Sätts till FALSE om man vill ha förändring i arbetslöshet totalt, dvs. inte uppdelat på inrikes/utrikes-födda
    returnera_data = FALSE, # Sätts till TRUE om man vill returnera data till R-studios global environment
    jamfor_antal_manader_bakat = 12,
    visa_dataetiketter = FALSE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {

# Ändrat så att man även kan jämföra förändring i arbetslöshet totalt, dvs. inte uppdelat på utrikes/inrikes födda Jon 2026-06-24
# Det går dessutom att returnera data till global environment
# SCB har lagt till ett längre streck mellan åren i åldersgrupper. Har ändrat nedan. Jon 2026-08-25

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

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/bas_arbloshet_jmfr_Sveriges län_juli_ar2023-2024.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  gg_list <- list()

  hamta_region <- if (all(is.na(vald_region))) rdverktyg::hamtaAllaLan(FALSE) else rdverktyg::hamtakommuner(lan = vald_region, tamedlan = TRUE, tamedriket = TRUE)

  # Om man vill dela upp utrikes hanteras det här. Begär explicit
  # "inrikes född"/"utrikes född" (inte "*") så att den eliminerbara
  # totalkategorin inte kommer med som ett tredje, oönskat facet-fönster.
  fodelseregion_val <- if (dela_upp_utrikes == TRUE) c("inrikes född","utrikes född") else "totalt"

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  # Räknar ut vilken månad som ska jämföras mot (N månader bakåt), på samma
  # sätt som den gamla hamta_data-funktionen gjorde: hitta positionen för
  # begärd månad i tabellens giltiga (kronologiskt sorterade) månadskoder,
  # och plocka koden N steg tidigare.
  giltiga_manader <- pxweb2r::pxweb2_get_values("TAB6260", "Tid")$code
  tid_nu <- if (tid_koder == "9999") max(giltiga_manader) else tid_koder
  idx_nu <- which(giltiga_manader == tid_nu)
  idx_da <- idx_nu - jamfor_antal_manader_bakat
  tid_hamta <- unique(c(tid_nu, giltiga_manader[idx_da]))

  bas_syss_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = hamta_region,
      Kon = "totalt",
      Alder = "20–64 år",
      Fodelseregion = fodelseregion_val,
      ContentsCode = "arbetslöshet",
      Tid = tid_hamta
    )) |>
    dplyr::rename(regionkod = region_kod, arbetslöshet = value) |>
    dplyr::select(-tabellinnehåll, -dplyr::any_of("kön")) |>
    rdverktyg::manader_bearbeta_scbtabeller() |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

  manad_nu <- as.character(dplyr::last(bas_syss_df$tid))
  manad_da <- as.character(dplyr::first(bas_syss_df$tid))

  chart_df <- bas_syss_df |>
    dplyr::select(-c(år, månad, månad_år, år_månad)) |>
    tidyr::pivot_wider(names_from = tid, values_from = arbetslöshet) |>
    dplyr::mutate(diff = .data[[manad_nu]] - .data[[manad_da]])

  # Returnerar data till R global environment
  if(returnera_data == TRUE){
    assign("forandring_arbetsloshet_df", chart_df, envir = .GlobalEnv)
  }

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(bas_syss_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  regionkod_txt <- if (region_start == region_txt) paste0(unique(bas_syss_df$regionkod), collapse = "_") else region_txt

  manad_txt <- as.character(dplyr::pull(dplyr::distinct(bas_syss_df, månad)))

  ar_start <- as.character(dplyr::first(dplyr::pull(dplyr::distinct(bas_syss_df, år))))

  ar_slut <- as.character(dplyr::last(dplyr::pull(dplyr::distinct(bas_syss_df, år))))

  diagramtitel <- glue::glue("Skillnad i arbetslöshet för invånare 20-64 år i {region_txt}\ni {manad_txt} år {ar_slut} jämfört med {manad_txt} år {ar_start}")

  # Ändrar namn på figur baserat på uppdelning
  if(dela_upp_utrikes == TRUE){
    diagramfil <- glue::glue("bas_arbloshet_jmfr_{regionfil_txt}_{manad_txt}_ar{ar_start}-{ar_slut}.png")}else{
      diagramfil <- glue::glue("bas_arbloshet_jmfr_totalt_{regionfil_txt}_{manad_txt}_ar{ar_start}-{ar_slut}.png")}


  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "region",
    skickad_y_var = "diff",
    skickad_x_grupp = if(dela_upp_utrikes == TRUE){"födelseregion"} else {NA},
    x_axis_sort_value = TRUE,
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    dataetiketter = visa_dataetiketter,
    manual_y_axis_title = "procentenheter",
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    manual_color = if(dela_upp_utrikes == TRUE){rddiagram::diagramfarger("rus_sex")} else {rddiagram::diagramfarger("rus_sex")[1]},
    output_mapp = output_mapp,
    facet_sort = TRUE,
    facet_grp = if(dela_upp_utrikes == TRUE){"födelseregion"} else {NULL},
    facet_scale = "free_x",
    facet_legend_bottom = FALSE
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

  return(gg_list)
}

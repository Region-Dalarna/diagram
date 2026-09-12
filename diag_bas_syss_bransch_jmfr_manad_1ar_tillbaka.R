diag_bas_syss_per_bransch_manad_jmfr_1ar_tillbaka <- function(
    region_vekt = "20",
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    diag_fargvekt = NA,
    tid_koder = "9999",
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    returnera_dataframe_global_environment = FALSE,
    manader_diagramtitel_kortnamn = TRUE,
    jamfor_antal_manader_bakat = 12,
    dagbefolkning = TRUE,                 # om FALSE så visas nattbefolkning, TRUE = dagbefolkning
    stodlinjer_avrunda_fem = TRUE,
    skriv_till_diagramfil = TRUE,
    ta_bort_diagramtitel = FALSE,                    # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/bas_syss_Dalarna_augusti_ar2024_jmfrt_med_augusti_ar2024.png")
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
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  # dplyr/tidyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("kon")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("kon")
  }

  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (all(is.na(output_mapp))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_mapp <- rdverktyg::utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }

  gg_list <- list()

  variabel_dag_nattbefolkning <- if (dagbefolkning) "sysselsatta efter arbetsställets belägenhet" else "sysselsatta efter bostadens belägenhet"

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210B/ArbStDoNMNN/
  # (samma tabell, TAB3784, som redan används i diagram_forandring_sysselsatta_bransch.R/_kommun.R)
  # Räknar ut vilken månad som ska jämföras mot (N månader bakåt), samma
  # teknik som i diag_bas_arbloshet_jmfr_manad_1ar_tillbaka_region_scb.R.
  giltiga_manader <- pxweb2r::pxweb2_get_values("TAB3784", "Tid")$code
  tid_nu <- if (tid_koder == "9999") max(giltiga_manader) else tid_koder
  idx_nu <- which(giltiga_manader == tid_nu)
  idx_da <- idx_nu - jamfor_antal_manader_bakat
  tid_hamta <- unique(c(tid_nu, giltiga_manader[idx_da]))

  bas_syss_df <- pxweb2r::pxweb2_get_data(
    table = "TAB3784",
    query = list(
      Region = region_vekt,
      Kon = "*",
      SNI2007 = "*",
      Fodelseregion = "*",
      ContentsCode = variabel_dag_nattbefolkning,
      Tid = tid_hamta
    )) |>
    dplyr::rename(regionkod = region_kod, sni2007kod = `näringsgren sni 2007_kod`) |>
    dplyr::rename(!!variabel_dag_nattbefolkning := value) |>
    dplyr::select(-tabellinnehåll) |>
    rdverktyg::manader_bearbeta_scbtabeller()

  nyckel_bransch <- readxl::read_xlsx("G:/skript/nycklar/Bransch_Gxx_farger.xlsx")

  manad_nu <- as.character(dplyr::last(bas_syss_df$tid))
  manad_da <- as.character(dplyr::first(bas_syss_df$tid))

  chart_df <- bas_syss_df |>
    dplyr::select(-c(år, månad, månad_år, år_månad)) |>
    tidyr::pivot_wider(names_from = tid, values_from = {{variabel_dag_nattbefolkning}}) |>
    dplyr::mutate(diff = .data[[manad_nu]] - .data[[manad_da]]) |>
    dplyr::filter(kön != "totalt",
           `näringsgren SNI 2007` != "Total",
           födelseregion != "totalt") |>
    dplyr::left_join(dplyr::select(nyckel_bransch, Br15kod, Bransch), by = c("sni2007kod" = "Br15kod")) |>
    dplyr::mutate(Bransch = ifelse(is.na(Bransch), stringr::str_to_sentence(`näringsgren SNI 2007`), Bransch))

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("chart_df", chart_df, envir = .GlobalEnv)
  }

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(bas_syss_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) paste0(unique(bas_syss_df$regionkod), collapse = "_") else region_txt

  manad_start <- as.character(dplyr::first(dplyr::pull(dplyr::distinct(bas_syss_df, månad))))

  if (manader_diagramtitel_kortnamn) manad_start <- stringr::str_sub(manad_start, 1, 3)

  manad_slut <- as.character(dplyr::last(dplyr::pull(dplyr::distinct(bas_syss_df, månad))))

  if (manader_diagramtitel_kortnamn) manad_slut <- stringr::str_sub(manad_slut, 1, 3)

  ar_start <- as.character(dplyr::first(dplyr::pull(dplyr::distinct(bas_syss_df, år))))

  ar_slut <- as.character(dplyr::last(dplyr::pull(dplyr::distinct(bas_syss_df, år))))

  # ändra diagramtitel baserat på om det är dag- eller nattbefolkning
  region_txt <- if(dagbefolkning) {
    region_ny <- stringr::str_replace(region_txt, " i ", " på ")
    region_ny <- glue::glue("{region_ny}s arbetsmarknad")
  } else {
    region_ny <- glue::glue(" och boende{region_txt}")
  }

  # ändra diagramfilnamn baserat på om det är dag- eller nattbefolkning
  dagnatt_filnamn <- if(dagbefolkning) "dagbef" else "nattbef"

  diagramtitel <- glue::glue("Skillnad i antal sysselsatta 15-74 år {region_txt} i {manad_slut} år {ar_slut} jämfört med {manad_start} år {ar_start}")
  # OBS: originalet upprepade av misstag manad_slut/ar_slut även i jämförelse-
  # delen av filnamnet (kopieringsfel - titeln ovan gjorde det redan rätt med
  # manad_start/ar_start). Fixat så att filnamnet faktiskt visar båda
  # jämförda perioderna, inte samma period två gånger.
  diagramfil <- glue::glue("bas_syss_{regionfil_txt}_{manad_slut}_ar{ar_slut}_jmfrt_med_{manad_start}_ar{ar_start}_{dagnatt_filnamn}.png")


  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_x_var = "Bransch",
    skickad_y_var = "diff",
    skickad_x_grupp = "kön",
    diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
    diagram_capt = diagram_capt,
    diagram_liggande = TRUE,
    stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
    filnamn_diagram = diagramfil,
    dataetiketter = visa_dataetiketter,
    manual_y_axis_title = "",
    x_axis_lutning = 0,
    manual_color = diag_fargvekt,
    output_mapp = output_mapp,
    lagg_pa_logga = ta_med_logga,
    logga_path = logga_sokvag,
    skriv_till_diagramfil = skriv_till_diagramfil,
    facet_grp = "födelseregion",
    facet_scale = "fixed",
    facet_legend_bottom = TRUE
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

  return(gg_list)
}

# ===========================================================================================================
#
# Skript för att skriva ut ett facetdiagram över arbetslöshet i procent (inskrivna arbetslösa på
# Arbetsförmedlingen / arbetskraften [förvärvsarbetande + arbetslösa]) per utbildningsnivå eller
# utbildningsinriktning, över tid (år). Från SCB:s tabell TAB6367 (Arbetsmarknadsställning (U1a) i
# befolkningen 20-64 år, nattbefolkning).
#
# TAB6367 har en enda utbildningsdimension (Utbildngrupp) som blandar utbildningsnivå, utbildnings-
# inriktning OCH ca 120 detaljerade utbildningsgrupper (SUN-koder) i samma dimension - alldeles för
# många för ett facetdiagram. Den här funktionen fokuserar därför på de två grova indelningarna:
#   utb_typ = "niva":        4 nivåer (förgymnasial, gymnasial, eftergymnasial <3 år, eftergymnasial 3+ år)
#   utb_typ = "inriktning":  8 inriktningar (pedagogik, humaniora/konst, samhällsvetenskap m.fl., natur-
#                            vetenskap/data, teknik, lant-/skogsbruk, hälso-/sjukvård, tjänster)
# Vill man ha andra/fler koder (t.ex. "09" okänd nivå, "9" okänd inriktning, eller någon av de ~120
# detaljerade SUN-grupperna) går det att skicka med egna koder i utb_vekt.
#
# ===========================================================================================================

diag_arbloshet_procent_utbildning_facet_ar_scb <- function(
    region_vekt = "20",                 # c("20", "17", "21"), flera regioner jämförs då som linjer inom varje facet
    utb_typ = c("niva", "inriktning"),   # vilken utbildningsindelning som ska facet:as på
    utb_vekt = NA,                       # NA = default-koder för utb_typ (se nedan), annars egna Utbildngrupp-koder
    titel_bredd = 80,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Arbetsmarknadsställning (U1a) i befolkningen 20-64 år\nefter utbildningsnivå/-inriktning (nattbefolkning)\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    diag_fargvekt = NA,
    skickad_logga_path = NA,               # NA om man vill köra med standard, bra att sätta en lokal om man är på resande fot
    ta_med_logga = TRUE,
    skriv_till_diagramfil = TRUE,
    facet_scale = "fixed",                 # "fixed" gör att nivåerna/inriktningarna går att jämföra rakt av, "free_y" ger varje facet egen skala
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {

  utb_typ <- match.arg(utb_typ)

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
  # stället för library(), samma mönster som diag_huvink_alla_kat_ar_scb.R.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om parametern demo är satt till TRUE: inga exempelbilder är uppladdade för det här skriptet ännu.
  if (demo){
    cat("Inga exempelbilder är uppladdade för det här skriptet ännu.\n")
    rdverktyg::stop_tyst()
  }

  # Default-koder för Utbildngrupp beroende på utb_typ. Koderna är SUN2020-nivå/inriktningskoder,
  # se https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__UF__UF0506/Educ3MM/ - "00N" (samtliga
  # utbildningsnivåer) och "00I" (samtliga utbildningsinriktningar) är totalsummor och "09"/"9" är
  # okänd nivå/inriktning - utelämnas som standard men kan läggas till via utb_vekt.
  if (all(is.na(utb_vekt))) {
    utb_vekt <- if (utb_typ == "niva") {
      c("02", "03", "04", "05")
    } else {
      c("1", "2", "3", "4", "5", "6", "7", "8")
    }
  }

  # Radbryter långa utbildningsnamn ("samhällsvetenskap, juridik, handel, administration",
  # "eftergymnasial utbildning, mindre än 3 år" m.fl.) vid ordgränser så att facet-rubrikerna
  # radbryts i stället för att klippas av i de smala facet-panelerna. "/" och "," räknas inte som
  # ordgränser av stringr::str_wrap(), så ett mellanslag läggs till efter dem innan radbrytningen.
  wrap_kat <- function(x) stringr::str_wrap(stringr::str_replace_all(x, "([/,])(?=\\S)", "\\1 "), width = 25)

  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("rd_alla_primar")
  }

  if (all(is.na(output_mapp))) {
    output_mapp <- rdverktyg::utskriftsmapp()
  }

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0208__AM0208C/ArbStallnU1Ar/
  # ContentsCode hämtas som text (inte kod) - A = förvärvsarbetande, B = inskrivna arbetslösa på
  # Arbetsförmedlingen. Arbetskraften = A + B (C, "inte i arbetskraften", räknas INTE in i nämnaren).
  arbl_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6367",
    query = list(
      Region = region_vekt,
      Utbildngrupp = utb_vekt,
      ContentsCode = c("Förvärvsarbetande (A)", "Inskrivna arbetslösa på Arbetsförmedlingen (B)"),
      Tid = "*"
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod) |>
    tidyr::pivot_wider(names_from = tabellinnehåll, values_from = value) |>
    dplyr::rename(forvarvsarbetande = `Förvärvsarbetande (A)`,
                  arbetslosa = `Inskrivna arbetslösa på Arbetsförmedlingen (B)`) |>
    dplyr::mutate(andel_arbetslosa = round(arbetslosa / (forvarvsarbetande + arbetslosa) * 100, 1),
                  utbildning = wrap_kat(utbildning))

  vald_region <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(region_vekt)$region, byt_ut_riket_mot_sverige = TRUE))

  utb_typ_titel <- if (utb_typ == "niva") "utbildningsnivå" else "utbildningsinriktning"
  diagram_titel <- stringr::str_wrap(paste0("Arbetslöshet i procent efter ", utb_typ_titel, " i ", vald_region), titel_bredd)
  diagramfil <- paste0("arbloshet_procent_utb_", utb_typ, "_facet_", paste0(region_vekt, collapse = "_"), ".png")

  flera_regioner <- length(unique(arbl_df$regionkod)) > 1

  gg_obj <- rddiagram::SkapaLinjeDiagram(
    skickad_df = arbl_df,
    skickad_x_var = "år",
    skickad_y_var = "andel_arbetslosa",
    skickad_x_grupp = if (flera_regioner) "region" else NULL,
    lagga_till_punkter = TRUE,
    diagram_titel = diagram_titel,
    diagram_capt = diagram_capt,
    manual_y_axis_title = "andel arbetslösa, procent",
    manual_color = if (flera_regioner) diag_fargvekt else diag_fargvekt[1],
    x_axis_lutning = 0,
    legend_vand_ordning = TRUE,
    legend_byrow = TRUE,
    facet_grp = "utbildning",
    facet_scale = facet_scale,
    facet_x_axis_storlek = 8,
    facet_legend_bottom = TRUE,
    stodlinjer_avrunda_fem = TRUE,
    skriv_till_diagramfil = skriv_till_diagramfil,
    lagg_pa_logga = ta_med_logga,
    logga_path = skickad_logga_path,
    output_mapp = output_mapp,
    filnamn_diagram = diagramfil)

  retur_list <- list(gg_obj)
  names(retur_list) <- stringr::str_remove(diagramfil, ".png")

  return(retur_list)
} # slut funktion

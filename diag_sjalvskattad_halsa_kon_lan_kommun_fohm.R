diag_sjalvskattad_halsa_kon_lan_kommun <- function(
    region_vekt = "20",
    andel_konfinterv_klartext = "Andel",
    kon_klartext = c("Kvinnor", "Män"), # Finns också "Totalt"
    tid_koder = "9999",         # "9999" = senaste år
    region_sort = FALSE,        # TRUE så sorteras regionerna enligt ordningen i region_vekt
    diagram_fargvekt = NA,      # skicka med en färgvektor om man önskar andra färger än standard
    output_mapp = NA,           # hit skrivs png-filen
    returnera_dataframe_global_environment = FALSE,
    diagram_capt = "Källa: Folkhälsomyndighetens öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: 4-årsmedelvärden",
    visa_dataetiketter = FALSE,
    ta_bort_diagramtitel = FALSE,                    # FALSE så skrivs ingen diagramtitel ut
    kortnamn_lan = TRUE,                    # TRUE så tas " län" bort ur länsnamnet
    diagram_0_till_100_procent = TRUE,       # TRUE så går skalan alltid från 0 till 100 procent, annars anpassas skalan efter data
    logga_path = NA                         # NULL för att köra utan logga
) {

  # ===============================================================================================
  #
  # Diagram för att skriva ut självskattad hälsa från Hälsa på lika villkor-enkäten hos
  # Folkhälsomyndigheten. Finns enbart "Bra eller mycket bra hälsa". För fler alternativ, se skript med snarlikt namn.
  # Det skriptet finns dock inte för kommuner
  # Skickas bara ett år med så skrivs bara ett diagram för det året, och finns fler regioner så läggs de
  # på x-axeln.
  #
  # Migrerad bort från source()/hamta_data-repot. Folkhälsomyndigheten har bara ett v1-PXWeb-API
  # (inget v2/pxweb2r-stöd som SCB) - hamta_sjalvskattad_halsa_lan_kommun_halsotillstand_kon_ar_fohm()
  # används bara av det här skriptet, så uttaget är skrivet om direkt här med pxweb-paketet, i modern
  # stil (fullt namespace, ingen source()/p_load()). Samma mönster som
  # diag_sjalvskattad_halsa_kon_lan_fohm.R (mot en annan FoHM-tabell, halsgodyreg.px i stället för
  # hlv1allmxreg.px - den här tabellen har bara ett hälsotillstånd men stödjer både län och kommuner).
  # ===============================================================================================

  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb", quietly = TRUE)) install.packages("pxweb")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen output_mapp är vald. Kolla om standardmappen existerar, om inte ges ett felmeddelande
  if (all(is.na(output_mapp))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_mapp <- rdverktyg::utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("kon")
  if (all(is.na(diagram_fargvekt))) diagram_fargvekt <- rddiagram::diagramfarger("kon")

  gg_list <- list()

  # =============================================== API-uttag ===============================================

  hamta_sjalvskattad_halsa_lan_kommun_fohm <- function(region_vekt, andel_och_konfidensintervall_klartext,
                                                       kon_klartext, tid_koder) {

    url <- "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/A_Mo8/Halsoutfall/01Overgrip/01.01halsgod/halsgodyreg.px"
    px_meta <- pxweb::pxweb_get(url)

    hamta_variabel <- function(text) px_meta$variables[[which(vapply(px_meta$variables, `[[`, "", "text") == text)]]

    kod_for_klartext <- function(variabel_text, klartext_vekt) {
      var <- hamta_variabel(variabel_text)
      if (identical(klartext_vekt, "*")) return(var$values)
      # case-okänslig matchning, som i den frusna func_API.R::sla_upp_varde_klartext_kod()
      var$values[match(tolower(klartext_vekt), tolower(var$valueTexts))]
    }

    giltiga_ar <- hamta_variabel("År")$values
    tid_koder <- ifelse(tid_koder == "9999", max(giltiga_ar), tid_koder)
    tid_vekt <- if (identical(tid_koder, "*")) giltiga_ar else tid_koder[tid_koder %in% giltiga_ar]

    varlista <- purrr::compact(list(
      Region = region_vekt,
      "Hälsotillstånd" = kod_for_klartext("Hälsotillstånd", "*"),
      "Andel och konfidensintervall" = kod_for_klartext("Andel och konfidensintervall", andel_och_konfidensintervall_klartext),
      "Kön" = if (!all(is.na(kon_klartext))) kod_for_klartext("Kön", kon_klartext) else NULL,
      "År" = tid_vekt
    ))

    px_df <- as.data.frame(pxweb::pxweb_get(url = url, query = varlista))

    # FoHM:s region-kolumn kommer som "<kod> <namn>" ihopklistrat i klartext - dela upp den i
    # regionkod/region (samma lösning som i original-hamta_data-funktionen).
    rdverktyg::region_kolumn_splitta_kod_klartext(px_df, "Region")
  }

  sjalvskattad_halsa_df <- hamta_sjalvskattad_halsa_lan_kommun_fohm(
    region_vekt = region_vekt,
    andel_och_konfidensintervall_klartext = andel_konfinterv_klartext,
    kon_klartext = kon_klartext,
    tid_koder = tid_koder
  )

  # Det är enbart möjligt att välja flera år för en region
  if (length(unique(sjalvskattad_halsa_df$region)) > 1 & length(unique(sjalvskattad_halsa_df$År)) > 1) {
    stop("Det är inte möjligt att välja flera år för flera regioner. Välj en region eller ett år.")
  }

  if (kortnamn_lan) sjalvskattad_halsa_df <- dplyr::mutate(sjalvskattad_halsa_df, region = rdverktyg::skapa_kortnamn_lan(region))

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(sjalvskattad_halsa_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(sjalvskattad_halsa_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(sjalvskattad_halsa_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- stringr::str_replace_all(region_txt, ", ", "_") |> stringr::str_replace_all(" och ", "_")
  regionkod_txt <- if (region_start == region_txt) paste0(unique(sjalvskattad_halsa_df$regionkod), collapse = "_") else region_txt
  if (region_start == region_txt) regionfil_txt <- region_txt

  # om region_sort är TRUE sorteras regionerna enligt ordningen i region_vekt
  if (region_sort) {
    sjalvskattad_halsa_df <- dplyr::mutate(
      sjalvskattad_halsa_df,
      region = factor(region, levels = unique(region[order(match(regionkod, region_vekt))]))
    )
  }

  if(returnera_dataframe_global_environment == TRUE & length(unique(sjalvskattad_halsa_df$region)) == 1){
    assign("sjalvskattad_halsa_df", sjalvskattad_halsa_df, envir = .GlobalEnv)
  }

  if(returnera_dataframe_global_environment == TRUE & length(unique(sjalvskattad_halsa_df$region)) > 1){
    assign("sjalvskattad_halsa_region_df", sjalvskattad_halsa_df, envir = .GlobalEnv)
  }

  if (min(sjalvskattad_halsa_df$År) == max(sjalvskattad_halsa_df$År)){
    tid_txt <- max(sjalvskattad_halsa_df$År)
    diagramtitel <- glue::glue("Andel med bra eller mycket bra självskattad hälsa år {tid_txt}")
  } else {
    tid_txt <- glue::glue("{min(sjalvskattad_halsa_df$År)} till {max(sjalvskattad_halsa_df$År)}")
    diagramtitel <- glue::glue("Andel med bra eller mycket bra självskattad hälsa i {region_txt}")
  }

  diagramfil <- stringr::str_replace_all(glue::glue("sjalvskattad_halsa_fohm_{regionfil_txt}_ar{tid_txt}.png"), "__", "_")

  flera_ar <- length(unique(sjalvskattad_halsa_df$År)) > 1
  konsuppdelat <- length(unique(sjalvskattad_halsa_df$Kön)) > 1
  flera_regioner <- length(unique(sjalvskattad_halsa_df$region)) > 1

  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = sjalvskattad_halsa_df,
                               skickad_x_var = if (flera_ar) "År" else "region",
                               skickad_y_var = "Hälsa efter region, kön och år. Andel",
                               skickad_x_grupp = if (konsuppdelat) "Kön" else NA,
                               diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               dataetiketter = visa_dataetiketter,
                               manual_y_axis_title = "procent",
                               procent_0_100_10intervaller = diagram_0_till_100_procent,
                               #x_axis_lutning = if (flera_ar) 45 else 0,
                               x_axis_lutning = 45,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               x_axis_sort_value = if (flera_ar) FALSE else TRUE,
                               # manual_x_axis_text_vjust = if (flera_ar) 1 else 0,
                               # manual_x_axis_text_hjust = if (flera_ar) 1 else 0.5,
                               manual_color = diagram_fargvekt,
                               lagg_pa_logga = if (is.null(logga_path)) FALSE else TRUE,
                               logga_path = logga_path,
                               output_mapp = output_mapp,
                               facet_grp = if (flera_ar & flera_regioner) "region" else NULL,
                               facet_scale = "free",
                               facet_legend_bottom = if (konsuppdelat) TRUE else FALSE
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, ".png")

  return(gg_list)
}

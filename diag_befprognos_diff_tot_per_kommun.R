diag_befprognos_diff_tot_per_region_scb <- function(
    region_vekt = c("20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085"),
    #kon_klartext = c("kvinnor", "män"),           # "män", "kvinnor"
    alder_koder = "*", 
    fokus_regionkod = "region",                    # NA = ingen, "region" på regioner (och inte kommuner). om man vill fokusera på någon speciell kommun eller region
    variabel_klartext = "Folkmängd",         # "Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring"
    befprognostabell_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
    jmfrtid = 10,
    stodlinjer_avrunda_fem = TRUE,
    ta_med_logga = TRUE,
    logga_path = NA,
    skapa_fil = TRUE,
    dataetiketter = FALSE,
    diag_fargvekt = NA,
    output_fold = NA,
    diagram_capt = "auto", # Lagt till så att man kan välja en diagram_capt själv /Jon
    skickad_facetinst = FALSE,
    andel_istallet_for_antal = FALSE
) {
  
  # ===========================================================================================================
  # Senast uppdaterat: september 2026
  #                     Namespace:at (dplyr::/stringr::/rdverktyg::/scales:: osv.) i stället för att förlita
  #                     sig på att func_text.R/func_SkapaDiagram.R/func_shinyappar.R redan source:ats/laddats
  #                     i sessionen. Antar att de egna hjälpfunktionerna nedan (skapa_kortnamn_lan,
  #                     list_komma_och, ar_alla_kommuner_i_ett_lan, ar_alla_lan_i_sverige, SkapaStapelDiagram,
  #                     diagramfarger, utskriftsmapp) numera bor i rdverktyg - justera prefixet nedan om någon
  #                     av dem faktiskt ligger i ett annat rd-paket.
  # ===========================================================================================================
  
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  
  if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
  
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", where = asNamespace("rddiagram"), mode = "function")) {
      diag_fargvekt <- rddiagram::diagramfarger("rus_sex")
    } else {
      diag_fargvekt <- scales::hue_pal()(9)
    }
  }
  
  # publicerad 11 juni 2024
  if (all(is.na(output_fold))) {
    if (exists("utskriftsmapp", where = asNamespace("rdverktyg"), mode = "function")) {
      if (dir.exists(rdverktyg::utskriftsmapp())) {
        output_fold <- rdverktyg::utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output_fold ett värde.")
      }
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output_fold ett värde.")
    }
  }
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_profet_scb_berakna_diff.R")
  prognos_diff_df <- hamta_befprognos_diff_data(
    region_vekt = region_vekt,
    tabeller_url = befprognostabell_url,
    jmfrtid = jmfrtid
    #cont_klartext = variabel_klartext,       
    #kon_klartext = kon_klartext,     
    #alder_list = alder_koder
  )
  prognos_diff_df <- dplyr::mutate(prognos_diff_df, region = rdverktyg::skapa_kortnamn_lan(region))
  
  if (diagram_capt == "auto") {
    if (stringr::str_detect(befprognostabell_url, "api.scb.se")) {
      diagram_capt <- paste0("Källa: SCB:s befolkningsframskrivning från juni år ", unique(prognos_diff_df$prognos_ar), "\nBearbetning: Samhällsanalys, Region Dalarna")
    } else if (befprognostabell_url == "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/") {
      diagram_capt <- "Källa: Region Dalarnas egna befolkningsprognos, bearbetning av Samhällsanalys, Region Dalarna\nPrognosen för Ludvika kommun baseras på ett scenario som i allt väsentligt liknar den som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt."
    }
  }
  
  y_lbl_axel <- "förändring antal invånare"
  y_lbl_diagram_titel <- paste("Förändring", tolower(variabel_klartext))
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(prognos_diff_df$region)))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(prognos_diff_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(prognos_diff_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- region_txt
  regionkod_txt <- if (region_start == region_txt) paste0(unique(prognos_diff_df$regionkod), collapse = "_") else region_txt
  
  if (!is.na(fokus_regionkod)) {
    if (fokus_regionkod == "region") fokus_regionkod <- unique(prognos_diff_df$regionkod)[nchar(unique(prognos_diff_df$regionkod)) == 2]
  }
  
  if (!is.na(fokus_regionkod)) {
    prognos_diff_df <- dplyr::mutate(prognos_diff_df, fokus = ifelse(regionkod %in% fokus_regionkod, 1, 0))
  }
  
  if (length(unique(prognos_diff_df$prognos_ar)) < 2) {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " ",
                           unique(prognos_diff_df$start_ar), "-", unique(prognos_diff_df$slut_ar), "\n(enligt befolkningsprognos våren ", 
                           unique(prognos_diff_df$prognos_ar), ")")
  } else {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " på ", skickad_jmfrtid,   # TODO: skickad_jmfrtid är odefinierad, troligen ska det vara jmfrtid
                           " års sikt")
  }
  
  enhet <- ifelse(andel_istallet_for_antal, "andel", "antal")
  filnamn <- paste0("befprogn_", tolower(region_txt), "_", enhet, "_alla_tot.png")
  
  if (andel_istallet_for_antal) {
    chart_df <- dplyr::filter(prognos_diff_df, aldergrp == "totalt")
  } else {
    chart_df <- dplyr::filter(prognos_diff_df, aldergrp == "totalt", regionkod != fokus_regionkod)
  }
  
  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = chart_df,
    skickad_y_var = ifelse(andel_istallet_for_antal, "andel", "antal"),
    skickad_x_var = "region",
    x_axis_sort_value = TRUE,
    x_var_fokus = if (!is.na(fokus_regionkod)) "fokus" else NA,
    #skickad_x_grupp = if(length(unique(prognos_diff_df$prognos_ar)) > 1) "ar_beskr" else NA,
    diagram_titel = diagramtitel,
    output_mapp = output_fold,
    diagram_capt = diagram_capt,
    #diagram_facet = skickad_facetinst,
    #facet_legend_bottom = if(length(unique(prognos_diff_df$prognos_ar)) > 1) TRUE else skickad_facetinst,
    stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
    #x_var_fokus = diagram_fokus_var,
    manual_color = diag_fargvekt[c(1, 2)], # if (length(unique(skickad_df$prognos_ar)) > 1) farger_diagram else farger_diagram[1],
    #logga_scaling = logga_storlek,
    manual_y_axis_title = ifelse(andel_istallet_for_antal, "procent", y_lbl_axel),
    x_axis_lutning = 45,
    manual_x_axis_text_vjust = 1,
    manual_x_axis_text_hjust = 1,
    #facet_x_axis_storlek = facet_x_axis_storlek,   # TODO: odefinierad variabel, fanns redan i original
    lagg_pa_logga = ta_med_logga,
    logga_path = logga_path,
    dataetiketter = dataetiketter,
    filnamn_diagram = filnamn,
    skriv_till_diagramfil = skapa_fil)
  
  gg_list <- list(gg_obj)
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(filnamn, ".png")
  return(gg_list)
  
} # slut funktion

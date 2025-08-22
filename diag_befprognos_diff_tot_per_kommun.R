


diag_befprognos_diff_tot_per_region_scb <- function(
    region_vekt = c("20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085"),
    #kon_klartext = c("kvinnor", "män"),           # "män", "kvinnor"
    alder_koder = "*", 
    fokus_regionkod = "region",                    # NA = ingen, "region" på regioner (och inte kommuner). om man vill fokusera på någon speciell kommun eller region
    variabel_klartext = "Folkmängd",         # "Folkmängd", "Födda", "Döda", "Inrikes inflyttning", "Inrikes utflyttning", "Invandring", "Utvandring"
    befprognostabell_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
    ta_med_logga = TRUE,
    logga_path = NA,
    skapa_fil = TRUE,
    dataetiketter = FALSE,
    diag_fargvekt = NA,
    output_fold = NA,
    diagram_capt = "Källa: SCB:s befolkningsframskrivning\nBearbetning: Samhällsanalys, Region Dalarna",
    skickad_facetinst = FALSE,
    andel_istallet_for_antal = FALSE
) {
    
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("rus_sex")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  # publicerad 11 juni 2024
  if (all(is.na(output_fold))) {
    if (exists("utskriftsmapp", mode = "function")) {
      if (dir.exists(utskriftsmapp())) output_fold <- utskriftsmapp() else stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output_fold ett värde.")
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output_fold ett värde.")
    }
  }
    
  source("G:/skript/hamta_data/hamta_befprognos_profet_scb_berakna_diff.R")
  prognos_diff_df <- hamta_befprognos_diff_data(
    region_vekt = region_vekt,
    tabeller_url = befprognostabell_url,
    #cont_klartext = variabel_klartext,       
    #kon_klartext = kon_klartext,     
    #alder_list = alder_koder
    ) %>% 
    mutate(region = region %>% skapa_kortnamn_lan())
  
  if (str_detect(befprognostabell_url, "api.scb.se")) {
    diagram_capt <- paste0("Källa: SCB:s befolkningsframskrivning från juni år ", unique(prognos_diff_df$prognos_ar), "\nBearbetning: Samhällsanalys, Region Dalarna")
  } else if (befprognostabell_url == "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/") {
    diagram_capt <- "Källa: Region Dalarnas egna befolkningsprognos, bearbetning av Samhällsanalys, Region Dalarna\nPrognosen för Ludvika kommun baseras på ett scenario som i allt väsentligt liknar den som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt."
  }
  
  y_lbl_axel <- "förändring antal invånare"
  y_lbl_diagram_titel <- paste("Förändring", tolower(variabel_klartext)) 
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(prognos_diff_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(prognos_diff_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(prognos_diff_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- region_txt
  regionkod_txt <- if (region_start == region_txt) unique(prognos_diff_df$regionkod) %>% paste0(collapse = "_") else region_txt
  
  
  if (!is.na(fokus_regionkod)) {
    if (fokus_regionkod == "region") fokus_regionkod <- unique(prognos_diff_df$regionkod) %>% .[nchar(.) == 2]
  }
  
  if (!is.na(fokus_regionkod)) prognos_diff_df <- prognos_diff_df %>% mutate(fokus = ifelse(regionkod %in% fokus_regionkod, 1, 0))
  
  if(length(unique(prognos_diff_df$prognos_ar)) < 2) {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " ",
                           prognos_diff_df$start_ar %>% unique(),"-", prognos_diff_df$slut_ar %>% unique(), "\n(enligt befolkningsprognos våren ", 
                           prognos_diff_df$prognos_ar %>% unique(), ")")
  } else {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " på ", skickad_jmfrtid, 
                           " års sikt")
  }
  
  enhet <- ifelse(andel_istallet_for_antal, "andel", "antal")
  filnamn <- paste0("befprogn_", tolower(region_txt),"_", enhet , "_alla_tot.png")
  
  if (andel_istallet_for_antal){
    chart_df <- prognos_diff_df %>% 
      filter(aldergrp == "totalt")
  } else {
    chart_df <- prognos_diff_df %>% 
      filter(aldergrp == "totalt",
             regionkod != fokus_regionkod)
  }
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
                     skickad_y_var = ifelse(andel_istallet_for_antal, "andel", "antal"),
                     skickad_x_var = "region",
                     x_axis_sort_value = TRUE,
                     x_var_fokus = if (!is.na(fokus_regionkod)) "fokus" else NA,
                     #skickad_x_grupp = if(length(unique(prognos_diff_df$prognos_ar)) > 1) "ar_beskr" else NA,
                     diagram_titel = diagramtitel,
                     output_mapp = output_fold,
                     diagram_capt = diagram_capt,
                     diagram_facet = skickad_facetinst,
                     #facet_legend_bottom = if(length(unique(prognos_diff_df$prognos_ar)) > 1) TRUE else skickad_facetinst,
                     stodlinjer_avrunda_fem = FALSE,
                     #x_var_fokus = diagram_fokus_var,
                     manual_color = diag_fargvekt[c(1,2)], # if (length(unique(skickad_df$prognos_ar)) > 1) farger_diagram else farger_diagram[1],
                     #logga_scaling = logga_storlek,
                     manual_y_axis_title = ifelse(andel_istallet_for_antal, "procent", y_lbl_axel),
                     x_axis_lutning = 45,
                     manual_x_axis_text_vjust = 1,
                     manual_x_axis_text_hjust = 1,
                     facet_x_axis_storlek = facet_x_axis_storlek,
                     utan_diagramtitel = FALSE,
                     lagg_pa_logga = ta_med_logga,
                     logga_path = logga_path,
                     dataetiketter = dataetiketter,
                     filnamn_diagram = filnamn,
                     skriv_till_diagramfil = skapa_fil)
  
  gg_list <- list(gg_obj)
  names(gg_list)[[length(gg_list)]] <- filnamn %>% str_remove(".png")
  return(gg_list)

} # slut funktion

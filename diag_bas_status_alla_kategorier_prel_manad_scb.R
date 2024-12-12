# ===========================================================================================================
#
# Skript för att skriva ut stapeldiagram över de olika kategorierna i BAS per månad (från BAS, SCB), främst 
# utan uppdelning på kön eller inrikes och utrikes födda, för de(n) region(er) man valt. 
# Default är åldersgruppen 20-64 år men det finns fler åldersgrupper att välja på i tabellen. 
# Gå in på url:en för att se vilka som finns tillgängliga. 
#
# Följande diagram finns i funktionen diag_bas_alla_kat():
#             1. diag_senaste_ar - senaste året med samtliga kategorier eller de man väljer själv att visa
#             2. diag_tidsserie - en tidssere med samtliga månader som finns i BAS
#             3. diag_fokusvariabel - en eller flera variabler som man vill titta extra på i en tidsserie
#
# ===========================================================================================================


source("G:/skript/hamta_data/hamta_bas_arbmarknstatus_manad.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)


diag_bas_status_alla_kat_prel_manad_scb <- function(
    region_vekt = "20",                 # c("20", "17", "21"),
    alder_vekt = "20-64 år",            # finns också: 15-19 år, 16-19 år, 20-24 år, 25-29 år, 30-34 år, 35-39 år, 40-44 år, 45-49 år, 50-54 år, 55-59 år, 60-64 år, 65-69 år, 70-74 år, 15-74 år, 16-64 år, 16-65 år, 20-64 år, 20-65 år
    kon_vekt = "Totalt",                # man måste välja totalt, män ELLER kvinnor
    fodelseregion_vekt = "totalt",      # man måste välja totalt, inrikes födda ELLER utrikes födda
    cont_uttag = c("antal sysselsatta", "antal arbetslösa", "antal studerande", "antal pensionärer", "antal sjuka", "antal övriga"),
    gruppering_namn = NA,
    visa_andelar = FALSE,
    fokus_variabel = "antal övriga",
    titel_bredd = 80,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    diag_fargvekt = NA,
    skickad_logga_path = NA,               # NA om man vill köra med standard, bra att sätta en lokal om man är på resande fot
    ta_med_logga = TRUE,
    skriv_till_diagramfil = TRUE,
    diag_senaste_ar = FALSE,
    diag_tidsserie = TRUE,
    diag_tidsserie_facet_kat = TRUE,
    diag_fokusvariabel = TRUE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/bas_alla_kat_facet_tid_20_64ar_20.png",
"https://region-dalarna.github.io/utskrivna_diagram/bas_alla_kat_tid_20_64ar_20.png",
"https://region-dalarna.github.io/utskrivna_diagram/bas_antal övriga_tid_20_64ar_20.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- rev(diagramfarger("rus_sex"))
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  retur_list <- list()
  
  bas_df <- hamta_bas_arbmarknstatus(region_vekt = region_vekt,
                                     alder_vekt_klartext = alder_vekt,
                                     kon_klartext_vekt = kon_vekt,
                                     fodelseregion_klartext_vekt = fodelseregion_vekt,
                                     cont_klartext_vekt = cont_uttag,
                                     tid_koder = "*",
                                     long_format = FALSE)
  
  bas_aggr <- bas_df %>% 
    pivot_longer(`antal sysselsatta`:`antal övriga`, names_to = "variabel", values_to = "varde") %>% 
    mutate(variabel = factor(variabel, levels = rev(c("antal sysselsatta", "antal arbetslösa", "antal studerande", "antal pensionärer", "antal sjuka", "antal övriga" )))) 
  
  vald_region <- if(is.na(gruppering_namn)) {
    hamtaregion_kod_namn(region_vekt)$region %>% 
      skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE) %>% 
      list_komma_och()
  } else {
    vald_region <- gruppering_namn  
  } # slut if-sats om gruppering_namn är medskickat eller inte
  
  
  if (!is.na(gruppering_namn)) {
    bas_aggr <- bas_aggr %>% 
      group_by(kön, ålder, födelseregion, tid, år, månad, år_månad, månad_år, variabel) %>% 
      summarise(varde := sum(varde, na.rm = TRUE)) %>% 
      mutate(andel = round((varde/sum(varde))*100,1)) %>% 
      ungroup() %>% 
      mutate(regionkod = "grp",
             region = gruppering_namn) %>% 
      relocate(regionkod, .before = 1) %>% 
      relocate(region, .after = regionkod)
    
  } else { # slut if-sats om man vill gruppera
    
    bas_aggr <- bas_aggr %>% 
      group_by(regionkod, region, kön, ålder, födelseregion, tid, år, månad, år_månad, månad_år, variabel) %>% 
      summarise(varde := sum(varde, na.rm = TRUE)) %>% 
      mutate(andel = round((varde/sum(varde))*100,1)) %>% 
      ungroup()
  }
  
  # hantera åldrar
  alla_aldrar <- str_extract_all(alder_vekt, "\\d+")
  alder_min <- min(alla_aldrar %>% unlist())
  alder_max <- max(alla_aldrar %>% unlist())
  
  aldrar_min_till_max <- c(alder_min:alder_max)        # gör en vektor med alla nummer mellan min- och maxvärden
  alla_ar_i_grupp <- map(alla_aldrar, ~ (.x %>% pluck(1)):(.x %>% pluck(2))) %>% unlist()       # gör sekvenser med alla åldersgrupper
  
  if (all(aldrar_min_till_max == alla_ar_i_grupp)) {
    # om sekvensen som går från min till max är samma som sekvenser för samtliga åldersgrupper så 
    # är alla åldrar i ordning och finns för alla år, då lägger vi ihop dem till "en" åldersgrupp
    
      aldrar_titel <- paste0(min(alla_ar_i_grupp), "-", max(alla_ar_i_grupp), " år")
      aldrar_txt <- paste0(min(alla_ar_i_grupp), "_", max(alla_ar_i_grupp), "ar")
  } else {
    # om sekvensom som går från min till max INTE är samma som sekvenser för samtliga åldersgrupper
    # så har användaren valt åldersgrupper som inte ligger "intill" varandra, då behöver åldersgrupperna
    # redovisas var och en för sig
    aldrar_titel <- aldrar_titel
    aldrar_txt <- aldrar_txt
    
  }
  
  if (diag_senaste_ar) {
    
    diagram_titel <- paste0("Arbetsmarknadsstatus för befolkningen ", aldrar_titel, " i ", vald_region, " i ", bas_aggr %>% filter(tid == max(tid)) %>% select(månad_år) %>% dplyr::pull() %>% unique())
    diagramfil <- paste0("bas_alla_kat_", aldrar_txt, "_", region_vekt %>% paste0(collapse = "_"), ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = bas_aggr %>% 
                                   filter(tid == max(tid)),  
                                 skickad_x_var = "region", 
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel",
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 legend_vand_ordning = TRUE,
                                 legend_byrow = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 dataetiketter = FALSE,
                                 dataetiketter_justering_hojdled = -1.7,
                                 manual_y_axis_title = "antal individer",
                                 manual_color = diag_fargvekt,
                                 logga_path = skickad_logga_path,
                                 lagg_pa_logga = ta_med_logga,
                                 skriv_till_diagramfil = skriv_till_diagramfil,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil)
    
    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- diagramfil %>% str_remove(".png")
    
  } # slut if-sats diag_senaste_ar
  
  
  if (diag_tidsserie) {
    
    diagram_titel <- paste0("Arbetsmarknadsstatus för befolkningen ", aldrar_titel, " i ", vald_region)
    diagramfil <- paste0("bas_alla_kat_tid_", aldrar_txt, "_", region_vekt %>% paste0(collapse = "_"), ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = bas_aggr,  
                                 skickad_x_var = "månad_år", 
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel",
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 legend_vand_ordning = TRUE,
                                 legend_byrow = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 dataetiketter = FALSE,
                                 dataetiketter_justering_hojdled = -1.7,
                                 manual_y_axis_title = "antal individer",
                                 manual_color = diag_fargvekt,
                                 diagram_facet = if (length(unique(bas_aggr$regionkod)) > 1) TRUE else FALSE,
                                 facet_grp = "region",
                                 facet_x_axis_storlek = 5,
                                 facet_legend_bottom = TRUE,
                                 #facet_scale = "fixed",
                                 skriv_till_diagramfil = skriv_till_diagramfil,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = skickad_logga_path,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil)
    
    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- diagramfil %>% str_remove(".png")
    
  } # slut if-sats diag_tidsserie
  
  if (diag_tidsserie_facet_kat) {
    
    diagram_titel <- paste0("Arbetsmarknadsstatus för befolkningen ", aldrar_titel, " i ", vald_region)
    diagramfil <- paste0("bas_alla_kat_facet_tid_", aldrar_txt, "_", region_vekt %>% paste0(collapse = "_"), ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = bas_aggr,  
                                 skickad_x_var = "månad_år", 
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel",
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 legend_vand_ordning = TRUE,
                                 legend_byrow = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_visa_var_xe_etikett = 2,
                                 dataetiketter = FALSE,
                                 dataetiketter_justering_hojdled = -1.7,
                                 manual_y_axis_title = "antal individer",
                                 manual_color = diag_fargvekt,
                                 diagram_facet = TRUE,
                                 facet_grp = "variabel",
                                 facet_x_axis_storlek = 5,
                                 facet_legend_bottom = FALSE,
                                 facet_scale = "free",
                                 skriv_till_diagramfil = skriv_till_diagramfil,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = skickad_logga_path,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil)
    
    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- diagramfil %>% str_remove(".png")
  } # slut if-sats diag_tidsserie_facet_kat
  
  
  
  
  if (diag_fokusvariabel) {
    
    diagram_titel <- paste0("Arbetsmarknadsstatus för befolkningen ", aldrar_titel, " i ", vald_region, " i kategorin ", fokus_variabel %>% str_remove("antal")) %>% 
      str_wrap(titel_bredd)
    diagramfil <- paste0("bas_", fokus_variabel, "_tid_", aldrar_txt, "_", region_vekt %>% paste0(collapse = "_"), ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = bas_aggr %>% 
                                   filter(variabel %in% fokus_variabel),  
                                 skickad_x_var = "månad_år", 
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = if (length(fokus_variabel) > 1) "variabel" else NA,
                                 #geom_position_stack = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 legend_vand_ordning = TRUE,
                                 legend_byrow = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 dataetiketter = FALSE,
                                 dataetiketter_justering_hojdled = -1.7,
                                 manual_y_axis_title = "antal individer",
                                 manual_color = if (length(fokus_variabel) > 1) diag_fargvekt else diag_fargvekt[1],
                                 diagram_facet = if (length(unique(bas_aggr$regionkod)) > 1) TRUE else FALSE,
                                 facet_grp = "region",
                                 facet_x_axis_storlek = 5,
                                 facet_legend_bottom = TRUE,
                                 facet_scale = "fixed",
                                 skriv_till_diagramfil = skriv_till_diagramfil,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = skickad_logga_path,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil)
    
    retur_list <- c(retur_list, list(gg_obj))
    names(retur_list)[length(retur_list)] <- diagramfil %>% str_remove(".png")
    
  } # slut if-sats diag_senaste_ar
  return(retur_list)
} # slut funktion

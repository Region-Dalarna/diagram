#test = diagram_fruktsamhet(spara_figur=FALSE,region_vekt = "20",visa_var_xte = 2)
diagram_fruktsamhet <- function(region_vekt = "20", # Val av kommun/län att fokusera på. De figurer som jämför kommuner i samma län väljs automatiskt utefter kommunens länstillhörighet 
                                output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                diag_capt = "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna\nSummerad fruktsamhet per kvinna är ett mått på hur många barn som en kvinna i genomsnitt skulle föda under\n sin fruktsamma period utifrån, den vid tidpunkten för beräkningen, gällande fruktsamheten.",
                                vald_period = c(2000:9999), # Vilka år skall väljas
                                visa_var_xte = 4, # Hur många år som skall visas på x-axeln. NA ger alla och 2 ger vartannat, 4 var fjärde osv.
                                spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                diag_fokus_tid = TRUE, # Skapa diagram för alla valda år i valt län
                                diag_facet = TRUE, # Skapa diagram för flyttnetto uppdelat
                                diag_jmf_lan = TRUE, # Skapa diagram för jämförelse på länsnivå
                                diag_forandring = TRUE, # Skapa diagram för förändring mellan första och sista år för samtliga kommuner i län
                                returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                returnera_data = FALSE){ # True om användaren vill returnera data från funktionen
  
  
  # ===========================================================================================================
  #
  # Skript som skapar diagram över fruktsamhet i vald region. Finns för vald region över tid, facet för alla kommuner i regionen
  # senaste år för alla kommuner i regionen och förändring mellan första och sista året för samtliga kommuner i regionen.
  # Skapad av Jon 2023-04-05 genom att ha kombinerat två av Peters skript (analys_befutv_berakna_summerad_fruktsamhet.R och diagram_summerad_fruktsamhet.R)
  # ===========================================================================================================
  
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_fodda_moderns_alder_region_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  objektnamn <- c()
  
  valt_lan = substr(region_vekt, 1, 2)
  
  dalarnas_kommuner <- hamtakommuner(lan = valt_lan)
  
  vald_region <- c(dalarnas_kommuner) %>% unique()
  
  # Hämta födelsetal för kvinnor 
  
  fodda_df <- hamta_fodda_moderns_alder_region_scb(region_vekt = vald_region,
                                                   alder_moder = c(15:48),
                                                   tid_koder = vald_period)
  
  bef_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = vald_region,
                                                 alder = c(15:48),
                                                 kon_klartext = "kvinnor",
                                                 tid_koder = vald_period)
  
  
  fodelsetal_df <- bef_df %>% 
    left_join(fodda_df, by = c("år", "regionkod", "region","ålder" = "moderns ålder")) %>% 
    mutate(födelsetal = födda / Folkmängd)
  
  sum_frukts_ar <- fodelsetal_df %>% 
    group_by(år, regionkod, region) %>% 
    summarise(sum_frukts_ar = round(sum(födelsetal, na.rm = TRUE), 2), .groups = "drop")
  
  if(returnera_data == TRUE){
    assign("fruktsamhet_df", sum_frukts_ar, envir = .GlobalEnv)
  }
  
  # Region att fokusera på
  fokus_region <- region_vekt
  fokus_region_txt <- hamtaregion_kod_namn(fokus_region)$region %>% skapa_kortnamn_lan()
  
  # spara diagram
  if (length(unique(sum_frukts_ar$regionkod)) > 1) facet_diagram <- TRUE else facet_diagram <- FALSE
  
  
  #diagtitel_txt <- if(facet_diagram) "" else paste0(" i ", unique(sum_frukts_ar$region) %>% skapa_kortnamn_lan())
  diagfilnamn_txt <- paste0(unique(sum_frukts_ar$regionkod), collapse = "_")
  
  # diagram med bara fokusregionen över tid
  if(diag_fokus_tid == TRUE){
    
    diagram_titel <- paste0("Summerad fruktsamhet per kvinna i ", fokus_region_txt)
    diagramfil <- paste0("summerad_fruktsamhet_", fokus_region, "_ar_", min(sum_frukts_ar$år), "_", max(sum_frukts_ar$år), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sum_frukts_ar %>% 
                                   filter(regionkod == fokus_region), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "sum_frukts_ar",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diag_capt,
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_visa_var_xe_etikett = visa_var_xte,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 facet_grp = "region",
                                 skriv_till_diagramfil = spara_figur,
                                 manual_color = vald_farg[1],
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  # facetdiagram med alla regioner över tid
  if(diag_facet==TRUE){
    
    diagram_titel <- paste0("Summerad fruktsamhet per kvinna")
    diagramfil <- paste0("summerad_fruktsamhet_facet_", valt_lan, "_ar_", min(sum_frukts_ar$år), "_", max(sum_frukts_ar$år), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sum_frukts_ar, 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "sum_frukts_ar",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diag_capt,
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = TRUE,
                                 x_axis_visa_var_xe_etikett = visa_var_xte,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 diagram_facet = facet_diagram,
                                 facet_grp = "region",
                                 facet_scale = "fixed",
                                 facet_x_axis_storlek = 6,
                                 skriv_till_diagramfil = spara_figur,
                                 manual_color = vald_farg[1],
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  # Diagram som visar jämförelse i länet
  if(diag_jmf_lan == TRUE){
    
    diagram_titel <- paste0("Summerad fruktsamhet per kvinna år ", max(sum_frukts_ar$år) %>% unique())
    diagramfil <- paste0("summerad_fruktsamhet_ar_", max(sum_frukts_ar$år), "_", valt_lan, "_ar.png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sum_frukts_ar %>% 
                                   filter(år == max(år)) %>% 
                                   mutate(fokus = ifelse(regionkod %in% fokus_region, 1, 0)), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "sum_frukts_ar",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diag_capt,
                                 x_axis_sort_value = TRUE,
                                 x_var_fokus = "fokus",
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 diagram_facet = FALSE,
                                 skriv_till_diagramfil = spara_figur,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  if(diag_forandring == TRUE){
    
    diagram_titel <- paste0("Förändring av summerad fruktsamhet per kvinna mellan år ", min(sum_frukts_ar$år), " och ", max(sum_frukts_ar$år))
    diagram_titel <- str_wrap(diagram_titel, width = 50)
    diagramfil <- paste0("forandring_summerad_fruktsamhet_ar_",min(sum_frukts_ar$år), "_", max(sum_frukts_ar$år), "_", valt_lan, "_ar.png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sum_frukts_ar %>% 
                                   group_by(regionkod, region) %>% 
                                   summarise(forandring = (sum(`sum_frukts_ar`[år == max(år)]) - sum(`sum_frukts_ar`[år == min(år)]))/ 
                                               sum(`sum_frukts_ar`[år == min(år)]), .groups = "drop") %>% 
                                   mutate(forandring = forandring * 100,
                                          fokus = case_when(regionkod %in% fokus_region ~ 1,
                                                            regionkod == "00"~ 2,
                                                            TRUE ~ 0)),
                                 skickad_x_var = "region", 
                                 skickad_y_var = "forandring",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diag_capt,
                                 x_axis_sort_value = TRUE,
                                 x_var_fokus = "fokus",
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "procent",
                                 diagram_facet = FALSE,
                                 skriv_till_diagramfil = spara_figur,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  names(gg_list) <- objektnamn
  if(returnera_figur==TRUE){
    return(gg_list)
  }
  
}

diagram_fruktsamhet <- function(region_vekt = hamtakommuner("20"), # Vilka kommuner skall väljas. Default är alla kommuner i Dalarna. Bör var minst ett par stycken för att diag_jmf_lan och diag_forandring skall se vettiga ut
                                fokus_region = "20", # Vilken region skall fokuseras på. Måste vara ett som finns i region_vekt
                                output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                diag_capt = "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna\nSummerad fruktsamhet per kvinna är ett mått på hur många barn som en kvinna i genomsnitt skulle föda under\n sin fruktsamma period utifrån, den vid tidpunkten för beräkningen, gällande fruktsamheten.",
                                vald_period = c(2000:9999), # Vilka år skall väljas. 9999 ger sista år och "*" ger alla år
                                visa_var_xte = 4, # Hur många år som skall visas på x-axeln. Automatiskt var 8:e vid facet-diagram.
                                spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                diag_fokus_tid = TRUE, # Skapa diagram för alla valda år i valda regioner
                                diag_facet = FALSE, # diag_fokus_tid som facet-diagram istället för ett per region
                                facet_skala = "free", # Finns free (varje diagram får en egen y-axel) eller fixed (alla diagram delar y-axel)
                                diag_jmf_lan = TRUE, # Skapa diagram för jämförelse mellan valda regioner
                                diag_forandring = TRUE, # Skapa diagram för förändring mellan första och sista år för valda regioner
                                demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                returnera_data = FALSE){ # True om användaren vill returnera data från funktionen
  
  
  # ===========================================================================================================
  #
  # Skript som skapar diagram över fruktsamhet i vald region. Finns för valda regioner över tid (både ett diagram per region och ett facet-diagram för alla regioner),
  # senaste år för alla valda regioner och förändring mellan första och sista året för samtliga valda regioner.
  # Skapad av Jon 2023-04-05 genom att ha kombinerat två av Peters skript (analys_befutv_berakna_summerad_fruktsamhet.R och diagram_summerad_fruktsamhet.R)
  # Revidering : har lagt till en map-funktion för att skapa diagram för alla valda regioner över tid./Jon
  # ===========================================================================================================
  
  
# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/forandring_summerad_fruktsamhet_Dalarna.png",
"https://region-dalarna.github.io/utskrivna_diagram/jmf_summerad_fruktsamhet_Dalarna.png",
"https://region-dalarna.github.io/utskrivna_diagram/summerad_fruktsamhet_Riket_facet.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_fodda_moderns_alder_region_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  options(dplyr.summarise.inform = FALSE)
  
  # Skapar två listor, en de diagram jämför regionerna och en för de diagram som tar ut en region i taget
  gg_list <- list()
  gg_list_map <- list()
  objektnamn <- c()
  objektnamn_map <- c()
  
  # Hämta födelsetal för kvinnor 
  
  fodda_df <- hamta_fodda_moderns_alder_region_scb(region_vekt = region_vekt,
                                                   alder_moder = c(15:48),
                                                   tid_koder = vald_period)
  
  bef_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt,
                                                 alder = c(15:48),
                                                 kon_klartext = "kvinnor",
                                                 tid_koder = vald_period)
  
  
  # Beräknar födelsetal och fruktsamhet
  fodelsetal_df <- bef_df %>% 
    left_join(fodda_df, by = c("år", "regionkod", "region","ålder" = "moderns ålder")) %>% 
    mutate(födelsetal = födda / Folkmängd)
  
  sum_frukts_ar <- fodelsetal_df %>% 
    group_by(år, regionkod, region) %>% 
    summarise(sum_frukts_ar = round(sum(födelsetal, na.rm = TRUE), 2), .groups = "drop") %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  # Returnera data om användaren vill det. Läggs i R-studios globala miljö så att data kan användas i markdown-rapporterna
  if(returnera_data == TRUE){
    assign("fruktsamhet_df", sum_frukts_ar, envir = .GlobalEnv)
  }
  
  # Region att fokusera på
  fokus_region_txt <- hamtaregion_kod_namn(fokus_region)$region %>% skapa_kortnamn_lan()
  
  # spara diagram
  #if (length(unique(sum_frukts_ar$regionkod)) > 1) facet_diagram <- TRUE else facet_diagram <- FALSE
  
  # Diagram som visar jämförelse i länet
  if(diag_jmf_lan == TRUE){
    
    diagram_titel <- paste0("Summerad fruktsamhet per kvinna år ", max(sum_frukts_ar$år) %>% unique())
    diagramfil <- paste0("jmf_summerad_fruktsamhet_", fokus_region_txt, ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sum_frukts_ar %>% 
                                   filter(år == max(år)) %>% 
                                   mutate(fokus = case_when(regionkod %in% fokus_region ~ 1,
                                                            regionkod == "00"~ 2,
                                                            TRUE ~ 0)), 
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
    names(gg_list) <- objektnamn
    
  }
  # Diagram som visar förändring i samtliga valda regioner
  if(diag_forandring == TRUE){
    
    diagram_titel <- paste0("Förändring av summerad fruktsamhet per kvinna mellan år ", min(sum_frukts_ar$år), " och ", max(sum_frukts_ar$år))
    diagram_titel <- str_wrap(diagram_titel, width = 50)
    diagramfil <- paste0("forandring_summerad_fruktsamhet_", fokus_region_txt, ".png")
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
    names(gg_list) <- objektnamn
    
  }
  
  # diagram med bara valda regioner över tid (ett för varje). Finns som enskilda och facet
  if(diag_fokus_tid == TRUE){
    
    
    skapa_diagram <- function(df,vald_region){
      vald_region_txt <- (hamtaregion_kod_namn(vald_region)$region %>% skapa_kortnamn_lan())[1]
      
      if(length(vald_region) > 1){
        vald_region_txt <- paste0(vald_region_txt,"_facet")
        diagram_titel <- paste0("Summerad fruktsamhet per kvinna")
      }else{
        diagram_titel <- paste0("Summerad fruktsamhet per kvinna i ", vald_region_txt)
      }
      
      diagramfil <- paste0("summerad_fruktsamhet_", vald_region_txt,".png")
      objektnamn_map <- c(objektnamn_map,diagramfil %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                     filter(regionkod == vald_region), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "sum_frukts_ar",
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diag_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   x_axis_visa_var_xe_etikett = ifelse(length(vald_region) > 1,8,visa_var_xte),
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   diagram_facet = length(vald_region) > 1,
                                   facet_grp = "region",
                                   facet_scale = facet_skala,
                                   facet_legend_bottom = TRUE,
                                   skriv_till_diagramfil = spara_figur,
                                   manual_color = vald_farg[1],
                                   output_mapp = output_mapp_figur,
                                   filnamn_diagram = diagramfil)
      
      # Sparar och namnger diagram
      gg_list_map <- c(gg_list_map, list(gg_obj))
      names(gg_list_map) <- objektnamn_map
      return(gg_list_map)
    } # Slut funktion skapa_diagram
    
    # Funktionen körs enbart med hjälp av map när vi inte har facet-diagram
    if (diag_facet) {
      diag <- skapa_diagram(sum_frukts_ar,region_vekt)
      
    } else {
      diag <- map(region_vekt, ~ skapa_diagram(sum_frukts_ar, .x)) %>% flatten()
      
    }
   
    gg_list <- c(gg_list, diag)
  }
  
  # Om användaren vill returnera en figur, gör detta
  if(returnera_figur==TRUE){
    return(gg_list)
  }
  
}

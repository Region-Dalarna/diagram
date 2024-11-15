
diag_utbniva_tidserie_och_lansjmfr <- function(
                                       region_vekt = c("00", "20"),
                                       output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",
                                       diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                       skapa_fil = TRUE,
                                       gruppering_namn = NA,   # om man vill gruppera ihop medskickade regioner ger man denna parameter ett namn
                                       valt_ar = NA,
                                       diagramtitel_tabort = FALSE,
                                       ta_med_logga = TRUE,
                                       logga_sokvag = NA,
                                       sverige_istallet_for_riket = TRUE,
                                       facet_x_axis_stlk = 8,
                                       region_lagg_forst = NA,
                                       diag_hogutb_over_tid = TRUE,
                                       diag_lagutb_over_tid = FALSE,
                                       diag_andel_alla_utbnivaer = TRUE,
                                       diag_andel_utbniva_jmfr_lan = FALSE,
                                       vald_utb_niva = "eftergymn"){

  # =======================================================================================================
  #
  # Fyra diagram totalt. Data kommer från SCB: https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning
  #
  # Tre diagram över hur utbildningsnivån har förändrats över tid + ett diagram där andel eftergymnasialt 
  # utbildade jämförs mellan länen för ett år 
  #
  # 1. Andelen högutbildade i befolkningen 25-64 år från 1985 och framåt 
  # 2. Andelen lågutbildade i befolkningen 25-64 år från 1985 och framåt
  # 3. Andelen per utbildningsnivå (förgymn, gymn, kortare eftergymn, längre eftergymn) år 1985, 1990, 2000, 
  #    2010 och senaste år, eller annat valfritt år som man skickar med i valt_ar-parametern.
  # 4. Andel eftergymnasialt utbildade i befolkningen per kön 25-64 år för valt år, jämförelse mellan länen
  #
  # Parametrar:
  #
  # region_vekt       - fler region = facet-diagram med en facet per region
  # valt_ar           - om man vill ha något annat år än senaste år i diagram 
  # region_lagg_forst - om man vill lägga någon eller några regioner först bland facetdiagrammen så skickar man
  #                     med dem här i den ordning man vill ha dem.
  # gruppering_namn   - NA om man vill lägga regioiner som facet-diagram, om ett namn skickas med så läggs regionerna ihop till en som döps till namnet som skickas med
  # skapa_fil         - om man vill skriva diagrammen till png-filer, annars returneras bara ggplot-objekt
  # output_mapp       - där png-filerna sparas
  # diagram_capt      - beskrivning av diagrammet
  # vald_utb_niva     - används i diagram 4, "eftergymn" är förvalt men finns också "hogutb", "gymn" och "forgymn"
  #
  # diag_hogutb_over_tid        - diagram 1 ovan, TRUE om man vill ha med det, annars FALSE
  # diag_lagutb_over_tid        - diagram 2 ovan, TRUE om man vill ha med det, annars FALSE
  # diag_andel_alla_utbnivaer   - diagram 3 ovan, TRUE om man vill ha med det, annars FALSE
  # diag_andel_utbniva_jmfr_lan - diagram 4 ovan, TRUE om man vill ha med det, annars FALSE
  #                utbildningsnivå i diagram fyra styrs med parametern "vald_utb_niva", "eftergymn" är förvalt
  #
  # 2024-04-12 - Har ändrat revideringen nedan så att man fritt kan välja vilken utbildningsnivå man vill jämföra mellan länen i diagram 4. 
  #              Jag har ändrat i instruktionerna ovan så att det framgår hur man väljer. "eftergymn" är fortsatt förvald utbildningsnivå
  #              i länsjämförelsediagrammet. /Peter
  #
  # 2024-01-05 - Har lagt till ett val (minst_3_ar) som gör det möjligt att göra en länsjämförelse (diagram 4) även för personer med minst 3 års eftergymnasial utbildning
  # Tidigare gick det enbart att göra denna jämförelse för alla typer av eftergymnasial utbildning. /Jon
  #
  # =======================================================================================================
  
  
  # Utbildningsgrupper från 1985 och framåt
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue,
         pxweb)
  
  # Skript som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_utbniva_SCB.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
  tab_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  
  alla_giltiga_ar <- hamta_giltiga_varden_fran_tabell(tab_url, "tid")
  
  region_txt <- region_vekt %>% paste0(., collapse = "_")
  
  if (length(valt_ar) > 1) print("Endast ett år kan skickas med i funktionen, bara första året i vektorn kommer att användas.")
  if (is.na(valt_ar[1])) valt_ar <- max(alla_giltiga_ar) else {
    valt_ar <- valt_ar[1]
    if (!valt_ar %in% alla_giltiga_ar) valt_ar <- max(alla_giltiga_ar)
  }
  
  # ========================================== Läser in data ============================================
  px_df <- hamta_data_utbniva(region = region_vekt,
                                kon_klartext = c("män","kvinnor"),
                                alder = c(as.character(25:64)),
                                utbildningsniva_klartext = "*",
                                tid = "*")
  
  px_df_utskrift_kon <- px_df %>%
    filter(regionkod %in% region_vekt) %>% 
    mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = sverige_istallet_for_riket))
  
  if (!is.na(gruppering_namn)) {
    px_df_utskrift_kon <- px_df_utskrift_kon %>% 
      group_by(år, ålder, kön, utbildningsnivå) %>% 
      summarise(Befolkning = sum(Befolkning, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(regionkod = "grp",
             region = gruppering_namn) %>% 
      relocate(regionkod, .before = 1) %>% 
      relocate(region, .after = regionkod)
    
    region_txt <- gruppering_namn                           # för att få rätt regionnamn i titel, filnamn etc.
    region_vekt <- c("grp", region_vekt)
    
  } # slut if-sats om man vill gruppera
  
  # om vi vill sortera om facet-diagrammen så gör vi det här
  if (!is.na(region_lagg_forst[1]) & length(unique(px_df_utskrift_kon$region)) > 1){
    # testa om region_lagg_forst-koderna finns i datasetet
    if (any(region_lagg_forst %in% unique(px_df_utskrift_kon$regionkod))) {
      
      regionkoder_i_df <- unique(px_df_utskrift_kon$regionkod)
      region_i_df <- unique(px_df_utskrift_kon$region)
      # skapa sorteringsvektorer
      reg_ej_fokus <- unique(region_i_df[!regionkoder_i_df %in% region_lagg_forst]) %>% sort()
      reg_fokus <- unique(region_i_df[regionkoder_i_df %in% region_lagg_forst])        # bara de regioner som finns i datasetet kommer med
      reg_fokus_koder <- unique(regionkoder_i_df[regionkoder_i_df %in% region_lagg_forst])        # bara de regioner som finns i datasetet kommer med
      reg_fokus <- reg_fokus[match(region_lagg_forst, reg_fokus_koder)]               # sortera utifrån ordningen i region_lagg_forst
      region_sort <- c(reg_fokus, reg_ej_fokus)    
      
      # sortera med hjälp av region_sort som vi skapade ovan
      px_df_utskrift_kon <- px_df_utskrift_kon %>% 
        mutate(region = factor(region, levels = region_sort))
    } # slut if-sats för att testa om region_lagg_forst-koderna finns i datasetet
  } # slut if-sats om vi skickat med lagg_forst_koder
  
  # bearbeta datasetet
  px_df_utskrift_kon <- px_df_utskrift_kon %>% 
    mutate(utb_niva = case_when(
      utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
      utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
      utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
      utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
      utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
      utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
      utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
      utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas"),
      utb_niva = factor(utb_niva, levels = c("Eftergymnasial utbildning, 3 år eller mer","Eftergymnasial utbildning, mindre än 3 år","Gymnasial utbildning","Förgymnasial utbildning"))) %>% 
    filter(utb_niva != "Uppgift saknas") %>% 
    group_by(år, regionkod, region, kön, utb_niva) %>% 
    summarize(antal = sum(Befolkning, na.rm = TRUE)) %>% 
    mutate(andel = (antal/sum(antal)) * 100) %>% 
    ungroup()
  
  # Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning - uppdelat på kön
  px_df_utskrift <- px_df_utskrift_kon %>%
    filter(utb_niva != "Uppgift saknas") %>% 
    group_by(år, regionkod, region, utb_niva) %>% 
    summarize(antal = sum(antal, na.rm = TRUE)) %>% 
    mutate(andel = (antal/sum(antal)) * 100) %>% 
    ungroup()
  
  region_titel <- if (length(region_vekt) > 1) "" else paste0(" i ", unique(px_df_utskrift$region))
  
    if (diag_hogutb_over_tid) {
    diagramtitel <- paste0("Andel högutbildade invånare 25-64 år", region_titel) %>% str_wrap()
    diagramfilnamn <- paste0("hogutb_andel_ar", region_txt, "_", min(px_df_utskrift_kon$år), "_", max(px_df_utskrift_kon$år), ".png")
    diagram_capt_hogutb <- paste0(diagram_capt, "\nDefinitionen av högutbildade är individer med minst 3 års eftergymnasial utbildning.")
    
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_utskrift_kon %>%
                                   filter(regionkod %in% region_vekt,
                                     utb_niva == "Eftergymnasial utbildning, 3 år eller mer"), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt_hogutb,
                                 diagram_facet = if (length(region_vekt) > 1) TRUE else FALSE,
                                 facet_grp = "region",
                                 facet_scale = "fixed",
                                 facet_x_axis_storlek = facet_x_axis_stlk,
                                 stodlinjer_avrunda_fem = TRUE,
                                 facet_legend_bottom = TRUE,
                                 #x_axis_lutning = 0, 
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 legend_vand_ordning=TRUE,
                                 utan_diagramtitel = diagramtitel_tabort,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- diagramfilnamn %>% str_remove(".png")
    } # slut if-sats om man vill skriva ut diagram över utvecklingen av högutbildade över tid 
  
  if (diag_lagutb_over_tid) {
    diagramtitel <- paste0("Andel lågutbildade invånare 25-64 år", region_titel) %>% str_wrap()
    diagramfilnamn <- paste0("lagutb_andel_ar", region_txt, "_", min(px_df_utskrift_kon$år), "_", max(px_df_utskrift_kon$år), ".png")
    diagram_capt_lagutb <- paste0(diagram_capt, "\nDefinitionen av lågutbildade är individer med endast förgymnasial utbildning.")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_utskrift_kon %>%
                                   filter(regionkod %in% region_vekt,
                                          utb_niva == "Förgymnasial utbildning"), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt_lagutb,
                                 diagram_facet = if (length(region_vekt) > 1) TRUE else FALSE,
                                 facet_grp = "region",
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 facet_x_axis_storlek = facet_x_axis_stlk,
                                 #x_axis_lutning = 0, 
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 utan_diagramtitel = diagramtitel_tabort,
                                 legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- diagramfilnamn %>% str_remove(".png")
  } # slut if-sats om man vill skriva ut diagram över utvecklingen av högutbildade över tid 
  
  
  if (diag_andel_alla_utbnivaer) {
    diagramtitel <- paste0("Utbildningsnivå för invånare 25-64 år", region_titel) %>% str_wrap()
    diagramfilnamn <- paste0("utbniva_andel_per_ar_", region_txt, "_", valt_ar, ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_utskrift %>%
                                   filter(regionkod %in% region_vekt,
                                          år %in% c("1985", "1990", "2000", "2010", valt_ar)) %>% 
                                   mutate(andel = andel - 0.001), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "utb_niva",
                                 #manual_x_axis_text_vjust=1,
                                 #manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("rd_bla"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = if (length(region_vekt) > 1) TRUE else FALSE,
                                 facet_grp = "region",
                                 facet_legend_bottom = TRUE,
                                 facet_scale = "fixed",
                                 facet_x_axis_storlek = facet_x_axis_stlk,
                                 geom_position_stack = TRUE,
                                 procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0, 
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 utan_diagramtitel = diagramtitel_tabort,
                                 legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 manual_x_axis_title = "år",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- diagramfilnamn %>% str_remove(".png")
  } # slut if-sats om man vill skriva ut diagram över alla utbildningsnivåer
  
  if (diag_andel_utbniva_jmfr_lan){
    
    pxweb_query_list <- 
      list(Region = hamtaAllaLan(F),
           Kon = "*",
           Alder = c(as.character(25:64)),
           UtbildningsNiva ="*",
           ContentsCode = "*",
           Tid = valt_ar)
    
    # Download data 
    px_data <- pxweb_get(url = tab_url, query = pxweb_query_list)
    
    # Convert to data.frame 
    px_df <- as.data.frame(px_data) %>% 
      bind_cols(as.data.frame(px_data, variable.value.type = "code") %>% 
                  select(regionkod = region)) %>% 
      relocate(regionkod, .before = "region") 
    
    px_df_jmfr_lan <- px_df %>%
      mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = sverige_istallet_for_riket))
    
    # bearbeta datasetet
    px_df_jmfr_lan <- px_df_jmfr_lan %>% 
      mutate(utb_niva = case_when(
        utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
        utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
        utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
        utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
        utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
        utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
        utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
        utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas"),
        utb_niva = factor(utb_niva, levels = c("Eftergymnasial utbildning, 3 år eller mer","Eftergymnasial utbildning, mindre än 3 år","Gymnasial utbildning","Förgymnasial utbildning"))) %>% 
      filter(utb_niva != "Uppgift saknas") %>% 
      group_by(år, regionkod, region, kön, utb_niva) %>% 
      summarize(antal = sum(Befolkning, na.rm = TRUE)) %>% 
      mutate(andel = (antal/sum(antal)) * 100) %>% 
      ungroup()

    # if(minst_3_ar == TRUE){
    #   utb_niva_vec = c("Eftergymnasial utbildning, 3 år eller mer")
    #   diagramtitel <- paste0("Andel invånare 25-64 år med minst 3 års eftergymnasial utbildning år ", valt_ar)
    #   diagramfilnamn <- paste0("hogutb_andel_ar_", valt_ar, ".png")
    # }else{
    #   utb_niva_vec <- c("Eftergymnasial utbildning, mindre än 3 år",
    #                     "Eftergymnasial utbildning, 3 år eller mer")
    #   diagramtitel <- paste0("Andel invånare 25-64 år med eftergymnasial utbildning år ", valt_ar) 
    #   diagramfilnamn <- paste0("eftergymn_utb_andel_ar_", valt_ar, ".png")
    # }
    
    if (vald_utb_niva == "hogutb") {
      
      utb_niva_vec = c("Eftergymnasial utbildning, 3 år eller mer")
      dia_titel_txt = "minst 3 års eftergymnasial"
      dia_filnamn_txt = "hog"
      
    } else if (vald_utb_niva == "eftergymn") {
      
      utb_niva_vec <- c("Eftergymnasial utbildning, mindre än 3 år",
                        "Eftergymnasial utbildning, 3 år eller mer")
      dia_titel_txt = "eftergymnasial"
      dia_filnamn_txt = "eftergymn_"
      
    } else if (vald_utb_niva == "gymn") {
      
      utb_niva_vec <- c("Gymnasial utbildning")
      dia_titel_txt = "gymnasial"
      dia_filnamn_txt = "gymn_"
      
    } else if (vald_utb_niva == "forgymn") {
      
      utb_niva_vec <- c("Förgymnasial utbildning")
      dia_titel_txt = "förgymnasial"
      dia_filnamn_txt = "forgymn_"
      
    } else {
      stop("Felaktig parameter för vald_utb_niva. Välj mellan 'hogutb', 'eftergymn', 'gymn', 'forgymn' eller 'alla'.")
    }
    
    diagramtitel <- glue("Andel invånare 25-64 år med {dia_titel_txt} utbildning år {valt_ar}")
    diagramfilnamn <- glue("{dia_filnamn_txt}utb_andel_ar_", valt_ar, ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_jmfr_lan %>% 
                                   filter(utb_niva %in% utb_niva_vec), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "kön",
                                 #manual_x_axis_text_vjust=1,
                                 #manual_x_axis_text_hjust=1,
                                 diagram_liggande = TRUE,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 facet_legend_bottom = TRUE,
                                 x_axis_lutning = 0, 
                                 x_axis_sort_value = TRUE,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 utan_diagramtitel = diagramtitel_tabort,
                                 #legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- diagramfilnamn %>% str_remove(".png")
  
  } # slut if-sats om man vill skriva ut länsjämförelsediagram
  
  return(gg_list)
  
} # slut funktion

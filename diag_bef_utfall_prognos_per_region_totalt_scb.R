
diag_bef_utfall_prognos_per_region_totalt <- function(
    region_vekt = c("20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085"),                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) för alla regioner
    diagram_capt = "auto",                    # diagram_capt skapa automatiskt och blir olika beroende på vilken tabell som används
        # om <prognos_ar> ligger med i diagram_capt så byts det ut mot det år prognosen gjordes
    output_mapp = NA,                                        # här sparas diagramet
    #region_txt = NA,                                      # om NA så används regionnamnet i datasetet, annars detta namn
    diagram_fargvekt = NA,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    jmfr_tid = 10,                                           # hur många år framåt från befolkningsprognosen vi ska ta med
    aldersgrupper = NA,                                      # NA = alla åldrar, annars list(c(20,65)), eller list(c(0,19), c(20,65)) om man vill ha fler
    returnera_dataframe_global_environment = FALSE,          
    ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
    antal_istallet_for_andel = FALSE,                        # default är andel då regioner jämförs (mest rimligt) men man kan få antal om man sätter denna parameter till TRUE
    x_axis_visa_var_xe_etikett = 3,                          # var x:e etikett visas enbart
    x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = FALSE, # man kan ta bort näst sista värdet om det hamnar på varandra
    url_befprognos_tabell = NA,                              # om NA så väljs standardtabell, annars kan man skicka med vilken tabell man vill använda (SCB eller sökväg till egna datafiler), SCB:s senaste: "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
    skriv_till_diagramfil = TRUE,
    skriv_till_excelfil = FALSE
) {

  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         writexl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  
  
  gg_list <- list()
  
  # Om url_befprognos_tabell är NA och förinställd mapp inte finns på datorn används SCB:s tabell för det senaste året som finns
  if (is.na(url_befprognos_tabell)) {
    url_befprognos_tabell <- if (!dir.exists("G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/")) {
      "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
    } else {
      "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/"
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diagram_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diagram_fargvekt <- rev(diagramfarger("rus_sex")[c(1,2)])
    } else {
      diagram_fargvekt <- hue_pal()(9)
    }
  }
  
  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_till_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(utskriftsmapp())) {
        output_mapp <- utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }
  
  # om man har valt åldersgrupper
  if (!any(is.na(aldersgrupper))) {
    aldrar_hamta <- map(aldersgrupper, ~ seq(.x[1],.x[2])) %>% unlist() %>% unique() %>% 
      as.character() %>% str_replace("100", "100+")
    aldervekt_max <- map(aldersgrupper, ~ .x[2]) %>% unlist() %>% unique() %>% max() +1
    till_aldervekt <- map(aldersgrupper, ~ .x[1]) %>% unlist() %>% unique() %>% c(., aldervekt_max)
    
    bef_folkmangd <- funktion_upprepa_forsok_om_fel(function()
      hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt, 
                                           alder_koder = aldrar_hamta,
                                           kon_klartext = NA)) %>% 
      mutate(alder_grp = skapa_aldersgrupper(ålder, till_aldervekt),
             alder_grp = if_else(str_detect(alder_grp, "-100 år"), str_replace(alder_grp, "-100 år", "+ år"), alder_grp))
    
  } else {
  
  bef_folkmangd <- funktion_upprepa_forsok_om_fel(function()
    hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt, 
                                                        kon_klartext = NA,
                                                        alder_koder = NA)) %>% 
    mutate(alder_grp = "alla åldrar")
  till_aldervekt <- NA
  }
  
  # special för att hantera förändring av namn på innehållsvariable från Folkmängd till Antal
  if ("Folkmängd" %in% names(bef_folkmangd)) bef_folkmangd <- bef_folkmangd %>% rename(Antal = Folkmängd)
  
  # välj år för prognosen utifrån senaste år för befolkning
  start_ar <- (as.numeric(max(bef_folkmangd$år)) + 1) %>% as.character()
  slut_ar <- (as.numeric(start_ar) + jmfr_tid) %>% as.character()
  
  hamta_region <- region_vekt[region_vekt != "00"]
  hamta_riket <- region_vekt[region_vekt == "00"]
  
  if (length(hamta_region) > 0) {
  bef_prognos <- funktion_upprepa_forsok_om_fel(function()
    hamta_befprognos_data(region_vekt = hamta_region,
                        tid_vekt = c(start_ar:slut_ar),
                        url_prognos_vektor = url_befprognos_tabell
                        ), max_forsok = 4
  )} else bef_prognos <- NULL
  
  if (!any(is.na(aldersgrupper))) {
    bef_prognos <- bef_prognos %>% 
      filter(ålder %in% paste0(aldrar_hamta, " år")) %>% 
      mutate(alder_grp = skapa_aldersgrupper(ålder, till_aldervekt),
             alder_grp = if_else(str_detect(alder_grp, "-100 år"), str_replace(alder_grp, "-100 år", "+ år"), alder_grp)) 
  } else {
    bef_prognos <- bef_prognos %>% 
      mutate(alder_grp = "alla åldrar")
  }
  # special för att hantera förändring av namn på innehållsvariable från Folkmängd till Antal
  if ("Folkmängd" %in% names(bef_prognos)) bef_prognos <- bef_prognos %>% rename(Antal = Folkmängd)
   
  if (length(hamta_riket) > 0) {
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_BefolkprognRevNb_scb.R")
    if (!any(is.na(aldersgrupper))) {
      # med åldersgrupper
      bef_prognos_riket <- funktion_upprepa_forsok_om_fel(function()
        hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb(tid_koder = c(start_ar:slut_ar)
        ), max_forsok = 4
      ) %>% 
        mutate(regionkod = "00",
               region = "Riket",
               alder_koder = aldrar_hamta,
               prognos_ar = as.character(as.numeric(start_ar) - 1)) %>% 
        group_by(regionkod, region, kön, år, prognos_ar) %>% 
        summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop") %>% 
        mutate(alder_grp = skapa_aldersgrupper(ålder, till_aldervekt))
      
    } else {
      # utan åldersgrupper
      bef_prognos_riket <- funktion_upprepa_forsok_om_fel(function()
        hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb(tid_koder = c(start_ar:slut_ar)
        ), max_forsok = 4
      ) %>% 
        mutate(regionkod = "00",
               region = "Riket",
               prognos_ar = as.character(as.numeric(start_ar) - 1)) %>% 
        group_by(regionkod, region, kön, år, prognos_ar) %>% 
        summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop") %>% 
        mutate(alder_grp = "alla åldrar")
      
    } # slut if-sats om vi har med åldersgrupper
    
  } else bef_prognos_riket <- NULL
    
  # lägg ihop regioner och riket, ta bara bort om en df = NULL,
  # finns två dataframes (både region(er) och riket) så läggs de
  # ihop med list_rbind() så resultatet blir alltid en df
  bef_prognos <- compact(list(bef_prognos, bef_prognos_riket)) %>% 
    list_rbind()
  
  bef_prognos2 <- bef_prognos %>% 
    mutate(typ = "prognos") %>% 
    group_by(regionkod, region, typ, alder_grp, år) %>% 
    summarise(Antal = round(sum(Antal, na.rm = TRUE)), .groups = "drop")
  
  
  bef_folk_progn <- bef_folkmangd %>%
    #filter(ålder != "totalt ålder") %>%
    mutate(typ = "utfall") %>% 
    group_by(regionkod, region, typ, alder_grp, år) %>% 
    summarise(Antal = round(sum(Antal, na.rm = TRUE)), .groups = "drop") %>% 
    bind_rows(bef_prognos2) %>% 
    arrange(regionkod, region, alder_grp, år) %>% 
    mutate(Befolkningsökning = Antal - lag(Antal, 1),
           bef_okning_rel = Befolkningsökning / lag(Antal) * 100) %>% 
    filter(år != min(år))
  
  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("bef_utfall_prognos_per_region_tot", bef_folk_progn, envir = .GlobalEnv)
  }
  
  # hantera diagram_capt
  if (diagram_capt == "auto") {
    diagram_capt <- case_when(
      str_detect(url_befprognos_tabell, "api.scb.se") & str_detect(url_befprognos_tabell, "Profet/datafiler") ~ 
        "Källa: SCB:s befolkningsprognos och Region Dalarnas egna befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna\nI Region Dalarnas befolkningsprognos baseras prognosen för Ludvika kommun på ett scenario som i allt väsentligt liknar det som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt.",
      str_detect(url_befprognos_tabell, "api.scb.se") ~ 
        "Källa: SCB:s befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna",
      str_detect(url_befprognos_tabell, "Profet/datafiler") & region_vekt %in% c("20", "2085") ~ 
        "Källa: Region Dalarnas egna befolkningsprognos från år <prognos_ar>, bearbetning av Samhällsanalys, Region Dalarna\nPrognosen för Ludvika kommun baseras på ett scenario som i allt väsentligt liknar den som Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt.",
      str_detect(url_befprognos_tabell, "Profet/datafiler") ~ 
        "Källa: Region Dalarnas egna befolkningsprognos från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna"
    )
  }
  
  # byt ut <prognos_ar> mot året som prognosen är från om det finns i textsträngen
  diagram_capt <- diagram_capt %>%
    str_replace_all("<prognos_ar>", unique(bef_prognos$prognos_ar))
  
  
  skapa_diagram <- function(skickad_regionkod, skickad_alder_grp) {
    
    diagram_df <- bef_folk_progn %>%
      filter(regionkod %in% skickad_regionkod,
             alder_grp %in% skickad_alder_grp)
    
    if (any(c("03", "19" ) %in% diagram_df$regionkod)) {
      diagram_df <- diagram_df %>% 
        mutate(bef_okning_rel = if_else(regionkod %in% c("03", "19") & år == "2006", 0, lag(bef_okning_rel)))
    }
    
    regionkoder <- unique(diagram_df$regionkod)
    region_filnamn <- regionkoder %>% paste0(collapse = "_")
    region_filnamn <- ar_alla_kommuner_i_ett_lan(regionkoder, returnera_text = TRUE, returtext = region_filnamn)
    startar_utfall <- diagram_df %>% filter(typ == "utfall") %>% dplyr::pull(år) %>% min()
    slutar_utfall <- diagram_df %>% filter(typ == "utfall") %>% dplyr::pull(år) %>% max()  
    startar_prognos <- diagram_df %>% filter(typ == "prognos") %>% dplyr::pull(år) %>% min()
    slutar_prognos <- diagram_df %>% filter(typ == "prognos") %>% dplyr::pull(år) %>% max()
    alder_grp_filnamn <- if(skickad_alder_grp == "alla åldrar") "" else paste0("_", skickad_alder_grp)
    alder_grp_txt <- if(skickad_alder_grp == "alla åldrar") "" else paste0(" för invånare ", skickad_alder_grp)
    enhet_txt <- if(antal_istallet_for_andel) "Befolkningsförändring" else "Relativ befolkningsförändring"
    enhet_filnamn <- if(antal_istallet_for_andel) "" else "rel_"
    
    diagramtitel <- glue("{enhet_txt} år {startar_utfall}-{slutar_utfall} samt befolkningsprognos {startar_prognos}-{slutar_prognos}{alder_grp_txt}")
    diagramfil <- glue("{enhet_filnamn}befforandring_utfall_progn_{region_filnamn}_ar{startar_utfall}-{slutar_prognos}{alder_grp_filnamn}.png")
  
  
    gg_obj <- SkapaStapelDiagram(skickad_df = diagram_df,
                                 skickad_x_var = "år",
                                 skickad_y_var = if (antal_istallet_for_andel) "Befolkningsökning" else "bef_okning_rel",
                                 skickad_x_grupp = "typ",
                                 #x_axis_sort_value = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 utan_diagramtitel = ta_bort_diagramtitel,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 #x_axis_lutning = 0,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title =  if (antal_istallet_for_andel) "förändring antal invånare" else "procent",
                                 manual_color = diagram_fargvekt,
                                 output_mapp = output_mapp,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 dataetiketter = visa_dataetiketter,
                                 legend_vand_ordning = TRUE,
                                 diagram_facet = TRUE,
                                 facet_grp = "region",
                                 facet_scale = "free_x",
                                 x_axis_storlek = 7,
                                 facet_x_axis_storlek = 5,
                                 x_axis_visa_var_xe_etikett = x_axis_visa_var_xe_etikett,
                                 x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = x_axis_var_xe_etikett_ta_bort_nast_sista_vardet,
                                 facet_legend_bottom = TRUE
                                 #diagram_spara_annat_format = "eps"
    ) # slut skriv ggplot_objekt
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove("\\.[^.]+$")
    
    return(gg_list) 
    
  } # slut funktion som skapar diagrammet
  
  #retur_list <- map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)) %>% purrr::flatten()
  
  retur_list <- map(unique(bef_folk_progn$alder_grp), ~ skapa_diagram(
    skickad_regionkod = region_vekt,
    skickad_alder_grp = .x)) %>% purrr::flatten()
  
  if (skriv_till_excelfil) {
    regionkoder <- unique(bef_folk_progn$regionkod)
    region_filnamn <- regionkoder %>% paste0(collapse = "_")
    region_filnamn <- ar_alla_kommuner_i_ett_lan(regionkoder, returnera_text = TRUE, returtext = region_filnamn)
    startar_utfall <- bef_folk_progn %>% filter(typ == "utfall") %>% dplyr::pull(år) %>% min()
    slutar_prognos <- bef_folk_progn %>% filter(typ == "prognos") %>% dplyr::pull(år) %>% max() 
    excefilnamn <- glue("befolkning_utfall_progn_{region_filnamn}_ar{startar_utfall}-{slutar_prognos}.xlsx")
    write_xlsx(bef_folk_progn, paste0(output_mapp, excefilnamn))
  }
  
  return(retur_list)
  
} # slut funktion

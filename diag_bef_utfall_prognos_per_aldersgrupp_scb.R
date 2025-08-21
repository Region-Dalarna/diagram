
diag_bef_utfall_prognos_per_aldersgrupp <- function(
    region_vekt = "20",                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region
    aldersindelning = c(0, 1, 6, 16, 20, 66, 80),
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningsframskrivning från år <prognos_ar>\nBearbetning: Samhällsanalys, Region Dalarna",
        # om <prognos_ar> ligger med i diagram_capt så byts det ut mot det år prognosen gjordes
    output_mapp = NA,                                        # här sparas diagramet
    diagram_fargvekt = NA,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    jmfr_tid = 10,                                           # hur många år framåt från befolkningsprognosen vi ska ta med
    returnera_dataframe_global_environment = FALSE,          
    ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
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
  
  bef_folkmangd <- funktion_upprepa_forsok_om_fel(function()
    hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt, 
                                                        kon_klartext = NA,
                                                        alder_koder = "*")
  )
  
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
                        url_prognos_vektor = c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN")
                        ), max_forsok = 4
  )} else bef_prognos <- NULL
  
  # special för att hantera förändring av namn på innehållsvariable från Folkmängd till Antal
  if ("Folkmängd" %in% names(bef_prognos)) bef_prognos <- bef_prognos %>% rename(Antal = Folkmängd)
   
  if (length(hamta_riket) > 0) {
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_BefolkprognRevNb_scb.R")
    bef_prognos_riket <- funktion_upprepa_forsok_om_fel(function()
      hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb(tid_koder = c(start_ar:slut_ar)
      ), max_forsok = 4
    ) %>% 
      mutate(regionkod = "00",
             region = "Riket",
             prognos_ar = as.character(as.numeric(start_ar) - 1),
             alder_num = parse_number(ålder),
             ålder = if_else(alder_num > 99, "100+ år", ålder)) %>% 
      group_by(regionkod, region, kön, ålder, år, prognos_ar) %>% 
      summarise(Antal = sum(Antal, na.rm = TRUE), .groups = "drop")
    
  } else bef_prognos_riket <- NULL
    
  # lägg ihop regioner och riket, ta bara bort om en df = NULL,
  # finns två dataframes (både region(er) och riket) så läggs de
  # ihop med list_rbind() så resultatet blir alltid en df
  bef_prognos <- compact(list(bef_prognos, bef_prognos_riket)) %>% 
    list_rbind()
  
  bef_prognos2 <- bef_prognos %>% 
    mutate(aldergrp = skapa_aldersgrupper(ålder, aldersindelning),
           typ = "prognos") %>% 
    group_by(regionkod, region, typ, aldergrp, år) %>% 
    summarise(Antal = round(sum(Antal, na.rm = TRUE)), .groups = "drop")
  
  
  bef_folk_progn <- bef_folkmangd %>%
    filter(ålder != "totalt ålder") %>%
    mutate(aldergrp = skapa_aldersgrupper(ålder, aldersindelning),
           typ = "utfall") %>% 
    group_by(regionkod, region, typ, aldergrp, år) %>% 
    summarise(Antal = round(sum(Antal, na.rm = TRUE)), .groups = "drop") %>% 
    bind_rows(bef_prognos2) %>% 
    arrange(regionkod, region, aldergrp, år) %>% 
    mutate(Befolkningsökning = Antal - lag(Antal, 1)) %>% 
    filter(år != min(år))
  
  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("bef_utfall_prognos_per_aldersgrupp", bef_folk_progn, envir = .GlobalEnv)
  }
  
  # byt ut <prognos_ar> mot året som prognosen är från om det finns i textsträngen
  diagram_capt <- diagram_capt %>%
    str_replace_all("<prognos_ar>", unique(bef_prognos$prognos_ar))
  
  
  skapa_diagram <- function(skickad_regionkod) {
    
    diagram_df <- bef_folk_progn %>%
      filter(regionkod %in% skickad_regionkod)
    
    region_txt <- unique(diagram_df$region) %>% skapa_kortnamn_lan()
    startar_utfall <- diagram_df %>% filter(typ == "utfall") %>% dplyr::pull(år) %>% min()
    slutar_utfall <- diagram_df %>% filter(typ == "utfall") %>% dplyr::pull(år) %>% max()  
    startar_prognos <- diagram_df %>% filter(typ == "prognos") %>% dplyr::pull(år) %>% min()
    slutar_prognos <- diagram_df %>% filter(typ == "prognos") %>% dplyr::pull(år) %>% max()
  
    diagramtitel <- glue("Befolkning i {region_txt} år {startar_utfall}-{slutar_utfall} samt befolkningsprognos {startar_prognos}-{slutar_prognos}")
    diagramfil <- glue("befolkning_utfall_progn_{region_txt}_ar{startar_utfall}-{slutar_prognos}.png")
  
  
    gg_obj <- SkapaStapelDiagram(skickad_df = diagram_df,
                                 skickad_x_var = "år",
                                 skickad_y_var = "Antal",
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
                                 manual_color = diagram_fargvekt,
                                 output_mapp = output_mapp,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 dataetiketter = visa_dataetiketter,
                                 legend_vand_ordning = TRUE,
                                 diagram_facet = TRUE,
                                 facet_grp = "aldergrp",
                                 facet_scale = "free",
                                 x_axis_storlek = 7,
                                 facet_x_axis_storlek = 6,
                                 x_axis_visa_var_xe_etikett = 3,
                                 facet_legend_bottom = TRUE
                                 #diagram_spara_annat_format = "eps"
    ) # slut skriv ggplot_objekt
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
    return(gg_list) 
    
  } # slut funktion som skapar diagrammet
  
  retur_list <- map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)) %>% purrr::flatten()
  
  if (skriv_till_excelfil) {
    region_xlsx <- unique(bef_folk_progn$region) %>% skapa_kortnamn_lan() %>% paste0(collapse = "_")
    startar_utfall <- bef_folk_progn %>% filter(typ == "utfall") %>% dplyr::pull(år) %>% min()
    slutar_prognos <- bef_folk_progn %>% filter(typ == "prognos") %>% dplyr::pull(år) %>% max() 
    excefilnamn <- glue("befolkning_utfall_progn_{region_xlsx}_ar{startar_utfall}-{slutar_prognos}.xlsx")
    write_xlsx(bef_folk_progn, paste0(output_mapp, excefilnamn))
  }
  
  return(retur_list)
  
} # slut funktion

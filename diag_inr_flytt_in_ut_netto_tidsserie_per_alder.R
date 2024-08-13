

diag_inr_flytt_in_ut_netto_tidsserie_per_alder <- function(region_vekt = "20",
                                     gruppera_namn = NA,
                                     ta_med_logga = TRUE,
                                     logga_path = NA,
                                     farg_vektor = NA,
                                     output_mapp = NA,
                                     kon_klartext = NA,
                                     visa_flyttnetto_linje = TRUE,
                                     skriv_diagramfil = TRUE,
                                     alder_koder = "*",
                                     alder_grp = NA,                                   # skicka med startåldern i de åldersgrupper man vill skapa, t.ex. om man hämtar alla åldrar och tar ut 20, 65, 80 så blir åldersgrupperna 0-19, 20-64, 65-79 samt 80+ 
                                     tid_koder = "*",
                                     relativt_flyttnetto = TRUE                                      # beräkna andel istället för antal
                                     ){

  # =======================================================================================================================
  #
  # Hämta hem data med funktionen hamta_bef_flyttningar_region_alder_kon_scb, skriv ut ett diagram. Defaultinställning är
  # att visa flyttnettolinje, relativt flyttnetto och att hämta alla år. Det finns möjlighet att skapa åldersgrupper direkt
  # i diagramskriptet genom att skicka med startålder för varje åldersgrupp i parametern "alder_grp". 
  #
  # Länk till SCB-tabell där data hämtas: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101J/Flyttningar97/   (flyttningar) 
  #                                       https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0101__BE0101A/BefolkningNy/    (folkmängd för beräkning av relativt flyttnetto)
  #
  # Skapat av: Peter Möller, Region Dalarna
  # 
  # =======================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  
  if(all(is.na(farg_vektor))) farg_vektor <- ifelse(exists("diagramfarger"), diagramfarger("rus_sex")[1], "green")
                                                     
  flytt_df <- hamta_bef_flyttningar_region_alder_kon_scb(region_vekt = region_vekt,
                                                         kon_klartext = kon_klartext,
                                                         alder_koder = alder_koder,
                                                         tid_koder = tid_koder,
                                                         cont_klartext = c("Inrikes inflyttningar", "Inrikes utflyttningar"))
  
  
  # kolla om kön är med som variabel
  if (!all(is.na(kon_klartext))) {
    kon_txt <- paste0("_", flytt_df$kön %>% unique, collapse = "_")             # fyll kon_txt med rätt textsträng
    farg_vektor <- diagramfarger("kon")                                         # fyll farg_vektor med rätt färger
  } else {
    kon_txt <- ""
  }
  
  kon_titel <- ""
  
  if ("kön" %in% names(flytt_df)) {
    if (flytt_df$kön %>% unique() %>% length() == 1) {
      farg_vektor <- if (flytt_df$kön %>% unique() == "kvinnor") farg_vektor[1] else farg_vektor[2]
      kon_titel <- flytt_df$kön %>% unique() %>% paste0("för ",., " ")
      kon_ett_varde <- TRUE
    } else kon_ett_varde <- FALSE
  } else kon_ett_varde <- FALSE
  
  # välj ut variabler till gruppering av netto-datasetet, dvs. med eller utan kön
  if (!all(is.na(kon_klartext))) {
    netto_grp_var <- c("år", "regionkod", "region", "alder_num", "kön", "ålder")
  } else {
    netto_grp_var <- c("år", "regionkod", "region", "alder_num", "ålder")
  }
  
  # skapa åldersgrupper om det har skickats med som parameter, annars gör ingenting
  if (!all(is.na(alder_grp))) {
    flytt_df <- flytt_df %>% 
      filter(ålder != "totalt ålder") %>% 
      mutate(ålder = skapa_aldersgrupper(ålder, alder_grp))
  } else {
    flytt_df <- flytt_df %>% 
      filter(ålder != "totalt ålder") %>% 
      mutate(alder_num = ålder %>% parse_number()) %>% 
      arrange(alder_num) %>% 
      mutate(ålder = factor(ålder, levels = unique(ålder)))
  }
  
  # grupperar datasetet om man angett värde för gruppera_namn
  if (!is.na(gruppera_namn)){
    flytt_df <- flytt_df %>% 
      group_by(across(where(is.character))) %>% 
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
      ungroup() %>% 
      mutate(region = gruppera_namn,
             regionkod = "gg")
  } 

  chart_df <- flytt_df %>% 
    mutate(varde = ifelse(variabel == "Inrikes utflyttningar", varde*-1, varde)) %>% 
    arrange(ålder)
  
  netto_df <- chart_df %>% 
    group_by(across(any_of(netto_grp_var))) %>% 
    summarise(varde = sum(varde[variabel == "Inrikes inflyttningar"]) + sum(varde[variabel == "Inrikes utflyttningar"])) %>% 
    ungroup() %>% 
    mutate(variabel = "Netto")
  
  if (relativt_flyttnetto) {
    
    hamta_ar <- netto_df$år %>% unique()
    
    # hämta total befolkningsmängd för att kunna räkna ut relativt flyttnetto
    befolkning_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt,
                                                          kon_klartext = kon_klartext,
                                                          alder_koder = alder_koder,
                                                          tid_koder = hamta_ar)
    
    if (!all(is.na(alder_grp))) {
      befolkning_df <- befolkning_df %>% 
        filter(ålder != "totalt ålder") %>% 
        mutate(ålder = skapa_aldersgrupper(ålder, alder_grp))
    } else {
      befolkning_df <- befolkning_df %>% 
        filter(ålder != "totalt ålder") %>% 
        mutate(alder_num = ålder %>% parse_number()) %>% 
        arrange(alder_num) %>% 
        mutate(ålder = factor(ålder, levels = unique(ålder)))
    }
    
    # grupperar datasetet om man angett värde för gruppera_namn
    if (!is.na(gruppera_namn)){
      befolkning_df <- befolkning_df %>% 
        group_by(across(where(is.character))) %>% 
        summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
        ungroup() %>% 
        mutate(region = gruppera_namn,
               regionkod = "gg")
    }
    
    bef_grp <- befolkning_df %>% 
      group_by(across(any_of(netto_grp_var))) %>% 
      summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) %>% 
      ungroup()
    
    join_var <- netto_grp_var[netto_grp_var != "alder_num"]
    
    chart_df <- netto_df %>% 
      left_join(bef_grp, by = join_var) %>% 
      mutate(andel = (varde / Folkmängd) * 100)
  } else chart_df <- netto_df
  
  aldergrupper_txt <- if (first(chart_df$ålder) != last(chart_df$ålder)) paste0(first(chart_df$ålder), "-", last(chart_df$ålder)) else paste0(first(chart_df$ålder))
  andel_txt <- if (relativt_flyttnetto) "rel_" else ""
  andel_titel <- if (relativt_flyttnetto) " relativt " else " "
  
  diagram_capt <- "Källa: Befolkningsstatistik, SCB:s öppna statistikdatabas\nBearbetning av Samhällsanalys, Region Dalarna"
  diagram_titel <- glue("Skillnad mellan in- och utflyttning (inrikes{andel_titel}flyttnetto) {kon_titel}i {unique(chart_df$region)} år {first(chart_df$år)}-{last(chart_df$år)}")
  diagram_filnamn <- glue("{andel_txt}flyttnetto_{unique(chart_df$region)}_alder_{aldergrupper_txt}_ar_{first(chart_df$år)}-{last(chart_df$år)}{kon_txt}.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
                               skickad_x_var = "år",
                               skickad_y_var = if (relativt_flyttnetto) "andel" else "varde",
                               skickad_x_grupp = if (!all(is.na(kon_klartext))) "kön" else NA,
                               #skickad_x_grupp = "variabel",
                               y_axis_minus_plus_samma_axel = TRUE,
                               #x_axis_visa_var_xe_etikett = 2,
                               #diagram_liggande = TRUE,
                               #geom_position_stack = TRUE,
                               manual_color = farg_vektor,
                               manual_y_axis_title = if (relativt_flyttnetto) "procent" else "Inrikes flyttnetto (skillnad mellan inrikes in- och utflyttade)",
                               manual_x_axis_text_hjust = 1, 
                               manual_x_axis_text_vjust = 1,
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               diagram_facet = TRUE,
                               facet_grp = "ålder",
                               facet_scale = "fixed",
                               #facet_legend_bottom = if (kon_ett_varde) FALSE else TRUE,
                               facet_x_axis_storlek = 6,
                               skriv_till_diagramfil = skriv_diagramfil,
                               filnamn_diagram = diagram_filnamn,
                               output_mapp = utskriftsmapp()
  )
  

}  # slut funktion

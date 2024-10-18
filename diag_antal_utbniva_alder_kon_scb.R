
diag_antal_utbniva_alder_kon <- function(
    region_vekt = "20",                       # Val av region. Finns: kommunkoder, länskoder eller riket
    gruppera_namn = NA,                       # NA = gör ingenting, annars anges namn på gruppering som medskickade regioner ska grupperas till
    valt_kon = c("män", "kvinnor"),           #  Finns: "män och kvinnor", "män", "kvinnor"
    alder_koder =  c(as.character(20:64)),     # antingen "tot16-74" eller annat intervall, exempelvis c(as.character(25:64)), "*" ger alla år
    tid_koder = "9999",			                  # "*" = alla år eller månader, "9999" = senaste, finns: "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
    vald_utbniva = "förgymnasial utbildning",                       # "*" = alla. "förgymnasial utbildning"   "gymnasial utbildning"      "eftergymnasial utbildning" "utbildningsnivå saknas"
    alder_grupper = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),                       # NA = gör ingenting utan åldrar som de är, annars tex c(20, 25, 30, 35, 40, 45, 50, 55, 60), dvs. start i varje åldersgrupp
    diagram_capt = "Källa: Utbildningsregistret, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    excelfil_mapp = NA,
    excelfil_namn = "utbniva.xlsx",
    returnera_df_rmarkdown = FALSE,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = TRUE,
    diag_fargvekt = NA
) {
"*"
  # ======================================================================================================
  #
  # Diagram som visar utbildningsnivå bland inrikes och utrikes födda samt även kön. Tre utbildningsnivåer.
  # Skapat 17 okt 2024 av Peter.
  #
  # ======================================================================================================
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_utbniva_SCB.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  gg_list <- list()
  
  # om ingen output_mapp är medskickad och funktionen utskriftsmapp finns, använd den, annars sätt skriv_diagramfil till FALSE
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp() 
    } else {
      skriv_diagramfil <- FALSE
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("kon")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }

  utbniva_df <- hamta_data_utbniva(
  			region = region_vekt,			   
  			kon_klartext = valt_kon,			 
  			utbildningsniva_klartext = "*",
  			alder = alder_koder,
  			tid = tid_koder,
  			#long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  			#wide_om_en_contvar = FALSE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  			output_mapp = excelfil_mapp,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			filnamn = excelfil_namn,			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  			returnera_data = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  ) %>% 
    rename(utbildningsnivå_alla = utbildningsnivå,
           varde = Befolkning) %>%
    mutate(utbildningsnivå = case_when(
      str_detect(utbildningsnivå_alla, "eftergymnasial|forskar") ~ "eftergymnasial utbildning",
      str_detect(utbildningsnivå_alla, "förgymnasial utbildning") ~ "förgymnasial utbildning",
      str_detect(utbildningsnivå_alla, "gymnasial utbildning") ~ "gymnasial utbildning",
      str_detect(utbildningsnivå_alla, "saknas") ~ "utbildningsnivå saknas",
      TRUE ~ utbildningsnivå_alla # behåller ursprungligt värde om ingen matchning
    )) %>% 
    relocate(utbildningsnivå, .before = utbildningsnivå_alla)

  if (!"ålder" %in% names(utbniva_df)) utbniva_df <- utbniva_df %>% mutate(ålder = "16-74 år")
  
  if (!all(is.na(alder_grupper))) {
    utbniva_df <- utbniva_df %>% 
      mutate(ålder = skapa_aldersgrupper(ålder, alder_grupper))
  }
  
  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!all(is.na(gruppera_namn))) {
    utbniva_df <- utbniva_df %>% 
      group_by(across(-c(regionkod, region, varde))) %>% 
      summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") %>% 
      mutate(regionkod = "gg",
             region = gruppera_namn) %>% 
      relocate(region, .before = 1) %>% 
      relocate(regionkod, .before = region)
    
    region_vekt <- "gg"
  }
  
  if(returnera_df_rmarkdown == TRUE){
    assign("utbniva_kon_alder_df", utbniva_df, envir = .GlobalEnv)
  }
  
  if (vald_utbniva != "*") {
    utbniva_df <- utbniva_df %>% filter(utbildningsnivå %in% vald_utbniva)
  }
  
  skapa_diagram <- function(chart_df, skickad_regionkod, valt_ar) {
    
    chart_df <- chart_df %>% 
      filter(regionkod %in% skickad_regionkod,
             år %in% valt_ar)
    
    # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
    region_start <- unique(chart_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
    region_txt <- ar_alla_kommuner_i_ett_lan(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_start)
    region_txt <- ar_alla_lan_i_sverige(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_txt)
    regionfil_txt <- region_txt
    region_txt <- paste0(" i ", region_txt)
    regionkod_txt <- if (region_start == region_txt) unique(chart_df$regionkod) %>% paste0(collapse = "_") else region_txt
    
    ar_txt <- if (min(chart_df$år) == max(chart_df$år)) max(chart_df$år) else paste0(min(chart_df$år), "-", max(chart_df$år))
    utbniva_txt <- unique(chart_df$utbildningsnivå) %>% paste0(collapse = "_")
    utbniva_titel <- if (length(unique(chart_df$utbildningsnivå)) == 1) paste0(" med ", unique(chart_df$utbildningsnivå)) else ""
    
    diagramtitel <- glue("Invånare{region_txt}{utbniva_titel} år {ar_txt}")
    diagramfil <- glue("utbniva_{utbniva_txt}_kon_{regionfil_txt}_ar{ar_txt}.png") %>% str_replace_all(" ", "_")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
    			 skickad_x_var = "ålder",
    			 skickad_y_var = "varde",
    			 skickad_x_grupp = "kön",
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 diagram_liggande = TRUE,
    			 geom_position_stack = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_y_axis_title = "Antal personer",
    			 x_axis_lutning = 0,
    			 manual_color = diag_fargvekt,
    			 output_mapp = output_mapp,
    			 skriv_till_diagramfil = skriv_diagramfil,
    			 facet_legend_bottom = TRUE,
    			 diagram_facet = if (length(unique(chart_df$utbildningsnivå)) == 1) FALSE else TRUE,
    			 facet_grp = if (length(unique(chart_df$utbildningsnivå)) == 1) NA else "utbildningsnivå",
    			 facet_scale = "fixed",
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
    return(gg_list)
    
  } # slut skapa_diagram
  
    arglist <- list(reg = region_vekt, valt_ar = unique(utbniva_df$år))                               # skapa lista med de två variabler vi vill göra diagram med
    crossarg <- expand.grid(arglist)
    # dia_lista <- map2(crossarg$reg, crossarg$bakgr, crossarg$valt_kon, ~skapa_diagram(vald_reg = .x, vald_bakgrund = .y, valt_kon = .z)) %>% flatten()
    retur_list <- pmap(crossarg, ~skapa_diagram(chart_df = utbniva_df, skickad_regionkod = ..1, valt_ar = ..2)) %>% purrr::flatten()
    
  #   retur_list <- map(unique(region_vekt), ~ skapa_diagram(chart_df = integration_df,
  #                                                          skickad_regionkod = .x)) %>% purrr::flatten()
  # } else {
  #   retur_list <- skapa_diagram(chart_df = integration_df, 
  #                               skickad_regionkod = region_vekt)
  # }
  return(retur_list)
} # slut funktion

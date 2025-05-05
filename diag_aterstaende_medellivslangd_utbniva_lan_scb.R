
diag_aterstaende_medellivslangd_utbniva_lan_scb <- function(
    region_vekt = "20",
    ar_vekt = "*",                                      # "*" = alla år, "9999" = senaste år
    cont_var_klartext = "Antal återstående år",        # Är främst den relevanta variabeln för oss att använda i denna tabell
    vald_alder = "30 år",                                 # ålder för återstående medellivslängd, finns: #  Finns: "30 år", "31 år", "32 år" ... tom  "93 år", "94 år", "95+ år"
    diag_fargvektor = NA,                               # valbar färgvektor för diagrammet
    visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
    logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
    diagramtitel_tabort = FALSE,                     # TRUE om diagramtitel ska tas bort, FALSE om diagramtitel ska visas
    diagram_capt = "Källa: Demografisk analys - Befolkning, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    x_axel_stlk = 10.5,                              # storlek på x-axelns text
    y_axel_stlk = 12,                                # storlek på y-axelns text
    skriv_diagramfil = TRUE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
    excel_mapp = NA,                                   # mapp där excelfil ska sparas, NA = sparas ingen fil
    demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    utmapp = "G:/Samhällsanalys/API/Fran_R/utskrift/"
    ) {

  
  # =======================================================================================================================
  #
  # Hämta hem data med funktionen hamta_aterstaende_medellivslangd, skriv ut ett diagram. Defaultinställning är
  # återstående medellivslängd vid 30 års ålder för kvinnor respektive män i Dalarna. Det finns fler innehålls-
  # variabler, vilka primärt används för att beräkna dödsrisker och återstående medellivslängd.
  #
  # Länk till SCB-tabell där data hämtas: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0701/LivslUtbLan/
  #
  # =======================================================================================================================
    
# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_medellivslangd_lan_utbildningsniva_kon_alder_tid_LivslUtbLan_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  diag_fargvektor <- if (all(is.na(diag_fargvektor)) & exists("diagramfarger")) diagramfarger("rus_sex")[c(5,1,3)] else c("darkred", "yellow", "darkgreen")
  
  gg_list <- list()
  
  medellivslangd_df <- hamta_medellivslangd_lan_utbildningsniva_kon_alder_tid_scb(
  			region_vekt = region_vekt,			# Val av region.
  			utbildningsniva_klartext = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning"),			 #  Finns: "samtliga utbildningsnivåer", "förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning", "uppgift om utbildningsnivå saknas"
  			kon_klartext = c("män", "kvinnor"),			 #  Finns: "samtliga män och kvinnor", "män", "kvinnor"
  			alder_klartext = vald_alder,			 #  Finns: "30 år", "31 år", "32 år", "33 år", "34 år", "35 år", "36 år", "37 år", "38 år", "39 år", "40 år", "41 år", "42 år", "43 år", "44 år", "45 år", "46 år", "47 år", "48 år", "49 år", "50 år", "51 år", "52 år", "53 år", "54 år", "55 år", "56 år", "57 år", "58 år", "59 år", "60 år", "61 år", "62 år", "63 år", "64 år", "65 år", "66 år", "67 år", "68 år", "69 år", "70 år", "71 år", "72 år", "73 år", "74 år", "75 år", "76 år", "77 år", "78 år", "79 år", "80 år", "81 år", "82 år", "83 år", "84 år", "85 år", "86 år", "87 år", "88 år", "89 år", "90 år", "91 år", "92 år", "93 år", "94 år", "95+ år"
  			cont_klartext = cont_var_klartext,			 #  Finns: "Medelfolkmängd", "Antal döda", "Antal döda efter födelsedagen", "Dödstal per 1 000", "Dödsrisk per 1 000", "Kvarlevande av 100 000 i 30 års ålder", "Döda i livslängdstabell", "Tid i ålder", "Tid i ålder och däröver", "Antal återstående år"
  			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2012-2016", "2013-2017", "2014-2018", "2015-2019", "2016-2020", "2017-2021", "2018-2022"
  			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  			excel_filnamn = "medellivslangd.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och excel_mapp anges, 
  			output_mapp = excel_mapp,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  )
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(medellivslangd_df$region) %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE) %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(medellivslangd_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(medellivslangd_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) unique(medellivslangd_df$regionkod) %>% paste0(collapse = "_") else region_txt
  
  diagramtitel <- glue("Återstående medellivslängd vid {unique(medellivslangd_df$ålder)} års ålder{region_txt}")
  diagramfil <- glue("medellivslangd_aterstaende_vid_{unique(medellivslangd_df$ålder)}_alder_{regionfil_txt}_ar{min(medellivslangd_df$årsintervall)}_{max(medellivslangd_df$årsintervall)}.png") %>% str_replace_all("__", "_")
  
  chart_df <- medellivslangd_df %>% 
    mutate(utbildningsnivå = factor(utbildningsnivå, levels = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning")))
  
  gg_obj <- SkapaLinjeDiagram(skickad_df = chart_df,
  			 skickad_x_var = "årsintervall",
  			 skickad_y_var = cont_var_klartext,
  			 skickad_x_grupp = "utbildningsnivå",
  			 diagram_titel = if(diagramtitel_tabort) NULL else diagramtitel,
  			 diagram_capt = diagram_capt,
  			 y_axis_borjar_pa_noll = FALSE,
  			 filnamn_diagram = diagramfil,
  			 #manual_y_axis_title = "",
  			 manual_x_axis_title = "årsintervall",
  			 manual_color = diag_fargvektor,
  			 lagg_pa_logga = visa_logga_i_diagram,
  			 facet_x_axis_storlek = x_axel_stlk,
  			 facet_y_axis_storlek = y_axel_stlk,
  			 logga_path = logga_sokvag,
  			 output_mapp = utmapp,
  			 diagram_facet = TRUE,
  			 facet_grp = "kön",
  			 facet_scale = "fixed",
  			 facet_legend_bottom = TRUE,
  			 skriv_till_diagramfil = skriv_diagramfil
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
  return(gg_list)

}

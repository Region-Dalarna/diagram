
diag_nettoinkomst_kon_aldersgrupp_scb <- function(
    region_vekt = "20",
    alder_koder = c("16-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84"),			 #  Finns: "20-64", "16", "16-19", "16+", "17", "18", "19", "20", "20+", "20-24", "21", "22", "23", "24", "25", "25-29", "26", "27", "28", "29", "30", "30-34", "31", "32", "33", "34", "35", "35-39", "36", "37", "38", "39", "40", "40-44", "41", "42", "43", "44", "45", "45-49", "46", "47", "48", "49", "50", "50-54", "51", "52", "53", "54", "55", "55-59", "56", "57", "58", "59", "60", "60-64", "61", "62", "63", "64", "65", "65-69", "65+", "66", "67", "68", "69", "70", "70-74", "71", "72", "73", "74", "75", "75-79", "76", "77", "78", "79", "80", "80-84", "81", "82", "83", "84", "85", "85+", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100+"
    cont_klartext = "Medianinkomst, tkr",			 #  Finns: "Medelinkomst, tkr", "Medianinkomst, tkr", "Totalsumma, mnkr", "Antal personer"
    output_mapp,
    logga_i_diagram = NA,                      # ange sökväg och filnamn till en logga om man vill ha en med i diagrammet
    diag_fargvekt = NA,                        # färgvektor som används i diagrammet, om inte anges används R:s standardfärger
    ar_fokus = "9999",                         # vilket år väljs i diag
    visa_dataetiketter = FALSE,                # om man vill ha med dataetiketter i diagrammet 
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    skriv_diagramfil = TRUE,                   # skriv en diagrambildfil 
    diag_tidsserie_kon_facet_aldersgrupper = TRUE,       # diagram med tidsserie över medianinkomst per kön där åldersgrupper ligger som facets
    diag_tidsserie_kon_diff_facet_aldersgrupper = TRUE,  # diagram med tidsserie över kvinnors andel av mäns inkomster där åldersgrupper ligger som facets
    demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    diag_valt_ar_kon_diff_aldersgrupper = TRUE           # diagram med diff i medianinkomst mellan män och kvinnor för valt år, åldersgrupper på x-axeln
    ){

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/diff_medianinkomst_Dalarna_2022.png",
"https://region-dalarna.github.io/utskrivna_diagram/diff_medianinkomst_Dalarna_ar2000_2022.png",
"https://region-dalarna.github.io/utskrivna_diagram/nettoinkomst_Dalarna_ar2000_2022.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_nettoinkomst_region_kon_alder_tid_NetInk02_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("kon")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  gg_list <- list()
  
  nettoinkomst_df <- hamta_nettoinkomst_region_kon_alder_tid_scb(
  			region_vekt = region_vekt,			   # Val av region. Finns: "00", "01", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "03", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "04", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "05", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "06", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "07", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "08", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "09", "0980", "10", "1060", "1080", "1081", "1082", "1083", "12", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "13", "1315", "1380", "1381", "1382", "1383", "1384", "14", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "17", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "18", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "19", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "21", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "22", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "23", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "24", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "25", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"
  			kon_klartext = c("män", "kvinnor"),			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
  			cont_klartext = cont_klartext, 
  			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
  			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  			wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			excel_filnamn = "nettoinkomst.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  
  )
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(nettoinkomst_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(nettoinkomst_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(nettoinkomst_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) unique(nettoinkomst_df$regionkod) %>% paste0(collapse = "_") else region_txt
  
  if (diag_tidsserie_kon_facet_aldersgrupper) {
    # diagram för medianinkomst med åldersgrupper som facets
    diagramtitel <- glue("Nettoinkomst för boende i {region_txt} år {min(nettoinkomst_df$år)} - {max(nettoinkomst_df$år)}")
    diagramfil <- glue("nettoinkomst_{regionfil_txt}_ar{min(nettoinkomst_df$år)}_{max(nettoinkomst_df$år)}.png") %>% str_replace_all("__", "_")
    
    chart_df <- nettoinkomst_df
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
    			 skickad_x_var = "år",
    			 skickad_y_var = "Medianinkomst, tkr",
    			 skickad_x_grupp = "kön",
    			 x_axis_sort_value = FALSE,
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_x_axis_text_vjust = 1,
    			 manual_x_axis_text_hjust = 1,
    			 manual_y_axis_title = NA,
    			 manual_color = diagramfarger("kon"),
    			 output_mapp = output_mapp,
    			 diagram_facet = TRUE,
    			 facet_grp = "ålder",
    			 facet_scale = "fixed",
    			 facet_x_axis_storlek = 6
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
  } # slut test om diag_tidsserie_kon_facet_aldersgrupper är TRUE
  
  if (diag_tidsserie_kon_diff_facet_aldersgrupper) {
    
    # diagram över diff i medianinkomst mellan män och kvinnor, åldersgrupper som facet
    diagramtitel <- glue("Kvinnors andel av mäns nettoinkomst i {region_txt} år {min(nettoinkomst_df$år)} - {max(nettoinkomst_df$år)}")
    diagramfil <- glue("diff_medianinkomst_{regionfil_txt}_ar{min(nettoinkomst_df$år)}_{max(nettoinkomst_df$år)}.png") %>% str_replace_all("__", "_")
    
    chart_diff_df <- nettoinkomst_df %>%
      pivot_wider(names_from = kön, values_from = `Medianinkomst, tkr`) %>% 
      mutate(diff = (kvinnor / män)*100,
             diff_over = ifelse(diff > 100, diff - 100, 0),
             diff_under = ifelse(diff < 100, diff, 100)) %>% 
      pivot_longer(cols = c("diff_over", "diff_under"), names_to = "diff_typ", values_to = "diff_varde")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_diff_df,
                                 skickad_x_var = "år",
                                 skickad_y_var = "diff_varde",
                                 skickad_x_grupp = "diff_typ",
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 dataetiketter = visa_dataetiketter,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagramfarger("rus_sex"),
                                 manual_y_axis_title = "procent",
                                 output_mapp = output_mapp,
                                 diagram_facet = TRUE,
                                 facet_grp = "ålder",
                                 facet_scale = "fixed",
                                 facet_x_axis_storlek = 7
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
  } # slut test om diag_tidsserie_kon_diff_facet_aldersgrupper är TRUE
  
  
  if (diag_valt_ar_kon_diff_aldersgrupper){
    # diagram över diff i medianinkomst mellan män och kvinnor, bara för senaste år, ej facetdiagram
    diagramtitel <- glue("Kvinnors andel av mäns nettoinkomst i {region_txt} år {max(nettoinkomst_df$år)}")
    diagramfil <- glue("diff_medianinkomst_{regionfil_txt}_{max(nettoinkomst_df$år)}.png") %>% str_replace_all("__", "_")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_diff_df %>% filter(år == max(år)),
                                 skickad_x_var = "ålder",
                                 skickad_y_var = "diff_varde",
                                 skickad_x_grupp = "diff_typ",
                                 geom_position_stack = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 dataetiketter = visa_dataetiketter,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagramfarger("rus_sex")[c(2,1)],
                                 manual_y_axis_title = "procent",
                                 output_mapp = output_mapp,
                                 legend_tabort = TRUE
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
    } # slut test om diag_valt_ar_kon_diff_aldersgrupper är TRUE
  
  return(gg_list)
} # slut funktion

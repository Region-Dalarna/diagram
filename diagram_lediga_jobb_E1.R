
diagram_lediga_jobb_tid_sektor_E1 <- function(region_vekt = "20",			# Val av region. Finns: "00", "FA00", "FA01", "FA02", "FA03", "FA04", "FA05", "FA06", "FA07", "FA08", "FA09", "FA10", "0114", "0115", "0117", "FA11", "0120", "0123", "0125", "0126", "0127", "0128", "FA12", "0136", "0138", "0139", "FA13", "0140", "FA14", "FA15", "0160", "0162", "0163", "FA16", "FA17", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "FA18", "0191", "0192", "FA19", "FA20", "FA21", "FA22", "FA23", "FA24", "FA25", "FA26", "FA27", "FA28", "FA29", "0305", "FA30", "0319", "FA31", "FA32", "0330", "0331", "FA33", "FA34", "FA35", "0360", "FA36", "FA37", "0380", "0381", "0382", "FA38", "FA39", "FA40", "FA41", "0428", "FA42", "FA43", "FA44", "FA45", "0461", "FA46", "FA47", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "FA48", "FA49", "0509", "FA50", "0512", "0513", "FA51", "FA52", "FA53", "FA54", "FA55", "0560", "0561", "0562", "0563", "FA56", "FA57", "0580", "0581", "0582", "0583", "0584", "0586", "FA58", "FA59", "0604", "FA60", "0617", "01", "03", "0642", "0643", "04", "05", "0662", "0665", "06", "07", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "08", "09", "10", "12", "13", "14", "17", "18", "0760", "0761", "0763", "0764", "0765", "0767", "19", "20", "0780", "0781", "21", "22", "23", "24", "0821", "25", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "0980", "1060", "1080", "1081", "1082", "1083", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "1315", "1380", "1381", "1382", "1383", "1384", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584" 
                                                      output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                                      tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"
                                                      kon_klartext = "totalt", # Finns: "män", "kvinnor", "totalt"
                                                      sektor_klartext = c("offentlig sektor", "privat sektor"),			 #  Finns: "offentlig sektor", "privat sektor", "totalt", "hela ekonomin", "näringslivet och hushållens icke-vinstdrivande organisationer", "offentlig förvaltning"
                                                      cont_klartext = "Lediga jobb",            # Finns: 
                                                      kvartal_klartext = "9999",     # finns: 1, 2, 3, 4  "9999" senaste tillfängliga kvartal, NA = alla kvartal
                                                      spara_figur = TRUE, # Skall diagrammet sparas
                                                      returnera_data = FALSE, # Skall data returneras
                                                      returnera_figur = TRUE){
  
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse,
         glue)
  
  #source(here("Skript","hamta_lediga_jobb_region_sektor_tid_RegionIndE1K_RegionIndE1KN_scb.R"), encoding="UTF-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_lediga_jobb_region_sektor_tid_RegionIndE1K_RegionIndE1KN_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  output_mapp <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"
  visa_dataetiketter <- FALSE
  gg_list <- list()
  
  lediga_jobb_df <- hamta_lediga_jobb_region_sektor_tid_scb(
    region_vekt = region_vekt,			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
    sektor_klartext = sektor_klartext,			 #  Finns: "offentlig sektor", "privat sektor", "totalt", "hela ekonomin", "näringslivet och hushållens icke-vinstdrivande organisationer", "offentlig förvaltning"
    cont_klartext = cont_klartext,			 #  Finns: "Lediga jobb", "Felmarginal ±", "Lediga jobb, totalt", "Lediga jobb, totalt, osäkerhetsmarginal", "Lediga jobb med omgående tillträde", "Lediga jobb med omgående tillträde, osäkerhetsmargnial"
    tid_koder = tid_koder,			 # "*" = alla år eller månader, "9999" = senaste, finns: "2006K1", "2006K2", "2006K3", "2006K4", "2007K1", "2007K2", "2007K3", "2007K4", "2008K1", "2008K2", "2008K3", "2008K4", "2009K1", "2009K2", "2009K3", "2009K4", "2010K1", "2010K2", "2010K3", "2010K4", "2011K1", "2011K2", "2011K3", "2011K4", "2012K1", "2012K2", "2012K3", "2012K4", "2013K1", "2013K2", "2013K3", "2013K4", "2014K1", "2014K2", "2014K3", "2014K4", "2015K1", "2015K2", "2015K3", "2015K4", "2016K1", "2016K2", "2016K3", "2016K4", "2017K1", "2017K2", "2017K3", "2017K4", "2018K1", "2018K2", "2018K3", "2018K4", "2019K1", "2019K2", "2019K3", "2019K4", "2020K1", "2020K2", "2020K3", "2020K4", "2021K1", "2021K2", "2021K3", "2021K4", "2022K1", "2022K2", "2022K3", "2022K4", "2023K1", "2023K2", "2023K3", "2023K4", "2024K1", "2024K2"
    kvartal_klartext = kvartal_klartext,      # finns: 1, 2, 3, 4  "9999" senaste tillfängliga kvartal, NA = alla kvartal    det går att blanda siffror med "9999" om man vill ha senaste kvartal + något eller ett par till
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "lediga_jobb.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
    
  )
  
  if(returnera_data == TRUE){
    assign("lediga_jobb_E1_df", lediga_jobb_df, envir = .GlobalEnv)
  }
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(lediga_jobb_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(lediga_jobb_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(lediga_jobb_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) unique(lediga_jobb_df$regionkod) %>% paste0(collapse = "_") else region_txt
  
  kvartal_titel <- case_when(length(unique(lediga_jobb_df$kvartal_num)) > 1 ~ paste0("kvartal ", list_komma_och(unique(lediga_jobb_df$kvartal_num))),
                             unique(lediga_jobb_df$kvartal_num) == 1 ~ "första kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 2 ~ "andra kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 3 ~ "tredje kvartalet",
                             unique(lediga_jobb_df$kvartal_num) == 4 ~ "fjärde kvartalet",
                             TRUE ~ "")
  
  diagramtitel <- glue("Lediga jobb{region_txt} {kvartal_titel} varje år")
  #diagramfil <- glue("lediga_jobb_{regionfil_txt}_ar{min(lediga_jobb_df$kvartal)}_{max(lediga_jobb_df$kvartal)}.png") %>% str_replace_all("__", "_")
  diagramfil <- glue("lediga_jobb_{regionfil_txt}_E1_{first(lediga_jobb_df$ar)}_{last(lediga_jobb_df$ar)}_kvartal{unique(lediga_jobb_df$kvartal_num)}.png") %>% str_replace_all("__", "_")
  
  chart_df <- lediga_jobb_df
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
                               skickad_x_var = "ar" ,
                               skickad_y_var = if ("varde" %in% names(chart_df)) "varde" else "Lediga jobb",
                               skickad_x_grupp = if ("sektor" %in% names(chart_df) & length(unique(chart_df$sektor)) > 1) "sektor" else NA,
                               x_axis_sort_value = FALSE,
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               dataetiketter = visa_dataetiketter,
                               geom_position_stack = TRUE,
                               manual_y_axis_title = "",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = if ("sektor" %in% names(chart_df) & length(unique(chart_df$sektor)) > 1) diagramfarger("rus_sex") else diagramfarger("rus_sex")[1],
                               output_mapp = output_mapp,
                               diagram_facet = FALSE,
                               facet_grp = NA,
                               facet_scale = "free",
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  return(gg_list)
}

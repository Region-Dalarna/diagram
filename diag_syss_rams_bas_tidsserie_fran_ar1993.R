
diag_syss_rams_bas_tidssserie_fran_ar1993 <- function(
  region_vekt = "20",
  inrikesutrikes_klartext = "*",      #  NA = tas inte med i uttaget,  Finns: "inrikes födda", "utrikes födda", "inrikes och utrikes födda"
  kon_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "kvinnor", "män", "kvinnor och män"
  tid_koder = "*", 
  visa_dataetiketter = FALSE,
  diagram_capt = "Källa: RAMS och BAS i SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna\nBeskrivning: Det är ett tidsseriebrott år 2020 så jämförelser med år innan detta bör göras med viss försiktighet.",
  output_mapp = NA, 
  skriv_diagramfil = TRUE,      # TRUE skrivs till fil (output_mapp måste finnas) annars returneras enbart ett ggplot-objekt
  excelfil_mapp = NA,
  excel_filnamn = NA
  ) {
    
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_syss_rams_bas_fran_ar_1993_region_inrikesutrikes_kon_tid_RAMSForvInt03_RAMSForvInt04_RamsForvInt04N_ArRegArbStatus_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(utskriftsmapp())) {
        output_mapp <- utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }
  
  gg_list <- list()
  
  rams_df <- hamta_rams_bas_region_inrikesutrikes_kon_tid_scb(
  			region_vekt = region_vekt,			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "0305", "0319", "0330", "0360", "0380", "0381", "0382", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "0980", "1060", "1080", "1081", "1082", "1083", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "1315", "1380", "1381", "1382", "1383", "1384", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "1904", "1907", "1917", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584", "0331"
  			inrikesutrikes_klartext = inrikesutrikes_klartext,			 #  NA = tas inte med i uttaget,  Finns: "inrikes födda", "utrikes födda", "inrikes och utrikes födda"
  			kon_klartext = kon_klartext,			 #  NA = tas inte med i uttaget,  Finns: "kvinnor", "män", "kvinnor och män"
  			cont_klartext = "*",			 #  Finns: "Förvärvsintensitet , procent"
  			tid_koder = tid_koder,			 # "*" = alla år eller månader, "9999" = senaste, finns: "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003"
  			output_mapp = excelfil_mapp,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			excel_filnamn = excel_filnamn,			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  )
  
  chart_df <- rams_df %>% 
    mutate(region = region %>% skapa_kortnamn_lan()) 
  
  if ("kön" %in% names(chart_df)) chart_df <- chart_df %>% filter(kön != "kvinnor och män")
  if ("födelseregion" %in% names(chart_df)) chart_df <- chart_df %>% filter(födelseregion != "inrikes och utrikes födda")
  
  alder_txt <- unique(chart_df$ålder)
  # anpassa diagramtitel och diagramfilnamn utifrån om kön är med eller inte och om båda könen är med
  if (!is.na(kon_klartext)) {
    kon_filnamn <- "_kon_"
    kon_titel <- if (length(unique(chart_df$kön)) == 1) unique(chart_df$kön) else "invånare"
  } else kon_filnamn <- ""
  
  if (!is.na(inrikesutrikes_klartext)) {
    bakgr_filnamn <- "_inr_utr_"
    bakgr_titel <- if (length(unique(chart_df$födelsregion)) == 1) paste0(unique(chart_df$födelsregion), " ") else ""  
  }  else bakgr_filnamn <- ""
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(chart_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(chart_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionkod_txt <- if (region_start == region_txt) unique(chart_df$regionkod) %>% paste0(collapse = "_") else region_txt
  
  diagramtitel <- glue("Sysselsättningsgrad {bakgr_titel}{kon_titel} {alder_txt} i {region_txt} år {min(chart_df$år)}-{max(chart_df$år)}")
  diagramfil <- glue("syss{bakgr_filnamn}{kon_filnamn}{regionkod_txt}_ar{min(chart_df$år)}_{max(chart_df$år)}.png") %>% 
    str_replace_all("__", "_")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
  			 skickad_x_var = "år",
  			 skickad_y_var = "sysselsättningsgrad",
  			 skickad_x_grupp = case_when(!is.na(kon_klartext) ~ "kön",
  			                             !is.na(inrikesutrikes_klartext) ~ "födelseregion",
  			                             TRUE ~ NA),
  			 x_axis_sort_value = FALSE,
  			 diagram_titel = diagramtitel,
  			 diagram_capt = diagram_capt,
  			 stodlinjer_avrunda_fem = TRUE,
  			 filnamn_diagram = diagramfil,
  			 dataetiketter = visa_dataetiketter,
  			 manual_y_axis_title = "procent",
  			 manual_x_axis_text_vjust = 1,
  			 manual_x_axis_text_hjust = 1,
  			 manual_color = if (!is.na(kon_klartext)) diagramfarger("kon") else diagramfarger("rus_sex"),
  			 output_mapp = output_mapp,
  			 diagram_facet = if (!is.na(kon_klartext) & !is.na(inrikesutrikes_klartext)) TRUE else FALSE,
  			 facet_grp = if (!is.na(kon_klartext) & !is.na(inrikesutrikes_klartext)) "födelseregion" else NA,
  			 facet_scale = "fixed"
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
  return(gg_list)
  
} # slut funktion

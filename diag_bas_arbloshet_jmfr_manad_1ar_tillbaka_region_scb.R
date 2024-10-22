

diag_bas_syss_manad_jmfr_1ar_tillbaka_scb <- function(
    vald_region = NA,     # NA = jämförelse mellan länen, annars jämförs kommuner i de regioner vars länskoder man skickar med
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
    tid_koder = "9999",
    jamfor_antal_manader_bakat = 12,
    visa_dataetiketter = FALSE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {
  
# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/bas_arbloshet_jmfr_Sveriges län_juli_ar2023-2024.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue,
         readxl)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  
  gg_list <- list()
  
  hamta_region <- if (all(is.na(vald_region))) hamtaAllaLan(F) else vald_region
  
  bas_syss_df <- hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(
    region_vekt = hamta_region,			# Val av region. Finns: "00", "01", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "03", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "04", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "05", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "06", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "07", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "08", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "09", "0980", "10", "1060", "1080", "1081", "1082", "1083", "12", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "13", "1315", "1380", "1381", "1382", "1383", "1384", "14", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "17", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "18", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "19", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "21", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "22", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "23", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "24", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "25", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"
    kon_klartext = NA,			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "totalt"
    alder_klartext = "20-64 år",			 #  Finns: "Total", "företag inom jordbruk, skogsbruk och fiske", "tillverkningsindustri; gruvor och mineralutvinningsindustri", "företag inom energi och miljö", "byggindustri", "handel; serviceverkstäder för motorfordon och motorcyklar", "transport- och magasineringsföretag", "hotell och restauranger", "informations- och kommunikationsföretag", "kreditinstitut och försäkringsbolag m.m.", "fastighetsbolag och fastighetsförvaltare", "företag inom juridik, ekonomi, vetenskap och teknik; företag inom uthyrning, fastighetsservice, resetjänster och andra stödtjänster", "civila myndigheter och försvaret", "utbildningsväsendet", "enheter för vård och omsorg, socialtjänst", "enheter för kultur, nöje och fritid; andra serviceföretag m.m.", "uppgift saknas"
    fodelseregion_klartext = "*",			#  NA = tas inte med i uttaget,  Finns: "inrikes född", "utrikes född", "totalt"
    cont_klartext = "arbetslöshet",			 #  Finns: "sysselsatta efter arbetsställets belägenhet", "sysselsatta efter bostadens belägenhet"
    tid_koder = tid_koder,			 # "*" = alla år eller månader, "9999" = senaste, finns: "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12", "2024M01", "2024M02", "2024M03", "2024M04", "2024M05"
    jmfr_manad_antal_bakat = jamfor_antal_manader_bakat,          # hur många månader bakåt man vill gå för jämförelsen
    long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
    wide_om_en_contvar = TRUE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
    output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "bas_syss.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
    
  ) %>% 
    manader_bearbeta_scbtabeller() 
  
  manad_nu <- last(bas_syss_df$tid) %>% as.character()
  manad_da <- first(bas_syss_df$tid) %>% as.character()
  
  chart_df <- bas_syss_df %>%
    select(-c(år, månad, månad_år, år_månad)) %>% 
    pivot_wider(names_from = tid, values_from = arbetslöshet) %>% 
    mutate(diff = .[[manad_nu]] - .[[manad_da]]) 
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(bas_syss_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(bas_syss_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  #region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) unique(bas_syss_df$regionkod) %>% paste0(collapse = "_") else region_txt
  
  manad_txt <- bas_syss_df %>% 
    distinct(månad) %>% 
    dplyr::pull() %>% 
    as.character()
  
  ar_start <- bas_syss_df %>% 
    distinct(år) %>% 
    dplyr::pull() %>%
    first() %>% 
    as.character()
  
  ar_slut <- bas_syss_df %>% 
    distinct(år) %>% 
    dplyr::pull() %>%
    last() %>% 
    as.character()
  
  diagramtitel <- glue("Skillnad i arbetslöshet för invånare 20-64 år i {region_txt}\ni {manad_txt} år {ar_slut} jämfört med {manad_txt} år {ar_start}")
  diagramfil <- glue("bas_arbloshet_jmfr_{regionfil_txt}_{manad_txt}_ar{ar_start}-{ar_slut}.png")
  
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
                               skickad_x_var = "region",
                               skickad_y_var = "diff",
                               skickad_x_grupp = "födelseregion",
                               x_axis_sort_value = TRUE,
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
                               #diagram_liggande = TRUE,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               dataetiketter = visa_dataetiketter,
                               manual_y_axis_title = "procentenheter",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = diagramfarger("rus_sex"),
                               output_mapp = output_mapp,
                               diagram_facet = TRUE,
                               facet_sort = TRUE,
                               facet_grp = "födelseregion",
                               facet_scale = "free_x",
                               facet_legend_bottom = FALSE
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
  return(gg_list) 
}

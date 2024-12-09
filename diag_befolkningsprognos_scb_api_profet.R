
SkapaBefPrognosDiagram <- function(region_vekt = "20", 
                                   eget_regionnamn = NA,                 # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
                                   jmfrtid = 10,                         # antal år i jämförelsen, alltså hur många års sikt vi vill titta på beräknat från sista året med befolkningsstatistik, alltså ett år före första prognosår
                                   #JmfrFleraPrognoser = FALSE,           # TRUE om vi vill jämföra med äldre prognoser, FALSE om vi bara vill se den senaste prognosen
                                   # om man skickar med flera url:er så görs en jämförelse
                                   tabeller_url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",  
                                                                         
                                                                         # För att använda Profet-filer: "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
                                                                         #  c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                                                         #    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN21",
                                                                         #    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20"),         # url-adresser till tabellerna med befolkningsprognoser
                                   facet_variabel = NA,                  # "region" om man vill ha regionerna som facet, annars skrivs ett diagram ut per region
                                   facet_x_axis_storlek = 5,             # storlek på x-axeln i facet-diagram
                                   aldersgrupper_vektor = c(0, 20, 66, 80), # åldersgrupper som används i diagrammet. Första siffran är start på gruppen så c(0, 20, 65, 80) blir 0-19 år, 20-64 år, 65-79 år och 80+ år
                                   output_fold = NA,        # mapp på datorn som diagrammet skrivs till
                                   gruppera_namn = NA,                   # ange namn om medskickade regioner ska grupperas, annars NA (= grupperas inte)
                                   logga_path = NA,       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
                                   logga_storlek = 20,
                                   facet_scale = "free",
                                   ta_med_logga = TRUE,                 # TRUE om vi vill ha med logga, annars FALSE
                                   skapa_fil = TRUE,
                                   konsuppdelat = FALSE,                # data kommer könsuppdelat, har ingen lösning idag för att använda könsuppdelad data men den finns där om vi vill framöver
                                   utan_diagramtitel = FALSE,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                   anvand_senaste_befar = FALSE,        # TRUE om vi vill använda senaste tillgängliga år för befolkningsstatistik, annars används första tillgängliga befolkningsprognosår
                                   andel_istallet_for_antal = FALSE,    # om man vill ha procent istället för absolut antal, för skillnad mellan start- och slutår
                                   prognos_ar = "9999",                 # om vi vill ha en prognos för ett specfikt år, "9999" = senaste prognosår - detta gäller endast profet-data, när man skickar med url för SCB finns bara ett år per url
                                   stodlinjer_avrunda_fem = TRUE,
                                   spara_dataframe_till_global_environment = FALSE,          # om man vill spara en dataframe till global environment (kan vara bra när man gör rmarkdownrapporter tex)
                                   dataetiketter = FALSE,
                                   spara_excelfil = FALSE,
                                   farger_diagram = NA,
                                   diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna"
                                   ) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         writexl,
         #png,
         tidyverse)
  
  # Ladda skript som sköter regionuttaget och kommuner per region
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")
  
  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)
  
  # om det inte skickats med någon färgvektor så används färgvektorn "rus_sex" från funktionen diagramfärger
  if (all(is.na(farger_diagram))) {
    if (konsuppdelat) farger_diagram <- diagramfarger("kon") else farger_diagram <- diagramfarger("rus_sex")
  } 

  # hämta värde för output_fold om det inte skickats med
  if (all(is.na(output_fold))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_fold <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  # om könsuppdelat så får man bara skicka med en prognos, inte flera
  if (konsuppdelat & length(tabeller_url) > 1) stop("Om man skriver ut könsuppdelade diagram så kan endast en prognos användas. Korrigera parametern 'tabeller_url' så att den bara innehåller en url och inte flera.")
  
  
  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================
  
  senaste_ar_bef <- as.numeric(max(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy", "tid")))
  
  gg_list <- list() 
  
  # jämförelsetid går mot senaste prognosår så vi behöver justera jmfrtid
  prognos_jmfr_ar <- if (anvand_senaste_befar) {
    test <- str_replace(prognos_ar, max(prognos_ar), as.character(senaste_ar_bef))
    as.numeric(prognos_ar) + jmfrtid %>% as.character
    senaste_ar_bef + jmfrtid
    
  } else {
    paste0("+", jmfrtid)
  }
  
  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================
  
  befprogn_df <- hamta_befprognos_data(region_vekt = region_vekt,
                                       url_prognos_vektor = tabeller_url,
                                       kon_klartext = c("kvinnor", "män"),
                                       tid_vekt = prognos_jmfr_ar,                        # ny metod, funkar? gammal: paste0("+", jmfrtid), 
                                       cont_klartext = "Folkmängd",
                                       prognos_ar = prognos_ar           # prognos_ar funkar bara för profet-uttag (för uttag från SCB:s API styr url:en vilket år som hämtas men i Profet kan flera år hämtas med samma url om det finns data för flera år i mappen)
                                       ) 
  
  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- befprogn_df %>% 
    group_by(regionkod, region, kön, år, prognos_ar) %>% 
    #summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%             # gammal grammatik gäller inte sedan dplyr 1.1.0
    summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate(ålder = "totalt ålder")

  # Lägg på total_df som rad på dfmalar
  befprogn_df <- bind_rows(befprogn_df, total_df) 
  
  prognosar <- befprogn_df$prognos_ar %>% unique()                         # lägg prognosår(en) i en vektor
  startar <-  prognosar %>% as.numeric() %>%  "-"(1) %>% as.character()    # lägg startår(en) i en vektor
  slutar <- startar %>% as.numeric() %>% "+"(jmfrtid) %>% as.character()   # lägg slutår(en) i en vektor
  
  # =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================
  
  px_df_bef <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt,
                                                     kon_klartext = c("män", "kvinnor"),
                                                     alder_koder = "*",
                                                     cont_klartext = "Folkmängd", 
                                                     tid_koder = startar) %>% 
    mutate(prognos_ar = år %>% as.numeric %>% "+"(1) %>% as.character())

  # ================= lägg ihop och bearbeta prognos- och befolkningsdata ==================
  
  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- bind_rows(px_df_bef, befprogn_df) 
  
  suppressWarnings(progn_bef <- progn_bef %>%              # skippa felmeddelanden då det inte är någon fara (kan inte översätta NA-värden till numeric)
    mutate(aldernum = parse_number(ålder)))                # Skapa en numerisk åldersvariabel 
  
  # Lägg ihop i åldersgrupper
  progn_bef <- progn_bef %>% 
    mutate(aldergrp = skapa_aldersgrupper(aldernum, aldersgrupper_vektor),
           aldergrp = ifelse(is.na(aldergrp), "totalt", as.character(aldergrp)),
           aldernum = ifelse(is.na(aldernum), -1, aldernum),
           start_ar = prognos_ar %>% as.numeric() %>%  "-"(1) %>% as.character(),
           slut_ar = start_ar %>% as.numeric() %>% "+"(jmfrtid) %>% as.character(),
           ar_beskr = paste0("Förändring ", start_ar, "-", slut_ar, " (prognos våren ", 
                              prognos_ar, ")"),
           Folkmängd = round(Folkmängd))
  
  # skapa en vektor av åldrar som ligger i ordning och där totalt ligger sist
  sort_vekt <- progn_bef %>% 
    group_by(aldergrp) %>% 
    summarise(sort_num = min(aldernum, na.rm = TRUE), .groups = "drop") %>% 
    arrange(sort_num) %>% 
    dplyr::pull(aldergrp)
  
  # använd sorteringesvektorn ovan för att sortera åldergrupperna i vektorn i en factor-variabel
  progn_bef <- progn_bef %>% 
    mutate(aldergrp = factor(aldergrp, levels = sort_vekt))

  # om vi inte vill ha könsuppdelad data (finns inte stöd för det än) så tar vi bort det här
  if (!konsuppdelat) {
    progn_bef <- progn_bef %>% 
      group_by(across(-c(kön, Folkmängd))) %>% 
      summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) %>%
      ungroup()
  }
  
  # om vi vill gruppera ihop regionerna till en så gör vi det här (dvs. om gruppera_ihop = TRUE)
  if (!is.na(gruppera_namn)) {
    progn_bef <- progn_bef %>% 
    group_by(across(c(-region, - regionkod, -Folkmängd))) %>% 
    summarise(Folkmängd = sum(Folkmängd)) %>% 
    mutate(regionkod = "grp", region = gruppera_namn) %>% 
    ungroup()
    
    eget_regionnamn <- gruppera_namn
  }
  # beräkna skillnad mellan startår och slutår, både som antal och som andel    
  prognos_diff_df <- progn_bef %>% 
    group_by(across(-c(ålder, aldernum, Folkmängd))) %>% 
    summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) %>%
    ungroup() %>% 
    pivot_wider(names_from = år, values_from = Folkmängd) %>% 
    mutate(antal = (rowSums(select(., all_of(slutar)), na.rm = TRUE)) - (rowSums(select(., all_of(startar)), na.rm = TRUE)),              # beräkna antal av förändring i åldersgrupper
           andel = round(((rowSums(select(., all_of(slutar)), na.rm = TRUE)) - (rowSums(select(., all_of(startar)), na.rm = TRUE))) /     # beräkna andel av förändring i åldergrupper
                           (rowSums(select(., all_of(startar)), na.rm = TRUE)) * 100,1),
           aldergrp = factor(aldergrp, levels = sort_vekt))      # Gör om aldergrp till factor som vi lägger i den ordning vi vill plotta diagrammet
  
  if(spara_dataframe_till_global_environment) {
    assign("bef_progn_nms_df", prognos_diff_df, envir = .GlobalEnv)
  }
  
  # skriv ut själva diagrammen som ligger i en funktion
  if (!is.na(facet_variabel)) {

    gg_list <- skrivut_befprognos_diagram(skickad_df = prognos_diff_df, # %>%
                                            # filter(befforandr == "Folkmängd",
                                            #        år == startar | år == slutar),
                                   regionkoder = prognos_diff_df$regionkod %>% unique(),
                                   skickad_jmfrtid = jmfrtid,
                                   facet_var = facet_variabel,
                                   facet_scale = facet_scale,
                                   facet_x_axis_storlek = facet_x_axis_storlek,
                                   eget_regionnamn = eget_regionnamn,
                                   farger_diagram = farger_diagram,
                                   konsuppdelat = konsuppdelat,
                                   dataetiketter = dataetiketter,
                                   output_fold = output_fold,
                                   skapa_fil = skapa_fil,
                                   spara_excelfil = spara_excelfil,
                                   logga_storlek = logga_storlek, 
                                   ta_med_logga = ta_med_logga,
                                   logga_path = logga_path,
                                   stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                   diagram_capt = diagram_capt,
                                   utan_diagramtitel = utan_diagramtitel,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                   skickad_andel = andel_istallet_for_antal, 
                                   x_var = "aldergrp", 
                                   filnamn_typ = "befprogn_")
  } else {
    gg_list <- map(prognos_diff_df$regionkod %>% unique(), 
                         ~ skrivut_befprognos_diagram(skickad_df = prognos_diff_df,
                                         regionkoder = .x,
                                         skickad_jmfrtid = jmfrtid,
                                         facet_var = NA,
                                         facet_scale = NA, 
                                         facet_x_axis_storlek = facet_x_axis_storlek,
                                         eget_regionnamn = eget_regionnamn,
                                         farger_diagram = farger_diagram,
                                         konsuppdelat = konsuppdelat,
                                         dataetiketter = dataetiketter,
                                         output_fold = output_fold,
                                         skapa_fil = skapa_fil,
                                         spara_excelfil = spara_excelfil,
                                         logga_storlek = logga_storlek,
                                         ta_med_logga = ta_med_logga,
                                         logga_path = logga_path,
                                         stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                         diagram_capt = diagram_capt,
                                         utan_diagramtitel = utan_diagramtitel,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                         skickad_andel = andel_istallet_for_antal,
                                         x_var = "aldergrp",
                                         filnamn_typ = "befprogn_")
                         ) %>% purrr::flatten()
    
  }
  
  return(gg_list)
  
} # slut funktion


# ===================================== Gör diagram =====================================================

skrivut_befprognos_diagram <- function(skickad_df, 
                                       regionkoder, 
                                       skickad_jmfrtid, 
                                       facet_var, 
                                       facet_scale,
                                       facet_x_axis_storlek,
                                       skickad_andel, 
                                       konsuppdelat, 
                                       x_var, 
                                       y_lbl = "", 
                                       logga_storlek,
                                       ta_med_logga,
                                       logga_path,
                                       eget_regionnamn,
                                       stodlinjer_avrunda_fem,
                                       farger_diagram,
                                       dataetiketter,
                                       output_fold,
                                       utan_diagramtitel,
                                       skapa_fil, 
                                       spara_excelfil, 
                                       diagram_capt,
                                       filnamn_typ) {
  
  if (!is.na(facet_var)) {
    if (length(unique(skickad_df[[facet_var]])) > 1) skickad_facetinst <- TRUE
  } else skickad_facetinst <- FALSE
  
  region_txt <- hamtaregion_kod_namn(regionkoder)$region %>% skapa_kortnamn_lan() %>% list_komma_och()
  
  
  etiketter_txt <- if (dataetiketter) "_lbl" else ""
  # om alla regionkoder kommer från samma län så döps regiontexten om till <län>s kommuner 
  if (ar_alla_kommuner_i_ett_lan(regionkoder)) {
    lan_txt <- hamtaregion_kod_namn(str_sub(regionkoder[1], 1, 2))$region %>% skapa_kortnamn_lan()
    region_txt <- paste0(lan_txt, "s kommuner")
  }
  
  if (!is.na(eget_regionnamn)) region_txt <- eget_regionnamn              # om eget_regionnamn skickas med trumfar de alla andra inställningar
  # Om det bara är en befolkningsprognos som plottas så används inte legend (se längre ner) och information 
  # om vilka år som avses + när prognosen är ifrån läggs i diagramtiteln, är det fler prognoser som plottas
  # läggs inte den informationen i diagramtiteln utan istället i legenden. 
  y_lbl_diagram_titel <- if(y_lbl == "") "Befolkningsförändring" else y_lbl
  y_lbl_axel <- if(y_lbl == "") "förändring antal invånare" else y_lbl
  
  if(length(unique(skickad_df$prognos_ar)) < 2) {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " ",
                           skickad_df$start_ar %>% unique(),"-", skickad_df$slut_ar %>% unique(), "\n(enligt befolkningsprognos våren ", 
                           skickad_df$prognos_ar %>% unique(), ")")
  } else {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " på ", skickad_jmfrtid, 
                           " års sikt")
  }
  
  # gör grupper
  if (skickad_facetinst) ifelse(facet_scale == "free", pre_facet_scale <- "_free", pre_facet_scale <- "_fixed") else pre_facet_scale <- "" 
  enhet <- if(skickad_andel) "_andel" else "_antal"
  prognos_ar_txt <- skickad_df$prognos_ar %>% unique() %>% paste0(collapse = "_")
  filnamn_pre <- paste0(filnamn_typ, region_txt, "_", prognos_ar_txt, "_", skickad_jmfrtid, "ars_sikt", ifelse(y_lbl == "", "", paste0("_", y_lbl)), etiketter_txt, enhet, pre_facet_scale)
  if (konsuppdelat) filnamn_pre <- paste0(filnamn_pre, "_kon")
  filnamn <- paste0(filnamn_pre, ".png")
  
  # lägg till fokus på åldersgruppen totalt om det finns i datasetet
  if ("aldergrp" %in% names(skickad_df)) {
  chart_df <- skickad_df %>% 
    mutate(fokus = ifelse(aldergrp == "totalt", 1,0))
  } else chart_df <- skickad_df
  
  # korrigera fokus-variabeln om den finns, annars NA
  diagram_fokus_var <- if ("fokus" %in% names(chart_df)) {
  if(length(unique(chart_df$prognos_ar)) > 1) NA else "fokus"
  } else NA
  
  # välj om man ska ha flera grupper eller inte - antingen kön, flera prognosår eller inga alls
  vald_xgrupp <- case_when(konsuppdelat == TRUE ~ "kön",
                           length(unique(skickad_df$prognos_ar)) > 1 ~ "ar_beskr",
                           TRUE ~ NA)
  
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df %>% 
                                 filter(regionkod %in% regionkoder),
                               skickad_x_var = x_var,
                               skickad_y_var = ifelse(skickad_andel, "andel", "antal"),
                               skickad_x_grupp = vald_xgrupp,
                               diagram_titel = diagramtitel,
                               output_mapp = output_fold,
                               diagram_capt = diagram_capt,
                               diagram_facet = skickad_facetinst,
                               facet_grp = facet_var,
                               facet_scale = facet_scale,
                               facet_legend_bottom = if(!is.na(vald_xgrupp)) TRUE else skickad_facetinst,
                               stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                               x_var_fokus = if(konsuppdelat) NA else diagram_fokus_var,
                               manual_color = farger_diagram, # if (length(unique(skickad_df$prognos_ar)) > 1) farger_diagram else farger_diagram[1],
                               logga_scaling = logga_storlek,
                               manual_y_axis_title = ifelse(skickad_andel, "procent", y_lbl_axel),
                               x_axis_lutning = ifelse(skickad_facetinst, 45, 0),
                               manual_x_axis_text_vjust = ifelse(skickad_facetinst, 1, 0),
                               manual_x_axis_text_hjust = ifelse(skickad_facetinst, 1, 0.5),
                               facet_x_axis_storlek = facet_x_axis_storlek,
                               utan_diagramtitel = utan_diagramtitel,
                               lagg_pa_logga = ta_med_logga,
                               logga_path = logga_path,
                               dataetiketter = dataetiketter,
                               filnamn_diagram = filnamn,
                               skriv_till_diagramfil = skapa_fil)
  
  retur_list <- list(gg_obj)
  names(retur_list)[length(retur_list)] <- filnamn  %>% str_remove(".png")
  
  if (spara_excelfil) write_xlsx(skickad_df, paste0(output_fold, filnamn %>% str_replace(".png", ".xlsx")))
  
  return(retur_list)
  
} # slut funktion skapa_diagram



# ============ Skapa befolkningsprognosdiagram för befolkningsförändringar ================

SkapaBefPrognosDiagram_befforandr <- function(region_vekt = "20", 
                                              diag_forandr_tot = TRUE,              # diagram där den totala förändringen för respektive kategori (naturlig bef. tillväxt resp. inr och utr flyttnetto) visas på mellan startår och jämförelseår
                                              diag_forandr_per_ar = FALSE,          # diagram där förändring per år visas för respektive kategori (födda, döda, in- och utr in- resp utflyttning) - från startår till jmfr-år
                                              eget_regionnamn = NA,                 # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
                                              jmfrtid = 10,                         # antal år i jämförelsen, alltså hur många års sikt vi vill titta på beräknat från sista året med befolkningsstatistik, alltså ett år före första prognosår
                                              # om man skickar med flera url:er så görs en jämförelse
                                              tabeller_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",
                                              #  c("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                                              #    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN21",
                                              #    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20"),         # url-adresser till tabellerna med befolkningsprognoser
                                              facet_variabel = "region",                  # om TRUE och det finns flera regioner så läggs de som facet, annars skrivs ett diagram ut per region
                                              output_fold ="G:/Samhällsanalys/API/Fran_R/Utskrift/",        # mapp på datorn som diagrammet skrivs till
                                              gruppera_namn = NA,                   # ange namn om medskickade regioner ska grupperas, annars NA (= grupperas inte)
                                              logga_path = NA,       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
                                              logga_storlek = 20,
                                              facet_scale = "free",
                                              ta_med_logga = TRUE,                 # TRUE om vi vill ha med logga, annars FALSE
                                              skapa_fil = TRUE,
                                              konsuppdelat = FALSE,                # data kommer könsuppdelat, har ingen lösning idag för att använda könsuppdelad data men den finns där om vi vill framöver
                                              utan_diagramtitel = FALSE,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                              anvand_senaste_befar = FALSE,        # TRUE om vi vill använda senaste tillgängliga år för befolkningsstatistik, annars används första tillgängliga befolkningsprognosår
                                              andel_istallet_for_antal = FALSE,    # om man vill ha procent istället för absolut antal, för skillnad mellan start- och slutår
                                              dataetiketter = FALSE,
                                              spara_excelfil = FALSE,
                                              farger_diagram = NA,
                                              diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna"
                                              ) {
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         writexl,
         #png,
         tidyverse)
  
  # Ladda skript som sköter regionuttaget och kommuner per region
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")
  
  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)
  
  # om det inte skickats med någon färgvektor så används färgvektorn "rd_gron" från funktionen diagramfärger
  if (is.na(farger_diagram[1])) farger_diagram <- diagramfarger("rd_gron")
  
  
  #tid_period <- as.character((as.numeric(startar[url_progn])+1):as.numeric(malar[url_progn]))
  
  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================
  
  befprogn_df <- hamta_befprognos_data(region_vekt = region_vekt,
                                       url_prognos_vektor = tabeller_url,
                                       kon_klartext = c("kvinnor", "män"),
                                       tid_vekt = paste0("+", c(1:jmfrtid)),
                                       cont_klartext = c("Folkmängd", "Födda", "Döda", "Inrikes inflyttning", 
                                                         "Inrikes utflyttning", "Invandring", "Utvandring"),
                                       prognos_ar = "9999"           # prognos_ar funkar bara för profet-uttag (för uttag från SCB:s API styr url:en vilket år som hämtas men i Profet kan flera år hämtas med samma url om det finns data för flera år i mappen)
  ) 
  
  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- befprogn_df %>% 
    group_by(regionkod, region, kön, år, prognos_ar) %>% 
    summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>% 
    ungroup() %>% 
    mutate(ålder = "totalt ålder")
  
  # Lägg på total_df som rad på dfmalar
  progn_bef <- bind_rows(befprogn_df, total_df) 
  
  prognosar <- befprogn_df$prognos_ar %>% unique()                         # lägg prognosår(en) i en vektor
  startar <-  prognosar %>% as.numeric() %>%  "-"(1) %>% as.character()    # lägg startår(en) i en vektor
  slutar <- startar %>% as.numeric() %>% "+"(jmfrtid) %>% as.character()   # lägg slutår(en) i en vektor
  
  suppressWarnings(progn_bef <- progn_bef %>%              # skippa felmeddelanden då det inte är någon fara (kan inte översätta NA-värden till numeric)
                     mutate(aldernum = parse_number(ålder)))                # Skapa en numerisk åldersvariabel 
  
  # Lägg ihop i åldersgrupper
  progn_bef_alla <- progn_bef %>% 
    mutate(aldergrp = case_when(aldernum < 20 ~ "0-19 år", 
                                between(aldernum, 20, 64) ~ "20-64 år",
                                between(aldernum, 65, 79) ~ "65-79 år",
                                aldernum > 79 ~ "80+ år", 
                                TRUE ~ "totalt"),
           aldergrp = factor(aldergrp, levels = c("totalt", "0-19 år", "20-64 år", "65-79 år", "80+ år")),      # Gör om aldergrp till factor som vi lägger i den ordning vi vill plotta diagrammet
           start_ar = prognos_ar %>% as.numeric() %>%  "-"(1) %>% as.character(),
           slut_ar = start_ar %>% as.numeric() %>% "+"(jmfrtid) %>% as.character(),
           ar_beskr = paste0("Förändring ", start_ar, "-", slut_ar, " (prognos våren ", 
                             prognos_ar, ")"),
           Folkmängd = round(Folkmängd),
           `Naturlig befolkningstillväxt` = Födda - Döda %>% round(),
           `Flyttnetto, inrikes` = `Inrikes inflyttning` - `Inrikes utflyttning` %>% round(),
           `Flyttnetto, utrikes` = Invandring - Utvandring %>% round()) %>% 
    select(-aldernum) %>% 
    pivot_longer(where(is.numeric), names_to = "befforandr", values_to = "antal") %>%
    group_by(regionkod, region, kön, aldergrp, år, prognos_ar, befforandr) %>% 
    summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop")
    
  if (!konsuppdelat) {
    progn_bef_alla <- progn_bef_alla %>% 
      group_by(across(-c(kön, antal))) %>% 
      summarise(antal = sum(antal, na.rm = TRUE)) %>%
      ungroup()
  }
  
  # om vi vill gruppera ihop regionerna till en så gör vi det här (dvs. om gruppera_ihop = TRUE)
  if (!is.na(gruppera_namn)) progn_bef_alla <- progn_bef_alla %>% 
    group_by(across(c(-region, - regionkod, -antal))) %>% 
    summarise(antal = sum(antal, na.rm = TRUE)) %>% 
    mutate(regionkod = "grp", region = gruppera_namn) %>% 
    ungroup()
  
  # # beräkna skillnad mellan startår och slutår, både som antal och som andel    
  # 
  # prognos_diff_df <- progn_bef %>% 
  #   group_by(across(c(!where(is.numeric), -any_of(c("ålder", "kön"))))) %>%                           # gruppera på alla kolumner som inte är numeriska men ta bort ålder och kön
  #   summarise(across(c(where(is.numeric), -"aldernum"), ~ round(sum(.x, na.rm = TRUE), 0))) %>%       # summera alla numeriska kolumner och avrunda värdena till heltal också
  #   mutate(Folkmängd = sum(Folkmängd, na.rm = TRUE),
  #                    `Naturlig befolkningstillväxt` = sum(`Naturlig befolkningstillväxt`, na.rm = TRUE),
  #                    `Flyttnetto, inrikes` = sum(`Flyttnetto, inrikes`, na.rm = TRUE),
  #                    `Flyttnetto, utrikes` = sum(`Flyttnetto, utrikes`, na.rm = TRUE)) %>% 
  #   ungroup() %>% 
  #   pivot_longer(where(is.numeric), names_to = "befforandr", values_to = "antal") 
  #          
  # 
  # prognos_diff_test <- progn_bef_alla %>% 
  #   #select(any_of(c("prognos_ar", "regionkod", "region", "kön", "aldergrp", "befforandr"))) %>%
  #   group_by(regionkod, region, prognos_ar, befforandr) %>% 
  #   summarise(diff = sum(antal[år == slutar])- sum(antal[år == startar])) %>% 
  #   #summarise(diff = antal[år == startar] - antal[år == slutar]) %>% 
  #   ungroup()
  

  # ovan har vi om vi vill ta med åldersgrupper (eller kön så småningom), men det har vi inte anpasast för ännu
  
  if (diag_forandr_tot) {
    
    prognos_diff_chart <- prognos_diff_df %>% 
      mutate(befforandr = factor(befforandr, levels = c("Naturlig befolkningstillväxt",         # för att lägga dem i rätt ordning
                                                        "Flyttnetto, inrikes", "Flyttnetto, utrikes"))) %>% 
      filter(!is.na(befforandr)) %>% 
      group_by(regionkod, region, prognos_ar, start_ar, slut_ar, ar_beskr, befforandr) %>% 
      summarise(antal = sum(antal, na.rm = TRUE)) %>% 
      ungroup()
      
    
    # skriv ut själva diagrammen som ligger i en funktion
    if (!is.na(facet_variabel)) {
      
      gg_list <- skrivut_befprognos_diagram(skickad_df = progn_bef_alla %>% 
                                              mutate(ar_beskr = paste0("Prognos från år", prognos_ar)),
                                            regionkoder = progn_bef_alla$regionkod %>% unique(),
                                            skickad_jmfrtid = jmfrtid,
                                            facet_var = facet_variabel,
                                            facet_scale = facet_scale,
                                            facet_x_axis_storlek = facet_x_axis_storlek,
                                            eget_regionnamn = eget_regionnamn,
                                            farger_diagram = farger_diagram,
                                            dataetiketter = dataetiketter,
                                            output_fold = output_fold,
                                            skapa_fil = skapa_fil,
                                            spara_excelfil = spara_excelfil,
                                            logga_storlek = logga_storlek,
                                            ta_med_logga = ta_med_logga,
                                            logga_path = logga_path,
                                            stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                            diagram_capt = diagram_capt,
                                            utan_diagramtitel = utan_diagramtitel,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                            skickad_andel = andel_istallet_for_antal,  
                                            x_var = "år",
                                            filnamn_typ = "befprogn_beffor_")
    } else {
      gg_list <- map(prognos_diff_chart$regionkod %>% unique(), 
                     ~ skrivut_befprognos_diagram(skickad_df = prognos_diff_chart,
                                                  regionkoder = .x,
                                                  skickad_jmfrtid = jmfrtid,
                                                  facet_var = NA,
                                                  skickad_andel = andel_istallet_for_antal,
                                                  x_var = "befforandr",
                                                  filnamn_typ = "befprogn_beffor_")
      ) %>% purrr::flatten()
      
    } # slut if-sats för region_facet
    
  } # slut på if-sats om man vill skriva ut diag_forandr_tot
  
  
  if (diag_forandr_per_ar) {
    
    if (is.na(facet_variabel)) facet_variabel <- "befforandr"
    
    if (facet_variabel == "region") {

      gg_list <- map(prognos_diff_df$befforandr %>% unique(), 
                     ~ skrivut_befprognos_diagram(skickad_df = prognos_diff_df %>% 
                                                    filter(befforandr == .x,
                                                           aldergrp == "totalt") %>% 
                                                    complete(regionkod, region, år, prognos_ar, ar_beskr,
                                                             fill = list(antal = 0, andel = 0)),
                                                  regionkoder = prognos_diff_df$regionkod %>% unique(),
                                                  skickad_jmfrtid = jmfrtid,
                                                  facet_var = "region",
                                                  skickad_andel = andel_istallet_for_antal, 
                                                  x_var = "år",
                                                  y_lbl = .x,
                                                  filnamn_typ = "befprogn_beffor_per_ar_")) %>% purrr::flatten()
      
    } else {
      gg_list <- map(prognos_diff_df$regionkod %>% unique(), 
                     ~ skrivut_befprognos_diagram(skickad_df = prognos_diff_df %>% 
                                                    filter(regionkod == .x,
                                                           aldergrp == "totalt") %>% 
                                                    complete(regionkod, region, år, prognos_ar, ar_beskr,
                                                             befforandr, fill = list(antal = 0, andel = 0)),
                                                  regionkoder = .x,
                                                  skickad_jmfrtid = jmfrtid,
                                                  facet_var = facet_variabel,
                                                  skickad_andel = andel_istallet_for_antal,
                                                  x_var = "år",
                                                  filnamn_typ = "befprogn_beffor_per_ar_")) %>% purrr::flatten()
      
    } # slut if-sats för region_facet
 } # slut if-sats för diag_forandr_per_ar
  
  
  return(gg_list)

} # slut på funktionen

SkapaBefPrognosDiagram_InrUtrFodda <- function(aktlan = "20", 
                                               bara_lan = TRUE,                      # TRUE om bara län ska visas, FALSE för att visa länets kommuner
                                               AktuellRegion = NULL,                 # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
                                               jmfrtid = 10,                         # antal år i jämförelsen, alltså hur många års sikt vi vill titta på
                                               url_tabeller = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgRegFakN",         # url-adresser till tabellerna med befolkningsprognoser
                                               output_fold ="G:/Samhällsanalys/API/Fran_R/Utskrift/",        # mapp på datorn som diagrammet skrivs till
                                               logga_path = "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png",       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
                                               logga_storlek = 20,
                                               stodlinjer_avrunda_fem = TRUE,
                                               facet_scale = "free",
                                               ta_med_logga = TRUE,                 # TRUE om vi vill ha med logga, annars FALSE
                                               skapa_fil = TRUE,
                                               utan_diagramtitel = FALSE,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                               #anvand_senaste_befar = FALSE,        # TRUE om vi vill använda senaste tillgängliga år för befolkningsstatistik, annars används första tillgängliga befolkningsprognosår
                                               #andel_istallet_for_antal = FALSE,    # om man vill ha procent istället för absolut antal, för skillnad mellan start- och slutår
                                               dataetiketter = FALSE,
                                               farger_diagram = diagramfarger("rd_gron")[c(1,4)],
                                               diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna"
                                               ) {
    
  # Initierar variabler
  malar <- 0
  startar <- 0
  px_alla <- NULL
  
  # ================================================================================================================
  
  # Fyll regiontabell
  regdf <- hamtaregtab()
  
  # Här väljer vi om vi ska ta med riket och länet också
  # Om vi bara vill ha län och inte kommuner, lägg in en vektor här nedan
  if (bara_lan){
    location_aktRegion <- aktlan
  } else {
    location_aktRegion <- hamtakommuner(aktlan, tamedriket = FALSE, tamedlan = FALSE) 
  }
  
  # plocka ut vanligaste län i vektorn med alla regioner i 
  vanlan <- names(sort(table(substr(location_aktRegion,1,2)),decreasing=TRUE)[1])
  if (is.null(AktuellRegion)) AktuellRegion <- ifelse(length(location_aktRegion) 
                                                      > 1, paste0(regdf$region[regdf$regionkod
                                                                               == vanlan], "s kommuner"), regdf$region[regdf$regionkod
                                                                                                                       == aktlan])
  
  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================
  
  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================
  # om vi valt att jämföra flera prognoser loopas hela vektorn med url:er till tabellerna
  # annars kör vi bara första tabellen
  slutloop <- 1   # vi kör bara en tabell i denna   #ifelse(JmfrFleraPrognoser, length(url_tabeller), 1) 
  # först hämtar vi startåret för alla tabeller i vektorn, och skapar samtidigt målåret också
  for (url_progn in 1:slutloop) {
    # först plockar vi ut ett pyttelitet api-uttag för att få tidigaste år i prognos att skapa startår från
    px_small <- pxweb_get(url = url_tabeller[url_progn],query = list(
      Region = "20",ContentsCode = "*", Tid = "*")) 
    px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
    startar[url_progn] <- as.character(as.numeric(min(px_df_small$år)) - 1)     # startår är alltid ett år innan första året i prognosen
    malar[url_progn] <- as.character(as.numeric(startar[url_progn]) + jmfrtid)             # målår skapas genom att addera jämförelsetid till startåret
    
    
    # API-uttag för att ta ut målåret som är det vi behöver från prognosen
    px_uttag <- pxweb_get(url = url_tabeller[url_progn],
                          query = list(
                            Region = location_aktRegion,
                            InrikesUtrikes = c("13","23"),
                            Alder = '*',
                            ContentsCode = '000005RC',
                            Tid = malar[url_progn])) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    px_df <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    #px_df <- px_df %>% select(regionkod, region, ålder, år, Folkmängd)
    
    # bind ihop med tidigare
    px_alla <- rbind(px_df, px_alla)
    # slut på loop med alla url:er
  }
  
  # =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================
  
  url_adress <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101E/InrUtrFoddaRegAlKon"
  
  # Välj variabler
  px_uttag_bef <- pxweb_get(url = url_adress,
                            query = list(
                              Region = location_aktRegion,
                              Alder = '*',
                              Fodelseregion = c("09","11"),
                              ContentsCode = '*',
                              Tid = startar)) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df_bef <- as.data.frame(px_uttag_bef) %>% 
    cbind(regionkod = as.data.frame(px_uttag_bef, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  
  # konvergera begreppen i denna tabell med tabellen för befolkningsprognosen
  px_df_bef$födelseregion[px_df_bef$födelseregion == "Född i Sverige"] <- "inrikes födda"
  px_df_bef$födelseregion[px_df_bef$födelseregion == "Utrikes född"] <- "utrikes födda"
  px_df_bef <- px_df_bef %>% 
    rename(`inrikes/utrikes född` = födelseregion) %>% 
    relocate(`inrikes/utrikes född`, .after = region)

  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- rbind(px_df_bef, px_alla)
  
  # Skapa en numerisk åldersvariabel 
  progn_bef$aldernum <- suppressWarnings(parse_number(progn_bef$ålder))
  
  # Lägg ihop i åldersgrupper
  progn_bef$aldergrp <- ifelse(progn_bef$aldernum < 20, "0-19 år", NA)
  progn_bef$aldergrp <- ifelse(progn_bef$aldernum > 19 & progn_bef$aldernum < 65 , "20-64 år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$aldernum > 64 & progn_bef$aldernum < 80 , "65-79 år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$aldernum > 79, "80+ år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$ålder == "totalt ålder", "totalt", progn_bef$aldergrp)
  
  # Skapa df för startår 
  dfstartar <- progn_bef %>%
    filter(år %in% startar) %>% 
    group_by(år, regionkod, region, `inrikes/utrikes född`, aldergrp) %>% 
    summarize(antal = sum(Folkmängd))
  
  # Skapa df för målår
  dfmalar <- progn_bef %>% 
    filter(år %in% malar) %>% 
    group_by(år, regionkod, region, `inrikes/utrikes född`, aldergrp) %>% 
    summarize(antal = sum(Folkmängd))
  
  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- dfmalar %>% 
    group_by(år, region, `inrikes/utrikes född`) %>% 
    summarize(regionkod = first(regionkod),
              region = first(region),
              antal = sum(antal))
  # Lägg på total_df som rad på dfmalar
  dfmalar <- rbind(dfmalar, total_df) %>% 
    arrange(år)
  
  # döp åldersgruppen för totalbef till "totalt"
  dfmalar$aldergrp[is.na(dfmalar$aldergrp)] <- "totalt"
  
  # se till att sorteringen blir korrekt
  dfmalar <- dfmalar %>% arrange(år, regionkod, aldergrp)
  
  # och så skapar vi en rad med total folkmängd i dfstartar =====================================
  total_df_start <- dfstartar %>% 
    group_by(år, region, `inrikes/utrikes född`) %>% 
    summarize(regionkod = first(regionkod),
              region = first(region),
              antal = sum(antal))
  # Lägg på total_df som rad på dfmalar
  dfstartar <- rbind(dfstartar, total_df_start) %>% 
    arrange(år)
  
  # döp åldersgruppen för totalbef till "totalt"
  dfstartar$aldergrp[is.na(dfstartar$aldergrp)] <- "totalt"
  
  # se till att sorteringen blir korrekt
  dfstartar <- dfstartar %>% arrange(år, regionkod, aldergrp)
  
  # ====================================== Skapa diff-df ==================================================
  dfdiff <- dfmalar
  dfdiff$år <- paste0("Förändring ", dfstartar$år,"-", dfmalar$år, " (prognos våren ", 
                      as.numeric(dfstartar$år) +1, ")")
  dfdiff$antal <- dfmalar$antal - dfstartar$antal
  
  # Gör om aldergrp till factor som vi lägger i den ordning vi vill 
  # plotta diagrammet
  dfdiff$aldergrp <- factor(dfdiff$aldergrp, levels = 
                              c("totalt", "0-19 år",
                                "20-64 år", "65-79 år", "80+ år"))
  
  AktuellRegion <- ifelse(length(unique(dfdiff$region)) == 1, dfdiff$region[1], AktuellRegion)

  if (length(unique(dfdiff$region)) == 1) facet_installning <- FALSE else facet_installning <- TRUE
  
  
    # ===================================== Gör diagram =====================================================
  
  diagramtitel <- paste0("Befolkningsförändring i ", AktuellRegion, " ",
                           dfstartar$år[1],"-", dfmalar$år[1], "\n(enligt befolkningsprognos våren ", 
                           as.numeric(dfstartar$år[1]) +1, ")")
  
  # Om det bara är en prognos som används, kör mörkgrönt, annars både ljus- och mörkgrönt
  if (is.na(farger_diagram[1])) {
    stapelfarger <- c(rgb(155,187,89, maxColorValue = 255), rgb(79,98,40, maxColorValue = 255))  
  } else {
    stapelfarger <- farger_diagram
  }
  #jmfr_pre <- ifelse(JmfrFleraPrognoser == TRUE, "_jmfr", "")   # lägger till ett "_jmfr" efter namnet om flera prognoser jämförs så att man kan skilja dessa prognoser från de med en prognos i filnamnet
  #grp_pre <- ifelse(gruppera_ihop, "_grp", "_perRegion")
  
  
  # filnamn_pre <- paste0("Befolkningsförändring inr utr födda i ", AktuellRegion, " på ", jmfrtid, " års sikt")
  # filnamn <- paste0(filnamn_pre, ".png")
  
  filnamn_pre <- paste0("Befolkningsförändring i ", AktuellRegion, " inr_utr födda ", startar[1], " - ", malar[1])
  filnamn <- paste0(filnamn_pre, ".png")
  
  # avrunda till heltal
  dfdiff$antal <- round(dfdiff$antal)
  
  SkapaStapelDiagram(skickad_df = dfdiff,
                     skickad_x_var = "aldergrp",
                     skickad_y_var = "antal",
                     skickad_x_grupp = "inrikes/utrikes född",
                     diagram_titel = diagramtitel,
                     output_mapp = output_fold,
                     diagram_capt = diagram_capt,
                     diagram_facet = facet_installning,
                     facet_grp = "region",
                     facet_scale = facet_scale,
                     facet_legend_bottom = facet_installning,
                     manual_color = stapelfarger,
                     stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                     #legend_titel = "test",
                     manual_y_axis_title = "förändring antal invånare",
                     x_axis_lutning = 0,
                     manual_x_axis_text_vjust = 1,
                     manual_x_axis_text_hjust = 1,
                     utan_diagramtitel = utan_diagramtitel,
                     skriv_till_diagramfil = skapa_fil,
                     lagg_pa_logga = ta_med_logga,
                     logga_path = logga_path,
                     dataetiketter = dataetiketter,
                     filnamn_diagram = filnamn)
  
# slut på funktionen
}


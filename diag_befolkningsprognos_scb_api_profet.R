
SkapaBefPrognosDiagram <- function(region_vekt = "20", 
                                   eget_regionnamn = NA,                 # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
                                   jmfrtid = 10,                         # antal år i jämförelsen, alltså hur många års sikt vi vill titta på beräknat från sista året med befolkningsstatistik, alltså ett år före första prognosår
                                   #JmfrFleraPrognoser = FALSE,           # TRUE om vi vill jämföra med äldre prognoser, FALSE om vi bara vill se den senaste prognosen
                                   # om man skickar med flera url:er så görs en jämförelse
                                   tabeller_url = "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/",  
                                                                         
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
                                   filformat = "png",                   # format på diagrammet
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
                                   #diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna"
                                   diagram_capt = "auto"
) {

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse). Anropas med fullt
  # namespace (dplyr::filter() osv.) i stället för library(). hamta_befprognos_scb_data.R (Profet-filer
  # eller SCB:s befolkningsprognos-API) sourcas fortfarande direkt - ska ersättas av ett anrop till vår
  # interna databas i ett senare steg, så den lämnas kvar oförändrad.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")

  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)

  if (stringr::str_sub(filformat, 1, 1) != ".") filformat <- paste0(".", filformat)

  # om det inte skickats med någon färgvektor så används färgvektorn "rus_sex" från funktionen diagramfärger
  if (all(is.na(farger_diagram))) {
    farger_diagram <- if (konsuppdelat) rddiagram::diagramfarger("kon") else rddiagram::diagramfarger("rus_sex")
  }

  # hämta värde för output_fold om det inte skickats med
  if (all(is.na(output_fold))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_fold <- rdverktyg::utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }

  # om mappen för datafiler från Profet eller Hallands befolkningsprognosskript inte finns så
  # används SCB:s API istället
  if (any(tabeller_url == "G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/") &
    !dir.exists("G:/Samhällsanalys/Statistik/Befolkningsprognoser/Profet/datafiler/")) {
    tabeller_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
  }

  # om könsuppdelat så får man bara skicka med en prognos, inte flera
  if (konsuppdelat & length(tabeller_url) > 1) stop("Om man skriver ut könsuppdelade diagram så kan endast en prognos användas. Korrigera parametern 'tabeller_url' så att den bara innehåller en url och inte flera.")


  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================

  # Senaste år med befolkningsstatistik hämtas nu direkt via v2-motsvarigheten till BE0101A/BefolkningNy
  # (samma tabellpar, TAB638 + TAB5557, som används för px_df_bef längre ner). SCB bytte metod (CKM,
  # röjandekontroll) för nya årgångar från och med 2025, så det verkliga senaste året finns numera i
  # CKM-tabellen (TAB5557), inte i historiktabellen (TAB638) - båda kollas här.
  senaste_ar_bef <- max(as.numeric(c(
    pxweb2r::pxweb2_get_values("TAB638", "Tid", quiet = TRUE)$code,
    pxweb2r::pxweb2_get_values("TAB5557", "Tid", quiet = TRUE)$code
  )))

  gg_list <- list()

  # jämförelsetid går mot senaste prognosår så vi behöver justera jmfrtid
  # (borttaget: två rader döda beräkningar som varken tilldelades någon variabel eller påverkade
  # resultatet - en test <- str_replace(...) som aldrig lästes, och en as.numeric(...) %>% as.character
  # som pga %>%:s låga precedens aldrig blev vad den ser ut att vara och heller aldrig sparades någonstans.)
  # jmfrtid räknas från "startår" (sista faktiska befolkningsstatistikåret, prognosår - 1, se
  # startar/slutar nedan) - inte från själva prognosåret. Efter bugfixen i hamta_befprognos_scb_data.R
  # (se den commiten) betyder "+N" numera "prognosår + N" (tidigare "prognosår - 1 + N"), så vi ber om
  # jmfrtid - 1 här för att fortfarande landa på samma slutår (startår + jmfrtid) som innan fixet.
  prognos_jmfr_ar <- if (anvand_senaste_befar) {
    senaste_ar_bef + jmfrtid
  } else {
    paste0("+", jmfrtid - 1)
  }

  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================

  if (length(region_vekt[region_vekt != "00"]) > 0) {
    befprogn_reg_df <- hamta_befprognos_data(region_vekt = region_vekt[region_vekt != "00"],
                                             url_prognos_vektor = tabeller_url,
                                             kon_klartext = c("kvinnor", "män"),
                                             tid_vekt = prognos_jmfr_ar,
                                             cont_klartext = "Folkmängd",
                                             prognos_ar = prognos_ar           # prognos_ar funkar bara för profet-uttag (för uttag från SCB:s API styr url:en vilket år som hämtas men i Profet kan flera år hämtas med samma url om det finns data för flera år i mappen)
    )
  } else befprogn_reg_df <- NULL

  if ("00" %in% region_vekt) {
    # Riket-uttaget (BefolkprognRevNb) är inte migrerat till pxweb2r ännu - hamta_befprogn_riket_...
    # ligger kvar på v1/pxweb och sourcas här tillsammans med func_API.R, som både den och raden nedan
    # (hamta_giltiga_varden_fran_tabell) behöver. Görs bara när riket faktiskt efterfrågas.
    source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_BefolkprognRevNb_scb.R")
    hamta_prognos_ar <- min(hamta_giltiga_varden_fran_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401A/BefolkprognRevNb", "tid"))

    befprogn_riket_df <- hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb(
      tid_koder = (senaste_ar_bef+jmfrtid)) |>
      dplyr::mutate(regionkod = "00",
             region = "Sverige",
             prognos_ar = hamta_prognos_ar) |>
      dplyr::rename(Folkmängd = Antal)

    # Bugfix (confirmed genom test): if (region_vekt == "00") kraschade ("the condition has length > 1")
    # så fort region_vekt hade fler än ett element och "00" var ett av dem (t.ex. c("20", "00")) - en
    # jämförelse med längd > 1 skickad rakt in i if(). Kollar nu i stället om riket är den ENDA
    # efterfrågade regionen.
    tabeller_url <- if (length(region_vekt) == 1 && region_vekt == "00") "api.scb.se" else c(tabeller_url, "api.scb.se")
  } else befprogn_riket_df <- NULL

  befprogn_df <- list(befprogn_reg_df, befprogn_riket_df) |>
    purrr::compact() |>              # tar bort NULL-element
    dplyr::bind_rows()

  # hantera diagram_capt
  if (diagram_capt == "auto") {
    diagram_capt <- dplyr::case_when(
      any(stringr::str_detect(tabeller_url, "api.scb.se")) & any(stringr::str_detect(tabeller_url, "Profet/datafiler")) ~
        "Källa: SCB:s befolkningsprognos och Region Dalarnas egna befolkningsprognos, bearbetning av Samhällsanalys, Region Dalarna\nI Region Dalarnas befolkningsprognos har prognosen för Ludvika kommun justerats för att fånga den expansion som pågår kring Hitachi. Detta scenario ligger något lägre än den prognos Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt men väsentligt högre än en ojusterad prognos.",
      any(stringr::str_detect(tabeller_url, "api.scb.se")) ~
        "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna",
      any(stringr::str_detect(tabeller_url, "Profet/datafiler")) & region_vekt %in% c("20", "2085") ~
        "Källa: Region Dalarnas egna befolkningsprognos, bearbetning av Samhällsanalys, Region Dalarna\nPrognosen för Ludvika kommun har justerats för att fånga den expansion som pågår kring Hitachi. Detta scenario ligger något lägre än den prognos Ludvika kommun\nsjälva tagit fram i deras scenario med medelstark tillväxt men väsentligt högre än en ojusterad prognos.",
      any(stringr::str_detect(tabeller_url, "Profet/datafiler")) ~
        "Källa: Region Dalarnas egna befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna"
    )
  }

  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- befprogn_df |>
    dplyr::group_by(regionkod, region, kön, år, prognos_ar) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    dplyr::mutate(ålder = "totalt ålder")

  # Lägg på total_df som rad på dfmalar
  befprogn_df <- dplyr::bind_rows(befprogn_df, total_df)

  prognosar <- unique(befprogn_df$prognos_ar)                              # lägg prognosår(en) i en vektor
  startar <- as.character(as.numeric(prognosar) - 1)                       # lägg startår(en) i en vektor
  slutar <- as.character(as.numeric(startar) + jmfrtid)                    # lägg slutår(en) i en vektor

  # =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================

  # Samma två v2-tabeller som senaste_ar_bef ovan (TAB638 = historik, TAB5557 = CKM). Civilstånd saknar
  # en riktig "totalt"-kod i TAB638 (bara fyra individuella civilstånd) - alla fyra hämtas explicit och
  # summeras ihop, eftersom det här skriptet aldrig vill dela upp på civilstånd. Ålder hämtas som alla
  # individuella åldrar PLUS en explicit "totalt"-kod (motsvarande alder_koder = "*" i originalet, som
  # gav både individuella åldrar och en total-rad från SCB). CKM-tabellen (TAB5557) dubblerar annars
  # "totalt"-etiketten över fyra koder (TotSA/TOT1/TOT10/TOT5) - "TotSA" är den kod originalskriptets
  # egen hamta_bef_folkmangd_alder_kon_ar_scb.R bytte till (str_replace("totalt", "TotSA")).
  hamta_individuella_aldrar <- function(table_id) {
    pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE) |>
      dplyr::filter(grepl("^[0-9]+\\+? år$", label)) |>
      dplyr::filter(!duplicated(label)) |>
      dplyr::pull(code)
  }
  civilstand_hamta <- c("ogifta", "gifta", "skilda", "änkor/änklingar")

  px_df_bef_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB638",
    query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                 Alder = c(hamta_individuella_aldrar("TAB638"), "tot"), Kon = c("män", "kvinnor"),
                 ContentsCode = "Folkmängd", Tid = startar),
    on_all_values_invalid = "null", quiet = TRUE)
  px_df_bef_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB5557",
    query = list(Region = region_vekt, Civilstand = civilstand_hamta,
                 Alder = c(hamta_individuella_aldrar("TAB5557"), "TotSA"), Kon = c("män", "kvinnor"),
                 ContentsCode = "Folkmängd", Tid = startar),
    on_all_values_invalid = "null", quiet = TRUE)

  px_df_bef <- dplyr::bind_rows(px_df_bef_historik, px_df_bef_ckm) |>
    dplyr::rename(regionkod = region_kod, Folkmängd = value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::mutate(ålder = ifelse(ålder == "totalt, samtliga åldrar", "totalt ålder", ålder),
           # Bugfix (confirmed genom test, pre-existing - inte en regression): SCB:s klartext för
           # regionkod "00" är "Riket", men hamta_befprogn_riket_inrikesutrikes_alder_kon_tid_scb()
           # hårdkodar "Sverige" för samma regionkod. De två namnen hamnade i separata grupper vid
           # group_by(region, ...) + pivot_wider() längre ner, vilket gav helt orimliga värden
           # (-100 %/Inf %) eftersom start- och slutårsdata för riket aldrig träffade samma rad.
           # Normaliserar till "Sverige" här, i linje med den redan existerande hårdkodningen.
           region = ifelse(regionkod == "00", "Sverige", region)) |>
    dplyr::group_by(dplyr::across(-c(civilstånd, Folkmängd))) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(prognos_ar = as.character(as.numeric(år) + 1))

  # ================= lägg ihop och bearbeta prognos- och befolkningsdata ==================

  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- dplyr::bind_rows(px_df_bef, befprogn_df)

  suppressWarnings(progn_bef <- dplyr::mutate(progn_bef, aldernum = readr::parse_number(ålder)))  # skippa felmeddelanden då det inte är någon fara (kan inte översätta NA-värden till numeric) - Skapa en numerisk åldersvariabel

  # Lägg ihop i åldersgrupper
  progn_bef <- progn_bef |>
    dplyr::mutate(aldergrp = rdverktyg::skapa_aldersgrupper(aldernum, aldersgrupper_vektor),
           aldergrp = ifelse(is.na(aldergrp), "totalt", as.character(aldergrp)),
           aldernum = ifelse(is.na(aldernum), -1, aldernum),
           start_ar = as.character(as.numeric(prognos_ar) - 1),
           slut_ar = as.character(as.numeric(start_ar) + jmfrtid),
           ar_beskr = paste0("Förändring ", start_ar, "-", slut_ar, " (prognos våren ",
                              prognos_ar, ")"),
           Folkmängd = round(Folkmängd))

  # skapa en vektor av åldrar som ligger i ordning och där totalt ligger sist
  sort_vekt <- progn_bef |>
    dplyr::group_by(aldergrp) |>
    dplyr::summarise(sort_num = min(aldernum, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(sort_num) |>
    dplyr::pull(aldergrp)

  # använd sorteringesvektorn ovan för att sortera åldergrupperna i vektorn i en factor-variabel
  progn_bef <- dplyr::mutate(progn_bef, aldergrp = factor(aldergrp, levels = sort_vekt))

  # om vi inte vill ha könsuppdelad data (finns inte stöd för det än) så tar vi bort det här
  if (!konsuppdelat) {
    progn_bef <- progn_bef |>
      dplyr::group_by(dplyr::across(-c(kön, Folkmängd))) |>
      dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  # om vi vill gruppera ihop regionerna till en så gör vi det här (dvs. om gruppera_ihop = TRUE)
  if (!is.na(gruppera_namn)) {
    progn_bef <- progn_bef |>
    dplyr::group_by(dplyr::across(c(-region, -regionkod, -Folkmängd))) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd)) |>
    dplyr::mutate(regionkod = "grp", region = gruppera_namn) |>
    dplyr::ungroup()

    eget_regionnamn <- gruppera_namn
  }
  # beräkna skillnad mellan startår och slutår, både som antal och som andel
  prognos_diff_df <- progn_bef |>
    dplyr::group_by(dplyr::across(-c(ålder, aldernum, Folkmängd))) |>
    dplyr::summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = år, values_from = Folkmängd) |>
    dplyr::mutate(antal = rowSums(dplyr::pick(dplyr::all_of(slutar)), na.rm = TRUE) - rowSums(dplyr::pick(dplyr::all_of(startar)), na.rm = TRUE),              # beräkna antal av förändring i åldersgrupper
           andel = round((rowSums(dplyr::pick(dplyr::all_of(slutar)), na.rm = TRUE) - rowSums(dplyr::pick(dplyr::all_of(startar)), na.rm = TRUE)) /     # beräkna andel av förändring i åldergrupper
                           rowSums(dplyr::pick(dplyr::all_of(startar)), na.rm = TRUE) * 100, 1),
           aldergrp = factor(aldergrp, levels = sort_vekt))      # Gör om aldergrp till factor som vi lägger i den ordning vi vill plotta diagrammet

  if(spara_dataframe_till_global_environment) {
    assign("bef_progn_nms_df", prognos_diff_df, envir = .GlobalEnv)
  }

  # skriv ut själva diagrammen som ligger i en funktion
  if (!is.na(facet_variabel)) {

    gg_list <- skrivut_befprognos_diagram(skickad_df = prognos_diff_df,
                                   regionkoder = unique(prognos_diff_df$regionkod),
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
                                   filformat = filformat,
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
    gg_list <- purrr::flatten(purrr::map(unique(prognos_diff_df$regionkod),
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
                                         filformat = filformat,
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
                         ))

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
                                       filformat,
                                       spara_excelfil, 
                                       diagram_capt,
                                       filnamn_typ) {
  
  # Bugfix (confirmed genom test): skickad_facetinst fick aldrig något värde alls när facet_var var
  # satt (inte NA) men datat bara innehöll ETT unikt värde för den variabeln - kraschade med "object
  # 'skickad_facetinst' not found" så fort SkapaBefPrognosDiagram_befforandr() (vars default för
  # facet_variabel är "region", till skillnad från SkapaBefPrognosDiagram() där den är NA) anropades
  # för en enda region.
  skickad_facetinst <- !is.na(facet_var) && length(unique(skickad_df[[facet_var]])) > 1

  region_txt <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(regionkoder)$region))


  etiketter_txt <- if (dataetiketter) "_lbl" else ""
  # om alla regionkoder kommer från samma län så döps regiontexten om till <län>s kommuner
  if (rdverktyg::ar_alla_kommuner_i_ett_lan(regionkoder)) {
    lan_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(stringr::str_sub(regionkoder[1], 1, 2))$region)
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
                           unique(skickad_df$start_ar),"-", unique(skickad_df$slut_ar), "\n(enligt befolkningsprognos våren ",
                           unique(skickad_df$prognos_ar), ")")
  } else {
    diagramtitel <- paste0(y_lbl_diagram_titel, " i ", region_txt, " på ", skickad_jmfrtid,
                           " års sikt")
  }

  # gör grupper
  if (skickad_facetinst) ifelse(facet_scale == "free", pre_facet_scale <- "_free", pre_facet_scale <- "_fixed") else pre_facet_scale <- ""
  enhet <- if(skickad_andel) "_andel" else "_antal"
  prognos_ar_txt <- paste0(unique(skickad_df$prognos_ar), collapse = "_")
  filnamn_pre <- paste0(filnamn_typ, region_txt, "_", prognos_ar_txt, "_", skickad_jmfrtid, "ars_sikt", ifelse(y_lbl == "", "", paste0("_", y_lbl)), etiketter_txt, enhet, pre_facet_scale)
  if (konsuppdelat) filnamn_pre <- paste0(filnamn_pre, "_kon")
  filnamn <- paste0(filnamn_pre, filformat)

  # lägg till fokus på åldersgruppen totalt om det finns i datasetet
  if ("aldergrp" %in% names(skickad_df)) {
  chart_df <- dplyr::mutate(skickad_df, fokus = ifelse(aldergrp == "totalt", 1, 0))
  } else chart_df <- skickad_df

  # korrigera fokus-variabeln om den finns, annars NA
  diagram_fokus_var <- if ("fokus" %in% names(chart_df)) {
  if(length(unique(chart_df$prognos_ar)) > 1) NA else "fokus"
  } else NA

  # välj om man ska ha flera grupper eller inte - antingen kön, flera prognosår eller inga alls
  vald_xgrupp <- dplyr::case_when(konsuppdelat == TRUE ~ "kön",
                           length(unique(skickad_df$prognos_ar)) > 1 ~ "ar_beskr",
                           TRUE ~ NA)


  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(chart_df, regionkod %in% regionkoder),
                               skickad_x_var = x_var,
                               skickad_y_var = ifelse(skickad_andel, "andel", "antal"),
                               skickad_x_grupp = vald_xgrupp,
                               diagram_titel = if (utan_diagramtitel) NULL else diagramtitel,
                               output_mapp = output_fold,
                               diagram_capt = diagram_capt,
                               facet_grp = if (skickad_facetinst) facet_var else NULL,
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
                               lagg_pa_logga = ta_med_logga,
                               logga_path = logga_path,
                               dataetiketter = dataetiketter,
                               filnamn_diagram = filnamn,
                               diagram_bildformat = stringr::str_sub(filformat, -3),
                               skriv_till_diagramfil = skapa_fil)

  retur_list <- list(gg_obj)
  names(retur_list)[length(retur_list)] <- stringr::str_remove(filnamn, filformat)

  if (spara_excelfil) writexl::write_xlsx(skickad_df, paste0(output_fold, stringr::str_replace(filnamn, filformat, ".xlsx")))

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
                                              facet_x_axis_storlek = 5,             # storlek på x-axeln i facet-diagram
                                              output_fold ="G:/Samhällsanalys/API/Fran_R/Utskrift/",        # mapp på datorn som diagrammet skrivs till
                                              gruppera_namn = NA,                   # ange namn om medskickade regioner ska grupperas, annars NA (= grupperas inte)
                                              logga_path = NA,       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
                                              logga_storlek = 20,
                                              facet_scale = "free",
                                              ta_med_logga = TRUE,                 # TRUE om vi vill ha med logga, annars FALSE
                                              skapa_fil = TRUE,
                                              filformat = "png",                   # format på diagrammet
                                              konsuppdelat = FALSE,                # data kommer könsuppdelat, har ingen lösning idag för att använda könsuppdelad data men den finns där om vi vill framöver
                                              utan_diagramtitel = FALSE,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                              anvand_senaste_befar = FALSE,        # TRUE om vi vill använda senaste tillgängliga år för befolkningsstatistik, annars används första tillgängliga befolkningsprognosår
                                              andel_istallet_for_antal = FALSE,    # om man vill ha procent istället för absolut antal, för skillnad mellan start- och slutår
                                              stodlinjer_avrunda_fem = TRUE,
                                              dataetiketter = FALSE,
                                              spara_excelfil = FALSE,
                                              farger_diagram = NA,
                                              diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Samhällsanalys, Region Dalarna"
                                              ) {

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse). Anropas med fullt
  # namespace (dplyr::filter() osv.) i stället för library(). hamta_befprognos_scb_data.R (Profet-filer
  # eller SCB:s befolkningsprognos-API) sourcas fortfarande direkt - ska ersättas av ett anrop till vår
  # interna databas i ett senare steg, så den lämnas kvar oförändrad.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  # dplyr/purrr/stringr/tidyr följer med som beroenden till rddiagram/rdverktyg.

  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprognos_scb_data.R")

  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)

  if (stringr::str_sub(filformat, 1, 1) != ".") filformat <- paste0(".", filformat)

  # om det inte skickats med någon färgvektor så används färgvektorn "rd_gron" från funktionen diagramfärger
  if (is.na(farger_diagram[1])) farger_diagram <- rddiagram::diagramfarger("rd_gron")


  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================

  # jmfrtid räknas från "startår" (prognosår - 1, se startar/slutar nedan). Efter bugfixen i
  # hamta_befprognos_scb_data.R betyder "+N" numera "prognosår + N" (tidigare "prognosår - 1 + N") - vi
  # ber därför om "+0" till "+(jmfrtid-1)" (i stället för "+1" till "+jmfrtid") för att fortfarande få
  # årsflödena från startår+1 till slutår (jmfrtid år totalt), precis som innan hamta_data-fixet.
  befprogn_df <- hamta_befprognos_data(region_vekt = region_vekt,
                                       url_prognos_vektor = tabeller_url,
                                       kon_klartext = c("kvinnor", "män"),
                                       tid_vekt = paste0("+", c(0:(jmfrtid - 1))),
                                       cont_klartext = c("Folkmängd", "Födda", "Döda", "Inrikes inflyttning",
                                                         "Inrikes utflyttning", "Invandring", "Utvandring"),
                                       prognos_ar = "9999"           # prognos_ar funkar bara för profet-uttag (för uttag från SCB:s API styr url:en vilket år som hämtas men i Profet kan flera år hämtas med samma url om det finns data för flera år i mappen)
  )

  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- befprogn_df |>
    dplyr::group_by(regionkod, region, kön, år, prognos_ar) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    dplyr::mutate(ålder = "totalt ålder")

  # Lägg på total_df som rad på dfmalar
  progn_bef <- dplyr::bind_rows(befprogn_df, total_df)

  prognosar <- unique(befprogn_df$prognos_ar)                              # lägg prognosår(en) i en vektor
  startar <- as.character(as.numeric(prognosar) - 1)                       # lägg startår(en) i en vektor
  slutar <- as.character(as.numeric(startar) + jmfrtid)                    # lägg slutår(en) i en vektor

  suppressWarnings(progn_bef <- dplyr::mutate(progn_bef, aldernum = readr::parse_number(ålder)))  # skippa felmeddelanden då det inte är någon fara (kan inte översätta NA-värden till numeric) - Skapa en numerisk åldersvariabel

  # Lägg ihop i åldersgrupper
  progn_bef_alla <- progn_bef |>
    dplyr::mutate(aldergrp = dplyr::case_when(aldernum < 20 ~ "0-19 år",
                                dplyr::between(aldernum, 20, 64) ~ "20-64 år",
                                dplyr::between(aldernum, 65, 79) ~ "65-79 år",
                                aldernum > 79 ~ "80+ år",
                                TRUE ~ "totalt"),
           aldergrp = factor(aldergrp, levels = c("totalt", "0-19 år", "20-64 år", "65-79 år", "80+ år")),      # Gör om aldergrp till factor som vi lägger i den ordning vi vill plotta diagrammet
           start_ar = as.character(as.numeric(prognos_ar) - 1),
           slut_ar = as.character(as.numeric(start_ar) + jmfrtid),
           ar_beskr = paste0("Förändring ", start_ar, "-", slut_ar, " (prognos våren ",
                             prognos_ar, ")"),
           Folkmängd = round(Folkmängd),
           `Naturlig befolkningstillväxt` = round(Födda - Döda),
           `Flyttnetto, inrikes` = round(`Inrikes inflyttning` - `Inrikes utflyttning`),
           `Flyttnetto, utrikes` = round(Invandring - Utvandring)) |>
    dplyr::select(-aldernum) |>
    tidyr::pivot_longer(dplyr::where(is.numeric), names_to = "befforandr", values_to = "antal") |>
    dplyr::group_by(regionkod, region, kön, aldergrp, år, prognos_ar, start_ar, slut_ar, ar_beskr, befforandr) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE), .groups = "drop")

  if (!konsuppdelat) {
    progn_bef_alla <- progn_bef_alla |>
      dplyr::group_by(dplyr::across(-c(kön, antal))) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  # om vi vill gruppera ihop regionerna till en så gör vi det här (dvs. om gruppera_ihop = TRUE)
  if (!is.na(gruppera_namn)) progn_bef_alla <- progn_bef_alla |>
    dplyr::group_by(dplyr::across(c(-region, -regionkod, -antal))) |>
    dplyr::summarise(antal = sum(antal, na.rm = TRUE)) |>
    dplyr::mutate(regionkod = "grp", region = gruppera_namn) |>
    dplyr::ungroup()

  # Rekonstruerad (var tidigare helt bortkommenterad - se git-historiken för de två övergivna
  # utkasten, prognos_diff_df/prognos_diff_test). progn_bef_alla innehåller redan alla kolumner som
  # behövs (regionkod, region, aldergrp, år, prognos_ar, start_ar, slut_ar, ar_beskr, befforandr,
  # antal) - vi behöver bara den samlade "totalt"-åldersgruppen (ingen åldersuppdelning görs här ännu,
  # se kommentaren nedan) och alla "förändrings"-kategorier utom stockvariabeln Folkmängd (som inte är
  # en flödesvariabel och därför inte kan summeras över flera år på ett meningsfullt sätt).
  # - diag_forandr_tot summerar denna över alla år (startår+1 till slutår) till EN totalsiffra per
  #   kategori och period (se group_by/summarise nedan, oförändrad sedan tidigare).
  # - diag_forandr_per_ar använder samma dataset rakt av, år för år (ingen summering över år).
  prognos_diff_df <- dplyr::filter(progn_bef_alla, aldergrp == "totalt", befforandr != "Folkmängd")

  # ovan har vi om vi vill ta med åldersgrupper (eller kön så småningom), men det har vi inte anpasast för ännu

  if (diag_forandr_tot) {

    prognos_diff_chart <- prognos_diff_df |>
      dplyr::mutate(befforandr = factor(befforandr, levels = c("Naturlig befolkningstillväxt",         # för att lägga dem i rätt ordning
                                                        "Flyttnetto, inrikes", "Flyttnetto, utrikes"))) |>
      dplyr::filter(!is.na(befforandr)) |>
      dplyr::group_by(regionkod, region, prognos_ar, start_ar, slut_ar, ar_beskr, befforandr) |>
      dplyr::summarise(antal = sum(antal, na.rm = TRUE)) |>
      dplyr::ungroup()


    # skriv ut själva diagrammen som ligger i en funktion
    if (!is.na(facet_variabel)) {

      # Bugfix (confirmed genom test): här skickades tidigare progn_bef_alla med (dvs. det ofiltrerade,
      # icke-summerade datasetet - fortfarande med Folkmängd kvar och en rad per år i stället för en
      # summerad totalsiffra per kategori och period). Det gav ett diagram i befolkningsmängdsskala
      # (~300 000) i stället för den avsedda totala förändringen (några tusen) så fort man körde med
      # fler än en region (facet_variabel default är "region") - med en enda region råkade
      # skalfelet inte synas lika tydligt men var ändå fel data. Ska vara prognos_diff_chart (samma
      # summerade data som används i grenen utan facet, några rader ned) och x_var "befforandr" (precis
      # som i den grenen) - regionen läggs som facet i stället för på x-axeln.
      gg_list <- skrivut_befprognos_diagram(skickad_df = prognos_diff_chart,
                                            regionkoder = unique(prognos_diff_chart$regionkod),
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
                                            filformat = filformat,
                                            spara_excelfil = spara_excelfil,
                                            logga_storlek = logga_storlek,
                                            ta_med_logga = ta_med_logga,
                                            logga_path = logga_path,
                                            stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                            diagram_capt = diagram_capt,
                                            utan_diagramtitel = utan_diagramtitel,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                            skickad_andel = andel_istallet_for_antal,
                                            x_var = "befforandr",
                                            filnamn_typ = "befprogn_beffor_")
    } else {
      gg_list <- purrr::flatten(purrr::map(unique(prognos_diff_chart$regionkod),
                     ~ skrivut_befprognos_diagram(skickad_df = prognos_diff_chart,
                                                  regionkoder = .x,
                                                  skickad_jmfrtid = jmfrtid,
                                                  facet_var = NA,
                                                  facet_scale = facet_scale,
                                                  facet_x_axis_storlek = facet_x_axis_storlek,
                                                  eget_regionnamn = eget_regionnamn,
                                                  farger_diagram = farger_diagram,
                                                  konsuppdelat = konsuppdelat,
                                                  dataetiketter = dataetiketter,
                                                  output_fold = output_fold,
                                                  skapa_fil = skapa_fil,
                                                  filformat = filformat,
                                                  spara_excelfil = spara_excelfil,
                                                  logga_storlek = logga_storlek,
                                                  ta_med_logga = ta_med_logga,
                                                  logga_path = logga_path,
                                                  stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                                  diagram_capt = diagram_capt,
                                                  utan_diagramtitel = utan_diagramtitel,
                                                  skickad_andel = andel_istallet_for_antal,
                                                  x_var = "befforandr",
                                                  filnamn_typ = "befprogn_beffor_")
      ))

    } # slut if-sats för region_facet

  } # slut på if-sats om man vill skriva ut diag_forandr_tot


  if (diag_forandr_per_ar) {

    if (is.na(facet_variabel)) facet_variabel <- "befforandr"

    if (facet_variabel == "region") {

      gg_list <- purrr::flatten(purrr::map(unique(prognos_diff_df$befforandr),
                     ~ skrivut_befprognos_diagram(skickad_df = tidyr::complete(
                                                    dplyr::filter(prognos_diff_df, befforandr == .x, aldergrp == "totalt"),
                                                    regionkod, region, år, prognos_ar, ar_beskr,
                                                    fill = list(antal = 0, andel = 0)),
                                                  regionkoder = unique(prognos_diff_df$regionkod),
                                                  skickad_jmfrtid = jmfrtid,
                                                  facet_var = "region",
                                                  facet_scale = facet_scale,
                                                  facet_x_axis_storlek = facet_x_axis_storlek,
                                                  eget_regionnamn = eget_regionnamn,
                                                  farger_diagram = farger_diagram,
                                                  konsuppdelat = konsuppdelat,
                                                  dataetiketter = dataetiketter,
                                                  output_fold = output_fold,
                                                  skapa_fil = skapa_fil,
                                                  filformat = filformat,
                                                  spara_excelfil = spara_excelfil,
                                                  logga_storlek = logga_storlek,
                                                  ta_med_logga = ta_med_logga,
                                                  logga_path = logga_path,
                                                  stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                                  diagram_capt = diagram_capt,
                                                  utan_diagramtitel = utan_diagramtitel,
                                                  skickad_andel = andel_istallet_for_antal,
                                                  x_var = "år",
                                                  y_lbl = .x,
                                                  filnamn_typ = "befprogn_beffor_per_ar_")))

    } else {
      gg_list <- purrr::flatten(purrr::map(unique(prognos_diff_df$regionkod),
                     ~ skrivut_befprognos_diagram(skickad_df = tidyr::complete(
                                                    dplyr::filter(prognos_diff_df, regionkod == .x, aldergrp == "totalt"),
                                                    regionkod, region, år, prognos_ar, ar_beskr,
                                                    befforandr, fill = list(antal = 0, andel = 0)),
                                                  regionkoder = .x,
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
                                                  filformat = filformat,
                                                  spara_excelfil = spara_excelfil,
                                                  logga_storlek = logga_storlek,
                                                  ta_med_logga = ta_med_logga,
                                                  logga_path = logga_path,
                                                  stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                                                  diagram_capt = diagram_capt,
                                                  utan_diagramtitel = utan_diagramtitel,
                                                  skickad_andel = andel_istallet_for_antal,
                                                  x_var = "år",
                                                  filnamn_typ = "befprogn_beffor_per_ar_")))

    } # slut if-sats för region_facet
 } # slut if-sats för diag_forandr_per_ar


  return(gg_list)

} # slut på funktionen

SkapaBefPrognosDiagram_InrUtrFodda <- function(aktlan = "20",
                                               bara_lan = TRUE,                      # TRUE om bara län ska visas, FALSE för att visa länets kommuner
                                               AktuellRegion = NULL,                 # Till diagramrubriken - NULL om namnet ska hämtas automatiskt
                                               jmfrtid = 10,                         # antal år i jämförelsen, alltså hur många års sikt vi vill titta på
                                               output_fold = NA,        # mapp på datorn som diagrammet skrivs till
                                               logga_path = NA,       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
                                               logga_storlek = 20,
                                               stodlinjer_avrunda_fem = TRUE,
                                               facet_scale = "free",
                                               ta_med_logga = TRUE,                 # TRUE om vi vill ha med logga, annars FALSE
                                               skapa_fil = TRUE,
                                               filformat = "png",                   # format på diagrammet
                                               utan_diagramtitel = FALSE,           # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                               dataetiketter = FALSE,
                                               spara_excelfil = FALSE,
                                               farger_diagram = NA,
                                               diagram_capt = "Källa: SCB:s befolkningsprognos och befolkningsstatistik\nBearbetning: Samhällsanalys, Region Dalarna"
                                               ) {

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget p_load(tidyverse). Anropas
  # med fullt namespace (dplyr::filter() osv.) i stället för library(). Till skillnad från de två andra
  # funktionerna i den här filen finns det ingen hamta_befprognos_scb_data.R-motsvarighet att sourca här
  # - v1-tabellerna som den här funktionen byggde på (BE0401A/BefProgRegFakN för prognosen,
  # BE0101E/InrUtrFoddaRegAlKon för befolkningsstatistiken, båda uppdelade på inrikes/utrikes födda)
  # hämtas i stället direkt via pxweb2r, precis som redan gjorts (och verifierats) i den snarlika
  # diag_bef_inrikes_utrikes_antal_forandring_prognos_IntRap.R:
  # - TAB6008 = v2-motsvarighet till BE0401A/BefProgRegFakN (rullande aktuell befolkningsframskrivning)
  # - TAB4823 = v2-motsvarighet till BE0101E/InrUtrFoddaRegAlKon, historik 2000-2024
  # - TAB6645 = samma uppgift men CKM (röjandekontroll), 2025- (SCB bytte metod för nya årgångar)
  # OBS: uppdelningen på inrikes/utrikes födda finns ännu inte alls i de lokala Profet-filerna (varken
  # för prognosen eller befolkningsstatistiken) - därför finns det ingen möjlighet att peka den här
  # funktionen mot en lokal mapp i stället för SCB:s API i dagsläget, till skillnad från
  # SkapaBefPrognosDiagram()/SkapaBefPrognosDiagram_befforandr().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  # dplyr/stringr/readr följer med som beroenden till rddiagram/rdverktyg.

  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)

  if (stringr::str_sub(filformat, 1, 1) != ".") filformat <- paste0(".", filformat)

  # om det inte skickats med någon färgvektor så används två nyanser av rd_gron (samma som tidigare
  # hårdkodade rgb-värden, vilka motsvarar just de två färgerna i den paletten)
  if (all(is.na(farger_diagram))) farger_diagram <- rddiagram::diagramfarger("rd_gron")[c(1, 4)]

  # hämta värde för output_fold om det inte skickats med
  if (all(is.na(output_fold))) {
    if (dir.exists(rdverktyg::utskriftsmapp())) {
      output_fold <- rdverktyg::utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output_fold ett värde.")
    }
  }

  # Fyll regiontabell
  regdf <- rdverktyg::hamtaregtab()

  # Här väljer vi om vi ska ta med riket och länet också
  # Om vi bara vill ha län och inte kommuner, lägg in en vektor här nedan
  if (bara_lan) {
    location_aktRegion <- aktlan
  } else {
    location_aktRegion <- rdverktyg::hamtakommuner(aktlan, tamedriket = FALSE, tamedlan = FALSE)
  }

  # plocka ut vanligaste län i vektorn med alla regioner i
  vanlan <- names(sort(table(substr(location_aktRegion, 1, 2)), decreasing = TRUE)[1])
  if (is.null(AktuellRegion)) {
    AktuellRegion <- if (length(location_aktRegion) > 1) {
      paste0(regdf$region[regdf$regionkod == vanlan], "s kommuner")
    } else {
      regdf$region[regdf$regionkod == aktlan]
    }
  }

  # Hjälpfunktion: välj en enda kod per verklig ettårsålder (samma mönster/motivering som i
  # diag_bef_inrikes_utrikes_antal_forandring_prognos_IntRap.R - CKM-tabellen har samma ålder
  # representerad under flera olika aggregeringshierarkier).
  hamta_individuella_aldrar <- function(table_id) {
    v <- pxweb2r::pxweb2_get_values(table_id, "Alder", quiet = TRUE)
    v <- v[grepl("^[0-9]+\\+? år$", v$label), ]
    v <- v[!duplicated(v$label), ]
    v$code
  }

  # =============== bestäm start- och målår utifrån prognosens faktiska tidsspann ================

  tab6008_ar <- as.numeric(pxweb2r::pxweb2_get_values("TAB6008", "Tid", quiet = TRUE)$code)
  startar <- min(tab6008_ar) - 1                # startår är alltid ett år innan första året i prognosen
  malar <- startar + jmfrtid                    # målår skapas genom att addera jämförelsetid till startåret

  # Felhantering: begripligt fel om jmfrtid pekar utanför prognosens faktiska tidsspann, i stället för
  # att pxweb2r-anropet nedan misslyckas långt senare med ett svårtolkat felmeddelande.
  if (!malar %in% tab6008_ar) {
    stop("SkapaBefPrognosDiagram_InrUtrFodda(): jmfrtid = ", jmfrtid, " ger målåret ", malar,
         ", vilket ligger utanför befolkningsprognosens (TAB6008) tillgängliga år (", min(tab6008_ar),
         "-", max(tab6008_ar), "). Minska jmfrtid.")
  }

  # ========== Hämta befolkningsprognos (uppdelat på inrikes/utrikes födda) för målåret ===========

  px_alla <- pxweb2r::pxweb2_get_data(
    table = "TAB6008",
    query = list(
      Region = location_aktRegion,
      InrikesUtrikes = c("inrikes födda", "utrikes födda"),
      Kon = "*",
      Alder = hamta_individuella_aldrar("TAB6008"),
      ContentsCode = "Antal",
      Tid = as.character(malar)
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, Antal = value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::relocate(regionkod, .before = region)

  # ===== hämta befolkningsstatistik (uppdelat på inrikes/utrikes födda) för startåret ============
  # välj rätt tabell (historik eller CKM) utifrån vilken av dem som faktiskt täcker startåret - med
  # ett begripligt fel om ingen av dem gör det, i stället för att pxweb2r-anropet misslyckas långt
  # senare med ett svårtolkat felmeddelande om ett ogiltigt Tid-värde.
  tab4823_ar <- as.numeric(pxweb2r::pxweb2_get_values("TAB4823", "Tid", quiet = TRUE)$code)
  tab6645_ar <- as.numeric(pxweb2r::pxweb2_get_values("TAB6645", "Tid", quiet = TRUE)$code)

  historik_table <- if (startar %in% tab4823_ar) {
    "TAB4823"
  } else if (startar %in% tab6645_ar) {
    "TAB6645"
  } else {
    stop("SkapaBefPrognosDiagram_InrUtrFodda(): befolkningsstatistik uppdelat på inrikes/utrikes födda ",
         "saknas för startåret ", startar, " - finns varken i TAB4823 (", min(tab4823_ar), "-",
         max(tab4823_ar), ") eller TAB6645 (", min(tab6645_ar), "-", max(tab6645_ar), "). Justera jmfrtid.")
  }

  px_df_bef <- pxweb2r::pxweb2_get_data(
    table = historik_table,
    query = list(
      Region = location_aktRegion,
      Alder = hamta_individuella_aldrar(historik_table),
      Kon = "*",
      Fodelseregion = c("född i Sverige", "utrikes född"),
      ContentsCode = "Antal",
      Tid = as.character(startar)
    ), quiet = TRUE) |>
    dplyr::rename(regionkod = region_kod, Antal = value) |>
    dplyr::select(-tabellinnehåll) |>
    dplyr::relocate(regionkod, .before = region)

  # konvergera begreppen i denna tabell med tabellen för befolkningsprognosen
  px_df_bef <- px_df_bef |>
    dplyr::mutate(`inrikes/utrikes född` = dplyr::case_when(
      födelseregion == "född i Sverige" ~ "inrikes födda",
      födelseregion == "utrikes född" ~ "utrikes födda",
      TRUE ~ födelseregion
    )) |>
    dplyr::select(-födelseregion) |>
    dplyr::relocate(`inrikes/utrikes född`, .after = region)

  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- dplyr::bind_rows(px_df_bef, px_alla)

  # Skapa en numerisk åldersvariabel
  progn_bef$aldernum <- suppressWarnings(readr::parse_number(progn_bef$ålder))

  # Lägg ihop i åldersgrupper
  progn_bef$aldergrp <- dplyr::case_when(progn_bef$aldernum < 20 ~ "0-19 år",
                                          dplyr::between(progn_bef$aldernum, 20, 64) ~ "20-64 år",
                                          dplyr::between(progn_bef$aldernum, 65, 79) ~ "65-79 år",
                                          progn_bef$aldernum > 79 ~ "80+ år",
                                          TRUE ~ NA_character_)

  # Skapa df för startår
  dfstartar <- progn_bef |>
    dplyr::filter(år == as.character(startar)) |>
    dplyr::group_by(år, regionkod, region, `inrikes/utrikes född`, aldergrp) |>
    dplyr::summarise(antal = sum(Antal, na.rm = TRUE), .groups = "drop")

  # Skapa df för målår
  dfmalar <- progn_bef |>
    dplyr::filter(år == as.character(malar)) |>
    dplyr::group_by(år, regionkod, region, `inrikes/utrikes född`, aldergrp) |>
    dplyr::summarise(antal = sum(Antal, na.rm = TRUE), .groups = "drop")

  # Här skapar vi en rad med total folkmängd i dfmalar (summan av de fyra åldersgrupperna ovan) =====
  total_df <- dfmalar |>
    dplyr::group_by(år, regionkod, region, `inrikes/utrikes född`) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop")
  dfmalar <- dplyr::bind_rows(dfmalar, total_df)
  dfmalar$aldergrp[is.na(dfmalar$aldergrp)] <- "totalt"

  # och så en rad med total folkmängd i dfstartar på samma sätt ====================================
  total_df_start <- dfstartar |>
    dplyr::group_by(år, regionkod, region, `inrikes/utrikes född`) |>
    dplyr::summarise(antal = sum(antal), .groups = "drop")
  dfstartar <- dplyr::bind_rows(dfstartar, total_df_start)
  dfstartar$aldergrp[is.na(dfstartar$aldergrp)] <- "totalt"

  # ====================================== Skapa diff-df ==================================================
  # Bugfix/optimering (confirmed genom kodgranskning): tidigare drogs dfstartar$antal från dfmalar$antal
  # rakt av, positionellt (dvs. rad 1 mot rad 1, rad 2 mot rad 2 osv.) utan någon join - det förutsätter
  # att båda dataframes råkar ha exakt samma rader i exakt samma ordning, annars blir differensen fel
  # helt tyst utan att något felmeddelande visas. Görs nu i stället som en explicit full_join på
  # regionkod/region/inrikes-utrikes-född/åldersgrupp, vilket dessutom ger oss ett naturligt sätt att
  # upptäcka och tydligt varna för databortfall (se felhanteringen nedan) - relevant eftersom
  # uppdelningen på inrikes/utrikes födda är relativt ny i flera av SCB:s tabeller och inte alltid finns
  # för alla kombinationer av region/år/ålder.
  dfdiff <- dplyr::full_join(
    dplyr::rename(dfstartar, antal_start = antal, ar_start = år),
    dplyr::rename(dfmalar, antal_mal = antal, ar_mal = år),
    by = c("regionkod", "region", "inrikes/utrikes född", "aldergrp")
  )

  # Felhantering: begripligt fel om data saknas för start- eller målåret för någon kombination av
  # region/inrikes-utrikes-född/åldersgrupp (dyker upp som NA efter full_join ovan), i stället för att
  # tyst räkna ut en felaktig differens eller krascha kryptiskt längre ned i diagramfunktionen.
  saknas <- dplyr::filter(dfdiff, is.na(antal_start) | is.na(antal_mal))
  if (nrow(saknas) > 0) {
    stop("SkapaBefPrognosDiagram_InrUtrFodda(): data uppdelat på inrikes/utrikes födda saknas för ",
         "start- eller målåret (", startar, "/", malar, ") för följande kombinationer av ",
         "region/inrikes-utrikes-född/åldersgrupp - kan inte beräkna förändringen:\n",
         paste(utils::capture.output(print(dplyr::select(saknas, regionkod, region, `inrikes/utrikes född`, aldergrp))), collapse = "\n"))
  }

  dfdiff <- dfdiff |>
    dplyr::mutate(år = paste0("Förändring ", ar_start, "-", ar_mal, " (prognos våren ", as.numeric(ar_start) + 1, ")"),
                  antal = round(antal_mal - antal_start),
                  aldergrp = factor(aldergrp, levels = c("totalt", "0-19 år", "20-64 år", "65-79 år", "80+ år"))) |>
    dplyr::select(regionkod, region, `inrikes/utrikes född`, aldergrp, år, antal)

  AktuellRegion <- if (length(unique(dfdiff$region)) == 1) dfdiff$region[1] else AktuellRegion

  facet_installning <- length(unique(dfdiff$region)) > 1

  # ===================================== Gör diagram =====================================================

  diagramtitel <- paste0("Befolkningsförändring i ", AktuellRegion, " ", startar, "-", malar,
                         "\n(enligt befolkningsprognos våren ", startar + 1, ")")

  filnamn <- paste0("Befolkningsförändring i ", AktuellRegion, " inr_utr födda ", startar, " - ", malar, filformat)

  gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dfdiff,
                     skickad_x_var = "aldergrp",
                     skickad_y_var = "antal",
                     skickad_x_grupp = "inrikes/utrikes född",
                     diagram_titel = if (utan_diagramtitel) NULL else diagramtitel,
                     output_mapp = output_fold,
                     diagram_capt = diagram_capt,
                     facet_grp = if (facet_installning) "region" else NULL,
                     facet_scale = facet_scale,
                     facet_legend_bottom = facet_installning,
                     manual_color = farger_diagram,
                     stodlinjer_avrunda_fem = stodlinjer_avrunda_fem,
                     manual_y_axis_title = "förändring antal invånare",
                     x_axis_lutning = 0,
                     manual_x_axis_text_vjust = 1,
                     manual_x_axis_text_hjust = 1,
                     logga_scaling = logga_storlek,
                     lagg_pa_logga = ta_med_logga,
                     logga_path = logga_path,
                     skriv_till_diagramfil = skapa_fil,
                     dataetiketter = dataetiketter,
                     diagram_bildformat = stringr::str_sub(filformat, -3),
                     filnamn_diagram = filnamn)

  if (spara_excelfil) writexl::write_xlsx(dfdiff, paste0(output_fold, stringr::str_replace(filnamn, filformat, ".xlsx")))

  retur_list <- list(gg_obj)
  names(retur_list)[1] <- stringr::str_remove(filnamn, filformat)
  return(retur_list)

# slut på funktionen
}


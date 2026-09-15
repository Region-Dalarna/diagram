diag_utbniva_tidserie_och_lansjmfr <- function(
                                       region_vekt = c("00", "20"),
                                       output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",
                                       diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                       skapa_fil = TRUE,
                                       gruppering_namn = NA,   # om man vill gruppera ihop medskickade regioner ger man denna parameter ett namn
                                       valt_ar = NA,
                                       diagramtitel_tabort = FALSE,
                                       diagram_capt_tabort = FALSE,
                                       ta_med_logga = TRUE,
                                       logga_sokvag = NA,
                                       visa_var_xe_etikett = NA,
                                       sverige_istallet_for_riket = TRUE,
                                       facet_x_axis_stlk = 8,
                                       region_lagg_forst = NA,
                                       diag_hogutb_over_tid = TRUE,
                                       diag_lagutb_over_tid = FALSE,
                                       diag_andel_alla_utbnivaer = TRUE,
                                       diag_andel_utbniva_jmfr_lan = FALSE,
                                       vald_utb_niva = "eftergymn",
                                       spara_dataset_excel = FALSE,
                                       excel_filnamn = NA){

  # =======================================================================================================
  #
  # Fyra diagram totalt. Data kommer från SCB: UF/UF0506/UF0506B/Utbildning, som i v2 motsvaras av TAB3981
  # ("Befolkning 16–74 år efter region, utbildningsnivå, ålder och kön. 1985-2025").
  #
  # Tre diagram över hur utbildningsnivån har förändrats över tid + ett diagram där andel eftergymnasialt
  # utbildade jämförs mellan länen för ett år
  #
  # 1. Andelen högutbildade i befolkningen 25-64 år från 1985 och framåt
  # 2. Andelen lågutbildade i befolkningen 25-64 år från 1985 och framåt
  # 3. Andelen per utbildningsnivå (förgymn, gymn, kortare eftergymn, längre eftergymn) år 1985, 1990, 2000,
  #    2010 och senaste år, eller annat valfritt år som man skickar med i valt_ar-parametern.
  # 4. Andel eftergymnasialt utbildade i befolkningen per kön 25-64 år för valt år, jämförelse mellan länen
  #
  # Parametrar:
  #
  # region_vekt       - fler region = facet-diagram med en facet per region
  # valt_ar           - om man vill ha något annat år än senaste år i diagram
  # region_lagg_forst - om man vill lägga någon eller några regioner först bland facetdiagrammen så skickar man
  #                     med dem här i den ordning man vill ha dem.
  # gruppering_namn   - NA om man vill lägga regioiner som facet-diagram, om ett namn skickas med så läggs regionerna ihop till en som döps till namnet som skickas med
  # skapa_fil         - om man vill skriva diagrammen till png-filer, annars returneras bara ggplot-objekt
  # output_mapp       - där png-filerna sparas
  # diagram_capt      - beskrivning av diagrammet
  # vald_utb_niva     - används i diagram 4, "eftergymn" är förvalt men finns också "hogutb", "gymn" och "forgymn"
  #
  # diag_hogutb_over_tid        - diagram 1 ovan, TRUE om man vill ha med det, annars FALSE
  # diag_lagutb_over_tid        - diagram 2 ovan, TRUE om man vill ha med det, annars FALSE
  # diag_andel_alla_utbnivaer   - diagram 3 ovan, TRUE om man vill ha med det, annars FALSE
  # diag_andel_utbniva_jmfr_lan - diagram 4 ovan, TRUE om man vill ha med det, annars FALSE
  #                utbildningsnivå i diagram fyra styrs med parametern "vald_utb_niva", "eftergymn" är förvalt
  #
  # spara_dataset_excel - TRUE om man vill spara dataseten bakom de diagram som skrivs ut till en
  #                       Excelfil, en flik per diagram. Fliknamnen byggs av samma namn som diagram-
  #                       filerna (utan ".png"), trunkerat till Excels gräns på 31 tecken.
  # excel_filnamn       - filnamn för Excelfilen (skrivs till output_mapp). NA = döps automatiskt
  #                       utifrån vald region.
  #
  # 2026-09-15 - Lagt till spara_dataset_excel/excel_filnamn för att kunna spara dataseten bakom
  #              diagrammen till en Excelfil (en flik per diagram).
  #
  # 2026-09-13 - Migrerad till pxweb2r/rddiagram/rdverktyg (fullt namespace, ingen source()/p_load()). Sedan
  #              v2-tabellen alltid ger en generisk "value"-kolumn (i st.f. att döpa kolumnen efter
  #              tabellinnehållets klartext som i v1) behövs inte längre 2025-01-08-fixet nedan som bytte
  #              namn på "befolkning" till "antal" - kolumnen heter nu alltid "Befolkning" direkt från
  #              hämtningen.
  #
  # 2025-01-23  - Lagt till möjligheten att ta bort diagram_capt
  #
  # 2025-01-22 - Lagt till möjligheten att visa var x:e etikett för diag hog respektive lag
  #
  # 2025-01-08 - Av någon anledning har SCB ändrat bytt namn på variabeln befolkning till antal. För att slippa ändra på för många ställen i skriptet har
  #              jag lagt till en rad som byter namn på variabeln befolkning till antal. /Jon
  #
  # 2024-04-12 - Har ändrat revideringen nedan så att man fritt kan välja vilken utbildningsnivå man vill jämföra mellan länen i diagram 4.
  #              Jag har ändrat i instruktionerna ovan så att det framgår hur man väljer. "eftergymn" är fortsatt förvald utbildningsnivå
  #              i länsjämförelsediagrammet. /Peter
  #
  # 2024-01-05 - Har lagt till ett val (minst_3_ar) som gör det möjligt att göra en länsjämförelse (diagram 4) även för personer med minst 3 års eftergymnasial utbildning
  #              Tidigare gick det enbart att göra denna jämförelse för alla typer av eftergymnasial utbildning. /Jon
  #
  # =======================================================================================================

  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.
  options(dplyr.summarise.inform = FALSE)

  gg_list <- list()       # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  dataset_list <- list()  # dataseten bakom diagrammen, ifyllt bara om spara_dataset_excel = TRUE

  # bygger ett giltigt och unikt Excel-fliknamn (max 31 tecken, inga otillåtna tecken) utifrån
  # diagramfilnamnet
  saklig_fliknamn <- function(namn, tagna_namn) {
    namn <- gsub("[\\\\/?*\\[\\]:]", "_", namn, perl = TRUE)  # otillåtna tecken i Excel-fliknamn
    namn <- substr(namn, 1, 31)
    unikt_namn <- namn
    i <- 1
    while (unikt_namn %in% tagna_namn) {
      tillagg <- paste0("_", i)
      unikt_namn <- paste0(substr(namn, 1, 31 - nchar(tillagg)), tillagg)
      i <- i + 1
    }
    unikt_namn
  }

  tabell_id <- "TAB3981"

  hamta_utbniva <- function(region_vekt, kon_klartext, alder_vekt, utbildningsniva_klartext, tid_vekt) {
    giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code
    tid_var <- if (identical(tid_vekt, "*")) giltiga_ar else as.character(tid_vekt)[as.character(tid_vekt) %in% giltiga_ar]

    px <- pxweb2r::pxweb2_get_data(
      table = tabell_id,
      query = list(
        Region = region_vekt,
        Alder = alder_vekt,
        UtbildningsNiva = utbildningsniva_klartext,
        Kon = kon_klartext,
        ContentsCode = "UF0506A1",
        Tid = tid_var
      ),
      on_all_values_invalid = "null", quiet = TRUE)

    dplyr::rename(px, regionkod = region_kod, Befolkning = value) |>
      dplyr::select(-tabellinnehåll)
  }

  alla_giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code

  region_txt <- paste0(region_vekt, collapse = "_")

  if (length(valt_ar) > 1) print("Endast ett år kan skickas med i funktionen, bara första året i vektorn kommer att användas.")
  if (is.na(valt_ar[1])) valt_ar <- max(alla_giltiga_ar) else {
    valt_ar <- valt_ar[1]
    if (!valt_ar %in% alla_giltiga_ar) valt_ar <- max(alla_giltiga_ar)
  }

  # ========================================== Läser in data ============================================
  px_df <- hamta_utbniva(region_vekt = region_vekt,
                                kon_klartext = c("män","kvinnor"),
                                alder_vekt = c(as.character(25:64)),
                                utbildningsniva_klartext = "*",
                                tid_vekt = "*")

  px_df_utskrift_kon <- px_df |>
    dplyr::filter(regionkod %in% region_vekt) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = sverige_istallet_for_riket))

  if (!is.na(gruppering_namn)) {
    px_df_utskrift_kon <- px_df_utskrift_kon |>
      dplyr::group_by(år, ålder, kön, utbildningsnivå) |>
      dplyr::summarise(Befolkning = sum(Befolkning, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(regionkod = "grp",
             region = gruppering_namn) |>
      dplyr::relocate(regionkod, .before = 1) |>
      dplyr::relocate(region, .after = regionkod)

    region_txt <- gruppering_namn                           # för att få rätt regionnamn i titel, filnamn etc.
    region_vekt <- c("grp", region_vekt)

  } # slut if-sats om man vill gruppera

  # om vi vill sortera om facet-diagrammen så gör vi det här
  if (!is.na(region_lagg_forst[1]) & length(unique(px_df_utskrift_kon$region)) > 1){
    # testa om region_lagg_forst-koderna finns i datasetet
    if (any(region_lagg_forst %in% unique(px_df_utskrift_kon$regionkod))) {

      regionkoder_i_df <- unique(px_df_utskrift_kon$regionkod)
      region_i_df <- unique(px_df_utskrift_kon$region)
      # skapa sorteringsvektorer
      reg_ej_fokus <- sort(unique(region_i_df[!regionkoder_i_df %in% region_lagg_forst]))
      reg_fokus <- unique(region_i_df[regionkoder_i_df %in% region_lagg_forst])        # bara de regioner som finns i datasetet kommer med
      reg_fokus_koder <- unique(regionkoder_i_df[regionkoder_i_df %in% region_lagg_forst])        # bara de regioner som finns i datasetet kommer med
      reg_fokus <- reg_fokus[match(region_lagg_forst, reg_fokus_koder)]               # sortera utifrån ordningen i region_lagg_forst
      region_sort <- c(reg_fokus, reg_ej_fokus)

      # sortera med hjälp av region_sort som vi skapade ovan
      px_df_utskrift_kon <- dplyr::mutate(px_df_utskrift_kon, region = factor(region, levels = region_sort))
    } # slut if-sats för att testa om region_lagg_forst-koderna finns i datasetet
  } # slut if-sats om vi skickat med lagg_forst_koder

  # bearbeta datasetet
  px_df_utskrift_kon <- px_df_utskrift_kon |>
    dplyr::mutate(utb_niva = dplyr::case_when(
      utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
      utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
      utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
      utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
      utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
      utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
      utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
      utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas"),
      utb_niva = factor(utb_niva, levels = c("Eftergymnasial utbildning, 3 år eller mer","Eftergymnasial utbildning, mindre än 3 år","Gymnasial utbildning","Förgymnasial utbildning"))) |>
    dplyr::filter(utb_niva != "Uppgift saknas") |>
    dplyr::group_by(år, regionkod, region, kön, utb_niva) |>
    dplyr::summarize(antal = sum(Befolkning, na.rm = TRUE)) |>
    dplyr::mutate(andel = (antal/sum(antal)) * 100) |>
    dplyr::ungroup()

  # Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning - uppdelat på kön
  px_df_utskrift <- px_df_utskrift_kon |>
    dplyr::filter(utb_niva != "Uppgift saknas") |>
    dplyr::group_by(år, regionkod, region, utb_niva) |>
    dplyr::summarize(antal = sum(antal, na.rm = TRUE)) |>
    dplyr::mutate(andel = (antal/sum(antal)) * 100) |>
    dplyr::ungroup()

  region_titel <- if (length(region_vekt) > 1) "" else paste0(" i ", unique(px_df_utskrift$region))

    if (diag_hogutb_over_tid) {
    diagramtitel <- stringr::str_wrap(paste0("Andel högutbildade invånare 25-64 år", region_titel))
    diagramfilnamn <- paste0("hogutb_andel_ar", region_txt, "_", min(px_df_utskrift_kon$år), "_", max(px_df_utskrift_kon$år), ".png")

    if (diagram_capt_tabort == TRUE){
      diagram_capt_hogutb <- NULL
    }else{
      diagram_capt_hogutb <- paste0(diagram_capt, "\nDefinitionen av högutbildade är individer med minst 3 års eftergymnasial utbildning.")
    }

    diagram_data <- dplyr::filter(px_df_utskrift_kon,
                                   regionkod %in% region_vekt,
                                   utb_niva == "Eftergymnasial utbildning, 3 år eller mer")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_data,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 diagram_titel = if (diagramtitel_tabort) NULL else diagramtitel,
                                 diagram_capt =  diagram_capt_hogutb,
                                 facet_grp = if (length(region_vekt) > 1) "region" else NULL,
                                 facet_scale = "fixed",
                                 facet_x_axis_storlek = facet_x_axis_stlk,
                                 stodlinjer_avrunda_fem = TRUE,
                                 facet_legend_bottom = TRUE,
                                 x_axis_visa_var_xe_etikett = visa_var_xe_etikett,
                                 #x_axis_lutning = 0,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- stringr::str_remove(diagramfilnamn, ".png")

    if (spara_dataset_excel) {
      fliknamn <- saklig_fliknamn(stringr::str_remove(diagramfilnamn, ".png"), names(dataset_list))
      dataset_list[[fliknamn]] <- diagram_data
    }
    } # slut if-sats om man vill skriva ut diagram över utvecklingen av högutbildade över tid

  if (diag_lagutb_over_tid) {
    diagramtitel <- stringr::str_wrap(paste0("Andel lågutbildade invånare 25-64 år", region_titel))
    diagramfilnamn <- paste0("lagutb_andel_ar", region_txt, "_", min(px_df_utskrift_kon$år), "_", max(px_df_utskrift_kon$år), ".png")

    if (diagram_capt_tabort == TRUE){
      diagram_capt_lagutb <- NULL
    }else{
      diagram_capt_lagutb <- paste0(diagram_capt, "\nDefinitionen av lågutbildade är individer med endast förgymnasial utbildning.")
    }

    diagram_data <- dplyr::filter(px_df_utskrift_kon,
                                   regionkod %in% region_vekt,
                                   utb_niva == "Förgymnasial utbildning")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_data,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = if (diagramtitel_tabort) NULL else diagramtitel,
                                 diagram_capt =  diagram_capt_lagutb,
                                 facet_grp = if (length(region_vekt) > 1) "region" else NULL,
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 facet_x_axis_storlek = facet_x_axis_stlk,
                                 x_axis_visa_var_xe_etikett = visa_var_xe_etikett,
                                 #x_axis_lutning = 0,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- stringr::str_remove(diagramfilnamn, ".png")

    if (spara_dataset_excel) {
      fliknamn <- saklig_fliknamn(stringr::str_remove(diagramfilnamn, ".png"), names(dataset_list))
      dataset_list[[fliknamn]] <- diagram_data
    }
  } # slut if-sats om man vill skriva ut diagram över utvecklingen av högutbildade över tid


  if (diag_andel_alla_utbnivaer) {
    diagramtitel <- stringr::str_wrap(paste0("Utbildningsnivå för invånare 25-64 år", region_titel))
    diagramfilnamn <- paste0("utbniva_andel_per_ar_", region_txt, "_", valt_ar, ".png")

    diagram_capt_niva <- if (diagram_capt_tabort == TRUE) NULL else diagram_capt

    diagram_data <- dplyr::mutate(
                       dplyr::filter(px_df_utskrift, regionkod %in% region_vekt,
                              år %in% c("1985", "1990", "2000", "2010", valt_ar)),
                       andel = andel - 0.001)

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_data,
                                 skickad_x_var = "år",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "utb_niva",
                                 #manual_x_axis_text_vjust=1,
                                 #manual_x_axis_text_hjust=1,
                                 manual_color = rddiagram::diagramfarger("rd_bla"),
                                 diagram_titel = if (diagramtitel_tabort) NULL else diagramtitel,
                                 diagram_capt =  diagram_capt_niva,
                                 facet_grp = if (length(region_vekt) > 1) "region" else NULL,
                                 facet_legend_bottom = TRUE,
                                 facet_scale = "fixed",
                                 facet_x_axis_storlek = facet_x_axis_stlk,
                                 geom_position_stack = TRUE,
                                 procent_0_100_10intervaller = TRUE,
                                 x_axis_lutning = 0,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 manual_x_axis_title = "år",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- stringr::str_remove(diagramfilnamn, ".png")

    if (spara_dataset_excel) {
      fliknamn <- saklig_fliknamn(stringr::str_remove(diagramfilnamn, ".png"), names(dataset_list))
      dataset_list[[fliknamn]] <- diagram_data
    }
  } # slut if-sats om man vill skriva ut diagram över alla utbildningsnivåer

  if (diag_andel_utbniva_jmfr_lan){

    px_df_jmfr_lan <- hamta_utbniva(region_vekt = rdverktyg::hamtaAllaLan(FALSE),
                                    kon_klartext = "*",
                                    alder_vekt = c(as.character(25:64)),
                                    utbildningsniva_klartext = "*",
                                    tid_vekt = valt_ar) |>
      dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = sverige_istallet_for_riket))

    # bearbeta datasetet
    px_df_jmfr_lan <- px_df_jmfr_lan |>
      dplyr::mutate(utb_niva = dplyr::case_when(
        utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
        utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
        utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
        utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
        utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
        utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
        utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
        utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas"),
        utb_niva = factor(utb_niva, levels = c("Eftergymnasial utbildning, 3 år eller mer","Eftergymnasial utbildning, mindre än 3 år","Gymnasial utbildning","Förgymnasial utbildning"))) |>
      dplyr::filter(utb_niva != "Uppgift saknas") |>
      dplyr::group_by(år, regionkod, region, kön, utb_niva) |>
      dplyr::summarize(antal = sum(Befolkning, na.rm = TRUE)) |>
      dplyr::mutate(andel = (antal/sum(antal)) * 100) |>
      dplyr::ungroup()

    # if(minst_3_ar == TRUE){
    #   utb_niva_vec = c("Eftergymnasial utbildning, 3 år eller mer")
    #   diagramtitel <- paste0("Andel invånare 25-64 år med minst 3 års eftergymnasial utbildning år ", valt_ar)
    #   diagramfilnamn <- paste0("hogutb_andel_ar_", valt_ar, ".png")
    #}else{
    #   utb_niva_vec <- c("Eftergymnasial utbildning, mindre än 3 år",
    #                     "Eftergymnasial utbildning, 3 år eller mer")
    #   diagramtitel <- paste0("Andel invånare 25-64 år med eftergymnasial utbildning år ", valt_ar)
    #   diagramfilnamn <- paste0("eftergymn_utb_andel_ar_", valt_ar, ".png")
    # }

    if (vald_utb_niva == "hogutb") {

      utb_niva_vec = c("Eftergymnasial utbildning, 3 år eller mer")
      dia_titel_txt = "minst 3 års eftergymnasial"
      dia_filnamn_txt = "hog"

    } else if (vald_utb_niva == "eftergymn") {

      utb_niva_vec <- c("Eftergymnasial utbildning, mindre än 3 år",
                        "Eftergymnasial utbildning, 3 år eller mer")
      dia_titel_txt = "eftergymnasial"
      dia_filnamn_txt = "eftergymn_"

    } else if (vald_utb_niva == "gymn") {

      utb_niva_vec <- c("Gymnasial utbildning")
      dia_titel_txt = "gymnasial"
      dia_filnamn_txt = "gymn_"

    } else if (vald_utb_niva == "forgymn") {

      utb_niva_vec <- c("Förgymnasial utbildning")
      dia_titel_txt = "förgymnasial"
      dia_filnamn_txt = "forgymn_"

    } else {
      stop("Felaktig parameter för vald_utb_niva. Välj mellan 'hogutb', 'eftergymn', 'gymn', 'forgymn' eller 'alla'.")
    }

    diagramtitel <- glue::glue("Andel invånare 25-64 år med {dia_titel_txt} utbildning år {valt_ar}")
    diagramfilnamn <- glue::glue("{dia_filnamn_txt}utb_andel_ar_", valt_ar, ".png")

    diagram_capt_jmfr <- if (diagram_capt_tabort == TRUE) NULL else diagram_capt

    diagram_data <- dplyr::filter(px_df_jmfr_lan, utb_niva %in% utb_niva_vec)

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = diagram_data,
                                 skickad_x_var = "region",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 #manual_x_axis_text_vjust=1,
                                 #manual_x_axis_text_hjust=1,
                                 diagram_liggande = TRUE,
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 diagram_titel = if (diagramtitel_tabort) NULL else diagramtitel,
                                 diagram_capt =  diagram_capt_jmfr,
                                 stodlinjer_avrunda_fem = TRUE,
                                 facet_legend_bottom = TRUE,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value = TRUE,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 #legend_vand_ordning=TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- stringr::str_remove(diagramfilnamn, ".png")

    if (spara_dataset_excel) {
      fliknamn <- saklig_fliknamn(stringr::str_remove(diagramfilnamn, ".png"), names(dataset_list))
      dataset_list[[fliknamn]] <- diagram_data
    }

  } # slut if-sats om man vill skriva ut länsjämförelsediagram

  if (spara_dataset_excel && length(dataset_list) > 0) {
    if (!requireNamespace("rdverktyg", quietly = TRUE)) {
      remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
    }
    if (is.na(excel_filnamn)) excel_filnamn <- paste0("utbniva_diagramdata_", region_txt, ".xlsx")
    rdverktyg::excelfil_spara_formaterad(indata = dataset_list, output_mapp = output_mapp,
                                         excelfil_namn = excel_filnamn)
  }

  return(gg_list)

} # slut funktion

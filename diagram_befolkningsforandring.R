diagram_befolkningsforandring <- function(region_vekt = rdverktyg::hamtaAllaLan(tamedriket = FALSE), # Vilka regioner/kommuner vill man titta på
                                          output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                          output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                          tid = c("2010","9999"), # Välj tidsintervall. Finns från 1968."*" ger alla. "9999" ger sista år
                                          spara_figur = TRUE,
                                          alder = as.character(20:64), # Välj ett åldersintervall. "tot" ger alla åldrar (inte "*")
                                          filnamn_data = "befolkningsforandring.xlsx", # Filnamn på sparad data
                                          vald_farg = rddiagram::diagramfarger("rus_sex"), # Val av diagramfärger
                                          returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                                          returnera_data = FALSE){ # Skall data returneras


  # =================================================================================================================
  # Diagram som beräknar befolkningsförändring under en tidsperiod uppdelat på inrikes/utrikes flytt och demografisk förändring
  # Diagrammet kan skapas för olika åldersintervall
  # Skapad av Jon 2024-01-19
  # Senast uppdaterad: 2024-10-18, snyggat till rubriker, Peter
  # Förbättringsmöjligheter: Markera total förändring i diagram (exepelvis som en svart linje)
  # =================================================================================================================

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
  # stället för library(). "here" togs bort - laddades men användes aldrig.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/tidyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge objekt i lista
  list_data <- list() # Skapa tom lista som används för att spara till Excel.

  # hamta_data-repots hamta_bef_folkmangd_alder_kon_ar_scb.R och
  # hamta_bef_flyttningar_region_alder_kon_scb.R hämtade var sitt par av
  # historik-/CKM-tabeller (v1: BE0101A/BefolkningNy+BefolkningCKM samt
  # BE0101J/Flyttningar97+Flyttningar97CKM). v2-motsvarigheterna är samma
  # tabeller som redan används i diagram_flytt_inrikes_aldersgrupper_SCB.R
  # (TAB1212/TAB6640) och diagram_fodelsenetto_region_SCB.R (TAB638/TAB5557),
  # verifierat via de gamla v1-tabellernas titlar.

  # Folkmängd/Folkökning: TAB638 (historik 1968-2024) + TAB5557 (CKM 2025-).
  # pxweb2r fyller en helt utelämnad variabel med "*" (alla värden) i stället
  # för PxWeb-API:ets "elimination"-summa, så en utelämnad Civilstand-variabel
  # ger fyra separata civilståndsrader, inte en färdigsummerad total - och
  # TAB638 (till skillnad från CKM-tabellen) har dessutom ingen egen
  # "totalt"-kod för Civilstand alls. Hämtar därför explicit alla civilstånd
  # (resp. den riktiga totalkoden i CKM-tabellen) och summerar sedan ihop dem
  # själva innan pivotering, i stället för att förlita oss på att SCB redan
  # gjort det år oss.
  bef_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB638",
    query = list(
      Region = region_vekt,
      Civilstand = "*",
      Kon = c("män","kvinnor"),
      Alder = alder,
      ContentsCode = c("Folkmängd","Folkökning"),
      Tid = tid
    ),
    on_all_values_invalid = "null")

  bef_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB5557",
    query = list(
      Region = region_vekt,
      Civilstand = "totalt, samtliga civilstånd",
      Kon = c("män","kvinnor"),
      Alder = alder,
      ContentsCode = c("Folkmängd","Folkökning"),
      Tid = tid
    ),
    on_all_values_invalid = "null")

  har_ckm_bef <- !is.null(bef_ckm) && nrow(bef_ckm) > 0

  bef_df <- dplyr::bind_rows(bef_historik, bef_ckm) |>
    dplyr::rename(variabel = tabellinnehåll, varde = value) |>
    dplyr::group_by(region, ålder, kön, år, variabel) |>
    dplyr::summarize(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variabel,values_from = varde)

  # Flyttningar: TAB1212 (historik 1997-2024) + TAB6640 (CKM 2025-). Ingen av
  # tabellerna har en egen "totalt"-kod för Kon (TAB1212 har bara män/kvinnor
  # alls), så av samma anledning som ovan hämtas båda könen explicit och
  # summeras ihop till en totalrad per region/ålder/år, i stället för att
  # utelämna Kon (vilket - som ovan - bara ger män+kvinnor var för sig,
  # inte en färdig totalsumma).
  flytt_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB1212",
    query = list(
      Region = region_vekt,
      Alder = alder,
      Kon = c("män","kvinnor"),
      ContentsCode = "*",
      Tid = tid
    ),
    on_all_values_invalid = "null")

  flytt_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6640",
    query = list(
      Region = region_vekt,
      Alder = alder,
      Kon = c("män","kvinnor"),
      ContentsCode = "*",
      Tid = tid
    ),
    on_all_values_invalid = "null")

  har_ckm_flytt <- !is.null(flytt_ckm) && nrow(flytt_ckm) > 0

  flytt_df <- dplyr::bind_rows(flytt_historik, flytt_ckm) |>
    dplyr::rename(variabel = tabellinnehåll, varde = value) |>
    dplyr::group_by(region, ålder, år, variabel) |>
    dplyr::summarize(varde = sum(varde, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = variabel,values_from = varde)


  # summerar flyttningar på regionnivå för åren 2010-senaste år
  flytt_df_sum <- flytt_df |>
    dplyr::group_by(region) |>
    dplyr::summarize(flyttningsoverskott = sum(Flyttningsöverskott),
              invandringsoverskott=sum(Invandringsöverskott),
              inrikes_flyttningsoverskott=sum(`Inrikes flyttningsöverskott`),
              .groups = "drop")

  # Summerar den totala folkökningen
  bef_df_sum <- bef_df |>
    dplyr::group_by(region) |>
      dplyr::summarize(folkokning_period = sum(Folkökning), .groups = "drop")

  # Tar ut befolkningen 2010 och slår ihop med folkökning (för att beräkna förändring)
  bef_df_min <- bef_df |>
    dplyr::filter(år == min (år)) |>
    dplyr::group_by(region) |>
      dplyr::summarize(folkmangd_forsta_ar = sum(Folkmängd), .groups = "drop")

  bef_df_sum <- merge(bef_df_sum,bef_df_min)

  # Slår ihop befolkning och flyttningar
  slutgiltig_df <-merge(flytt_df_sum,bef_df_sum)

  # Beräknar procentuell förändring av befolkning baserat på olika kompontenter
  slutgiltig_df <- slutgiltig_df |>
    dplyr::mutate(alderskomponent = folkokning_period - flyttningsoverskott) |>
      dplyr::mutate("Inrikes flyttnetto" = round((inrikes_flyttningsoverskott/folkmangd_forsta_ar)*100,2),
             "Utrikes flyttnetto" = round((invandringsoverskott/folkmangd_forsta_ar)*100,2),
             "Demografisk förändring" = round((alderskomponent/folkmangd_forsta_ar)*100,2),
             "Total förändring" = round((folkokning_period/folkmangd_forsta_ar)*100,2),
             "region" = rdverktyg::skapa_kortnamn_lan(region)) |>
        dplyr::select(region,`Inrikes flyttnetto`,`Utrikes flyttnetto`,`Demografisk förändring`,`Total förändring`) |>
          tidyr::pivot_longer(!c(region),names_to="variabel",values_to="forandring")

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Befolkningsförändring" = slutgiltig_df))
    }

    if(returnera_data == TRUE){
      assign("befolkningsforandring", slutgiltig_df, envir = .GlobalEnv)
    }

  diagram_capt <- "Källa: Befolkningsregistret i SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  if (har_ckm_bef || har_ckm_flytt) {
    diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, TRUE, fran_ar = 2025)
  }

  if(all(alder == "tot")) {
    diagramtitel <- paste0("Befolkningsförändring invånare alla åldrar år ",min(bef_df$år),"-",max(bef_df$år))
  }else diagramtitel <- paste0("Befolkningsförändring invånare ",stringr::str_remove(min(bef_df$ålder), " år"),"-",max(bef_df$ålder)," under perioden år ",min(bef_df$år),"-",max(bef_df$år))

  objektnamn <- "befolkningsforandring_20_64"
  diagramfilnamn <- paste0(objektnamn,".png")

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = slutgiltig_df |>
      dplyr::filter(variabel!="Total förändring"),
    skickad_x_var = "region",
    skickad_y_var = "forandring",
    skickad_x_grupp = "variabel",
    manual_color = vald_farg,
    diagram_titel = diagramtitel,
    diagram_capt =  diagram_capt,
    x_axis_lutning = 0,
    x_axis_sort_value = TRUE,
    diagram_liggande = TRUE,
    stodlinjer_avrunda_fem = TRUE,
    geom_position_stack = TRUE,
    manual_y_axis_title="procent",
    output_mapp = output_mapp_figur,
    filnamn_diagram = diagramfilnamn,
    skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))

  names(gg_list) <- c(objektnamn)

  if(returnera_figur == TRUE) return(gg_list)

  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }

}

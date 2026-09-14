diag_befutv_per_komponent_ar <- function(
    region_vekt = "20",                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region
    gruppera_namn = NA,                                     # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,                                        # här sparas
    enbart_inrikes_flyttnetto = TRUE,                        # FALSE = inr flyttnetto delas upp på eget län och övriga Sverige, annars blir det bara en kategori för inr flyttnetto
    diagram_fargvekt = NA,
    x_axis_storlek = 7, # Ändra storleken på x-axeln
    facet_x_axis_storlek = 6, # Ändra storleken på x-axeln i facet. Standardvärde är 8
    x_axis_visa_var_xe_etikett = NA, # Möjlighet att visa var x:e etikett på x-axeln
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    returnera_dataframe_global_environment = FALSE,
    ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
    skriv_till_diagramfil = TRUE
) {

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
  # stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rus_sex")
  if (all(is.na(diagram_fargvekt))) {
    diagram_fargvekt <- rddiagram::diagramfarger("rus_sex")
  }

  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_till_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(rdverktyg::utskriftsmapp())) {
        output_mapp <- rdverktyg::utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }

  # hamta_data-repots hamta_bef_forandringar_region_period_kon_scb.R (v1:
  # BE0101G/BefforandrKvRLK + BefforandrKvRLKCKM) hämtas här direkt via
  # v2-motsvarigheterna TAB5169 (historik 2000-2024) och TAB6481 (CKM
  # fr.o.m. 2025). Kön har elimination i båda tabellerna men pxweb2r
  # summerar INTE automatiskt när variabeln utelämnas (den fylls i stället
  # i med "*", dvs. alla enskilda värden) - originalet skickade kon_klartext
  # = NA för att låta v1-apiet eliminera (=summera) kön automatiskt, så här
  # begärs i stället den riktiga totalkoden explicit. Precis som i övriga
  # migrerade BE0101-skript skiljer sig CKM-tabellens klartext för totalt
  # kön ("totalt, samtliga män och kvinnor"/"TotSa") från historiktabellens
  # ("totalt"/"1+2") - och här skiljer sig även ContentsCode-klartexten
  # ("Antal personer" resp. "Befolkningsstatistik antal personer"), trots
  # att båda tabellerna bara har en enda innehållsvariabel.
  forandringar_klartext <- c("flyttningsöverskott eget län", "flyttningsöverskott övriga Sverige",
                              "invandringsöverskott", "födelseöverskott")

  beffor_df_historik <- pxweb2r::pxweb2_get_data(
    table = "TAB5169",
    query = list(
      Region = region_vekt,
      Forandringar = forandringar_klartext,
      Period = "hela året",
      Kon = "totalt",
      ContentsCode = "Antal personer",
      Tid = "*"
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  beffor_df_ckm <- pxweb2r::pxweb2_get_data(
    table = "TAB6481",
    query = list(
      Region = region_vekt,
      Forandringar = forandringar_klartext,
      Period = "hela året",
      Kon = "TotSa",
      ContentsCode = "Befolkningsstatistik antal personer",
      Tid = "*"
    ),
    on_all_values_invalid = "null", quiet = TRUE)

  beffor_df <- dplyr::bind_rows(beffor_df_historik, beffor_df_ckm) |>
    dplyr::rename(regionkod = region_kod, personer = value) |>
    dplyr::select(-tabellinnehåll, -kön)

  # Lägg till en CKM-notering i captionen om CKM-tabellen (TAB6481, data
  # fr.o.m. 2025) faktiskt bidragit med rader till uttaget.
  har_ckm_data <- !is.null(beffor_df_ckm) && nrow(beffor_df_ckm) > 0
  if (har_ckm_data) {
    diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, TRUE, fran_ar = 2025)
  }

  # Ändra första bokstaven till versal i befolkningsförändringar
  diagram_df <- beffor_df |>
    dplyr::filter(!is.na(personer)) |>
    dplyr::mutate(förändringar = stringr::str_c(stringr::str_to_upper(stringr::str_sub(förändringar, 1, 1)), stringr::str_sub(förändringar, 2)))

  # Om det bara finns flyttningsöverskott övriga Sverige så (om man tex valt län) så ändras kategorinamnet till Inrikes flyttnetto
  if (!"Flyttningsöverskott eget län" %in% unique(diagram_df$förändringar)) {
    diagram_df <- diagram_df |>
      dplyr::mutate(förändringar = dplyr::if_else(förändringar == "Flyttningsöverskott övriga Sverige", "Inrikes flyttnetto", förändringar))
  }

  # Om både flyttningsöverskott eget län och övriga Sverige finns men man enbart vill ha inrikes flyttnetto
  # totalt så läggs de ihop här
  if (("Flyttningsöverskott övriga Sverige" %in% unique(diagram_df$förändringar)) & enbart_inrikes_flyttnetto) {
    diagram_df <- diagram_df |>
      dplyr::bind_rows(
        diagram_df |>
          dplyr::filter(förändringar %in% c("Flyttningsöverskott eget län", "Flyttningsöverskott övriga Sverige")) |>
          dplyr::group_by(år, regionkod, region, period) |>
          dplyr::summarise(
            förändringar = "Inrikes flyttnetto",
            personer = sum(personer, na.rm = TRUE),
            .groups = "drop"
          )
      ) |>
      dplyr::filter(!förändringar %in% c("Flyttningsöverskott övriga Sverige", "Flyttningsöverskott eget län"))
  }

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("befutv_per_komponent_ar_scb_df", diagram_df, envir = .GlobalEnv)
  }

  if (length(unique(diagram_df$förändringar)) == 4) diagram_fargvekt <- diagram_fargvekt[c(1,2,2,3)]

  # Vi gör factor-variabel av förändringar för att få kategorier i rätt ordning
  diagram_df <- diagram_df |>
    dplyr::mutate(förändringar = stringr::str_replace(förändringar, "Flyttningsöverskott", "Flyttnetto"),
           förändringar = factor(förändringar, levels = c("Födelseöverskott", "Inrikes flyttnetto", "Flyttnetto eget län", "Flyttnetto övriga Sverige", "Invandringsöverskott")))

  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!all(is.na(gruppera_namn))) {
    diagram_df <- diagram_df |>
      dplyr::group_by(dplyr::across(-c(regionkod, region, personer))) |>
      dplyr::summarise(personer = sum(personer, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(regionkod = "gg",
             region = gruppera_namn) |>
      dplyr::relocate(region, .before = 1) |>
      dplyr::relocate(regionkod, .before = region)

    region_vekt <- "gg"
  }

  skapa_diagram <- function(skickad_regionkod) {

    skriv_diagram_df <- diagram_df |>
      dplyr::filter(regionkod %in% skickad_regionkod)

    region_txt <- if (skickad_regionkod == "gg") gruppera_namn else rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(skickad_regionkod)$region))
    region_filnamn <- if (skickad_regionkod == "gg") stringr::str_replace_all(tolower(gruppera_namn), " ", "_") else paste0(rdverktyg::hamtaregion_kod_namn(skickad_regionkod)$region, collapse = "_")
    startar <- min(skriv_diagram_df$år)
    slutar <- max(skriv_diagram_df$år)

    diagramtitel <- glue::glue("Befolkningsutveckling i {region_txt} {startar}-{slutar}")
    diagramfil <- glue::glue("befolkningsforandring_per_komponent_{region_filnamn}_ar{startar}-{slutar}.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = skriv_diagram_df,
      skickad_x_var = "år",
      skickad_y_var = "personer",
      skickad_x_grupp = "förändringar",
      diagram_titel = if (ta_bort_diagramtitel) NULL else diagramtitel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      filnamn_diagram = diagramfil,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_color = diagram_fargvekt,
      output_mapp = output_mapp,
      legend_vand_ordning = TRUE,
      lagg_pa_logga = ta_med_logga,
      logga_path = logga_sokvag,
      dataetiketter = visa_dataetiketter,
      skriv_till_diagramfil = skriv_till_diagramfil,
      facet_grp = "förändringar",
      facet_scale = "fixed",
      x_axis_storlek = x_axis_storlek,
      facet_x_axis_storlek = facet_x_axis_storlek,
      x_axis_visa_var_xe_etikett = x_axis_visa_var_xe_etikett,
    )

    ett_diagram <- list(gg_obj)
    names(ett_diagram) <- stringr::str_remove(diagramfil, "\\.png")
    return(ett_diagram)
  } # slut funktion för att skriva diagram

  retur_list <- purrr::flatten(purrr::map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)))

  return(retur_list)

} # slut funktion

diag_narstaendepenning <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   diag_stapel = TRUE,
                                   diag_forandring = FALSE,
                                   variabel = "Antal vårdare", # Finns även "Antal sjuka", "Belopp","Nettodagar" Går att välja flera
                                   spara_diagrambildfil = FALSE,
                                   spara_dataframe_till_global_environment = FALSE){

  ## =================================================================================================================
  # Skript som skapar två diagram för närståendepenning (stapeldiagram för antal/belopp och linjediagram). Går att få ut för de fyra variablerna:
  # "Antal vårdare", "Antal sjuka", "Belopp" och "Nettodagar". Flera kan väljas samtidigt.
  # Används i första hand i rapporten "Kvinnor och män i Dalarna"
  # Skapad av Jon Frank 2025-11-04
  # Källa: https://www.dataportal.se/datasets/547_21402
  # =============================================== Uttag ===============================================
  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # Ingen SCB-hämtning här - data kommer från Försäkringskassans egen publika
  # Excel-export. "here" togs bort - laddades men användes aldrig.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/stringr/purrr följer med som beroenden till rddiagram/rdverktyg.

  # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/np-antal-mottagare-belopp-dagar/NPAntalBeloppDagarLan.xlsx")

  gg_list = list()
  objektnamn <- c()

  narstaendepenning_df = rdverktyg::hamta_excel_dataset_med_url(path[1],skippa_rader = 2) |>
    dplyr::filter(substr(Län,1,2) %in% region_vekt) |>
    dplyr::mutate(Län = stringr::str_replace(Län, "^[^\\p{L}]*", "")) |>
    dplyr::select(-kolumnnamn)

  if(spara_dataframe_till_global_environment){
    assign("narstaendepenning_df", narstaendepenning_df, envir = .GlobalEnv)
  }

  skapa_diagram <- function(df, vald_variabel){

       df = df |>
        dplyr::select(År,Kön,Län,dplyr::all_of(vald_variabel))

       file_fragment <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(iconv(names(df)[ncol(df)], to = "ASCII//TRANSLIT"))))

       if(diag_stapel == TRUE){

      # Omvandla kolumnnamn

       if(vald_variabel == "Belopp") df$Belopp = df$Belopp/1000 # Omvandla till tusentals kronor

      # Antal nettodagar
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- glue::glue("Närståendepenning, {tolower(dplyr::last(names(df)))} i  " ,rdverktyg::skapa_kortnamn_lan(unique(df$Län)))
      diagramfilnamn <- paste0("narstaendepenning_",file_fragment,"_",rdverktyg::skapa_kortnamn_lan(unique(df$Län)),".png")

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = df |>
          dplyr::filter(Kön != "Kvinnor och män") |>
          dplyr::mutate(Kön = tolower(Kön)),
        skickad_x_var = "År",
        skickad_y_var = dplyr::last(names(df)),
        skickad_x_grupp = "Kön",
        x_axis_lutning = 45,
        manual_x_axis_text_vjust=1,
        manual_x_axis_text_hjust=1,
        manual_y_axis_title = ifelse(vald_variabel == "Belopp", "Tusentals kronor", ""),
        manual_color = rddiagram::diagramfarger("kon"),
        stodlinjer_avrunda_fem = TRUE,
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_diagrambildfil)

      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
       }

       if(diag_forandring == TRUE){

         # Omvandla kolumnnamn

         #if(vald_variabel == "Belopp") df$Belopp = df$Belopp/1000 # Omvandla till tusentals kronor

         # Antal nettodagar
         diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
         diagramtitel <- glue::glue("Närståendepenning, förändring i {tolower(dplyr::last(names(df)))} i  " ,rdverktyg::skapa_kortnamn_lan(unique(df$Län)))
         diagramfilnamn <- paste0("narstaendepenning_forandring_",file_fragment,"_",rdverktyg::skapa_kortnamn_lan(unique(df$Län)),".png")

         gg_obj <- rddiagram::SkapaLinjeDiagram(
           skickad_df = df |>
             dplyr::filter(Kön != "Kvinnor och män") |>
             dplyr::rename(år = År) |>
             dplyr::mutate(Kön = tolower(Kön)),
           skickad_x_var = "år",
           skickad_y_var = dplyr::last(names(df)),
           skickad_x_grupp = "Kön",
           x_axis_lutning = 45,
           manual_y_axis_title = "Index, startår 1999",
           manual_color = rddiagram::diagramfarger("kon"),
           stodlinjer_avrunda_fem = TRUE,
           berakna_index = TRUE,
           diagram_titel = diagramtitel,
           diagram_capt =  diagram_capt,
           output_mapp = output_mapp,
           filnamn_diagram = diagramfilnamn,
           skriv_till_diagramfil = spara_diagrambildfil)

         gg_list <- c(gg_list, list(gg_obj))
         names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
       }

    return(gg_list)

  }

  diag <- purrr::flatten(purrr::map(variabel, ~ skapa_diagram(narstaendepenning_df, .x)))
  return(diag)
}

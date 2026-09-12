diag_inkomst_scb <- function(regionvekt = "20", # Enbart ett i taget. går även att välja kommuner, men då genereras inget kommundiagram
                             visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                             logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                             output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                             inkomst_typ = "Medianinkomst, tkr", # Finns "Medianinkomst, tkr", "Medelinkomst, tkr". Max 1 åt gången
                             diag_tid = TRUE,
                             diag_linje = TRUE,
                             diag_kommun = TRUE,
                             skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                             alder_klartext = c("20-64 år"),			 #  Finns: "20+ år", "20-64 år", "20-65 år", "65+ år", "66+ år". OBS!! Funkar ej med "*"
                             returnera_data_rmarkdown = FALSE
) {


  # =======================================================================================================================
  #
  # Tre diagram per åldersgrupp för inkomst. Finns på såväl län som kommunnivå. Används i första hand rapporten "Kvinnor och män i Dalarna".
  #
  # Uppdatering 2026-07-03 - Ny version av PXweb används. /Jon
  # =======================================================================================================================

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # Skriptet använde redan func_pxweb2.R (PxWeb v2, tabell TAB5278) - det är
  # samma funktion (med samma tabell-id och query-syntax) som pxweb2r::
  # pxweb2_get_data() byggdes vidare från, så bytet här är rakt av. TAB5278
  # ("Sammanräknad förvärvsinkomst"-registret) täcker 2000-2024 i en enda
  # tabell, ingen CKM-uppdelning ännu. "glue" togs bort - laddades men
  # användes aldrig.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # SCB skriver åldersintervallen med tankstreck ("20–64 år", U+2013),
  # inte vanligt bindestreck - ett vanligt bindestreck (som i standardvärdet
  # ovan och i funktionens egen dokumentation) matchar därför ingen giltig
  # etikett. Normaliserar bindestreck mellan siffror till tankstreck här så
  # att både "20-64 år" och "20–64 år" fungerar som klartext.
  alder_klartext <- gsub("(?<=[0-9])-(?=[0-9])", "–", alder_klartext, perl = TRUE)

  forvarvsinkomst_df <- pxweb2r::pxweb2_get_data(
    table = "TAB5278",
    query = list(
      Region = rdverktyg::hamtakommuner(regionvekt, tamedriket = FALSE),
      Kon = "*",
      Fodelseregion = "samtliga",
      VistelsetidUF = "samtliga",
      Alder = alder_klartext,
      ContentsCode = inkomst_typ,
      Tid = "*"
    ))

  # Koden nedan används för att byta namn på den sista variabeln i df (för att efterlikna tidigare hämtning med gamla PXweb)
  new_name <- unique(forvarvsinkomst_df$tabellinnehåll)

  forvarvsinkomst_df <- forvarvsinkomst_df |>
      dplyr::rename(regionkod = region_kod,
             !!new_name := value) |>
        dplyr::select(-tabellinnehåll) |>
          dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))


  if(returnera_data_rmarkdown == TRUE){
    assign("forvarvsinkomst_df", forvarvsinkomst_df, envir = .GlobalEnv)
  }

  gg_list <- list()
  objektnamn <- c()
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nSammanräknad förvärvsinkomst, dvs. alla skattepliktiga inkomster före skatt (dock ej kapitalinkomster)."

  skapa_diagram <- function(df, vald_aldersgrupp){ # Start map-funktion

    if(diag_tid){
      df_lan <- dplyr::filter(df, regionkod == regionvekt, ålder == vald_aldersgrupp)

      variabel = sub(",.*", "", dplyr::last(names(df_lan)))
      diagramtitel <- paste0(variabel," (", unique(df_lan$ålder),") i ",unique(df_lan$region)," ",max(df_lan$år))
      diagramfilnamn <- paste0(variabel,"_",stringr::str_replace_all(unique(df_lan$ålder), c(" år" = "", "\\+" = "", "-" = "_")),"_",unique(df_lan$region),"_tid.png")
      objektnamn <- c(objektnamn, stringr::str_remove(diagramfilnamn, ".png"))

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::filter(df_lan, kön != "totalt"),
        skickad_x_var = "år",
        skickad_y_var = inkomst_typ,
        skickad_x_grupp = "kön",
        manual_color = rddiagram::diagramfarger("kon"),
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        manual_x_axis_text_vjust=1,
        manual_x_axis_text_hjust=1,
        stodlinjer_avrunda_fem = TRUE,
        vand_sortering = TRUE,
        x_axis_lutning = 45,
        manual_y_axis_title = "Tusentals kronor",
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = skriv_diagrambildfil
      )


      gg_list <- c(gg_list, list(gg_obj))
    }


    if(diag_linje){
      df_lan <- dplyr::filter(df, regionkod == regionvekt, ålder == vald_aldersgrupp)

      variabel = sub(",.*", "", dplyr::last(names(forvarvsinkomst_df)))
      diagramtitel <- paste0(variabel," (", unique(df_lan$ålder),") i ",unique(df_lan$region)," ",max(df_lan$år))
      diagramfilnamn <- paste0(variabel,"_",stringr::str_replace_all(unique(df_lan$ålder), c(" år" = "", "\\+" = "", "-" = "_")),"_",unique(df_lan$region),"_tid_linje.png")
      objektnamn <- c(objektnamn, stringr::str_remove(diagramfilnamn, ".png"))

      gg_obj <- rddiagram::SkapaLinjeDiagram(
        skickad_df = dplyr::filter(df_lan, kön != "totalt"),
        skickad_x_var = "år",
        skickad_y_var = inkomst_typ,
        skickad_x_grupp = "kön",
        manual_color = rddiagram::diagramfarger("kon"),
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        stodlinjer_avrunda_fem = TRUE,
        berakna_index = TRUE,
        x_axis_lutning = 45,
        manual_y_axis_title = "Tusentals kronor",
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = skriv_diagrambildfil
      )

      gg_list <- c(gg_list, list(gg_obj))
    }

    if(diag_kommun && nchar(regionvekt)<3){
      df_kommun <- dplyr::filter(df, ålder == vald_aldersgrupp)

      variabel = sub(",.*", "", dplyr::last(names(df_kommun)))
      diagramtitel <- paste0(variabel," (", unique(df_kommun$ålder),") år ",max(df_kommun$år))
      diagramfilnamn <- paste0(variabel,"_",stringr::str_replace_all(unique(df_kommun$ålder), c(" år" = "", "\\+" = "", "-" = "_")),"_",dplyr::first(df_kommun$region),"_kommun.png")
      objektnamn <- c(objektnamn, stringr::str_remove(diagramfilnamn, ".png"))

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = dplyr::filter(df_kommun, kön != "totalt", år == max(år)),
        skickad_x_var = "region",
        skickad_y_var = inkomst_typ,
        skickad_x_grupp = "kön",
        manual_color = rddiagram::diagramfarger("kon"),
        diagram_titel = diagramtitel,
        diagram_capt =  diagram_capt,
        stodlinjer_avrunda_fem = TRUE,
        x_axis_sort_value = TRUE,
        x_axis_sort_grp = 1,
        manual_x_axis_text_vjust=1,
        manual_x_axis_text_hjust=1,
        x_axis_lutning = 45,
        vand_sortering = TRUE,
        manual_y_axis_title = "Tusentals kronor",
        output_mapp = output_mapp,
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = skriv_diagrambildfil
      )

      gg_list <- c(gg_list, list(gg_obj))
    }

    names(gg_list) <- objektnamn
    return(gg_list)

  }

  diag <- purrr::flatten(purrr::map(alder_klartext, ~ skapa_diagram(forvarvsinkomst_df, .x)))

  return(diag)

}

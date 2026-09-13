diag_fohm <- function(alder = "16- år", # Finns även "16-84 år"
                      diag_soc_rel = TRUE,                                 # TRUE om diagram för sociala relationer ska skapas
                      sociala_relationer_klartext = "Låg tillit till samhällets institutioner",			 #  Finns: "Avstått från att gå ut ensam på grund av rädsla", "Utsatt för fysiskt våld eller hot om våld", "Utsatt för fysiskt våld", "Utsatt för hot om våld", "Saknar emotionellt stöd", "Saknar praktiskt stöd", "Lågt socialt deltagande", "Svårt att lita på andra", "Utsatt för kränkande behandling eller bemötande", "Låg tillit till samhällets institutioner"
                      diag_sjalvskattad_halsa_tid = TRUE,
                      diag_sjalvskattad_halsa_kon = TRUE,
                      visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                      logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet.
                      output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                      skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                      returnera_data_rmarkdown = FALSE,
                      demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {


  # =======================================================================================================================
  # Tre diagram kopplat till Folkhälsomyndighetens öppna statistikdatabas. Samtliga för riket (finns ej uppdelat på bakgrund på länsnivå)
  # Används primärt i integrationsrapporten
  #
  # Migrerad bort från source()/hamta_data-repot. Folkhälsomyndigheten har bara ett v1-PXWeb-API
  # (inget v2/pxweb2r-stöd som SCB). hamta_sociala_relationer_fodelseland_alder_kon_ar() och
  # hamta_sjalvskattad_halsa_riket_alder_fodelseland_kon_ar() används bara av det här skriptet, och
  # båda hämtar mot samma sorts FoHM-tabell (riksnivå, ingen Region-variabel i frågan trots att
  # hamta-funktionerna hade ett oanvänt region_vekt-argument) - uttaget är därför skrivet om med en
  # gemensam intern hjälpfunktion mot pxweb-paketet (fortsatt v1, ingen v2 finns), i modern stil.
  # =======================================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb", quietly = TRUE)) install.packages("pxweb")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()

  # =============================================== API-uttag ===============================================

  # Gemensam hämtfunktion för FoHM:s "cfod"-tabeller (riksnivå, uppdelat på hälsotillstånd/social
  # relation, andel och konfidensintervall, ålder, födelseland, kön och år).
  hamta_fohm_cfod_tabell <- function(url, forsta_varnamn, forsta_klartext, andel_klartext,
                                     alder_klartext, fodelseland_klartext, kon_klartext, tid_koder) {

    px_meta <- pxweb::pxweb_get(url)

    hamta_variabel <- function(text) px_meta$variables[[which(vapply(px_meta$variables, `[[`, "", "text") == text)]]

    kod_for_klartext <- function(variabel_text, klartext_vekt) {
      var <- hamta_variabel(variabel_text)
      if (identical(klartext_vekt, "*")) return(var$values)
      # case-okänslig matchning, som i den frusna func_API.R::sla_upp_varde_klartext_kod()
      var$values[match(tolower(klartext_vekt), tolower(var$valueTexts))]
    }

    giltiga_ar <- hamta_variabel("År")$values
    tid_koder <- ifelse(tid_koder == "9999", max(giltiga_ar), tid_koder)
    tid_vekt <- if (identical(tid_koder, "*")) giltiga_ar else tid_koder[tid_koder %in% giltiga_ar]

    varlista <- purrr::compact(stats::setNames(
      list(
        kod_for_klartext(forsta_varnamn, forsta_klartext),
        kod_for_klartext("Andel och konfidensintervall", andel_klartext),
        kod_for_klartext("Ålder", alder_klartext),
        if (!all(is.na(fodelseland_klartext))) kod_for_klartext("Födelseland", fodelseland_klartext) else NULL,
        if (!all(is.na(kon_klartext))) kod_for_klartext("Kön", kon_klartext) else NULL,
        tid_vekt
      ),
      c(forsta_varnamn, "Andel och konfidensintervall", "Ålder", "Födelseland", "Kön", "År")
    ))

    as.data.frame(pxweb::pxweb_get(url = url, query = varlista))
  }

  url_soc_rel <- "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/B_HLV/eSocialarel/aSocialarel/hlv1soccfod.px"
  url_sjalvskattad_halsa <- "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/A_Mo8/Halsoutfall/01Overgrip/01.01halsgod/halsgodcfod.px"

  if(diag_soc_rel){
    sociala_relationer_df <- hamta_fohm_cfod_tabell(url_soc_rel, "Sociala relationer", sociala_relationer_klartext,
                                                    andel_klartext = "Andel", alder_klartext = alder,
                                                    fodelseland_klartext = "*", kon_klartext = "*", tid_koder = "9999") |>
      dplyr::filter(Födelseland != "Totalt")

    namn <- chartr("åäö", "aao", tolower(stringr::str_replace_all(sociala_relationer_klartext, " ", "_")))

    if(returnera_data_rmarkdown == TRUE){
      assign(paste0(namn,"_df"), sociala_relationer_df, envir = .GlobalEnv)
    }

    diagram_capt <- paste0("Källa: Folkhälsomyndighetens öppna statistikdatabas, bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Enkätundersökning. Andel som svarar ja på frågan: ",unique(sociala_relationer_df$`Sociala relationer`))

    sociala_relationer_df$Födelseland<- factor(sociala_relationer_df$Födelseland, levels = c("Sverige","Övriga Norden","Övriga Europa","Övriga världen"))

    diagramtitel <- paste0(sociala_relationer_klartext," i Sverige år ", max(sociala_relationer_df$År)," (",unique(sociala_relationer_df$Ålder),")")

    namn <- chartr("åäö", "aao", tolower(stringr::str_replace_all(sociala_relationer_klartext, " ", "_")))
    diagramfilnamn <- paste0(namn,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(sociala_relationer_df, Kön != "Totalt"),
                                 skickad_x_var = "Födelseland",
                                 skickad_y_var = dplyr::last(names(sociala_relationer_df)),
                                 skickad_x_grupp = "Kön",
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 procent_0_100_10intervaller = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Födelseland",
                                 stodlinjer_avrunda_fem = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)



    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  if(diag_sjalvskattad_halsa_tid){

    sjalvskattad_halsa_df <- hamta_fohm_cfod_tabell(url_sjalvskattad_halsa, "Hälsotillstånd", "*",
                                                    andel_klartext = "Andel", alder_klartext = alder,
                                                    fodelseland_klartext = "*", kon_klartext = "*", tid_koder = "*") |>
      dplyr::filter(Födelseland != "Totalt",
             !is.na(`Hälsa efter region, kön och år. Andel`))



    if(returnera_data_rmarkdown == TRUE){
      assign("sjalvskattad_halsa_tid_df", sjalvskattad_halsa_df, envir = .GlobalEnv)
    }


    diagram_capt <- paste0("Källa: Folkhälsomyndighetens öppna statistikdatabas, bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Enkätundersökning. Andel som svarar ja på frågan: ",unique(sjalvskattad_halsa_df$Hälsotillstånd))

    sjalvskattad_halsa_df$Födelseland<- factor(sjalvskattad_halsa_df$Födelseland, levels = c("Sverige","Övriga Norden","Övriga Europa","Övriga världen"))

    diagramtitel <- paste0(unique(sjalvskattad_halsa_df$Hälsotillstånd)," i Sverige"," (",unique(sjalvskattad_halsa_df$Ålder),")")

    namn <- chartr("åäö", "aao", tolower(stringr::str_replace_all(unique(sjalvskattad_halsa_df$Hälsotillstånd), " ", "_")))
    diagramfilnamn <- paste0(namn,".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::filter(sjalvskattad_halsa_df, Kön == "Totalt"),
                                 skickad_x_var = "Födelseland",
                                 skickad_y_var = "Hälsa efter region, kön och år. Andel",
                                 skickad_x_grupp = "År",
                                 manual_color = rddiagram::diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 procent_0_100_10intervaller = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Födelseland",
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)



    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  if(diag_sjalvskattad_halsa_kon){

    sjalvskattad_halsa_kon_df <- hamta_fohm_cfod_tabell(url_sjalvskattad_halsa, "Hälsotillstånd", "*",
                                                        andel_klartext = "Andel", alder_klartext = alder,
                                                        fodelseland_klartext = "*", kon_klartext = c("kvinnor","män"), tid_koder = "9999") |>
      dplyr::filter(Födelseland != "Totalt")



    if(returnera_data_rmarkdown == TRUE){
      assign("sjalvskattad_halsa_kon_df", sjalvskattad_halsa_kon_df, envir = .GlobalEnv)
    }

    diagram_capt <- paste0("Källa: Folkhälsomyndighetens öppna statistikdatabas, bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Enkätundersökning. Andel som svarar ja på frågan: ",unique(sjalvskattad_halsa_kon_df$Hälsotillstånd),".")

    sjalvskattad_halsa_kon_df$Födelseland<- factor(sjalvskattad_halsa_kon_df$Födelseland, levels = c("Sverige","Övriga Norden","Övriga Europa","Övriga världen"))

    diagramtitel <- paste0(unique(sjalvskattad_halsa_kon_df$Hälsotillstånd)," i Sverige år ", max(sjalvskattad_halsa_kon_df$År)," (",unique(sjalvskattad_halsa_kon_df$Ålder),")")

    namn <- chartr("åäö", "aao", tolower(stringr::str_replace_all(unique(sjalvskattad_halsa_kon_df$Hälsotillstånd), " ", "_")))
    diagramfilnamn <- paste0(namn,"_kon.png")

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = sjalvskattad_halsa_kon_df,
                                 skickad_x_var = "Födelseland",
                                 skickad_y_var = "Hälsa efter region, kön och år. Andel",
                                 skickad_x_grupp = "Kön",
                                 manual_color = rddiagram::diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 procent_0_100_10intervaller = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Födelseland",
                                 stodlinjer_avrunda_fem = TRUE,
                                 lagg_pa_logga = visa_logga_i_diagram,
                                 logga_path = logga_sokvag,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)



    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")
  }

  return(gg_list)

}

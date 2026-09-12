#test = diagram_inflyttlan_utflyttlan(spara_figur = FALSE,diag_senaste_ar = TRUE, diag_flera_ar = TRUE, diag_facet = FALSE, returnera_figur = TRUE)
diagram_inflyttlan_utflyttlan <- function(output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                          vald_farg = rddiagram::diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                          inflyttningsl_klartext = "*",			 #  Finns: " Stockholms län (Inflyttningslän)", " Uppsala län (Inflyttningslän)", " Södermanlands län (Inflyttningslän)", " Östergötlands län (Inflyttningslän)", " Jönköpings län (Inflyttningslän)", " Kronobergs län (Inflyttningslän)", " Kalmar län (Inflyttningslän)", " Gotlands län (Inflyttningslän)", " Blekinge län (Inflyttningslän)", " Skåne län (Inflyttningslän)", " Hallands län (Inflyttningslän)", " Västra Götalands län (Inflyttningslän)", " Värmlands län (Inflyttningslän)", " Örebro län (Inflyttningslän)", " Västmanlands län (Inflyttningslän)", " Dalarnas län (Inflyttningslän)", " Gävleborgs län (Inflyttningslän)", " Västernorrlands län (Inflyttningslän)", " Jämtlands län (Inflyttningslän)", " Västerbottens län (Inflyttningslän)", " Norrbottens län (Inflyttningslän)"
                                          utflyttningsl_klartext = " Dalarnas län (Utflyttningslän)",			 #  Finns: " Stockholms län (Utflyttningslän)", " Uppsala län (Utflyttningslän)", " Södermanlands län (Utflyttningslän)", " Östergötlands län (Utflyttningslän)", " Jönköpings län (Utflyttningslän)", " Kronobergs län (Utflyttningslän)", " Kalmar län (Utflyttningslän)", " Gotlands län (Utflyttningslän)", " Blekinge län (Utflyttningslän)", " Skåne län (Utflyttningslän)", " Hallands län (Utflyttningslän)", " Västra Götalands län (Utflyttningslän)", " Värmlands län (Utflyttningslän)", " Örebro län (Utflyttningslän)", " Västmanlands län (Utflyttningslän)", " Dalarnas län (Utflyttningslän)", " Gävleborgs län (Utflyttningslän)", " Västernorrlands län (Utflyttningslän)", " Jämtlands län (Utflyttningslän)", " Västerbottens län (Utflyttningslän)", " Norrbottens län (Utflyttningslän)"
                                          tid = "*", # Avsluta med 9999 för senaste år
                                          spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                          diag_senaste_ar = TRUE, # Skapar ett diagram för antingen in eller utflytt från ett län till alla andra valda län. Enbart senaste valda år
                                          diag_flera_ar = FALSE, # Skapar ett diagram per vald destination (in- eller utflyttningslan). Enbart intressant om flera år väljs
                                          diag_facet = FALSE, # Sätts till TRUE om man istället vill ha diag_flera_ar som ett facet-diagram
                                          demo = FALSE,                                     # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                          returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                          returnera_data = FALSE # True om användaren vill returnera data från funktionen
){

  # ===========================================================================================================
  # Stapeldiagram för inflyttningslan respektive utflyttningslan (i antal).Går att få för senaste år (1 diagram) eller över tid (flera diagram eller facet).
  # Skapad: 2024-06-25 av Jon
  # SCB har ändrat namn på variabel inrikes omflyttning mellan län till antal. Jag ändrar tillbaka med en mutate/Jon
  # Har justerat skriptet så att det tar hänsyn till CKM (tagit bort två variabler total in/utflytt vid datahämtning) Jon 2026-04-08
  # ===========================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på
  if (demo){
    # om diagramskriptet skriver ut flera diagram läggs länkarna som vektor i demo_url nedan
    demo_url <-
        c("https://region-dalarna.github.io/utskrivna_diagram/Flytt_fran_Dalarna.png",
          "https://region-dalarna.github.io/utskrivna_diagram/Flytt_fran_Dalarna_facet.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }


  if(length(inflyttningsl_klartext) > 1 && length(utflyttningsl_klartext) >1 ){
    stop("Max 1 län får väljas för antingen inflyttningsl_klartext eller utflyttningsl_klartext")
  }

  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # Hämtar samma data som tidigare kom via
  # hamta_data/hamta_inflyttningslan_utflyttningslan_kon_tid_scb.R (som
  # redan hade byggts om för CKM, se kommentar ovan), men direkt mot SCB:s
  # PxWeb-API v2 med pxweb2r, från två tabeller (CKM - röjandekontroll -
  # för nya årgångar, med en ny tabell från och med 2025):
  # - TAB4409 = gamla "InOmflytt", 2000-2024.
  # - TAB6672 = nya CKM-tabellen "InOmflyttCKM", från 2025.
  # Till skillnad från tidigare CKM-diagram är Kön elimineringsbart i BÅDA
  # tabellerna, så Kon = NA (utelämnas helt) fungerar likadant för båda -
  # ingen "totalkod"-krångel den här gången. In-/utflyttningslän är inte
  # elimineringsbara i någon av tabellerna, men eftersom en explicit
  # länklartext (eller "*" = alla) alltid skickas med räcker det - CKM-
  # tabellens extra "Totalt, samtliga in-/utflyttningslän"-kategorier
  # filtreras bort precis som i originalet.
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna.\nDiagramförklaring: Diagrammet avser flyttningar och inte personer.\nUnder ett år kan en person flytta fler än en gång."

  gg_list <- list()
  gg_list_map <- list()
  objektnamn <- c()
  objektnamn_map <- c()

  hamta_flytt <- function(table_id) {
    pxweb2r::pxweb2_get_data(
      table_id,
      query = list(InflyttningsL = inflyttningsl_klartext, UtflyttningsL = utflyttningsl_klartext, Kon = NA, Tid = tid),
      on_all_values_invalid = "null"
    )
  }
  flytt_hist <- hamta_flytt("TAB4409")
  flytt_ckm  <- hamta_flytt("TAB6672")

  har_ckm_data <- !is.null(flytt_ckm) && nrow(flytt_ckm) > 0
  ckm_fran_ar <- if (har_ckm_data) min(as.integer(flytt_ckm$år)) else NULL
  diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, har_ckm_data, fran_ar = ckm_fran_ar)

  inflytt_utflytt_df <- dplyr::bind_rows(flytt_hist, flytt_ckm) |>
    dplyr::rename(Antal_flyttar = value) |>
    dplyr::select(-dplyr::any_of(c("tabellinnehåll", "inflyttningslän_kod", "utflyttningslän_kod"))) |>
    dplyr::filter(Inflyttningslän != "Totalt, samtliga inflyttningslän",
                  Utflyttningslän != "Samtliga utflyttningslän")

  # Byter dåliga namn på regioner till bättre
  inflytt_utflytt_df <- dplyr::mutate(
    inflytt_utflytt_df,
    Inflyttningslän = rdverktyg::skapa_kortnamn_lan(stringr::str_trim(stringr::str_remove(Inflyttningslän, "\\(Inflyttningslän\\)"))),
    Utflyttningslän = rdverktyg::skapa_kortnamn_lan(stringr::str_trim(stringr::str_remove(Utflyttningslän, "\\(Utflyttningslän\\)")))
  )


  if(returnera_data == TRUE){
    if(length(unique(inflytt_utflytt_df$Inflyttningslän)) == 1){
    assign("inflytt_lan_df", inflytt_utflytt_df, envir = .GlobalEnv)
    } else {
      assign("utflytt_lan_df", inflytt_utflytt_df, envir = .GlobalEnv)
    }
  }

  if(diag_senaste_ar == TRUE){

    if(length(unique(inflytt_utflytt_df$Inflyttningslän)) == 1){
      diagram_titel<- paste0("Antal flyttar till ",unique(inflytt_utflytt_df$Inflyttningslän)," år ",max(inflytt_utflytt_df$år))
      diagramfil <- paste0("Flytt_till_",unique(inflytt_utflytt_df$Inflyttningslän),".png")
      objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))
      ut_df <- dplyr::filter(inflytt_utflytt_df, Utflyttningslän != unique(inflytt_utflytt_df$Inflyttningslän))
    } else {
      diagram_titel<- paste0("Antal flyttar från ",unique(inflytt_utflytt_df$Utflyttningslän)," år ",max(inflytt_utflytt_df$år))
      diagramfil <- paste0("Flytt_fran_",unique(inflytt_utflytt_df$Utflyttningslän),".png")
      objektnamn <- c(objektnamn, stringr::str_remove(diagramfil, ".png"))
      ut_df <- dplyr::filter(inflytt_utflytt_df, Inflyttningslän != unique(inflytt_utflytt_df$Utflyttningslän))
    }

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = dplyr::filter(ut_df, år == max(år)),
      skickad_x_var = ifelse(length(unique(ut_df$Inflyttningslän)) == 1,"Utflyttningslän","Inflyttningslän"),
      skickad_y_var = "Antal_flyttar",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = TRUE,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = "Antal flyttar",
      x_axis_sort_value = TRUE,
      manual_color = vald_farg,
      output_mapp = output_mapp_figur,
      skriv_till_diagramfil = spara_figur,
      filnamn_diagram = diagramfil
    )
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn

  }

  if(diag_flera_ar){

    skapa_diagram <- function(df, vald_region){

      if(length(unique(df$Inflyttningslän)) == 1){
        df <- dplyr::filter(df, Utflyttningslän %in% vald_region)
        if(length(unique(df$Utflyttningslän)) > 1){
          diagram_titel<- paste0("Antal flyttar till ",unique(df$Inflyttningslän)," från")
          diagramfil <- paste0("Flytt_till_",unique(df$Inflyttningslän),"_facet.png")
        }else{
          diagram_titel<- paste0("Antal flyttar till ",unique(df$Inflyttningslän)," från ",unique(df$Utflyttningslän))
          diagramfil <- paste0("Flytt_till_",unique(df$Inflyttningslän),"_fran_",unique(df$Utflyttningslän),".png")
        }
        objektnamn_map <- c(objektnamn_map, stringr::str_remove(diagramfil, ".png"))
        facet_var <- "Utflyttningslän"

      }else{
        df <- dplyr::filter(df, Inflyttningslän %in% vald_region)
        if(length(unique(df$Inflyttningslän)) > 1){
          diagram_titel<- paste0("Antal flyttar från ",unique(df$Utflyttningslän)," till")
          diagramfil <- paste0("Flytt_fran_",unique(df$Utflyttningslän),"_facet.png")
        }else{
          diagram_titel<- paste0("Antal flyttar från ",unique(df$Utflyttningslän)," till ",unique(df$Inflyttningslän))
          diagramfil <- paste0("Flytt_fran_",unique(df$Utflyttningslän),"_till_",unique(df$Inflyttningslän),".png")
        }

        objektnamn_map <- c(objektnamn_map, stringr::str_remove(diagramfil, ".png"))
        facet_var <- "Inflyttningslän"
      }

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = df,
        skickad_x_var = "år",
        skickad_y_var = "Antal_flyttar",
        diagram_titel = diagram_titel,
        diagram_capt = diagram_capt,
        stodlinjer_avrunda_fem = TRUE,
        manual_x_axis_text_vjust = 1,
        manual_x_axis_text_hjust = 1,
        manual_y_axis_title = "Antal flyttar",
        x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,4,NA),
        facet_grp = if (diag_facet) facet_var else NULL,
        facet_scale = "free",
        facet_legend_bottom = TRUE,
        manual_color = vald_farg[1],
        output_mapp = output_mapp_figur,
        skriv_till_diagramfil = spara_figur,
        filnamn_diagram = diagramfil
      )

      gg_list_map <- c(gg_list_map, list(gg_obj))
      names(gg_list_map) <- objektnamn_map
      return(gg_list_map)
    }

    if(length(unique(inflytt_utflytt_df$Inflyttningslän)) == 1){
      region_vekt <- unique(dplyr::pull(dplyr::filter(inflytt_utflytt_df, Utflyttningslän != unique(inflytt_utflytt_df$Inflyttningslän)), Utflyttningslän))
    } else {
      region_vekt <- unique(dplyr::pull(dplyr::filter(inflytt_utflytt_df, Inflyttningslän != unique(inflytt_utflytt_df$Utflyttningslän)), Inflyttningslän))
    }


    if (diag_facet) {
      diag <- skapa_diagram(inflytt_utflytt_df,region_vekt)

    } else {

      diag <- purrr::flatten(purrr::map(region_vekt, ~ skapa_diagram(inflytt_utflytt_df, .x)))

    }
    gg_list <- c(gg_list, diag)
  }

  if(returnera_figur==TRUE) return(gg_list)
}

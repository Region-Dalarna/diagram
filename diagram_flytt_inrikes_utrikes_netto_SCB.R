diagram_inr_utr_flytt <- function(region_vekt = "20", # Val av kommuner
                                  output_mapp_figur= NA, # Vart hamnar figur om den skall sparas
                                  vald_farg = NA, # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                  tid = "*", # Avsluta med 9999 för senaste år
                                  spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                  diag_facet = FALSE, # Sätts till TRUE om man istället vill ha ett facet-diagram
                                  diag_flyttnetto = TRUE, # Skapa diagram för flyttnetto
                                  diag_uppdelat = TRUE, # Skapa diagram för flyttnetto uppdelat
                                  demo = FALSE,                                     # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat    visa_totalvarden = TRUE,                          # skriver ut ett streck för netto både inrikes och utrikes födda
                                  returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                  returnera_data = FALSE # True om användaren vill returnera data från funktionen
){

  # ===========================================================================================================
  # Diagram för inrikes och utrikes flyttar. Vid flera regioner går det att välja mellan enskilda diagram eller facet
  # Skapad: 2024-04-24
  # Förbättringsmöjligheter: Går för tillfället inte att summera  flera regioner
  # Pga ändring nedan sparas ett extra ggplot (två för flyttnetto med lite olika namn). Jag ville inte ändra namnet på flyttnetto då detta skript kanske används av andra
  # Förbättringsmöjlighet: Ändra så att tre objekt skapas, Flyttnetto respektive inrikes och utrikes flyttningar (med dessa namn)
  # Pga CKM så blir det lite felaktigt när man summerar inrikes och utrikes flyttningar, snarare än man väljer variabeln Flyttningsöverskott. Detta har justerats i skriptet Jon 6/3-2026
  # ===========================================================================================================

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

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på
  if (demo){
    # om diagramskriptet skriver ut flera diagram läggs länkarna som vektor i demo_url nedan
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/Flyttnetto_Dalarna.png",
        "https://region-dalarna.github.io/utskrivna_diagram/Inrikes%20flyttningsöverskott_Dalarna.png",
        "https://region-dalarna.github.io/utskrivna_diagram/invandringsöverskott_Dalarna.png")

    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  # om ingen output-mapp anges, används rdverktyg::utskriftsmapp() (garanterat
  # tillgänglig - paketet krävs ovan, ingen anledning längre att kolla exists())
  if (all(is.na(output_mapp_figur))) output_mapp_figur <- rdverktyg::utskriftsmapp()

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("kon")
  if (all(is.na(vald_farg))) vald_farg <- rddiagram::diagramfarger("kon")


  if(diag_uppdelat == TRUE && "00" %in% region_vekt){
    stop("Region 00 (Riket) saknar inrikes flyttnetto och kan inte användas.\nÄndra region_vekt eller sätt diag_uppdelat = FALSE")
  }


  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."

  gg_list <- list()
  gg_list_uppdelat <- list()
  objektnamn_uppdelat <- c()
  objektnamn <- c()

  # Hämtar samma data som tidigare kom via
  # hamta_data/hamta_bef_flyttningar_region_alder_kon_scb.R, men direkt mot
  # SCB:s PxWeb-API v2 med pxweb2r, från två tabeller (CKM - röjandekontroll -
  # för nya årgångar, med en ny tabell från och med 2025):
  # - TAB1212 = gamla "Flyttningar97", 1997-2024. Ålder kan utelämnas
  #   (elimineringsbar).
  # - TAB6640 = nya CKM-tabellen "Flyttningar97CKM", från 2025. Ålder är INTE
  #   elimineringsbar där längre - måste anges explicit som totalkoden "tot"
  #   ("totalt ålder" - samma tabell som i åldersgrupps-diagrammet, som också
  #   har trippel-/fyrdubblerade koder för 100+/andra totalvarianter, men
  #   "tot" är den enda koden med just etiketten "totalt ålder"), vilket
  #   också gör den till en egen kolumn i svaret; select() plockar bort den
  #   igen innan bind_rows().
  # ContentsCode = "Flyttningsöverskott" hämtas direkt som egen variabel (i
  # stället för att summera ihop "Inrikes flyttningsöverskott" +
  # "Invandringsöverskott" själva) - se skriptkommentaren ovan om varför:
  # CKM gör att en egen summering av två separata celler kan avvika mer från
  # verkligheten än att hämta den redan färdigsummerade cellen direkt.
  hamta_flytt <- function(table_id, alder) {
    pxweb2r::pxweb2_get_data(
      table_id,
      query = list(Region = region_vekt, Kon = c("Kvinnor", "Män"), Alder = alder,
                   ContentsCode = c("Inrikes flyttningsöverskott", "Invandringsöverskott", "Flyttningsöverskott"),
                   Tid = tid),
      on_all_values_invalid = "null"
    )
  }

  flytt_hist <- hamta_flytt("TAB1212", NA)
  flytt_ckm  <- hamta_flytt("TAB6640", "tot")
  if (!is.null(flytt_ckm)) flytt_ckm <- dplyr::select(flytt_ckm, -dplyr::any_of("ålder"))

  har_ckm_data <- !is.null(flytt_ckm) && nrow(flytt_ckm) > 0
  ckm_fran_ar <- if (har_ckm_data) min(as.integer(flytt_ckm$år)) else NULL
  diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, har_ckm_data, fran_ar = ckm_fran_ar)

  flytt_df <- dplyr::bind_rows(flytt_hist, flytt_ckm) |>
    dplyr::rename(regionkod = region_kod, variabel = tabellinnehåll, varde = value)

  if(returnera_data == TRUE){
    assign("flytt_df", flytt_df, envir = .GlobalEnv)
  }

  skapa_diagram <- function(df, vald_region){

    df <- dplyr::filter(df, regionkod %in% vald_region)

    if(diag_flyttnetto == TRUE){

      df_tot_flytt <- dplyr::filter(df, variabel == "Flyttningsöverskott")

      if(diag_facet == FALSE){
        diff <- max(df_tot_flytt$varde) - min(df_tot_flytt$varde) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
        totalvarden_linjebredd <- 0.002*diff      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
        total_list <- list()
        unika_ar <- unique(df_tot_flytt$år)
        vald_regionkod = vald_region
        unika_reg <- unique(vald_regionkod)
        unika_reg_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(unika_reg)$region)

        for (reg in 1:length(unika_reg)) {
          for (ar in 1:length(unika_ar)){
            arsvarde <- df_tot_flytt |>
              dplyr::filter(år == unika_ar[ar], regionkod == unika_reg[reg]) |>
              dplyr::group_by(år, regionkod) |>
              dplyr::summarize(varde = sum(varde), .groups = "drop") |>
              dplyr::pull(varde)
            #arsvarde <- sum(arsvarde, na.rm = TRUE)
            total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))

          } # slut for-loop unika_ar
        } # slut for_loop unika_reg
      } else total_list <- NA

      reg_txt <- (rdverktyg::skapa_kortnamn_lan(unique(df$region), TRUE))[1]

      if(length(unique(df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Flyttnetto")
      }else{
        diagram_titel <- paste0("Flyttnetto i ", reg_txt)
      }

      #diagram_titel <- paste0("Flyttnetto i ", reg_txt)
      diagramfil <- paste0("Flyttnetto_", reg_txt, ".png")
      #objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))

      gg_obj <- rddiagram::SkapaStapelDiagram(
        skickad_df = df_tot_flytt,
        skickad_x_var = "år",
        skickad_y_var = "varde",
        skickad_x_grupp = "kön",
        diagram_titel = diagram_titel,
        diagram_capt = diagram_capt,
        #x_axis_storlek = 8,
        stodlinjer_avrunda_fem = TRUE,
        manual_x_axis_text_vjust = 1,
        manual_x_axis_text_hjust = 1,
        manual_y_axis_title = "",
        geom_position_stack = TRUE,
        x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
        facet_grp = if (length(unique(df$region)) > 1) "region" else NULL,
        facet_scale = "free",
        facet_legend_bottom = TRUE,
        legend_vand_ordning = TRUE,
        fokusera_varden = total_list,
        manual_color = vald_farg,
        output_mapp = output_mapp_figur,
        skriv_till_diagramfil = spara_figur,
        filnamn_diagram = diagramfil
      )

      dia_med_legend <- gg_obj +
        ggplot2::geom_line(ggplot2::aes(color="line"))+
        ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "Flyttnetto")+
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"),
              legend.box.just = "bottom")

      # 12x7 - samma standardstorlek som SkapaStapelDiagram() annars använder
      # (diagramfil_bredd/diagramfil_hojd) - var tidigare 8x6, vilket klippte
      # av bildtexten när CKM-noteringen gjorde den tvåradig.
      ggplot2::ggsave(paste0(output_mapp_figur, diagramfil), dia_med_legend, width = 12, height = 7, dpi = 300)

      gg_list <- c(gg_list, list(dia_med_legend))
      names(gg_list) <- stringr::str_remove(diagramfil, ".png")
    }

    if(diag_uppdelat == TRUE){

      skapa_diagram_uppdelat <- function(df,vald_variabel){

        #reg_txt <- ar_alla_kommuner_i_ett_lan(df$regionkod %>% unique(), returnera_text = TRUE)
        #if (reg_txt == FALSE) reg_txt <- df$region %>% unique() %>% skapa_kortnamn_lan(T) %>% paste0(collapse = ", ")

        reg_txt <- (rdverktyg::skapa_kortnamn_lan(unique(df$region), TRUE))[1]

        if(length(unique(df$region)) > 1){
          reg_txt <- paste0(reg_txt,"_facet")
          diagram_titel <- paste0(vald_variabel)
        }else{
          diagram_titel <- paste0(vald_variabel," ", reg_txt)
        }

        diagramfil <- paste0(vald_variabel, "_", reg_txt,".png")
        objektnamn_uppdelat <- c(objektnamn_uppdelat, stringr::str_remove(diagramfil, ".png"))

        gg_obj <- rddiagram::SkapaStapelDiagram(
          skickad_df = dplyr::filter(df, variabel == vald_variabel, !is.na(varde), varde != 0),
          skickad_x_var = "år",
          skickad_y_var = "varde",
          skickad_x_grupp = "kön",
          diagram_titel = diagram_titel,
          diagram_capt = diagram_capt,
          #x_axis_storlek = 8,
          stodlinjer_avrunda_fem = FALSE,
          manual_x_axis_text_vjust = 1,
          manual_x_axis_text_hjust = 1,
          manual_y_axis_title = "",
          geom_position_stack = TRUE,
          x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
          facet_grp = if (length(unique(df$region)) > 1) "region" else NULL,
          facet_scale = "free",
          facet_legend_bottom = TRUE,
          facet_x_axis_storlek = 6,
          manual_color = vald_farg,
          output_mapp = output_mapp_figur,
          skriv_till_diagramfil = spara_figur,
          filnamn_diagram = diagramfil
        )


        gg_list_uppdelat <- c(gg_list_uppdelat, list(gg_obj))
        names(gg_list_uppdelat) <- objektnamn_uppdelat
        return(gg_list_uppdelat)

      }

      diag_uppdelning <- purrr::flatten(purrr::map(unique(df$variabel), ~ skapa_diagram_uppdelat(df, .x)))
      gg_list <- c(gg_list, diag_uppdelning)
    }
    return(gg_list)
  }


  if (diag_facet) {
    diag <- skapa_diagram(flytt_df,region_vekt)

  } else {
    diag <- purrr::flatten(purrr::map(region_vekt, ~ skapa_diagram(flytt_df, .x)))

  }

  if(returnera_figur==TRUE) return(diag)
}

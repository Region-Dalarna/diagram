diag_inr_flyttnetto_inr_utr_fodda <- function(
    region_vekt = "20",                               # regionkod eller vektor med regionkoder
    gruppera_namn = NA,                               # om NA skapas ett diagram per region, annars grupperas de ihop och får namnet som anges här
    facet_diagram = FALSE,                            # om TRUE skapas ett diagram för alla regioner, annars ett diagram för varje region
    farg_vekt = rddiagram::diagramfarger("rd_gron")[c(1,4)],     # färgvektor för diagrammet
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nInrikes flyttnetto är skillnaden mellan de som flyttat in till och de som flyttat ut från en kommun/region, från och till andra kommuner/regioner",
    output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",
    skriv_diagram = TRUE,                             # TRUE om vi vill skriva ut diagrammet
    skriv_excel = FALSE,                              # TRUE om vi vill skriva ut data till excel
    returnera_data = FALSE,                           # TRUE om vi vill returnera data till R:s globala miljö
    #spara_som_svg = FALSE,                            # TRUE om vi vill spara diagrammet som svg
    filformat = "png",                                # filformat för diagrammet
    visa_totalvarden = TRUE,                          # skriver ut ett streck för netto både inrikes och utrikes födda
    fixa_y_axel_varden_jamna_tal = TRUE,              # TRUE om vi vill ha vettigare värden på y-axeln, men funkar inte alltid och då kan man stänga av detta.
    demo = FALSE,                                     # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat    visa_totalvarden = TRUE,                          # skriver ut ett streck för netto både inrikes och utrikes födda
    visa_totalvarden_dataetiketter = FALSE,           # skriver ut dataetiketter för totalvärdena
    totalvarden_dataetiketter_farg = "black",         # välj färg på totalstrecken
    totalvarden_dataetiketter_hjust = 20,             # justerar dataetiketter för totalvärden i höjdled
    totalvarden_dataetiketter_textstorlek = 2,        # justerar textstorlek för dataetiketter för totalvärden
    totalvarden_linjetjocklek = 4                     # tjocklek på totalstrecken i tiondels % av hela diffen i datasetet
) {

  # ===========================================================================================================
  # Diagram för inrikes flyttnetto uppdelat på bakgrund (inrikes och utrikes födda)
  # Går att skapa diagram för flera regioner samtidigt eller som ett facet-diagram
  # Senast uppdaterad: 2024-04-25
  # Förbättringsmöjligheter: Går för tillfället inte att summera  flera regioner
  # ===========================================================================================================

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på
  if (demo){
    demo_url <-
        "https://region-dalarna.github.io/utskrivna_diagram/Flyttnetto_bakgrund_Dalarna.png"
    browseURL(demo_url)
    rdverktyg::stop_tyst()
  }

  if("00" %in% region_vekt){
    stop("Region 00 (Riket) saknar inrikes flyttnetto och kan inte användas.\nÄndra region_vekt")
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

  gg_list <- list()
  objektnamn <- c()

  if (is.na(farg_vekt[1])) farg_vekt <- rddiagram::diagramfarger("rd_gron")

  # =====================================================================================================
  # Hämtar samma data som tidigare via pxweb_get() mot BE0101J/FlyttFodReg
  # (v1-API), men direkt mot SCB:s PxWeb-API v2 med pxweb2r, från två
  # tabeller (SCB bytte metod - CKM, röjandekontroll - för nya årgångar, med
  # en ny tabell från och med 2025):
  # - TAB4693 = gamla "FlyttFodReg", 2002-2024. Ålder kan utelämnas
  #   (elimineringsbar), ger då totalen automatiskt.
  # - TAB6657 = nya CKM-tabellen, från 2025. Ålder är INTE elimineringsbar
  #   där längre - måste anges explicit som totalkoden "TotSA" (som i
  #   befolknings-/flyttnetto-diagrammen), vilket också gör den till en egen
  #   kolumn i svaret; select() plockar bort den igen så tabellernas
  #   kolumner stämmer överens innan de binds ihop.
  # Födelseregion (född i Sverige/utrikes född) och kön (utelämnas, samma
  # motiv som i flyttnetto-per-åldersdiagrammet - CKM gör att en egen
  # summering av delarna kan avvika från den riktiga totalen) hanteras
  # likadant i båda tabellerna.
  hamta_flytt_bakgrund <- function(table_id, alder) {
    pxweb2r::pxweb2_get_data(
      table_id,
      query = list(Region = region_vekt, Kon = NA, Alder = alder,
                   Fodelseregion = c("född i Sverige", "utrikes född"),
                   ContentsCode = c("Flyttningsnetto, eget län", "Flyttningsnetto, övriga län"),
                   Tid = "*"),
      on_all_values_invalid = "null"
    )
  }

  flytt_hist <- hamta_flytt_bakgrund("TAB4693", NA)
  flytt_ckm  <- hamta_flytt_bakgrund("TAB6657", "TotSA")
  if (!is.null(flytt_ckm)) flytt_ckm <- dplyr::select(flytt_ckm, -dplyr::any_of("ålder"))

  har_ckm_data <- !is.null(flytt_ckm) && nrow(flytt_ckm) > 0
  ckm_fran_ar <- if (har_ckm_data) min(as.integer(flytt_ckm$år)) else NULL
  diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt, har_ckm_data, fran_ar = ckm_fran_ar)

  # Lägg ihop till px_df: döp om till regionkod/varde, gör om län till
  # kortnamn (riket -> Sverige). Flyttningsnetto totalt (eget län + övriga
  # län) fås genom att summera "varde" utan att gruppera på tabellinnehåll -
  # samma resultat som att summera två breda kolumner, men enklare i long-format.
  px_df <- dplyr::bind_rows(flytt_hist, flytt_ckm) |>
    dplyr::rename(regionkod = region_kod, varde = value) |>
    dplyr::mutate(region = rdverktyg::skapa_kortnamn_lan(region, byt_ut_riket_mot_sverige = TRUE))

  if (!is.na(gruppera_namn)){
    px_df <- px_df |>
      dplyr::group_by(år, födelseregion) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(varde, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(region = gruppera_namn)

  } else {
    px_df <- px_df |>
      dplyr::group_by(år, regionkod, region, födelseregion) |>
      dplyr::summarise(Inrikes_flyttnetto = sum(varde, na.rm = TRUE), .groups = "drop")
  }


  if (skriv_excel){
    reg_namn <- ifelse(!is.na(gruppera_namn), gruppera_namn, paste0(region_vekt, collapse = "_"))
    excelfil <- paste0("andel_arblosa_", min(arblosa_bakgr$tid), "_", max(arblosa_bakgr$tid) ,".xlsx")
    openxlsx::write.xlsx(px_df, paste0("Flyttnetto_", reg_namn, "_ar", min(px_df$år), "_", max(px_df$år), ".xlsx"), overwrite = TRUE)
  }

  # Returnerar data till R globala miljö
  if(returnera_data == TRUE){
    assign("flytt_bakgrund_df", px_df, envir = .GlobalEnv)
  }


  # ============================================= Skapa diagram ==============================================

  #for (reg in unique(px_df$region)) {
  skapa_diagram <- function(df,vald_regionkod) {  # skapa en funktion som skapar diagram för varje region

    df <- dplyr::filter(df, regionkod %in% vald_regionkod)

    #retur_list <- list()
    #vald_regionkod = "20"
    #chart_df <- px_df %>% filter(regionkod %in% vald_regionkod)

    reg_txt <- (rdverktyg::skapa_kortnamn_lan(unique(df$region), byt_ut_riket_mot_sverige = TRUE))[1]

    if(length(unique(df$region)) > 1){
      reg_txt <- paste0(reg_txt,"_facet")
      diagram_titel <- paste0("Inrikes flyttnetto")
    }else{
      diagram_titel <- paste0("Inrikes flyttnetto i ", reg_txt)
    }
    # =================================== visa totalvärden ========================================
    if (visa_totalvarden == TRUE && facet_diagram == FALSE){

      diff <- max(df$Inrikes_flyttnetto) - min(df$Inrikes_flyttnetto) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- diff * (totalvarden_linjetjocklek/1000)      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()
      unika_ar <- unique(df$år)
      unika_reg <- unique(vald_regionkod)
      unika_reg_txt <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(unika_reg)$region)

      for (reg in 1:length(unika_reg)) {
        for (ar in 1:length(unika_ar)){
          arsvarde <- dplyr::pull(
            dplyr::select(
              dplyr::filter(df, år == unika_ar[ar], regionkod == unika_reg[reg]),
              Inrikes_flyttnetto
            )
          )
          arsvarde <- sum(arsvarde, na.rm = TRUE)
          total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))
          if (visa_totalvarden_dataetiketter) {
            total_list <- c(total_list, list(list(geom = "text", y=arsvarde+totalvarden_dataetiketter_hjust, x = ar, size = totalvarden_dataetiketter_textstorlek, angle=0, fontface = "plain", label =arsvarde, color = totalvarden_dataetiketter_farg)))
          } # slut if-sats om man vill vissa dataetiketter
        } # slut for-loop unika_ar
      } # slut for_loop unika_reg
    } else total_list <- NA # slut if-sats visa_totalvärden

    # ======================= skapa ggplot-objekt =================

    #diagtitel_txt <- if (facet_diagram) " i" else paste0(" i ", reg_txt)

    #ar_alla_kommuner_i_ett_lan(vald_regionkod)

    #diagram_titel <- paste0("Inrikes flyttnetto", diagtitel_txt)
    diagramfil <- paste0("Flyttnetto_bakgrund_", paste0(reg_txt, collapse = "_"),".png")

    gg_obj <- rddiagram::SkapaStapelDiagram(
      skickad_df = df,
      skickad_x_var = "år",
      skickad_y_var = "Inrikes_flyttnetto",
      skickad_x_grupp = "födelseregion",
      diagram_titel = diagram_titel,
      diagram_capt = diagram_capt,
      stodlinjer_avrunda_fem = fixa_y_axel_varden_jamna_tal,
      manual_x_axis_text_vjust = 1,
      manual_x_axis_text_hjust = 1,
      manual_y_axis_title = "",
      geom_position_stack = TRUE,
      fokusera_varden = total_list,
      facet_grp = if (facet_diagram) "region" else NULL,
      skriv_till_diagramfil = !visa_totalvarden,
      manual_color = farg_vekt,
      #diagram_som_svg = spara_som_svg,
      diagram_bildformat = filformat,
      output_mapp = output_mapp,
      filnamn_diagram = diagramfil
    )

    if (visa_totalvarden){
      gg_obj <- gg_obj +
        ggplot2::geom_line(ggplot2::aes(color="line"))+
        ggplot2::scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto totalt")+
        ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white"),
              legend.box.just = "bottom")
    } # slut if-sats visa_totalvarden

    gg_list <- c(gg_list, list(gg_obj))

    if (skriv_diagram) {                           # skriv en diagramfil om så önskas
      rddiagram::skriv_till_diagramfil(gg_obj,
                            output_mapp = output_mapp,
                            filnamn_diagram = diagramfil)
    } # slut if-sats
    names(gg_list) <- stringr::str_remove(diagramfil, ".png")
    return(gg_list)
  } # slut skapa diagram-funktion för varje region


  if (facet_diagram) {
    diag <- skapa_diagram(px_df,region_vekt)

  } else {
    diag <- purrr::flatten(purrr::map(unique(region_vekt), ~skapa_diagram(px_df,.x)))

  }

  return(diag)
} # slut funktion

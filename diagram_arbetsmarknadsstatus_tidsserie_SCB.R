diagram_arbetsmarknadsstatus_tidsserie <-function(region_vekt = "20", # Max 1 region åt gången
                                        output_mapp_data = NA, # Outputmapp för data
                                        filnamn_data = "arbetsmarknadsstatus.xlsx", # Filnamn för datafil
                                        output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                        spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                        returnera_figur = TRUE, # Returnerar en figur
                                        fodelseregion_klartext = "totalt", # Finns även c("inrikes född", "utrikes född"). Bara 1 åt gången
                                        diagram_ej_upp = TRUE, # Diagram som inte är uppdelat (facet). Antingen totalt, eller inrikes/utrikes födda.
                                        diagram_facet = FALSE, # Dela upp diagrammet på födelseregion
                                        legend_rader = NULL, # Hur många rader vill man ha i legenden
                                        marginal_yaxis_facet = c(0,0), # Marginaler för y-axeln i facet-diagram
                                        alder_klartext = "20–64 år", #Välj enbart 1. Finns: "15–19 år" "16–19 år" "20–24 år" "25–29 år" "30–34 år" "35–39 år" "40–44 år" "45–49 år" "50–54 år" "55–59 år" "60–64 år" "65–69 år" "70–74 år" "15–74 år" "16–64 år" "16–65 år" "16–66 år" "20–64 år" "20–65 år" "20–66 år"
                                        valda_farger = rddiagram::diagramfarger("rus_gradient"), # Ändra till kon om man vill ha de färgerna. "rus_sex" (6 färger) räckte inte längre - tidsserien går från start_ar och växer med ett år i taget, och passerade 6 år hösten 2025. "rus_gradient" har 12 färger och ger dessutom en naturlig ljus-till-mörk-gradient över åren.
                                        returnera_data = FALSE, # Skall data returneras
                                        start_ar ="2020", # Startår för data. Finns från 2020
                                        data_namm = "arbetsmarknadsstatus_tidsserie"){ # Vad skall returnerat dataset heta. Viktigt om data returneras två gånger i samma projekt (annars skrivs de över)

  ## =================================================================================================================
  # Linjediagram för arbetslöshet för valda år
  # Går att skapa facet-diagram för inrikes/utrikes födda.
  # Skapad av Jon Frank (2024-04-18) -
  # Potentiell förbättring: Facet-diagram blir i samma skala av någon oklar anledning (har testat både fixed och free).
  #
  #
  # Uppdaterat och lagt till datahämtning via PXweb 2. Jon 2026-06-30
  # Rättat märkligt fel där SCB verkar ha förlängt strecket mellan åren i åldersgrupper. Jon 2026-07-17
  # =================================================================================================================
  # Bara paket, ingen source() mot funktioner-repot. Anropas med fullt
  # namespace (dplyr::filter() osv.) i stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()

  if(diagram_facet == TRUE){
    fodelseregion = "*"
    } else {
      fodelseregion = fodelseregion_klartext
    }

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/
  arbetsmarknadsstatus_df <- pxweb2r::pxweb2_get_data(
    table = "TAB6260",
    query = list(
      Region = region_vekt,
      Kon = "totalt",
      Alder = alder_klartext,
      Fodelseregion = fodelseregion,
      ContentsCode = "arbetslöshet",
      Tid = "*"
    )) |>
    dplyr::rename(varde = value,
           regionkod = region_kod,
           variabel = tabellinnehåll) |>
    dplyr::mutate(ar=substr(månad,1,4),
           manad_long=format(as.Date(paste(ar, stringr::str_sub(månad, 6,7),"1", sep = "-")), "%B"),
           Period=paste(ar, stringr::str_sub(månad, 6,7),sep = "-")) |>
    dplyr::select(-månad) |>
      dplyr::filter(ar>=start_ar)


  # Tar bort län i länsnamn och gör om riket till Sverige
  arbetsmarknadsstatus_df$region = rdverktyg::skapa_kortnamn_lan(arbetsmarknadsstatus_df$region,byt_ut_riket_mot_sverige = TRUE)

  # Sparar data
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    flik_lista=list("Arbetsmarknadsstatus" = arbetsmarknadsstatus_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp_data,filnamn_data))
  }

  # Returnerar data
  if(returnera_data == TRUE){
    assign(data_namm, arbetsmarknadsstatus_df, envir = .GlobalEnv)
  }

  arb_df = arbetsmarknadsstatus_df |>
    dplyr::mutate(region = ifelse(region=="Riket","Sverige",region),
           manad_long = stringr::str_to_title(manad_long),
           manad_long = factor(manad_long,levels =c("Januari","Februari","Mars","April","Maj","Juni","Juli","Augusti","September","Oktober","November","December")))


    if(diagram_ej_upp){

      if(fodelseregion_klartext == "totalt") diagram_titel <- paste0("Arbetslöshet i åldersgruppen " ,alder_klartext," i ",unique(arbetsmarknadsstatus_df$region))
      if(fodelseregion_klartext == "inrikes född")  diagram_titel <- paste0("Arbetslöshet för inrikes födda i åldersgruppen " ,alder_klartext," i ",unique(arbetsmarknadsstatus_df$region))
      if(fodelseregion_klartext == "utrikes född")  diagram_titel <- paste0("Arbetslöshet för utrikes födda i åldersgruppen " ,alder_klartext," i ",unique(arbetsmarknadsstatus_df$region))

      diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna"

      diagramfilnamn <- paste0("arbetslöshet_tidsserie_",fodelseregion_klartext,"_",unique(arbetsmarknadsstatus_df$region),".png")
      objektnamn <- c(objektnamn,stringr::str_remove(diagramfilnamn,".png"))

      gg_obj <- rddiagram::SkapaLinjeDiagram(
        skickad_df = arb_df |>
          dplyr::filter(födelseregion == fodelseregion_klartext),
        skickad_x_var = "manad_long",
        skickad_y_var = "varde",
        skickad_x_grupp = "ar",
        manual_color = valda_farger,
        lagga_till_punkter = TRUE,
        diagram_titel = diagram_titel,
        legend_rader = legend_rader,
        diagram_capt =  diagram_capt,
        output_mapp = output_mapp_figur,
        stodlinjer_avrunda_fem = TRUE,
        manual_y_axis_title = "procent",
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_figur)

        gg_list <- c(gg_list, list(gg_obj))

    }

    if(diagram_facet == TRUE){

      diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna"
      diagram_titel <- paste0("Arbetslöshet i åldersgruppen " ,alder_klartext," i ",unique(arbetsmarknadsstatus_df$region))

      diagramfilnamn <- paste0("arbetslöshet_tidsserie_facet_",fodelseregion_klartext,"_",unique(arbetsmarknadsstatus_df$region),".png")
      objektnamn <- c(objektnamn,stringr::str_remove(diagramfilnamn,".png"))

      gg_obj <- rddiagram::SkapaLinjeDiagram(
        skickad_df = arb_df |>
          dplyr::filter(födelseregion != "totalt") |>
            dplyr::mutate(födelseregion = factor(födelseregion,levels = c("inrikes född","utrikes född"))),
        skickad_x_var = "manad_long",
        skickad_y_var = "varde",
        skickad_x_grupp = "ar",
        facet_scale = "free",
        facet_grp = "födelseregion",
        facet_legend_bottom = TRUE,
        manual_color = valda_farger,
        marginal_y_axis = marginal_yaxis_facet,
        lagga_till_punkter = TRUE,
        diagram_titel = diagram_titel,
        legend_rader = legend_rader,
        diagram_capt =  diagram_capt,
        output_mapp = output_mapp_figur,
        stodlinjer_avrunda_fem = FALSE,
        manual_y_axis_title = "procent",
        filnamn_diagram = diagramfilnamn,
        skriv_till_diagramfil = spara_figur)

      gg_list <- c(gg_list, list(gg_obj))
    }


    names(gg_list) <- objektnamn

    if(returnera_figur == TRUE){
      return(gg_list)
    }

}

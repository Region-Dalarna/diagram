diag_trangboddhet_inrikes_utrikes <- function(region = "20", # Enbart ett i taget.
                                              visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                              logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
                                              output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                                              skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                                              diag_antal = TRUE, # Antal
                                              diag_andel = TRUE, # Andel, summerar till 100 procent
                                              jmf_ar = c("2012","2018","9999"), # 9999 ger senaste år, 2013 är första år
                                              returnera_data_rmarkdown = FALSE
) {
  
  
  # =======================================================================================================================
  #
  # Två diagram för trångboddhet uppdelat på inrikes och utrikes födda, finns som antal respektive andel
  # Används primärt i integrationsrapporten
  #
  # Uppdaterad med ny version av PXweb2 mm. Av oklar anledning hade Claude inte gjort detta. Jon 2026-09-17
  # =======================================================================================================================
  
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
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("tidyverse")
  
  
  gg_list <- list()
  
  # Hämta data med den nya versionen av PXweb
  trangboddhet_df <- pxweb2r::pxweb2_get_data(
    table = "TAB5089",
    query = list(
      Region = region,
      Trangboddhet = "*",
      Fodelseregion = "*",
      Alder = "totalt",
      Kon = "totalt",
      ContentsCode = "Samtliga i populationen",
      Tid = jmf_ar
    ))  |> 
    dplyr::filter(födelseregion != "totalt",
                  trångboddhet != "totalt") |> 
    dplyr::mutate(trångboddhet = dplyr::case_when(
      trångboddhet == "trångbodda enligt norm 2" ~ "Trångbodda",
      trångboddhet == "ej trångbodda enligt norm 2" ~ "Ej trångbodda",
      TRUE ~ trångboddhet
    ),
    födelseregion = ifelse(födelseregion == "Sverige", "Inrikes född", "Utrikes född"),
    region = rdverktyg::skapa_kortnamn_lan(region))  |> 
    dplyr::group_by(region, år, födelseregion,trångboddhet,)  |> 
    dplyr::summarise(varde = sum(value,na.rm = TRUE))  |> 
    dplyr::mutate(andel = varde/sum(varde)*100)  |> 
    dplyr::ungroup()
  
  
  
  if(returnera_data_rmarkdown == TRUE){
    assign("trangboddhet_df", trangboddhet_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Trångbodd enligt norm 2. Norm två infördes på 1960-talet och definieras\nsom högst två personer per rum och dessutom ska det finnas ett kök och ett vardagsrum."
  
  # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  trangboddhet_df$trångboddhet <- factor(trangboddhet_df$trångboddhet, levels = rev(c("Trångbodda","Ej trångbodda",
                                                                                      "uppgift saknas")))
  
  if(diag_antal == TRUE){
    
    diagramtitel <- paste0("Antal trångbodda och ej trångbodda hushåll i  ",unique(trangboddhet_df$region))
    diagramfilnamn <- paste0("trangboddhet_antal_",unique(trangboddhet_df$region),".png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = trangboddhet_df  |>  
                                              dplyr::filter(trångboddhet != "uppgift saknas"),
                                            skickad_x_var = "trångboddhet",
                                            skickad_y_var = "varde",
                                            skickad_x_grupp = "år",
                                            manual_color = rddiagram::diagramfarger("rus_sex"),
                                            facet_grp = "födelseregion",
                                            facet_scale = "fixed",
                                            facet_legend_bottom = TRUE, 
                                            stodlinjer_avrunda_fem = TRUE,
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt,
                                            manual_y_axis_title = "",
                                            manual_x_axis_title = "",
                                            x_axis_lutning = 0,
                                            lagg_pa_logga = visa_logga_i_diagram,
                                            logga_path = logga_sokvag,
                                            output_mapp = output_mapp,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn  |>  stringr::str_remove(".png")
    
  }
  
  if(diag_andel == TRUE){
    diagramtitel <- paste0("Andel trångbodda och ej trångbodda hushåll i  ",unique(trangboddhet_df$region))
    diagramfilnamn <- paste0("trangboddhet_andel_",unique(trangboddhet_df$region),".png")
    
    # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = trangboddhet_df ,
                                            skickad_x_var = "år",
                                            skickad_y_var = "andel",
                                            skickad_x_grupp = "trångboddhet",
                                            manual_color = rddiagram::diagramfarger("rus_sex"),
                                            facet_grp = "födelseregion",
                                            facet_scale = "fixed",
                                            geom_position_stack = TRUE,
                                            facet_legend_bottom = TRUE, 
                                            stodlinjer_avrunda_fem = TRUE,
                                            procent_0_100_10intervaller = TRUE,
                                            legend_vand_ordning = TRUE,
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt,
                                            manual_y_axis_title = "procent",
                                            manual_x_axis_title = "",
                                            x_axis_lutning = 0,
                                            lagg_pa_logga = visa_logga_i_diagram,
                                            logga_path = logga_sokvag,
                                            output_mapp = output_mapp,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn  |>  stringr::str_remove(".png")
  }
  
  return(gg_list)
  
}

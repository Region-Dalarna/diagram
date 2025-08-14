diagram_ek_stod_bakgrund_SCB <- function(region_vekt = "20",
                                            visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                            logga_sokvag = NA,  
                                            diag_bakgrund = TRUE,
                                            diag_totalt = TRUE,
                                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                            alder_klartext = "15-74 år",			 #  Finns: "15-19 år", "20-24 år", "25-54 år", "55-64 år", "65-74 år", "15-74 år", "16-64 år", "16-65 år", "20-64 år", "20-65 år" 
                                            ta_bort_nast_sista_varde = TRUE, # Ta bort näst sista värdet på x-axeln
                                            skriv_diagrambildfil = FALSE, # Skall diagrammet sparas
                                            spara_figur = TRUE, # Skall diagrammet sparas
                                            returnera_data_rmarkdown = FALSE # Skall data returneras till global enviroment
){
  
  # Diagram som skapar två figurer för ekonomiskt stöd, används både i det samhällsekonomiska läget i Dalarna och integrationsrapporten. API från SCB
  # Om man vill veta vad ekonomiskt stöd innefattar: https://www.scb.se/contentassets/592dcafe2a3b4e65b8e5434796bab0af/huvudsaklig-inkomstkalla-och-arbetsrelaterad-inkomstniva_x.pdf
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         here,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bas_huvink_region_huvudfot1m_kon_alder_fodelseregion_tid_ArbStatFoT1_scb.R")
  
  ekonomiskt_bistand_df<- hamta_bas_huvink_region_huvudfot1m_kon_alder_fodelseregion_tid_scb(region = "20",
                                                                                             huvudfot1m_klartext = "ekonomiskt stöd",
                                                                                             fodelseregion_klartext = "*",
                                                                                             cont_klartext = "antal totalt",
                                                                                             alder_klartext = alder_klartext,
                                                                                             kon_klartext = "totalt")%>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  # Fixar lite med data
  ekonomiskt_bistand_df <- ekonomiskt_bistand_df %>% 
    rename(antal = `antal totalt`) %>% 
    manader_bearbeta_scbtabeller()
  
  gg_list <- list()
  
  if(returnera_data_rmarkdown == TRUE){
    assign("ekonomiskt_stod_df", ekonomiskt_bistand_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Antal individer som har ekonomiskt stöd som huvudsaklig inkomstkälla."
  
  if(diag_totalt){
  
    diagram_titel <- paste0("Antal individer ",unique(ekonomiskt_bistand_df$ålder), " med ekonomiskt stöd i ",unique(ekonomiskt_bistand_df$region))
    diagramfilnamn <- paste0("ekonomiskt_bistand_alla_",unique(ekonomiskt_bistand_df$region),".png")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = ekonomiskt_bistand_df %>% 
                                  filter(födelseregion == "totalt"),
                                skickad_x_var = "månad_år", 
                                skickad_y_var = "antal",
                                berakna_index = FALSE,
                                diagram_titel = diagram_titel,
                                manual_color = diagramfarger("rus_sex")[1],
                                x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = ta_bort_nast_sista_varde,
                                diagram_capt =  diagram_capt,
                                stodlinjer_avrunda_fem = TRUE,
                                manual_y_axis_title = "",
                                x_axis_visa_var_xe_etikett = 6,
                                output_mapp = output_mapp,
                                filnamn_diagram = diagramfilnamn,
                                lagg_pa_logga = visa_logga_i_diagram,
                                skriv_till_diagramfil = skriv_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  if(diag_bakgrund){
  
    diagram_titel <- paste0("Antal individer ",unique(ekonomiskt_bistand_df$ålder), " med ekonomiskt stöd i ",unique(ekonomiskt_bistand_df$region))
    diagramfilnamn <- paste0("ekonomiskt_bistand_fodelseland_",unique(ekonomiskt_bistand_df$region),".png")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = ekonomiskt_bistand_df %>% 
                                  filter(födelseregion != "totalt"),
                                skickad_x_var = "månad_år", 
                                skickad_y_var = "antal",
                                skickad_x_grupp = "födelseregion",
                                berakna_index = FALSE,
                                diagram_titel = diagram_titel,
                                manual_color = rev(diagramfarger("rus_sex")[1:2]),
                                x_axis_var_xe_etikett_ta_bort_nast_sista_vardet = ta_bort_nast_sista_varde,
                                diagram_capt =  diagram_capt,
                                stodlinjer_avrunda_fem = TRUE,
                                manual_y_axis_title = "",
                                x_axis_visa_var_xe_etikett = 6,
                                output_mapp = output_mapp,
                                filnamn_diagram = diagramfilnamn,
                                lagg_pa_logga = visa_logga_i_diagram,
                                skriv_till_diagramfil = skriv_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
    return(gg_list)
 
  
}

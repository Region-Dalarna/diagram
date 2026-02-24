diagram_befolkningsforandring_ar <- function(region_vekt = "20", # Val av kommuner
                                             output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                             vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                             spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                             diag_folkmangd = TRUE, # Skapa diagram för flyttnetto
                                             diag_facet = FALSE, # Sätts till TRUE om man istället vill ha ett facet-diagram
                                             tid = "*",# Finns från 1968 till senaste år (som skrivs "9999")
                                             etiketter_xaxel = 4, # Intervall för etiketter på x-axeln (ej för facet där 12 används automatiskt)
                                             kon_klartext = NA, # Alternativet är c("kvinnor","män") där det görs en uppdelning på kön
                                             diag_forandring = TRUE, # Skapa diagram för flyttnetto uppdelat
                                             avrunda_fem = TRUE, # Avrunda till närmaste fem på y-axeln
                                             returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                             returnera_data = TRUE # True om användaren vill returnera data från funktionen
){
  
  # ===========================================================================================================
  # Diagram för befolkningsutveckling och folkökning på årsbasis. Vid flera regioner går det att välja mellan enskilda diagram eller facet
  # Uppdelning på kön är möjlig
  # Skapad: 2024-04-23
  # Förbättringsmöjligheter: Går för tillfället inte att summera  flera regioner
  # Uppdaterat så att ggplot-objekten inte innehåller år i sina namn (för att undvika problem i rapporter). /Jon
  # ===========================================================================================================
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_folkmangd_alder_kon_ar_civilstand_scb_CKM_2025.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."
  
  gg_list <- list()
  objektnamn <- c()
  
  befolkning_df <- suppress_specific_warning(
    hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt,
                                                        tid_koder = tid,
                                                        kon_klartext = kon_klartext,
                                                        cont_klartext = c("folkmängd", "folkökning"))) 
  
  befolkning_df_CKM <- suppress_specific_warning(
    hamta_folkmangd_civilstand_alder_kon_ar_CKM(region_vekt = region_vekt,
                                                     tid_koder = tid,
                                                     alder_koder = "TotSA",
                                                     civilstand_klartext = "totalt, samtliga civilstånd",
                                                     kon_klartext = kon_klartext,
                                                     cont_klartext = c("folkmängd", "folkökning"))) %>% 
    select(-c(ålder, civilstånd))
  
  befolkning_df <- bind_rows(befolkning_df,befolkning_df_CKM)
  
  if(returnera_data == TRUE){
    assign("befolkning_df", befolkning_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(bef, vald_region){
    if(diag_folkmangd == TRUE){
      
      ut_df <- bef %>% 
        filter(regionkod %in% vald_region)
      
      reg_txt <- (ut_df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(ut_df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Folkmängd")
      }else{
        diagram_titel <- paste0("Folkmängd i ", reg_txt)
      }

      #diagramfil <- paste0("Folkmangd_", reg_txt, "_ar_", min(ut_df$år), "_", max(ut_df$år), ".png")
      diagramfil <- paste0("Folkmangd_", reg_txt, ".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      if(length(kon_klartext)>1){
        farg = diagramfarger("kon")} else{
          farg = vald_farg[1]
        }
      
      gg_obj <- SkapaStapelDiagram(skickad_df = ut_df %>% 
                                     filter(variabel == "Folkmängd"), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = ifelse(length(kon_klartext)==1,NA,"kön"),
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   #x_axis_storlek = 8,
                                   x_axis_visa_var_xe_etikett = ifelse(length(unique(ut_df$region)) > 1,12,etiketter_xaxel),
                                   stodlinjer_avrunda_fem = avrunda_fem,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = TRUE,
                                   diagram_facet = length(unique(ut_df$region)) > 1,
                                   facet_grp = "region",
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   legend_vand = TRUE,
                                   manual_color =farg ,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
    }
    
    
    if(diag_forandring == TRUE){
      # # Skapa ny variabel för befolkningsförändring
      # befolkning_df_forandring <- befolkning_df %>% 
      #   filter(förändringar == "folkökning") %>%
      #   mutate(kategori = ifelse(personer>0,"Ökad befolkning","Minskad befolkning"))
      ut_df <- bef %>% 
        filter(regionkod %in% vald_region)
      
      reg_txt <- (ut_df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(ut_df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Befolkningsutveckling")
      }else{
        diagram_titel <- paste0("Befolkningsutveckling i ", reg_txt)
      }
      
      
      #diagramfil <- paste0("Befolkningsutveckling_", reg_txt, "_ar_", min(ut_df$år), "_", max(ut_df$år), ".png")
      diagramfil <- paste0("Befolkningsutveckling_", reg_txt, ".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      if(length(kon_klartext)>1) vald_farg = diagramfarger("kon")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = ut_df %>% 
                                     filter(variabel == "Folkökning",år>min(år)) %>%
                                     mutate(kategori = ifelse(varde>=0,"Ökad befolkning","Minskad befolkning")), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = ifelse(length(kon_klartext)==1,"kategori","kön"),
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   #x_axis_storlek = 8,
                                   x_axis_visa_var_xe_etikett = ifelse(length(unique(ut_df$region)) > 1,12,etiketter_xaxel),
                                   stodlinjer_avrunda_fem = avrunda_fem,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = TRUE,
                                   diagram_facet = length(unique(ut_df$region)) > 1,
                                   facet_grp = "region",
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   legend_vand = TRUE,
                                   manual_color = vald_farg,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      
    }
    names(gg_list) <- objektnamn
    return(gg_list)
  }
  
  if (diag_facet) {
    diag <- skapa_diagram(befolkning_df,region_vekt)
    
  } else {
    diag <- map(region_vekt, ~ skapa_diagram(befolkning_df, .x)) %>% purrr::flatten()
    
  }
  
  if(returnera_figur==TRUE)
    return(diag)
}

diagram_inr_utr_flytt <- function(region_vekt = "20", # Val av kommuner
                                  output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                  vald_farg = diagramfarger("kon"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                  tid = "*", # Avsluta med 9999 för senaste år
                                  spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                  diag_facet = FALSE, # Sätts till TRUE om man istället vill ha ett facet-diagram
                                  diag_flyttnetto = TRUE, # Skapa diagram för flyttnetto
                                  diag_uppdelat = TRUE, # Skapa diagram för flyttnetto uppdelat
                                  returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                  returnera_data = FALSE # True om användaren vill returnera data från funktionen
){
  
  # ===========================================================================================================
  # Diagram för inrikes och utrikes flyttar. Vid flera regioner går det att välja mellan enskilda diagram eller facet
  # Skapad: 2024-04-24
  # Förbättringsmöjligheter: Går för tillfället inte att summera  flera regioner
  # ===========================================================================================================
  
  if(diag_uppdelat == TRUE && "00" %in% region_vekt){
    stop("Region 00 (Riket) saknar inrikes flyttnetto och kan inte användas.\nÄndra region_vekt eller sätt diag_uppdelat = FALSE")
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."
  
  gg_list <- list()
  gg_list_uppdelat <- list()
  objektnamn_uppdelat <- c()
  objektnamn <- c()
  
  flytt_df <- hamta_bef_flyttningar_region_alder_kon_scb(region_vekt = region_vekt,
                                                         cont_klartext = c("Inrikes flyttningsöverskott", "Invandringsöverskott"),
                                                         kon_klartext = c("Kvinnor", "Män"),
                                                         tid_koder = tid)
  
  if(returnera_data == TRUE){
    assign("flytt_df", flytt_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(df, vald_region){
    
    df <- df %>% filter(regionkod %in% vald_region)
    
    if(diag_flyttnetto == TRUE){
      
      if(diag_facet == FALSE){
        diff <- max(df$varde) - min(df$varde) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
        totalvarden_linjebredd <- 0.002*diff      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
        total_list <- list()
        unika_ar <- unique(df$år)
        vald_regionkod = vald_region
        unika_reg <- unique(vald_regionkod)
        unika_reg_txt <- hamtaregion_kod_namn(unika_reg)$region %>% skapa_kortnamn_lan()
        
        for (reg in 1:length(unika_reg)) { 
          for (ar in 1:length(unika_ar)){
            arsvarde <- df %>% 
              filter(år == unika_ar[ar],
                     regionkod == unika_reg[reg]) %>% 
              group_by(år,regionkod) %>% 
              summarize(varde = sum(varde)) %>% 
              .$varde 
            #dplyr::pull()
            #arsvarde <- sum(arsvarde, na.rm = TRUE)
            total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))
            
          } # slut for-loop unika_ar
        } # slut for_loop unika_reg   
      } else total_list <- NA
      
      reg_txt <- (df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Flyttnetto")
      }else{
        diagram_titel <- paste0("Flyttnetto i ", reg_txt)
      }
      
      #diagram_titel <- paste0("Flyttnetto i ", reg_txt)
      diagramfil <- paste0("Flyttnetto_", reg_txt, "_ar_", min(df$år), "_", max(df$år), ".png")
      #objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df, 
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
                                   diagram_facet = length(unique(df$region)) > 1,
                                   x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
                                   facet_grp = "region",
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   legend_vand = TRUE,
                                   fokusera_varden = total_list,
                                   manual_color = vald_farg,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = FALSE,
                                   filnamn_diagram = diagramfil)
      
      dia_med_legend <- gg_obj +
        geom_line(aes(color="line"))+
        scale_color_manual(name = "", values = c("line" = "black"), labels = "Flyttnetto")+
        theme(legend.key = element_rect(fill = "white"),
              legend.box.just = "bottom")
      
      ggsave(paste0(output_mapp_figur, diagramfil), dia_med_legend, width = 8, height = 6, dpi = 300)
      
      gg_list <- c(gg_list, list(dia_med_legend))
      names(gg_list) <- diagramfil %>% str_remove(".png")
    }
    
    if(diag_uppdelat == TRUE){
      
      skapa_diagram_uppdelat <- function(df,vald_variabel){
        
        #reg_txt <- ar_alla_kommuner_i_ett_lan(df$regionkod %>% unique(), returnera_text = TRUE)
        #if (reg_txt == FALSE) reg_txt <- df$region %>% unique() %>% skapa_kortnamn_lan(T) %>% paste0(collapse = ", ")
        
        reg_txt <- (df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
        
        if(length(unique(df$region)) > 1){
          reg_txt <- paste0(reg_txt,"_facet")
          diagram_titel <- paste0(vald_variabel)
        }else{
          diagram_titel <- paste0(vald_variabel," ", reg_txt)
        }
        
        diagramfil <- paste0(vald_variabel, "_", reg_txt, "_ar_", min(df$år), "_", max(df$år), ".png")
        objektnamn_uppdelat <- c(objektnamn_uppdelat,diagramfil %>% str_remove(".png"))
        
        gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                       filter(variabel == vald_variabel) %>% 
                                       filter(!is.na(varde),
                                              varde != 0), 
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
                                     diagram_facet = length(unique(df$region)) > 1,
                                     x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
                                     facet_grp = "region",
                                     facet_scale = "free",
                                     facet_legend_bottom = TRUE,
                                     facet_x_axis_storlek = 6,
                                     manual_color = vald_farg,
                                     output_mapp = output_mapp_figur,
                                     skriv_till_diagramfil = spara_figur,
                                     filnamn_diagram = diagramfil)
        
        
        gg_list_uppdelat <- c(gg_list_uppdelat, list(gg_obj))
        names(gg_list_uppdelat) <- objektnamn_uppdelat
        return(gg_list_uppdelat)
        
      }

      diag_uppdelning <- map(unique(df$variabel), ~ skapa_diagram_uppdelat(df , .x)) %>% flatten
      gg_list <- c(gg_list, diag_uppdelning)
    }
    return(gg_list)
  }
  
  
  if (diag_facet) {
    diag <- skapa_diagram(flytt_df,region_vekt)
    
  } else {
    diag <- map(region_vekt, ~ skapa_diagram(flytt_df, .x)) %>% flatten()
    
  }
  
  if(returnera_figur==TRUE) return(diag)
}

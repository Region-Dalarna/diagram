diagram_fodelsenetto <- function(region_vekt = "20", # Val av kommuner
                               output_mapp = NA, # Vart hamnar figur om den skall sparas
                               vald_farg = NA, # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                               spara_diagrambild = TRUE, # Sparar figuren till output_mapp
                               diag_facet = FALSE, # Skall ett facetdiagram skapas
                               visa_totalvarden = TRUE, # Visa totalvärden i diagrammet. Funkar om diag_facet = FALSE
                               etiketter_xaxel = 4, # Intervall för etiketter på x-axeln
                               tid = "*", # Välj tid, finns från 1968 till senaste år (som skrivs "9999")
                               returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                               returnera_dataframe_global_environment = FALSE, # True om användaren vill returnera data från funktionen
                               diagram_capt = "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna.",
                               demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                               ) {
  
  # ===========================================================================================================
  # Diagram för födelsenettot (födda - döda). Finns som facet eller enskilda diagram. Ej uppdelat på kön
  # Skapat 2024-04-23
  # Förbättringsmöjligheter: Svart linje för födelsenetto funkar inte med facet.
  # ===========================================================================================================
  
# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- c("https://region-dalarna.github.io/utskrivna_diagram/fodelsenetto_Dalarna.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_fodda_moderns_alder_region_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_doda_alder_kon_region_scb.R")
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(vald_farg))) {
    if (exists("diagramfarger", mode = "function")) {
      vald_farg <- diagramfarger("rus_sex")
    } else {
      vald_farg <- hue_pal()(9)
    }
  }
  
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  
  gg_list <- list()
  objektnamn <- c()
  
  fodda_df <- hamta_fodda_moderns_alder_region_scb(region_vekt = region_vekt,
                                                   alder_moder = "tot",
                                                   tid_koder = tid) %>% 
    select(-'moderns ålder')
  
  doda_df <- hamta_doda_alder_kon_region_scb(region_vekt = region_vekt,
                                             alder = "*",
                                             tid_koder = tid) %>% 
    filter(ålder == "totalt ålder") %>% 
    select(-'ålder')
  
  df <- fodda_df %>% 
    left_join(doda_df, by = c("regionkod","region", "år")) %>% 
    mutate(netto = födda-Döda)%>% 
    pivot_longer(cols = c("födda", "Döda", "netto"),
                 names_to = "variabel",
                 values_to = "varde")
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("fodda_doda_df", df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(df, vald_region){ 
    
    df <- df %>% 
      filter(regionkod %in% vald_region)
    
    if (visa_totalvarden == TRUE && diag_facet == FALSE){
      
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
            filter(variabel == "netto") %>% .$varde 
          #dplyr::pull()
          #arsvarde <- sum(arsvarde, na.rm = TRUE)
          total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))
          
        } # slut for-loop unika_ar
      } # slut for_loop unika_reg        
    } else total_list <- NA # slut if-sats visa_totalvärden
    
    reg_txt <- (df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
    
    if(length(unique(df$region)) > 1){
      reg_txt <- paste0(reg_txt,"_facet")
      diagram_titel <- paste0("Födelsenetto")
      
    }else{
      diagram_titel <- paste0("Födelsenetto i ", reg_txt) 
    }
    
    diagramfil <- paste0("fodelsenetto_", reg_txt,".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>%
                                   filter(variabel != "netto") %>% 
                                   mutate(varde = ifelse(variabel=="Döda",varde*-1,varde),
                                          variabel = stringr::str_to_title(variabel)), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 #x_axis_storlek = 8,
                                 x_axis_visa_var_xe_etikett = ifelse(length(unique(df$region)) > 1,12,etiketter_xaxel),
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 geom_position_stack = TRUE,
                                 diagram_facet = length(unique(df$region)) > 1,
                                 facet_grp = "region",
                                 facet_scale = "free",
                                 facet_legend_bottom = TRUE,
                                 fokusera_varden = total_list,
                                 legend_vand = TRUE,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp,
                                 skriv_till_diagramfil = FALSE,
                                 filnamn_diagram = diagramfil)
    
    dia_med_legend <- gg_obj +
      geom_line(aes(color="line"))+
      scale_color_manual(name = "", values = c("line" = "black"), labels = "Födelsenetto")+
      theme(legend.key = element_rect(fill = "white"),
            legend.box.just = "bottom")
    
    if(spara_diagrambild == TRUE){
      ggsave(paste0(output_mapp, diagramfil), dia_med_legend, width = 8, height = 6, dpi = 300)
    }
    
    gg_list <- c(gg_list, list(dia_med_legend))
    names(gg_list) <- objektnamn
    
    return(gg_list)
  }
  
  if (diag_facet) {
    diag <- skapa_diagram(df,region_vekt)
    
  } else {
    diag <- map(region_vekt, ~ skapa_diagram(df, .x)) %>% flatten()
    
  }
  
  if(returnera_figur == TRUE){
    return(diag)
  }
}

diagram_inrikes_flytt_alder <- function(region_vekt = "20", # Val av kommuner
                                        output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                        gruppera_namn = NA, # Välj namn på gruppen
                                        vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                        spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                        tid = "*", # Vilken tid skall användas
                                        diag_flyttnetto_alder = TRUE, # Skapa diagram för flyttnetto
                                        diag_alder_fokus = TRUE, # Skapa diagram för flyttnetto uppdelat
                                        diag_facet = FALSE, # Skall diagrammet göras med facet eller ej
                                        alder_grupp = c(20, 30, 40, 50, 60), # Vilka åldersgrupper skall användas. Välj enligt principen upp till första, sedan intervall mellan och sedan från sista
                                        alder_grupp_fokus = "20-29 år", # Vilken åldersgrupp skall fokuseras i diag_alder_fokus. Måste finnas bland grupperna ovan
                                        valda_ar = c("2021","2022","2023"), # Vilka år skall användas i diag_flyttnetto_alder
                                        returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                        returnera_data = TRUE
){
  
  # ===========================================================================================================
  # Diagram för inrikes flyttar uppdelat på åldersgrupper. Går även att fokusera på en specifik åldersgrupp. 
  # Finns både med och utan facet. Det är även möjligt att skapa grupper av regioner. Gruppen namnges med gruppera_namn
  # Skapad: 2024-04-24
  # Uppdatering: Ändrat så att det går att gruppera på namn. 
  # ===========================================================================================================
  
  if("00" %in% region_vekt){
    stop("Region 00 (Riket) saknar inrikes flyttnetto och kan inte användas.\nÄndra region_vekt")
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."
  
  gg_list <- list()
  objektnamn <- c()
  
  flytt_df <- hamta_bef_flyttningar_region_alder_kon_scb(region_vekt = region_vekt,
                                                         cont_klartext = c("Inrikes flyttningsöverskott", "Invandringsöverskott"),
                                                         kon_klartext = c("Kvinnor", "Män"),
                                                         tid_koder = tid ,
                                                         alder_koder = "*") %>%
    filter(ålder != "totalt ålder") %>% 
    mutate(alder_grupper = skapa_aldersgrupper(ålder,alder_grupp))
  
  if(!is.na(gruppera_namn)){
    # Tar bort regionkod och region i gruppering vilket ger en summering på grupp-nivå
    flytt_df <- flytt_df %>% 
      group_by(år,variabel , alder_grupper) %>% 
      summarize(varde = sum(varde)) %>% 
      mutate(region = gruppera_namn)
    # Regionvekt måste sättas till ett värde för att map-funktionen bara skall köra en gång
    # Används inte vid gruppera namn
    region_vekt = "00"
    
  } else {
    flytt_df <- flytt_df %>% 
      group_by(år,regionkod,region, variabel , alder_grupper) %>% 
      summarize(varde = sum(varde)) %>% 
      ungroup()
  }
  
  # Returnerar data till R globala miljö
  if(returnera_data == TRUE){
    assign("flytt_aldersgrupper_df", flytt_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(df, vald_region){ # Start map-funktion
    if(diag_flyttnetto_alder == TRUE){
      
      if(is.na(gruppera_namn)) df <- df %>% filter(regionkod %in% vald_region)
      
      reg_txt <- (df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Inrikes flyttnetto")
      }else{
        diagram_titel <- paste0("Inrikes flyttnetto i ", reg_txt)
      }
      
      diagramfil <- paste0("Inrikes_flyttnetto_alder_", reg_txt,".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                     filter(år%in%valda_ar,
                                            variabel == "Inrikes flyttningsöverskott"), 
                                   skickad_x_var = "alder_grupper", 
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = "år",
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = FALSE,
                                   diagram_facet = length(unique(df$region)) > 1,
                                   facet_grp = "region",
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   facet_x_axis_storlek = 6,
                                   legend_vand = FALSE,
                                   dataetiketter = ifelse(diag_facet==TRUE,FALSE,TRUE),
                                   manual_color = vald_farg,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
    }# diag_flyttnetto_alder
    
    if(diag_alder_fokus == TRUE){
      
      if(is.na(gruppera_namn)) df <- df %>% filter(regionkod %in% vald_region)
      
      reg_txt <- (df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Inrikes flyttnetto (",alder_grupp_fokus ,")")
      }else{
        diagram_titel <- paste0("Inrikes flyttnetto (",alder_grupp_fokus ,") i ", reg_txt)
      }
      
      diagramfil <- paste0("Inrikes flyttnetto_alder_", reg_txt,".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                     filter(alder_grupper == alder_grupp_fokus, 
                                            variabel == "Inrikes flyttningsöverskott"), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "varde",
                                   #skickad_x_grupp = "år",
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   stodlinjer_avrunda_fem = FALSE,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = FALSE,
                                   diagram_facet = length(unique(df$region)) > 1,
                                   x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
                                   facet_grp = "region",
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   facet_x_axis_storlek = 6,
                                   legend_vand = FALSE,
                                   dataetiketter = FALSE,
                                   manual_color = vald_farg[1],
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      
    } # Slut diag_alder_fokus
    names(gg_list) <- objektnamn
    return(gg_list)
  } # Slut map-funktion
  # Om facet väljs körs ingen map-funktion (vi har bara 1 diagram)
  if (diag_facet) {
    diag <- skapa_diagram(flytt_df,region_vekt)
    
  } else {
    diag <- map(region_vekt, ~ skapa_diagram(flytt_df, .x)) %>% flatten()
    
  }
  # Går inte att använda facet och gruppera namn samtidigt
  if(!is.na(gruppera_namn)&&diag_facet==TRUE){
    print("OBS! --- Gruppera namn och facet kan ej användas samtidigt. Sätt gruppera namn till NA om du vill ha ett facet-diagram --- OBS!")
  }
  
  if(returnera_figur==TRUE) return(diag)
  
}

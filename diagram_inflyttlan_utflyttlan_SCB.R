#test = diagram_inflyttlan_utflyttlan(spara_figur = FALSE,diag_senaste_ar = TRUE, diag_flera_ar = TRUE, diag_facet = FALSE, returnera_figur = TRUE)
diagram_inflyttlan_utflyttlan <- function(output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                          vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                          inflyttningsl_klartext = "*",			 #  Finns: " Stockholms län (Inflyttningslän)", " Uppsala län (Inflyttningslän)", " Södermanlands län (Inflyttningslän)", " Östergötlands län (Inflyttningslän)", " Jönköpings län (Inflyttningslän)", " Kronobergs län (Inflyttningslän)", " Kalmar län (Inflyttningslän)", " Gotlands län (Inflyttningslän)", " Blekinge län (Inflyttningslän)", " Skåne län (Inflyttningslän)", " Hallands län (Inflyttningslän)", " Västra Götalands län (Inflyttningslän)", " Värmlands län (Inflyttningslän)", " Örebro län (Inflyttningslän)", " Västmanlands län (Inflyttningslän)", " Dalarnas län (Inflyttningslän)", " Gävleborgs län (Inflyttningslän)", " Västernorrlands län (Inflyttningslän)", " Jämtlands län (Inflyttningslän)", " Västerbottens län (Inflyttningslän)", " Norrbottens län (Inflyttningslän)"
                                          utflyttningsl_klartext = " Dalarnas län (Utflyttningslän)",			 #  Finns: " Stockholms län (Utflyttningslän)", " Uppsala län (Utflyttningslän)", " Södermanlands län (Utflyttningslän)", " Östergötlands län (Utflyttningslän)", " Jönköpings län (Utflyttningslän)", " Kronobergs län (Utflyttningslän)", " Kalmar län (Utflyttningslän)", " Gotlands län (Utflyttningslän)", " Blekinge län (Utflyttningslän)", " Skåne län (Utflyttningslän)", " Hallands län (Utflyttningslän)", " Västra Götalands län (Utflyttningslän)", " Värmlands län (Utflyttningslän)", " Örebro län (Utflyttningslän)", " Västmanlands län (Utflyttningslän)", " Dalarnas län (Utflyttningslän)", " Gävleborgs län (Utflyttningslän)", " Västernorrlands län (Utflyttningslän)", " Jämtlands län (Utflyttningslän)", " Västerbottens län (Utflyttningslän)", " Norrbottens län (Utflyttningslän)"
                                          tid = "*", # Avsluta med 9999 för senaste år
                                          spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                          diag_senaste_ar = TRUE, # Skapar ett diagram för antingen in eller utflytt från ett län till alla andra valda län. Enbart senaste valda år
                                          diag_flera_ar = FALSE, # Skapar ett diagram per vald destination (in- eller utflyttningslan). Enbart intressant om flera år väljs
                                          diag_facet = FALSE, # Sätts till TRUE om man istället vill ha diag_flera_ar som ett facet-diagram
                                          returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                          returnera_data = FALSE # True om användaren vill returnera data från funktionen
){
  
  # ===========================================================================================================
  # Stapeldiagram för inflyttningslan respektive utflyttningslan (i antal).Går att få för senaste år (1 diagram) eller över tid (flera diagram eller facet).
  # Skapad: 2024-06-25 av Jon
  # ===========================================================================================================
  
  if(length(inflyttningsl_klartext) > 1 && length(utflyttningsl_klartext) >1 ){
    stop("Max 1 län får väljas för antingen inflyttningsl_klartext eller utflyttningsl_klartext")
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_inflyttningslan_utflyttningslan_kon_tid_scb.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna.\nDiagramförklaring: Diagrammet avser flyttningar och inte personer.\nUnder ett år kan en person flytta fler än en gång."
  
  gg_list <- list()
  gg_list_map <- list()
  objektnamn <- c()
  objektnamn_map <- c()
  
  inflytt_utflytt_df = hamta_inflyttningslan_utflyttningslan_kon_tid_scb(inflyttningsl_klartext = inflyttningsl_klartext,
                                                                         utflyttningsl_klartext = utflyttningsl_klartext ,
                                                                         kon_klartext = NA,
                                                                         cont_klartext = "*",
                                                                         tid_koder = tid,
                                                                         returnera_df = TRUE)
  
  # Byter dåliga namn på regioner till bättre
  inflytt_utflytt_df <- inflytt_utflytt_df %>% 
    mutate(Inflyttningslän = case_when(
      Inflyttningslän == " Stockholms län (Inflyttningslän)" ~ "Stockholm",
      Inflyttningslän == " Uppsala län (Inflyttningslän)" ~ "Uppsala",
      Inflyttningslän == " Södermanlands län (Inflyttningslän)" ~ "Södermanland",
      Inflyttningslän == " Östergötlands län (Inflyttningslän)" ~ "Östergötland",
      Inflyttningslän == " Jönköpings län (Inflyttningslän)" ~ "Jönköping",
      Inflyttningslän == " Kronobergs län (Inflyttningslän)" ~ "Kronoberg",
      Inflyttningslän == " Kalmar län (Inflyttningslän)" ~ "Kalmar",
      Inflyttningslän == " Gotlands län (Inflyttningslän)" ~ "Gotland",
      Inflyttningslän == " Blekinge län (Inflyttningslän)" ~ "Blekinge",
      Inflyttningslän == " Skåne län (Inflyttningslän)" ~ "Skåne",
      Inflyttningslän == " Hallands län (Inflyttningslän)" ~ "Halland",
      Inflyttningslän == " Västra Götalands län (Inflyttningslän)" ~ "Västra Götaland",
      Inflyttningslän == " Värmlands län (Inflyttningslän)" ~ "Värmland",
      Inflyttningslän == " Örebro län (Inflyttningslän)" ~ "Örebro",
      Inflyttningslän == " Västmanlands län (Inflyttningslän)" ~ "Västmanland",
      Inflyttningslän == " Dalarnas län (Inflyttningslän)" ~ "Dalarna",
      Inflyttningslän == " Gävleborgs län (Inflyttningslän)" ~ "Gävleborg",
      Inflyttningslän == " Västernorrlands län (Inflyttningslän)" ~ "Västernorrland",
      Inflyttningslän == " Jämtlands län (Inflyttningslän)" ~ "Jämtland",
      Inflyttningslän == " Västerbottens län (Inflyttningslän)" ~ "Västerbotten",
      Inflyttningslän == " Norrbottens län (Inflyttningslän)" ~ "Norrbotten"
    )) %>% 
    mutate(Utflyttningslän = case_when(
      Utflyttningslän == " Stockholms län (Utflyttningslän)" ~ "Stockholm",
      Utflyttningslän == " Uppsala län (Utflyttningslän)" ~ "Uppsala",
      Utflyttningslän == " Södermanlands län (Utflyttningslän)" ~ "Södermanland",
      Utflyttningslän == " Östergötlands län (Utflyttningslän)" ~ "Östergötland",
      Utflyttningslän == " Jönköpings län (Utflyttningslän)" ~ "Jönköping",
      Utflyttningslän == " Kronobergs län (Utflyttningslän)" ~ "Kronoberg",
      Utflyttningslän == " Kalmar län (Utflyttningslän)" ~ "Kalmar",
      Utflyttningslän == " Gotlands län (Utflyttningslän)" ~ "Gotland",
      Utflyttningslän == " Blekinge län (Utflyttningslän)" ~ "Blekinge",
      Utflyttningslän == " Skåne län (Utflyttningslän)" ~ "Skåne",
      Utflyttningslän == " Hallands län (Utflyttningslän)" ~ "Halland",
      Utflyttningslän == " Västra Götalands län (Utflyttningslän)" ~ "Västra Götaland",
      Utflyttningslän == " Värmlands län (Utflyttningslän)" ~ "Värmland",
      Utflyttningslän == " Örebro län (Utflyttningslän)" ~ "Örebro",
      Utflyttningslän == " Västmanlands län (Utflyttningslän)" ~ "Västmanland",
      Utflyttningslän == " Dalarnas län (Utflyttningslän)" ~ "Dalarna",
      Utflyttningslän == " Gävleborgs län (Utflyttningslän)" ~ "Gävleborg",
      Utflyttningslän == " Västernorrlands län (Utflyttningslän)" ~ "Västernorrland",
      Utflyttningslän == " Jämtlands län (Utflyttningslän)" ~ "Jämtland",
      Utflyttningslän == " Västerbottens län (Utflyttningslän)" ~ "Västerbotten",
      Utflyttningslän == " Norrbottens län (Utflyttningslän)" ~ "Norrbotten"
    )) %>% 
    rename("Antal_flyttar" = `Inrikes omflyttning mellan län `)
  
  if(returnera_data == TRUE){
    assign("inflytt_utflytt_lan_df", inflytt_utflytt_df, envir = .GlobalEnv)
  }
  
  if(diag_senaste_ar == TRUE){
    
    if(length(unique(inflytt_utflytt_df$Inflyttningslän)) == 1){
      diagram_titel<- paste0("Antal flyttar till ",unique(inflytt_utflytt_df$Inflyttningslän)," år ",max(inflytt_utflytt_df$år))
      diagramfil <- paste0("Flytt_till_",unique(inflytt_utflytt_df$Inflyttningslän),".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      ut_df <- inflytt_utflytt_df %>% 
        filter(Utflyttningslän != unique(inflytt_utflytt_df$Inflyttningslän))
    }else{
      diagram_titel<- paste0("Antal flyttar från ",unique(inflytt_utflytt_df$Utflyttningslän)," år ",max(inflytt_utflytt_df$år))
      diagramfil <- paste0("Flytt_fran_",unique(inflytt_utflytt_df$Utflyttningslän),".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      ut_df <- inflytt_utflytt_df %>% 
        filter(Inflyttningslän != unique(inflytt_utflytt_df$Utflyttningslän))
    }
    
    gg_obj <- SkapaStapelDiagram(skickad_df = ut_df %>% 
                                   filter(år == max(år)), 
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
                                 filnamn_diagram = diagramfil)
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn
    
  }
  
  if(diag_flera_ar){
    
    skapa_diagram <- function(df, vald_region){
      
      if(length(unique(df$Inflyttningslän)) == 1){
        df <- df %>% filter(Utflyttningslän %in% vald_region)
        if(length(unique(df$Utflyttningslän)) > 1){
          diagram_titel<- paste0("Antal flyttar till ",unique(df$Inflyttningslän)," från")
          diagramfil <- paste0("Flytt_till_",unique(df$Inflyttningslän),"_facet.png")
        }else{
          diagram_titel<- paste0("Antal flyttar till ",unique(df$Inflyttningslän)," från ",unique(df$Utflyttningslän))
          diagramfil <- paste0("Flytt_till_",unique(df$Inflyttningslän),"_fran_",unique(df$Utflyttningslän),".png")
        }
        objektnamn_map <- c(objektnamn_map,diagramfil %>% str_remove(".png"))
        
      }else{
        df <- df %>% filter(Inflyttningslän %in% vald_region)
        if(length(unique(df$Inflyttningslän)) > 1){
          diagram_titel<- paste0("Antal flyttar från ",unique(df$Utflyttningslän)," till")
          diagramfil <- paste0("Flytt_fran_",unique(df$Utflyttningslän),"_facet.png")
        }else{
          diagram_titel<- paste0("Antal flyttar från ",unique(df$Utflyttningslän)," till ",unique(df$Inflyttningslän))
          diagramfil <- paste0("Flytt_fran_",unique(df$Utflyttningslän),"_till_",unique(df$Inflyttningslän),".png")
        }
        
        objektnamn_map <- c(objektnamn_map,diagramfil %>% str_remove(".png"))
      }
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df , 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "Antal_flyttar",
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "Antal flyttar",
                                   diagram_facet = diag_facet == TRUE,
                                   x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,4,NA),
                                   facet_grp = ifelse(length(unique(df$Inflyttningslän)) == 1,"Utflyttningslän","Inflyttningslän"),
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   manual_color = vald_farg[1],
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list_map <- c(gg_list_map, list(gg_obj))
      names(gg_list_map) <- objektnamn_map 
      return(gg_list_map)
    }
    
    if(length(unique(inflytt_utflytt_df$Inflyttningslän)) == 1){
      region_vekt <- unique(inflytt_utflytt_df %>% 
                              filter(Utflyttningslän != unique(inflytt_utflytt_df$Inflyttningslän)) %>% .$Utflyttningslän)
    } else {
      region_vekt <- unique(inflytt_utflytt_df %>% 
                              filter(Inflyttningslän != unique(inflytt_utflytt_df$Utflyttningslän)) %>% .$Inflyttningslän)
    }
    
    
    if (diag_facet) {
      diag <- skapa_diagram(inflytt_utflytt_df,region_vekt)
      
    } else {
      
      diag <- map(region_vekt, ~ skapa_diagram(inflytt_utflytt_df, .x)) %>% flatten()
      
    }
    gg_list <- c(gg_list, diag)
  }
  
  if(returnera_figur==TRUE) return(gg_list)
}

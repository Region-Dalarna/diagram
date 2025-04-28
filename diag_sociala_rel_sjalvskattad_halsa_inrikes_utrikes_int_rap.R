diag_fohm <- function(visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                      alder = "16- år", # Finns även "16-84 år"
                      diag_soc_rel = TRUE,                                 # TRUE om diagram för sociala relationer ska skapas
                      sociala_relationer_klartext = "Låg tillit till samhällets institutioner",			 #  Finns: "Avstått från att gå ut ensam på grund av rädsla", "Utsatt för fysiskt våld eller hot om våld", "Utsatt för fysiskt våld", "Utsatt för hot om våld", "Saknar emotionellt stöd", "Saknar praktiskt stöd", "Lågt socialt deltagande", "Svårt att lita på andra", "Utsatt för kränkande behandling eller bemötande", "Låg tillit till samhällets institutioner"
                      diag_sjalvskattad_halsa_tid = TRUE,
                      diag_sjalvskattad_halsa_kon = TRUE,
                      output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                      skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                      returnera_data_rmarkdown = FALSE,
                      demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
) {
  
  
  # =======================================================================================================================
  # Tre diagram kopplat till Folkhälsomyndighetens öppna statistikdatabas. Samtliga för riket (finns ej uppdelat på bakgrund på länsnivå)
  # Används primärt i integrationsrapporten
  # =======================================================================================================================
  
  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
    walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    stop_tyst()
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sociala_relationer_riket_fodelseland_kon_ar_hlv1soccfod_fohm.R")
  
  gg_list <- list()
  
  if(diag_soc_rel){
    sociala_relationer_df <- hamta_sociala_relationer_fodelseland_alder_kon_ar (alder_klartext = alder,
                                                                                kon_klartext = "*",
                                                                                fodelseland_klartext = "*",
                                                                                andel_och_konfidensintervall_klartext = "Andel",
                                                                                tid_koder = "9999",
                                                                                sociala_relationer_klartext = sociala_relationer_klartext) %>%
      filter(Födelseland != "Totalt")
    
    
    if(returnera_data_rmarkdown == TRUE){
      assign("sociala_relationer_df", sociala_relationer_df, envir = .GlobalEnv)
    }
    
    diagram_capt <- paste0("Källa: Folkhälsomyndighetens öppna statistikdatabas, bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Enkätundersökning. Andel som svarar ja på frågan: ",unique(sociala_relationer_df$`Sociala relationer`),".")
    
    sociala_relationer_df$Födelseland<- factor(sociala_relationer_df$Födelseland, levels = c("Sverige","Övriga Norden","Övriga Europa","Övriga världen"))
    
    diagramtitel <- paste0(sociala_relationer_klartext," i Sverige år ", max(sociala_relationer_df$År)," (",unique(sociala_relationer_df$Ålder),")")
    
    namn <- chartr("åäö", "aao", tolower(str_replace_all(sociala_relationer_klartext, " ", "_")))
    diagramfilnamn <- paste0(namn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sociala_relationer_df %>%
                                   filter(Kön != "Totalt"),
                                 skickad_x_var = "Födelseland",
                                 skickad_y_var = last(names(sociala_relationer_df)),
                                 skickad_x_grupp = "Kön",
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 procent_0_100_10intervaller = TRUE,
                                 #geom_position_stack = TRUE,
                                 legend_vand_ordning = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Födelseland",
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_sjalvskattad_halsa_tid){
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sjalvskattad_halsa_riket_halsotillstand_andel_och_konfidensintervall_fodelseland_kon_ar_halsgodcfod_fohm.R")
    
    sjalvskattad_halsa_df <- hamta_sjalvskattad_halsa_riket_alder_fodelseland_kon_ar  (alder_klartext = alder,
                                                                                       kon_klartext = kon_klartext,
                                                                                       fodelseland_klartext = "*",
                                                                                       andel_och_konfidensintervall_klartext = "Andel",
                                                                                       tid_koder = "*") %>%
      filter(Födelseland != "Totalt",
             !is.na(`Hälsa efter region, kön och år. Andel`))
    
    
    
    if(returnera_data_rmarkdown == TRUE){
      assign("sjalvskattad_halsa_tid_df", sjalvskattad_halsa_df, envir = .GlobalEnv)
    }
    
    
    diagram_capt <- paste0("Källa: Folkhälsomyndighetens öppna statistikdatabas, bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Enkätundersökning. Andel som svarar ja på frågan: ",unique(sjalvskattad_halsa_df$Hälsotillstånd),".")
    
    sjalvskattad_halsa_df$Födelseland<- factor(sjalvskattad_halsa_df$Födelseland, levels = c("Sverige","Övriga Norden","Övriga Europa","Övriga världen"))
    
    diagramtitel <- paste0(unique(sjalvskattad_halsa_df$Hälsotillstånd)," i Sverige"," (",unique(sjalvskattad_halsa_df$Ålder),")")
    
    namn <- chartr("åäö", "aao", tolower(str_replace_all(unique(sjalvskattad_halsa_df$Hälsotillstånd), " ", "_")))
    diagramfilnamn <- paste0(namn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sjalvskattad_halsa_df %>%
                                   filter(Kön == "Totalt"),
                                 skickad_x_var = "Födelseland",
                                 skickad_y_var = "Hälsa efter region, kön och år. Andel",
                                 skickad_x_grupp = "År",
                                 manual_color = diagramfarger("rus_sex"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 procent_0_100_10intervaller = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Födelseland",
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  if(diag_sjalvskattad_halsa_kon){
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sjalvskattad_halsa_riket_halsotillstand_andel_och_konfidensintervall_fodelseland_kon_ar_halsgodcfod_fohm.R")
    
    sjalvskattad_halsa_kon_df <- hamta_sjalvskattad_halsa_riket_alder_fodelseland_kon_ar(alder_klartext = alder,
                                                                                         kon_klartext = c("kvinnor","män"),
                                                                                         fodelseland_klartext = "*",
                                                                                         andel_och_konfidensintervall_klartext = "Andel",
                                                                                         tid_koder = "9999") %>%
      filter(Födelseland != "Totalt")
    
    
    
    if(returnera_data_rmarkdown == TRUE){
      assign("sjalvskattad_halsa_kon_df", sjalvskattad_halsa_kon_df, envir = .GlobalEnv)
    }
    
    diagram_capt <- paste0("Källa: Folkhälsomyndighetens öppna statistikdatabas, bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Enkätundersökning. Andel som svarar ja på frågan: ",unique(sjalvskattad_halsa_df$Hälsotillstånd),".")
    
    sjalvskattad_halsa_kon_df$Födelseland<- factor(sjalvskattad_halsa_kon_df$Födelseland, levels = c("Sverige","Övriga Norden","Övriga Europa","Övriga världen"))
    
    diagramtitel <- paste0(unique(sjalvskattad_halsa_kon_df$Hälsotillstånd)," i Sverige år ", max(sjalvskattad_halsa_kon_df$År)," (",unique(sjalvskattad_halsa_kon_df$Ålder),")")
    
    namn <- chartr("åäö", "aao", tolower(str_replace_all(unique(sjalvskattad_halsa_df$Hälsotillstånd), " ", "_")))
    diagramfilnamn <- paste0(namn,"_kon.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = sjalvskattad_halsa_kon_df,
                                 skickad_x_var = "Födelseland",
                                 skickad_y_var = "Hälsa efter region, kön och år. Andel",
                                 skickad_x_grupp = "Kön",
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 45,
                                 procent_0_100_10intervaller = TRUE,
                                 diagram_liggande = FALSE,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "Födelseland",
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skriv_diagrambildfil)
    
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
}

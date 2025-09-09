diagram_storsta_fodelseland <-function(region_vekt = "20",# Max 1, län
                                       output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                       spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                       tid_koder = "*", # Finns från 2000 och framåt
                                       returnera_figur = TRUE, # Returnerar en figur
                                       jmf_ar = 2010, # Första och sista år i tid_koder jämförs med detta år
                                       antal_lander = 10, # Antal länder som skall visas i diagrammet
                                       valda_farger = diagramfarger("rus_sex"),
                                       visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                       logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet.
                                       returnera_data = FALSE) # Skall data returneras)
{
  
  ## =================================================================================================================
  # Ett diagram för största födelseland bland utrikes födda i regionen. Går att jämföra tre år (första, sista och jämförelseår)
  # eller bara titta på sista år
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_bef_fodelseland_region_fodelseregion_kon_tid_FolkmRegFlandK_scb.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()
  region_namn <- skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)
  
  # Hämta data
  antal_fodelseland_df <- hamta_bef_fodelseland_region_fodelseregion_kon_tid_scb(region_vekt = region_vekt,
                                                                                 kon_klartext = NA,
                                                                                 tid_koder = tid_koder) %>%
    mutate(region = skapa_kortnamn_lan(region))
  
  # # select 10 largest födelseland in the first year
  # top_10_fodelseland_forstaar <- antal_fodelseland_df %>%
  #   filter(år == min(år),!(födelseregion %in% c("Samtliga födelseländer","Sverige"))) %>%
  #     arrange(desc(Antal)) %>%
  #       head(10) %>%.$födelseregion
  
  top_10_fordelseland_sistaar <- antal_fodelseland_df %>%
    filter(år == max(år),!(födelseregion %in% c("Samtliga födelseländer","Sverige"))) %>%
    arrange(desc(Antal)) %>%
    head(antal_lander) %>%.$födelseregion
  
  storsta_fodelseland_df <- antal_fodelseland_df %>%
    filter(födelseregion %in% top_10_fordelseland_sistaar,
           år %in% c(min(år),jmf_ar,max(år)))
  
  if(returnera_data == TRUE){
    assign("storsta_fodelseland_df", storsta_fodelseland_df, envir = .GlobalEnv)
  }
  
  diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  
  if(length(unique(storsta_fodelseland_df$år)) == 1){
    diagram_titel = paste0("Vanligaste födelseland bland utrikes födda i ",region_namn, " år ",unique(storsta_fodelseland_df$år))
    diagramfilnamn <- paste0("storsta_fodelseland_",region_namn,"_",unique(storsta_fodelseland_df$år),".png")
  } else {
    diagram_titel = paste0("Vanligaste födelseland bland utrikes födda i ",region_namn)
    diagramfilnamn <- paste0("storsta_fodelseland_",region_namn,".png")
  }
  
  
  
  gg_obj <- SkapaStapelDiagram(skickad_df = storsta_fodelseland_df,
                               skickad_x_var = "födelseregion",
                               skickad_y_var = "Antal",
                               skickad_x_grupp = "år",
                               manual_color = valda_farger,
                               diagram_titel = diagram_titel,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               x_axis_sort_value = TRUE,
                               vand_sortering = TRUE,
                               x_axis_sort_grp = 3,
                               legend_tabort = ifelse(length(unique(storsta_fodelseland_df$år)) == 1,TRUE,FALSE),
                               diagram_capt =  diagram_capt,
                               output_mapp = output_mapp_figur,
                               stodlinjer_avrunda_fem = TRUE,
                               lagg_pa_logga = visa_logga_i_diagram,
                               logga_path = logga_sokvag,
                               manual_y_axis_title = "",
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  
  if(returnera_figur == TRUE){
    return(gg_list)
  }
  
}

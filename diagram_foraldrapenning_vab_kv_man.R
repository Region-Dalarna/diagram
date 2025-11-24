diag_foraldrapenning_vab <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                     diag_foraldrapenning_mottagare = TRUE,
                                     diag_foraldrapenning_andel_nettodagar = TRUE,
                                     diag_foraldrapenning_andel_senaste_ar_lanets_kommuner = TRUE,
                                     diag_foraldrapenning_antal_nettodagar = TRUE,
                                     diag_vab_antal_nettodagar = TRUE,
                                     diag_vab_forandring_nettodagar = TRUE,
                                     output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                                     spara_diagrambildfil = FALSE,
                                     spara_dataframe_till_global_environment = FALSE){
  
  ## =================================================================================================================
  # Skript som skapar tre diagram för föräldrapenning och två diagram för vård av barn (VAB) i valt län.
  # Används i första hand i rapporten "Kvinnor och män i Dalarna"
  # Skapad av Jon Frank 2025-07-04
  #
  # Reviderad av Peter Möller 2025-11-24. 
  # Nu går det att hämta Riket, län och kommuner. Ersatt län/kommun (bara namn, inte kod) med 
  # Regionkod och Region. Lagt till TRUE/FALSE för varje enskild diagram
  # =============================================== Uttag ===============================================
  
  # # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         here)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/fp-antal-mottagare-nettodagar-belopp/FPAntalDagarBeloppLanKommun.xlsx",
           "https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/tfp-vab-antal-mottagare-belopp/tfpVabAntalDagarBeloppLanKommun.xlsx")
  
  # Med Peters nya skript
  flik_lista = list()
  
  gg_list = list()
  
  # om något av föräldrapenningsdiagrammen är TRUE så hämtas data för föräldrapenning, annars inte
  if(any(
    diag_foraldrapenning_mottagare, diag_foraldrapenning_andel_nettodagar,
         diag_foraldrapenning_andel_sen_ar_lanets_kommuner,
         diag_foraldrapenning_antal_nettodagar)){

    foraldrapenning_df = hamta_excel_dataset_med_url(path[1],skippa_rader = 2) %>%
      rename(Region = Kommun) %>% 
      mutate(Region = if_else(Region == "Riket", "00 Riket", Region)) %>% 
      separate(Region, into = c("Regionkod", "Region"), sep = " ", extra = "merge") %>% 
      rename(Antal_mottagare = `Antal mottagare`,
             Andel = `Andel nettodagar per kön`) %>% 
      select(-c(Län, kolumnnamn))
    
    lanskod <- str_sub(region_vekt, 1, 2)
    lan_txt <- hamtaregion_kod_namn(lanskod)$region %>% skapa_kortnamn_lan(TRUE)
    
    foraldrapenning_lan_df <- foraldrapenning_df %>% 
      filter(str_sub(Regionkod, 1, 2) == lanskod)
    
    foraldrapenning_df <- foraldrapenning_df %>% 
      filter(Regionkod %in% region_vekt)
    
    if(spara_dataframe_till_global_environment) {
      assign("foraldrapenning_df", foraldrapenning_df, envir = .GlobalEnv)
    }
    
    # Antal mottagare
    if (diag_foraldrapenning_mottagare) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Antal mottagare av föräldrapenning i " , unique(foraldrapenning_df$Region) %>% skapa_kortnamn_lan() %>% list_komma_och())
      diagramfilnamn <- paste0("Foraldrapenning_antal_", unique(foraldrapenning_df$Region) %>% skapa_kortnamn_lan() %>% paste0(collapse = "_"),".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_df %>%
                                     filter(Kön != "Kvinnor och män") %>% 
                                     mutate(Kön = tolower(Kön),
                                            Antal_mottagare = as.numeric(Antal_mottagare)), 
                                   skickad_x_var = "År", 
                                   skickad_y_var = "Antal_mottagare", 
                                   skickad_x_grupp = "Kön",
                                   x_axis_lutning = 45,
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_y_axis_title = "Antal mottagare",
                                   manual_color = diagramfarger("kon"),
                                   stodlinjer_avrunda_fem = TRUE,
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    } # slut if-sats för diagram
    
    # Andel nettodagar
    if (diag_foraldrapenning_andel_nettodagar) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Föräldrapenning, andel nettodagar per kön i " , unique(foraldrapenning_df$Region) %>% skapa_kortnamn_lan() %>% list_komma_och())
      diagramfilnamn <- paste0("Foraldrapenning_andel_", unique(foraldrapenning_df$Region) %>% skapa_kortnamn_lan() %>% list_komma_och(),".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_df%>%
                                           filter(Kön != "Kvinnor och män") %>% 
                                           mutate(Kön = tolower(Kön),
                                                  Andel = as.numeric(Andel)), 
                                         skickad_x_var = "År", 
                                         skickad_y_var = "Andel", 
                                         skickad_x_grupp = "Kön",
                                         x_axis_lutning = 45,
                                         manual_x_axis_text_vjust=1,
                                         manual_x_axis_text_hjust=1,
                                         manual_color = diagramfarger("kon"),
                                         stodlinjer_avrunda_fem = TRUE,
                                         manual_y_axis_title = "procent",
                                         procent_0_100_10intervaller = TRUE,
                                         diagram_titel = diagramtitel,
                                         diagram_capt =  diagram_capt,
                                         output_mapp = output_mapp,
                                         filnamn_diagram = diagramfilnamn,
                                         skriv_till_diagramfil = spara_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    } # slut if-sats för diagram
    
    # Andel per kommun i ett län
    if (diag_foraldrapenning_andel_senaste_ar_lanets_kommuner) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Föräldrapenning, andel nettodagar per kön i " , lan_txt %>% list_komma_och(), " år ", max(foraldrapenning_df$År))
      diagramfilnamn <- paste0("Foraldrapenning_andel_kommun_", lan_txt %>% list_komma_och(),".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_lan_df %>%
                                           filter(Kön != "Kvinnor och män",
                                                  År == max(År)) %>% 
                                           mutate(Kön = tolower(Kön),
                                                  Andel = as.numeric(Andel)), 
                                         skickad_x_var = "Region", 
                                         skickad_y_var = "Andel", 
                                         skickad_x_grupp = "Kön",
                                         x_axis_lutning = 45,
                                         manual_color = diagramfarger("kon"),
                                         manual_y_axis_title = "procent",
                                         manual_x_axis_text_vjust=1,
                                         manual_x_axis_text_hjust=1,
                                         x_axis_sort_value = TRUE,
                                         procent_0_100_10intervaller = TRUE,
                                         stodlinjer_avrunda_fem = TRUE,
                                         x_axis_sort_grp = 1,
                                         vand_sortering = FALSE,
                                         diagram_titel = diagramtitel,
                                         diagram_capt =  diagram_capt,
                                         output_mapp = output_mapp,
                                         filnamn_diagram = diagramfilnamn,
                                         skriv_till_diagramfil = spara_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    } # slut if-sats för diagram
    
    # föräldrapenning antal nettodagar
    if (diag_foraldrapenning_antal_nettodagar) {
      
      diagramtitel <- paste0("Föräldrapenning, antal nettodagar per kön i " , unique(foraldrapenning_df$Region) %>% skapa_kortnamn_lan() %>% list_komma_och())
      diagramfilnamn <- paste0("Foraldrapenning_antal_", unique(foraldrapenning_df$Region) %>% skapa_kortnamn_lan() %>% list_komma_och(),".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_df %>%
                                     filter(Kön != "Kvinnor och män") %>% 
                                     mutate(Kön = tolower(Kön),
                                            Nettodagar = as.numeric(Nettodagar)), 
                                   skickad_x_var = "År", 
                                   skickad_y_var = "Nettodagar", 
                                   skickad_x_grupp = "Kön",
                                   x_axis_lutning = 45,
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_color = diagramfarger("kon"),
                                   stodlinjer_avrunda_fem = TRUE,
                                   manual_y_axis_title = "Antal nettodagar",
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    } # slut if-sats för diagram
  } # slut if-sats om det finns några föräldrapenningsdiagram
    
  if(any(diag_vab_antal_nettodagar, diag_vab_forandring_nettodagar)) {
    
    vab_df = hamta_excel_dataset_med_url(path[2],skippa_rader = 2) %>%
      rename(Region = Kommun) %>% 
      mutate(Region = if_else(Region == "Riket", "00 Riket", Region)) %>% 
      separate(Region, into = c("Regionkod", "Region"), sep = " ", extra = "merge") %>% 
      rename(Antal_mottagare = `Antal mottagare`,
             Antal_nettodagar = `Antal nettodagar`,
             Andel = `Andel nettodagar per kön`) %>% 
      select(-c(Län, kolumnnamn)) %>% 
      filter(Regionkod %in% region_vekt)
    
    if(spara_dataframe_till_global_environment) {
      assign("vab_df", vab_df, envir = .GlobalEnv)
    }
    
    # Antal uttagna nettodagar
    if (diag_vab_antal_nettodagar) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Vård av barn, antal uttagna nettodagar i " , unique(vab_df$Region) %>% skapa_kortnamn_lan() %>% list_komma_och())
      diagramfilnamn <- paste0("vab_antal_", unique(vab_df$Region) %>% skapa_kortnamn_lan() %>% paste0(collapse = "_"),".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = vab_df%>%
                                      filter(Kön != "Kvinnor och män") %>% 
                                      mutate(Kön = tolower(Kön),
                                             Antal_nettodagar = as.numeric(Antal_nettodagar)), 
                                    skickad_x_var = "År", 
                                    skickad_y_var = "Antal_nettodagar", 
                                    skickad_x_grupp = "Kön",
                                    x_axis_lutning = 45,
                                    manual_x_axis_text_vjust=1,
                                    manual_x_axis_text_hjust=1,
                                    manual_y_axis_title = "",
                                    manual_color = diagramfarger("kon"),
                                    diagram_titel = diagramtitel,
                                    diagram_capt =  diagram_capt,
                                    stodlinjer_avrunda_fem = TRUE,
                                    output_mapp = output_mapp,
                                    filnamn_diagram = diagramfilnamn,
                                    skriv_till_diagramfil = spara_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    } # slut if-sats för diagram

    # diagram över förändring av nettodagar vab
    if (diag_vab_forandring_nettodagar) {
      diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
      diagramtitel <- paste0("Vård av barn, förändring i antal uttagna nettodagar i " , unique(vab_df$Region) %>% skapa_kortnamn_lan() %>% list_komma_och())
      diagramfilnamn <- paste0("vab_antal_linje_", unique(vab_df$Region) %>% skapa_kortnamn_lan() %>% paste0(collapse = "_"),".png")
      
      gg_obj <- SkapaLinjeDiagram(skickad_df = vab_df %>%
                                           filter(Kön != "Kvinnor och män") %>% 
                                           mutate(Kön = tolower(Kön),
                                                  `Antal nettodagar` = as.numeric(Antal_nettodagar),
                                                  "år" = År), 
                                         skickad_x_var = "år", 
                                         skickad_y_var = "Antal nettodagar", 
                                         skickad_x_grupp = "Kön",
                                         manual_color = diagramfarger("kon"),
                                         berakna_index = TRUE,
                                         x_axis_lutning = 45,
                                         diagram_titel = diagramtitel,
                                         diagram_capt =  diagram_capt,
                                         stodlinjer_avrunda_fem = TRUE,
                                         output_mapp = output_mapp,
                                         filnamn_diagram = diagramfilnamn,
                                         skriv_till_diagramfil = spara_diagrambildfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    } # slut if-sats för diagram
  } # slut if-sats om det finns några vab-diagram
  
  return(gg_list)

}

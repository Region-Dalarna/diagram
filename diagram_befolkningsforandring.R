diagram_befolkningsforandring <- function(region_vekt = hamtaAllaLan(tamedriket = FALSE), # Vilka regioner/kommuner vill man titta på
                                          output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                          output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                          tid = c("2010":"9999"), # Välj tidsintervall. Finns från 1968."*" ger alla. "9999" ger sista år
                                          spara_figur = TRUE,
                                          alder = as.character(20:64), # Välj ett åldersintervall. "tot" ger alla åldrar (inte "*")
                                          filnamn_data = "befolkningsforandring.xlsx", # Filnamn på sparad data
                                          vald_farg = diagramfarger("rus_sex"), # Val av diagramfärger
                                          returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                                          returnera_data = FALSE){ # Skall data returneras
  
  
  # =================================================================================================================
  # Diagram som beräknar befolkningsförändring under en tidsperiod uppdelat på inrikes/utrikes flytt och demografisk förändring
  # Diagrammet kan skapas för olika åldersintervall
  # Skapad av Jon 2024-01-19
  # Senast uppdaterad:
  # Förbättringsmöjligheter: Markera total förändring i diagram (exepelvis som en svart linje)
  # =================================================================================================================
  
  # Bibliotek som behövs
  if (!require("pacman")) install.packages("pacman")
  p_load(here,
         tidyverse)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge objekt i lista
  list_data <- lst() # Skapa tom lista som används för att spara till Excel.
  
  # Hämtar funktioner och laddar skript som hämtar data
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")

  bef_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt, 
                                                 cont_klartext = c("Folkmängd","Folkökning"),
                                                 tid_koder = tid,
                                                 alder_koder = alder) %>% 
              pivot_wider(names_from = variabel,values_from = varde)
  
  flytt_df <- hamta_bef_flyttningar_region_alder_kon_scb(region_vekt = region_vekt, 
                                                         tid_koder = tid,
                                                         alder_koder = alder) %>% 
              pivot_wider(names_from = variabel,values_from = varde)
  
  
  # summerar flyttningar på regionnivå för åren 2010-senaste år
  flytt_df_sum <- flytt_df %>%
    group_by(region) %>% 
    summarize(flyttningsoverskott = sum(Flyttningsöverskott),
              invandringsoverskott=sum(Invandringsöverskott),
              inrikes_flyttningsoverskott=sum(`Inrikes flyttningsöverskott`))
  
  # Summerar den totala folkökningen
  bef_df_sum <- bef_df %>%
    group_by(region) %>% 
      summarize(folkokning_period = sum(Folkökning))
  
  # Tar ut befolkningen 2010 och slår ihop med folkökning (för att beräkna förändring)
  bef_df_min <- bef_df %>% 
    filter(år == min (år)) %>% 
    group_by(region) %>% 
      summarize(folkmangd_forsta_ar = sum(Folkmängd))
  
  bef_df_sum <- merge(bef_df_sum,bef_df_min)
  
  # Slår ihop befolkning och flyttningar
  slutgiltig_df <-merge(flytt_df_sum,bef_df_sum)
  
  # Beräknar procentuell förändring av befolkning baserat på olika kompontenter
  slutgiltig_df <- slutgiltig_df %>% 
    mutate(alderskomponent = folkokning_period - flyttningsoverskott) %>% 
      mutate("Inrikes flyttnetto" = round((inrikes_flyttningsoverskott/folkmangd_forsta_ar)*100,2),
             "Utrikes flyttnetto" = round((invandringsoverskott/folkmangd_forsta_ar)*100,2),
             "Demografisk förändring" = round((alderskomponent/folkmangd_forsta_ar)*100,2),
             "Total förändring" = round((folkokning_period/folkmangd_forsta_ar)*100,2),
             "region" = skapa_kortnamn_lan(region)) %>% 
        select(region,`Inrikes flyttnetto`,`Utrikes flyttnetto`,`Demografisk förändring`,`Total förändring`) %>% 
          pivot_longer(!c(region),names_to="variabel",values_to="forandring")

    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Befolkningsförändring" = slutgiltig_df))
    }
    
    if(returnera_data == TRUE){
      assign("befolkningsforandring", slutgiltig_df, envir = .GlobalEnv)
    }
    
  diagram_capt <- "Källa: Befolkningsregistret i SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  
  if(all(alder == "tot")) {
    diagramtitel <- paste0("Befolkningsförändring (hela befolkningen) år ",min(bef_df$år),"-",max(bef_df$år))
  }else diagramtitel <- paste0("Befolkningsförändring (",min(bef_df$ålder)," - ",max(bef_df$ålder),") år ",min(bef_df$år),"-",max(bef_df$år))
  
  objektnamn <- "befolkningsforandring_20_64"
  diagramfilnamn <- paste0(objektnamn,".png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df =slutgiltig_df %>% 
                                        filter(variabel!="Total förändring"),
                                      skickad_x_var = "region",
                                      skickad_y_var = "forandring",
                                      skickad_x_grupp = "variabel",
                                      manual_color = diagramfarger("rus_sex"),
                                      diagram_titel = diagramtitel,
                                      diagram_capt =  diagram_capt,
                                      diagram_facet = FALSE,
                                      x_axis_lutning = 0,
                                      x_axis_sort_value = TRUE,
                                      diagram_liggande = TRUE,
                                      stodlinjer_avrunda_fem = TRUE,
                                      geom_position_stack = TRUE,
                                      manual_y_axis_title="procent",
                                      berakna_index = FALSE,
                                      output_mapp = "",
                                      filnamn_diagram = "diagramfilnamn",
                                      skriv_till_diagramfil = FALSE)
  
    gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- c(objektnamn)
  
  if(returnera_figur == TRUE) return(gg_list)
  
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  }
  
}

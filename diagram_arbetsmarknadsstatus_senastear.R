diagram_arbetsmarknadsstatus_kommun <-function(region_vekt = hamtakommuner("20"), # Använd förslagsvis hamtakommuner och hamtaallalan
                                               fokus_lan = "20", # Måste väljas. Det län som, vid sidan om riket, fokuseras i figuren. Gäller inte vid könsuppdelat
                                               output_mapp_data = NA, # Outputmapp för data
                                               filnamn_data = "arbetsmarknadsstatus.xlsx", # Filnamn för datafil
                                               output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                               spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                               returnera_figur = TRUE, # Returnerar en figur
                                               kon_klartext = "totalt", # Finns även c("kvinnor","män") om man vill ha könsuppdelat
                                               fodelseregion_klartext_vekt = "totalt", # Finns även c("inrikes född", "utrikes född"). Då skapas ett facet diagram. Ej uppdelat
                                               alder_klartext = "20-64 år", # finns: 15-19 år, 16-19 år, 20-24 år, 25-29 år, 30-34 år, 35-39 år, 40-44 år, 45-49 år, 50-54 år, 55-59 år, 60-64 år, 65-69 år, 70-74 år, 15-74 år, 16-64 år, 16-65 år, 20-64 år, 20-65 år
                                               valda_farger = diagramfarger("rus_tre_fokus"), # Ändra till kon om man vill ha de färgerna
                                               diag_arbetslosthet = TRUE, # TRUE för figur för arbetslöshet
                                               diag_arbetskraftsdeltagande = TRUE, #  arbetskraftsdeltagande
                                               diag_sysselsattningsgrad = TRUE, # "" sysselsättningsgrad
                                               returnera_data = FALSE){ # "" sysselsättningsgrad
  
  ## =================================================================================================================
  # Diagram för arbetslöshet, sysselsättningsgrad och arbetskraftsdeltagande för senaste observation. 
  # Går att dela upp på kön och ändra åldersgrupp (max 1 åt gången). Det går även att dela upp på utrikes/inrikes födda, vilket ger ett facet-diagram
  # Funkar både för län och kommuner, men bara senaste år
  # diag_arbetsloshet, diag_arbetskraftsdeltagande och diag_sysselsattningsgrad sätts till TRUE baserat på vilka variabler man vill ha
  # Senast uppdaterad av Jon Frank (2024-01-23)
  # Potentiell förbättring: Ändra så att man kan köra för utrikes/inrikes separat. Funkar nu, men diagramtitel hänger inte med.
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/sarbarhetsanalys/main/Skript/arbetsmarknadsstatus_kommun.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn<-c()

  source("G:/skript/hamta_data/hamta_bas_arbmarknstatus_manad.R")
  arbetsmarknadsstatus_df = hamta_bas_arbmarknstatus(region_vekt = region_vekt,
                                                     kon_klartext_vekt = kon_klartext,
                                                     alder_vekt_klartext = alder_klartext,
                                                     fodelseregion_klartext_vekt = fodelseregion_klartext_vekt,
                                                     cont_klartext_vekt = c("arbetslöshet","arbetskraftsdeltagande", "sysselsättningsgrad"),
                                                     tid_koder = "9999") 
  

  # Län att fokusera på
  valt_lan = skapa_kortnamn_lan(hamtaregion_kod_namn(fokus_lan)$region)
  # Tar bort län i länsnamn och gör om riket till Sverige
  arbetsmarknadsstatus_df$region = skapa_kortnamn_lan(arbetsmarknadsstatus_df$region,byt_ut_riket_mot_sverige = TRUE)
  
  # Sparar data
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    flik_lista=lst("Arbetsmarknadsstatus" = arbetsmarknadsstatus_df)
    write.xlsx(flik_lista,paste0(output_mapp_data,filnamn_data))
  }
  
  # Returnerar data 
  if(returnera_data == TRUE){
    assign("arbetsmarknadsstatus", arbetsmarknadsstatus_df, envir = .GlobalEnv)
  }
  
  if(diag_sysselsattningsgrad==TRUE){
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av befolkningen som är sysselsatt (sysselsättningsgrad)."
    diagramtitel <- paste0("Sysselsättningsgrad i åldersgruppen ",unique(arbetsmarknadsstatus_df$ålder), " i ",unique(arbetsmarknadsstatus_df$månad)," ",unique(arbetsmarknadsstatus_df$år))
    objektnamn <-c(objektnamn,("sysselsattningsgrad_senastear"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>% 
                                   filter(variabel == "sysselsättningsgrad") %>% 
                                     mutate(fokus = ifelse(region == valt_lan,1,
                                                           ifelse(region == "Sverige",2,0))), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "varde", 
                                 skickad_x_grupp =ifelse("totalt" %in% kon_klartext,NA,"kön"),
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 diagram_facet = ifelse(all(c("inrikes född","utrikes född")==fodelseregion_klartext_vekt),TRUE,FALSE),
                                 facet_grp = "födelseregion",
                                 manual_color = valda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_var_fokus = ifelse("totalt" %in% kon_klartext,"fokus",NA),
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = "sysselsattningsgrad_senastear.png",
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if(diag_arbetslosthet==TRUE){
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av personer i arbetskraften som är arbetslösa."
    diagramtitel <- paste0("Arbetslöshet i åldersgruppen ",unique(arbetsmarknadsstatus_df$ålder), " i ",unique(arbetsmarknadsstatus_df$månad)," ",unique(arbetsmarknadsstatus_df$år))
    objektnamn <-c(objektnamn,("arbetslosthet_senastear"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>%
                                   filter(variabel == "arbetslöshet") %>% 
                                     mutate(fokus = ifelse(region == valt_lan,1,
                                                           ifelse(region == "Sverige",2,0))), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "varde", 
                                 skickad_x_grupp = ifelse("totalt" %in% kon_klartext,NA,"kön"),
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = valda_farger,
                                 diagram_facet = ifelse(all(c("inrikes född","utrikes född")==fodelseregion_klartext_vekt),TRUE,FALSE),
                                 facet_grp = "födelseregion",
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_var_fokus = ifelse("totalt" %in% kon_klartext,"fokus",NA),
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = "arbetslöshet_senastear.png",
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if(diag_arbetskraftsdeltagande == TRUE){
    
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av personer i arbetskraften som är arbetslösa."
    diagramtitel <- paste0("Arbetslöshet i åldersgruppen",unique(arbetsmarknadsstatus_df$ålder), " i ",unique(arbetsmarknadsstatus_df$månad)," ",unique(arbetsmarknadsstatus_df$år))
    objektnamn <-c(objektnamn,("arbetskraftsdeltagande_senastear"))
    
    # Skapar diagram 
    gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df %>%
                                   filter(variabel == "arbetskraftsdeltagande") %>%  
                                     mutate(fokus = ifelse(region == valt_lan,1,
                                                           ifelse(region == "Sverige",2,0))), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "varde", 
                                 skickad_x_grupp = ifelse("totalt" %in% kon_klartext,NA,"kön"),
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 diagram_facet = ifelse(all(c("inrikes född","utrikes född")==fodelseregion_klartext_vekt),TRUE,FALSE),
                                 facet_grp = "födelseregion",
                                 manual_color = valda_farger,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 x_var_fokus = ifelse("totalt" %in% kon_klartext,"fokus",NA),
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = "arbetskraftsdeltagande_senastear.png",
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if(returnera_figur == TRUE){
    names(gg_list) <- objektnamn
    return(gg_list)
  }
  
}


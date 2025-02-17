diagram_arbetsmarknadsstatus_tidsserie <-function(region_vekt = "20", # Max 1 region åt gången
                                        output_mapp_data = NA, # Outputmapp för data
                                        filnamn_data = "arbetsmarknadsstatus.xlsx", # Filnamn för datafil
                                        output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                        spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                        returnera_figur = TRUE, # Returnerar en figur
                                        fodelseregion_klartext = "totalt", # Finns även c("inrikes född", "utrikes född"). Bara 1 åt gången
                                        diagram_facet = FALSE, # Dela upp diagrammet på födelseregion
                                        marginal_yaxis_facet = c(0,0), # Marginaler för y-axeln i facet-diagram
                                        alder_klartext = "20-64 år", #Välj enbart 1. Finns: 15-19 år, 16-19 år, 20-24 år, 25-29 år, 30-34 år, 35-39 år, 40-44 år, 45-49 år, 50-54 år, 55-59 år, 60-64 år, 65-69 år, 70-74 år, 15-74 år, 16-64 år, 16-65 år, 20-64 år, 20-65 år
                                        valda_farger = diagramfarger("rus_sex"), # Ändra till kon om man vill ha de färgerna
                                        returnera_data = FALSE, # Skall data returneras
                                        start_ar ="2020", # Startår för data. Finns från 2020
                                        data_namm = "arbetsmarknadsstatus_tidsserie"){ # Vad skall returnerat dataset heta. Viktigt om data returneras två gånger i samma projekt (annars skrivs de över)
  
  ## =================================================================================================================
  # Linjediagram för arbetslöshet för valda år
  # Går att skapa facet-diagram för inrikes/utrikes födda.
  # Skapad av Jon Frank (2024-04-18) - 
  # Potentiell förbättring: Facet-diagram blir i samma skala av någon oklar anledning (har testat både fixed och free).
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()
  
  if(diagram_facet == TRUE){
    fodelseregion = "*"
    } else {
      fodelseregion = fodelseregion_klartext
    }

  arbetsmarknadsstatus_df = hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(region_vekt = region_vekt,
                                                                                              alder_klartext = alder_klartext,
                                                                                              kon_klartext = "totalt",
                                                                                              fodelseregion_klartext = fodelseregion,
                                                                                              cont_klartext = "arbetslöshet",
                                                                                              wide_om_en_contvar = FALSE,
                                                                                              tid_koder = "*")  %>% 
    mutate(ar=substr(månad,1,4),
           manad_long=format(as.Date(paste(ar, str_sub(månad, 6,7),"1", sep = "-")), "%B"),
           Period=paste(ar, str_sub(månad, 6,7),sep = "-")) %>% 
    select(-månad) %>% 
      filter(ar>=start_ar) 
  
  
  # Tar bort län i länsnamn och gör om riket till Sverige
  arbetsmarknadsstatus_df$region = skapa_kortnamn_lan(arbetsmarknadsstatus_df$region,byt_ut_riket_mot_sverige = TRUE)
  
  # Sparar data
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    flik_lista=lst("Arbetsmarknadsstatus" = arbetsmarknadsstatus_df)
    write.xlsx(flik_lista,paste0(output_mapp_data,filnamn_data))
  }
  
  # Returnerar data 
  if(returnera_data == TRUE){
    assign(data_namm, arbetsmarknadsstatus_df, envir = .GlobalEnv)
  }
    
    
    if(fodelseregion_klartext == "totalt") diagram_titel <- paste0("Arbetslöshet i åldersgruppen " ,alder_klartext," i ",unique(arbetsmarknadsstatus_df$region))
    if(fodelseregion_klartext == "inrikes född")  diagram_titel <- paste0("Arbetslöshet för inrikes födda i åldersgruppen " ,alder_klartext," i ",unique(arbetsmarknadsstatus_df$region))
    if(fodelseregion_klartext == "utrikes född")  diagram_titel <- paste0("Arbetslöshet för utrikes födda i åldersgruppen " ,alder_klartext," i ",unique(arbetsmarknadsstatus_df$region))
   
    diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna" 
    
    diagramfilnamn <- paste0("arbetslöshet_tidsserie_",fodelseregion_klartext,"_",unique(arbetsmarknadsstatus_df$region),".png")
    objektnamn <- c(objektnamn,str_remove(diagramfilnamn,".png"))
    
    # arbetsmarknadsstatus_tidsserie$manad_long <- factor(arbetsmarknadsstatus_tidsserie$manad_long,levels  =c("januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december"))
    
    arb_df = arbetsmarknadsstatus_df %>% 
        mutate(region = ifelse(region=="Riket","Sverige",region),
               manad_long = str_to_title(manad_long)) %>% 
          mutate(manad_long = factor(.$manad_long,levels =c("Januari","Februari","Mars","April","Maj","Juni","Juli","Augusti","September","Oktober","November","December")))
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = arb_df %>% 
                                  filter(födelseregion == fodelseregion_klartext),
                                            skickad_x_var = "manad_long", 
                                            skickad_y_var = "varde", 
                                            skickad_x_grupp = "ar",
                                            manual_color = valda_farger,
                                            lagga_till_punkter = TRUE,
                                            diagram_titel = diagram_titel,
                                            diagram_capt =  diagram_capt,
                                            output_mapp = output_mapp_figur,
                                            stodlinjer_avrunda_fem = TRUE,
                                            manual_y_axis_title = "procent",
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    
    if(diagram_facet == TRUE){
      
      diagram_capt = "Källa: SCB:s öppna statistikdatabas, Befolkningens arbetsmarknadsstatus (BAS)\nBearbetning: Samhällsanalys, Region Dalarna" 
      diagram_titel <- paste0("Arbetslöshet i åldersgruppen " ,alder_klartext," i ",unique(arbetsmarknadsstatus_df$region))
      
      diagramfilnamn <- paste0("arbetslöshet_tidsserie_facet_",fodelseregion_klartext,"_",unique(arbetsmarknadsstatus_df$region),".png")
      objektnamn <- c(objektnamn,str_remove(diagramfilnamn,".png"))
      
      gg_obj <- SkapaLinjeDiagram(skickad_df = arb_df %>%
                                    filter(födelseregion != "totalt") %>% 
                                      mutate(födelseregion = factor(födelseregion,levels = c("inrikes född","utrikes född"))),
                                  skickad_x_var = "manad_long", 
                                  skickad_y_var = "varde", 
                                  skickad_x_grupp = "ar",
                                  diagram_facet = TRUE,
                                  facet_scale = "free",
                                  facet_grp = "födelseregion",
                                  facet_legend_bottom = TRUE,
                                  manual_color = valda_farger,
                                  marginal_y_axis = marginal_yaxis_facet,
                                  lagga_till_punkter = TRUE,
                                  diagram_titel = diagram_titel,
                                  diagram_capt =  diagram_capt,
                                  output_mapp = output_mapp_figur,
                                  stodlinjer_avrunda_fem = FALSE,
                                  manual_y_axis_title = "procent",
                                  filnamn_diagram = diagramfilnamn,
                                  skriv_till_diagramfil = spara_figur)
      
      gg_list <- c(gg_list, list(gg_obj))
    }
  

    names(gg_list) <- objektnamn
  
    if(returnera_figur == TRUE){
      return(gg_list)
    }
  
}


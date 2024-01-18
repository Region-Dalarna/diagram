diag_bransch_utb_alder <- function(output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                   output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   diag_utbildningsniva = TRUE, # Jämför antingen kommuner eller län (beroende på val under jmf_omrade nedan)
                                   diag_alder = TRUE, # Jämför bransch i valt län/kommun
                                   spara_figur = TRUE, # Skall figur sparas
                                   filnamn_data = "bransch_utbildning_alder.xlsx", # Filnamn på sparad data
                                   vald_farg = diagramfarger("rus_sex"), # Val av diagramfärger
                                   returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                                   returnera_data = FALSE){ # Skall data returneras till R-studios globala miljö
  
  # =================================================================================================================
  # Diagram som jämför utbildningsnivå och ålder för förvärvsarbetande inom olika branscher i Dalarna
  # Data hämtas från NMS
  # Skapad av Jon 2024-01-18
  # Senast uppdaterad:
  # Data uppdaterat senast: 2024-01-18 
  # Fil för att hämta data finns på MONA/NMS: P1079_GEM/Jon/Kompetensförsörjning/utbildningsniva_bransch_korrekt.R
  # =================================================================================================================
  
  # Utbildningsgrupper för samtliga län senaste år
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         openxlsx)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  # Skapar listor
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c() # Används för att namnge objekt i lista
  list_data <- lst() # Skapa tom lista som används för att spara till Excel.

  # Läser in data
  bransch_utb_alder_df <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/18_jan_24_utdata_forvarvsarbetande_bransch.xlsx") %>% 
    mutate(AstLan_namn = skapa_kortnamn_lan(AstLan_namn))

  diagram_capt <- "Källa: NMS-databasen (SCB), databasen Stativ\nBearbetning: Samhällsanalys, Region Dalarna"

  if(diag_utbildningsniva == TRUE){
    
    # Variabler att gruppera på
    variabellista = c("year","AstLan_namn","SNI2007_Grupp_namn","Sun2020Niva_grov_namn")
    bransch_utb_alder_df[bransch_utb_alder_df == "Eftergymnasial utbildning"] <- "eftergymnasial"
    bransch_utb_alder_df[bransch_utb_alder_df == "Förgymnasial utbildning"] <- "förgymnasial"
    bransch_utb_alder_df[bransch_utb_alder_df == "grundskola"] <- "förgymnasial"
    bransch_utb_alder_df[bransch_utb_alder_df == "Gymnasial utbildning"] <- "gymnasial"
    bransch_utb_alder_df[bransch_utb_alder_df == "Uppgift saknas"] <- "Okänd"
    
    # Grupperar på år, län, bransch och utbildning 
    #  Drar bort en obefintlig summa för att diagrammet skall gå till 100.
    bransch_utb_df_sum <- bransch_utb_alder_df %>%
      group_by(across(any_of(variabellista))) %>%  
        summarize(Antal=sum(antal)) %>% 
          mutate(Andel=(Antal/sum(Antal)*100)-0.001)
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Utbildningsnivå" = bransch_utb_df_sum))
    }
    
    if(returnera_data == TRUE){
      assign("bransch_utbildningsniva", bransch_utb_df_sum, envir = .GlobalEnv)
    }
    
    # Diagrammets titel
    diagram_titel <- paste0("Utbildningsnivå för förvärvsarbetande 16-74 år per bransch i ",unique(bransch_utb_df_sum$AstLan_namn)," ",unique(bransch_utb_alder_df$year))
    objektnamn <- paste0("utbildningsniva_bransch_",unique(bransch_utb_df_sum$AstLan_namn))
    diagramfil <-  paste0(objektnamn,".png")
    
    # Skapar en separat färgkod, så att okänd blir ljusgrå
    farger_gra <- c(rgb(211,211,211, maxColorValue = 255),diagramfarger("rus_sex"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = bransch_utb_df_sum %>% 
                                            filter(SNI2007_Grupp_namn!="Okänt") %>% 
                                              mutate(Sun2020Niva_grov_namn = factor(Sun2020Niva_grov_namn,levels = c("Okänd","eftergymnasial","gymnasial","förgymnasial"))), 
                                          skickad_x_var = "SNI2007_Grupp_namn", 
                                          skickad_y_var = "Andel", 
                                          skickad_x_grupp = "Sun2020Niva_grov_namn",
                                          manual_color = farger_gra,
                                          x_axis_lutning = 0,
                                          diagram_titel = diagram_titel,
                                          x_axis_sort_value = TRUE,
                                          x_axis_sort_grp = 4,
                                          diagram_capt = diagram_capt,
                                          #procent_0_100_10intervaller = TRUE,
                                          stodlinjer_avrunda_fem = TRUE,
                                          legend_vand_ordning = TRUE,
                                          diagram_liggande = TRUE,
                                          geom_position_stack = TRUE,
                                          manual_y_axis_title = "procent",
                                          output_mapp = output_mapp_figur,
                                          filnamn_diagram = diagramfil,
                                          skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }


  if(diag_alder == TRUE){
    
    variabellista = c("year","AstLan_namn","SNI2007_Grupp_namn","alder_grupper")
    
    # Grupperar på år, län, bransch och utbildning
    #  Drar bort en obefintlig summa för att diagrammet skall gå till 100.
    bransch_alder_df_sum <- bransch_utb_alder_df %>%
      group_by(across(any_of(variabellista))) %>%  
        summarize(Antal=sum(antal)) %>% 
          mutate(Andel=(Antal/sum(Antal)*100)-0.001)
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Ålder" = bransch_alder_df_sum))
    }
    
    if(returnera_data == TRUE){
      assign("bransch_alder", bransch_utb_df_sum, envir = .GlobalEnv)
    }

    diagram_titel <- paste0("Åldersfördelning för förvärvsarbetande 16-74 år per bransch i ",unique(bransch_utb_df_sum$AstLan_namn)," ",unique(bransch_utb_df_sum$bransch_alder_df_sum)," ",unique(bransch_alder_df_sum$year))
    objektnamn <- c(objektnamn,paste0("alder_bransch_",unique(bransch_alder_df_sum$AstLan_namn)))
    diagramfil <- paste0(objektnamn,".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = bransch_alder_df_sum %>% 
                                              filter(SNI2007_Grupp_namn!="Okänt") %>% 
                                                mutate(alder_grupper = factor(alder_grupper,levels = c("71-74 år","65-70 år","50-64 år","35-49 år","20-34 år","16-19 år"))), 
                                            skickad_x_var = "SNI2007_Grupp_namn", 
                                            skickad_y_var = "Andel", 
                                            skickad_x_grupp = "alder_grupper",
                                            manual_x_axis_text_vjust=1,
                                            manual_x_axis_text_hjust=1,
                                            manual_color = diagramfarger("rus_sex"),
                                            diagram_titel = diagram_titel,
                                            x_axis_sort_value = TRUE,
                                            x_axis_lutning = 0,
                                            x_axis_sort_grp = 6,
                                            diagram_capt = diagram_capt,
                                            stodlinjer_avrunda_fem = TRUE,
                                            diagram_liggande = TRUE,
                                            legend_vand_ordning = TRUE,
                                            geom_position_stack = TRUE,
                                            manual_y_axis_title = "procent",
                                            output_mapp = output_mapp_figur,
                                            filnamn_diagram = diagramfil,
                                            skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
  }
  
  names(gg_list) <- c(objektnamn)
  
  if(returnera_figur == TRUE) return(gg_list)
  
  if (!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp,filnamn))
  }

                                   }

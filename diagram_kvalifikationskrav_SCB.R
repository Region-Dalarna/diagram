diagram_kvalifikationskrav <- function(region_vekt = "20", # Vilken region vill man titta på. Enbart en får väljas.
                                              output_mapp_data = NA, # Om man vill spara data. Används primärt i Rmarkdown-rapporter.
                                              output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                              diag_jmf_omrade = TRUE, # Jämför antingen kommuner eller län (beroende på val under jmf_omrade nedan)
                                              diag_jmf_bransch = TRUE, # Jämför bransch i valt län/kommun
                                              jmf_omrade = "lan", # Om "lan", jämförs sveriges alla län. Om "kommun", jämförs kommuner i valt län (region_vekt) eller i samma län som vald kommun (region_vekt)
                                              spara_figur = TRUE,
                                              filnamn_data = "kvalifikationskrav.xlsx", # Filnamn på sparad data
                                              kon_klartext = c("män","kvinnor"), # män och kvinnor ger totalt. Det går även att välja ett av könen. Jämförelse mellan kön är inte möjlig.
                                              vald_farg = diagramfarger("rus_sex"), # Val av diagramfärger
                                              returnera_figur = TRUE, # Skall figuren returneras som ett ggplot-objekt
                                              returnera_data = FALSE){ # Skall data returneras
  
  
  # =================================================================================================================
  # Diagram som jämför kvalifikationskrav inom olika områden (kommun eller län) och bransch i valt län/kommun 
  # Funkar enbart för senaste år
  # Skapad av Jon 2024-01-17
  # Senast uppdaterad:
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
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_yrke_bransch_SCB.R")
  
  valt_lan = hamtaregion_kod_namn(region_vekt)[[2]]
  
  if(jmf_omrade == "lan"){
    region_vekt = hamtaAllaLan()
  }
  if(jmf_omrade == "kommun"){
    region_vekt = hamtakommuner(substr(region_vekt,1,2))
  }
  
  # Sourcar data för arbetslöshet
  px_df <-  hamta_data_yrken_bransch(region_vekt = region_vekt,
                                     kon_klartext = kon_klartext,
                                     tid = "9999",
                                     returnera_data = TRUE) 
  
  # Ändra namn på vissa branscher
  px_df$Branschgrupp <- case_when(
    px_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske" ~ "Jordbruk och skogsbruk",
    px_df$`näringsgren SNI 2007` == "tillverkning och utvinning" ~ "Tillverkning och utvinning",
    px_df$`näringsgren SNI 2007` == "energiförsörjning; miljöverksamhet" ~ "Energi och miljö",
    px_df$`näringsgren SNI 2007` == "byggverksamhet" ~ "Bygg",
    px_df$`näringsgren SNI 2007` == "handel" ~ "Handel",
    px_df$`näringsgren SNI 2007` == "transport och magasinering"~ "Transport",
    px_df$`näringsgren SNI 2007` == "hotell- och restaurangverksamhet" ~ "Hotell och restaurang",
    px_df$`näringsgren SNI 2007` == "information och kommunikation" ~ "IT och kommunikation",
    px_df$`näringsgren SNI 2007` == "finans- och försäkringsverksamhet" ~ "Finans och försäkring",
    px_df$`näringsgren SNI 2007` == "fastighetsverksamhet" ~ "Fastighet",
    px_df$`näringsgren SNI 2007` == "företagstjänster" ~ "Företagstjänster",
    px_df$`näringsgren SNI 2007` == "offentlig förvaltning och försvar"~ "Offentlig förvaltning",
    px_df$`näringsgren SNI 2007` == "utbildning " ~ "Utbildning",
    px_df$`näringsgren SNI 2007` == "vård och omsorg; sociala tjänster" ~ "Vård och omsorg",
    px_df$`näringsgren SNI 2007` == "kulturella och personliga tjänster m.m." ~ "Kultur m.m.",
    px_df$`näringsgren SNI 2007` == "okänd verksamhet"~ "Okänd verksamhet")
  
  # Klassificerar yrken utifrån hur avancerade de är
  px_df$kompetensniva <- case_when(
    substr(px_df$yrkeskod,1,1)=="1" ~ "Chefsyrken",
      between(as.integer(substr(px_df$yrkeskod,1,1)),2,2) ~ "Motsvarande fördjupad högskolekompetens",
        between(as.integer(substr(px_df$yrkeskod,1,1)),3,3) ~ "Motsvarande högskolekompetens",
         between(as.integer(substr(px_df$yrkeskod,1,1)),4,8) ~ "Motsvarande gymnasial kompetens",
          substr(px_df$yrkeskod,1,1) =="0" & px_df$yrkeskod !="0002" ~ "Motsvarande gymnasial kompetens",
             px_df$yrkeskod =="0002" ~ "Okänt",
              substr(px_df$yrkeskod,1,1) =="9" ~ "Enklare yrken")
  
  if(diag_jmf_omrade == TRUE){
    if("kvinnor" %in% unique(px_df$kön) & "män" %in% unique(px_df$kön)) {
      variabellista = c("år","region","kompetensniva")
      diagram_titel <- paste0("Kvalifikationskrav för anställda (16-64 år) ",max(px_df$år))
      objektnamn <- paste0("kvalifikationskrav_jmf_",jmf_omrade)
    }else {
      variabellista = c("år","kön","region","kompetensniva")
      diagram_titel <- paste0("Kvalifikationskrav för anställda ",unique(px_df$kön) ," (16-64 år) i ",max(px_df$år))
      objektnamn <- paste0("kvalifikationskrav_jmf_",unique(px_df$kön),"_",jmf_omrade)
    }

    diagram_capt <- "Källa: Yrkesregistret i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    
    px_df_sum <- px_df %>% 
        group_by(across(any_of(variabellista))) %>%
          summarize(Antal = sum(`Anställda 16-64 år (dagbef)`)) %>%
            mutate(Andel = ((Antal/sum(Antal))*100)-0.001) %>% 
              ungroup()
    
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Jmf" = px_df_sum))
    }
    
    if(returnera_data == TRUE){
      assign("kvalifikationskrav_jmf", px_df_sum, envir = .GlobalEnv)
    }
    
    diagramfil <- paste0("kompetenskrav_jmf_",jmf_omrade,".png")
              
   
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_sum %>% 
                                              mutate(region = skapa_kortnamn_lan(region),
                                                     kompetensniva = factor(kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])),
                                            skickad_x_var = "region",
                                            skickad_y_var = "Andel",
                                            skickad_x_grupp = "kompetensniva",
                                            manual_color = c(rgb(211,211,211, maxColorValue = 255),vald_farg),
                                            diagram_titel = diagram_titel,
                                            x_axis_lutning = 0,
                                            diagram_capt = diagram_capt,
                                            legend_vand_ordning = TRUE,
                                            diagram_liggande = TRUE,
                                            geom_position_stack = TRUE,
                                            manual_y_axis_title = "procent",
                                            stodlinjer_avrunda_fem = TRUE,
                                            x_axis_sort_value = TRUE,
                                            x_axis_sort_grp = 6,
                                            vand_sortering = TRUE,
                                            output_mapp = output_mapp_figur,
                                            filnamn_diagram = diagramfil,
                                            skriv_till_diagramfil = spara_figur)
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  
  if(diag_jmf_bransch == TRUE){
    if("kvinnor" %in% unique(px_df$kön) & "män" %in% unique(px_df$kön)) {
      variabellista = c("år","region","Branschgrupp","kompetensniva")
        diagram_titel <- paste0("Kvalifikationskrav för anställda (16-64 år) i ",valt_lan," ",max(px_df$år))
          objektnamn <- c(objektnamn,paste0("kvalifikationskrav_senastear_",valt_lan))
    }else {
      variabellista = c("år","kön","region","Branschgrupp","kompetensniva")
        diagram_titel <- paste0("Kvalifikationskrav för anställda ",unique(px_df$kön) ," (16-64 år) i ",valt_lan," ",max(px_df$år))
          objektnamn <- c(objektnamn,paste0("kvalifikationskrav_senastear_",unique(px_df$kön),"_",valt_lan))
    }
    
    diagram_capt <- "Källa: Yrkesregistret i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
    
    px_df_sum <- px_df %>%
      filter(år == max(år),region == valt_lan) %>%
        group_by(across(any_of(variabellista))) %>%
      summarize(Antal = sum(`Anställda 16-64 år (dagbef)`)) %>%
            mutate(Andel=((Antal/sum(Antal))*100),
                   Andel = ifelse(Andel<0.001,Andel,Andel-0.001))
  
    if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("Bransch" = px_df_sum))
    }
    
    if(returnera_data == TRUE){
      assign("kvalifikationskrav_jmf_bransch", px_df_sum, envir = .GlobalEnv)
    }
   
    diagramfil <- "kompetenskrav_bransch.png"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_sum %>% 
                                                  filter(Branschgrupp != "Okänd verksamhet") %>% 
                                                  mutate(kompetensniva = factor(kompetensniva, levels = c("Enklare yrken","Motsvarande gymnasial kompetens","Motsvarande högskolekompetens","Motsvarande fördjupad högskolekompetens","Chefsyrken","Okänt")[6:1])), 
                                                skickad_x_var = "Branschgrupp",
                                                skickad_y_var = "Andel",
                                                skickad_x_grupp = "kompetensniva",
                                                manual_color = c(rgb(211,211,211, maxColorValue = 255),diagramfarger("rus_sex")),
                                                diagram_titel = diagram_titel,
                                                legend_vand_ordning = TRUE,
                                                diagram_capt = diagram_capt,
                                                x_axis_lutning = 0,
                                                diagram_liggande = TRUE,
                                                x_axis_sort_value = TRUE,
                                                x_axis_sort_grp = 6,
                                                vand_sortering = TRUE,
                                                geom_position_stack = TRUE,
                                                manual_y_axis_title = "procent",
                                                stodlinjer_avrunda_fem = TRUE,
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

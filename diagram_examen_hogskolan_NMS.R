diagram_examen_hogskolan_NMS <- function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                  output_mapp_data = NA, # Här hamnar sparad data
                                  filnamn_data = "hogsoleexamen.xlsx",
                                  filnamn_figur = "hogskoleexamen.png",
                                  valda_farger = diagramfarger("rus_sex"), # Vilka färger skall användas i diagram
                                  valda_ar = c(2010,2015,"9999"), # "99" ger första året, "9999" ger senaste året
                                  caption = "Källa: NMS-databasen (SCB), högskoleregistret\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Enbart program/områden med minst 5 examinerade",
                                  spara_figur = TRUE, # Om true sparas figuren till output_mapp
                                  returnera_figur = TRUE, # Skall figur returneras (i en lista)
                                  returnera_data = FALSE){ # Skall data returneras (till R-studios globla miljö)
  
  # ========================================== Allmän info ============================================
  
  # 1: Skapar diagram för examinerade från Högskolan Dalarna, för antingen ett valt år eller ett antal år
  # Skapat av Jon Frank 2024-03-08
  # Data hämtas från NMS-databasen (SCB): P1079gem/Jon/kompetensförsörjning/hogskoleexamen_ny_2024.R
  # Om ny data har kommit behöver en ny databas läggas till i koden på NMS.
  # Data hämtades senaste 2024-03-07 (finns till och med 2021)
  # ========================================== Inställningar ============================================
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
  # =============================================== API-uttag ===============================================
  
  
  # Läs in datafil med högskoleexamen
  df <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/7_mar_24_Dalarna_hogskoleexamen.xlsx")
  # Mall för att få klartext (utbildningsnamn)
  mall_sun <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/mall_sun2020inr.xlsx", sheet = 1)
  mall_sun_2siffer <- read.xlsx("G:/skript/projekt/data/kompetensforsorjning/mall_sun2020inr.xlsx", sheet = 2) %>% 
    mutate("SUN2020Inr_2siffer" = as.character(SUN2020Inr_2siffer))
  
  # Alla utbildningar
  # Plockar ut de två första siffrorna i sun-koden (för att matcha mot en bredare mall)
  df <- df %>% 
    mutate("SUN2020Inr_2siffer"= substr(SUN2020Inr,1,2)) %>% 
      left_join(mall_sun,by = "SUN2020Inr") %>% 
        left_join(mall_sun_2siffer, by = "SUN2020Inr_2siffer") %>% 
          relocate(antal, .after = SUN2020Inr_2siffer_namn)
  
  if("99" %in% valda_ar) valda_ar <- replace(valda_ar,valda_ar=="99",min(df$Lar))
  if("9999" %in% valda_ar) valda_ar <- replace(valda_ar,valda_ar=="9999",max(df$Lar))
  
  # Grupperar på år och den bredare utbildningsnivån (2 siffror). 
  df <- df %>% 
    filter(Lar %in%valda_ar) %>%
      group_by(Lar,SUN2020Inr_2siffer_namn) %>% 
        summarize(antal=sum(antal))
  
  # Returnerar data till R globala miljö
  if(returnera_data == TRUE){
    assign("hogskoleexamen_df", df, envir = .GlobalEnv)
  }
  
  # =============================================== Diagram ===============================================
  
  if(length(valda_ar) == 1) diagram_titel <- paste0("Antal examinerade från Högskolan Dalarna låsåret ",max(df$Lar)," per program") else diagram_titel <- "Antal examinerade från Högskolan Dalarna per program"
  diagramfil <- filnamn_figur
  
  gg_obj <- SkapaStapelDiagram(skickad_df = df%>%
                                 filter(SUN2020Inr_2siffer_namn%in%unique(df %>%
                                                                            filter(Lar == max(.$Lar)) %>%
                                                                            filter(antal > 4) %>% 
                                                                            .$SUN2020Inr_2siffer_namn)),
                               skickad_x_var = "SUN2020Inr_2siffer_namn",
                               skickad_y_var = "antal",
                               skickad_x_grupp = ifelse(length(valda_ar) == 1,NA,"Lar"),
                               manual_color = diagramfarger("rus_sex"),
                               diagram_titel = diagram_titel,
                               x_axis_sort_value = TRUE,
                               diagram_capt = caption,
                               x_axis_lutning = 0,
                               diagram_liggande = TRUE,
                               manual_y_axis_title = "",
                               manual_x_axis_title = "",
                               output_mapp = output_mapp_figur,
                               filnamn_diagram = diagramfil,
                               skriv_till_diagramfil = spara_figur)
  
  
  gg_list <- c(gg_list, list(gg_obj))
  
  names(gg_list) <- "hogskoleexamen"
  if(returnera_figur == TRUE) return(gg_list)
  
  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    write.xlsx(df,paste0(output_mapp_data,filnamn_data))
  }
  
}

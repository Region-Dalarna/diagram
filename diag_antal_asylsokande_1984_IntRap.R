diagram_asylsokande_tidsserie <-function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Outputmapp för figur
                                         spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                         visa_logga_i_diagram = FALSE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
                                         logga_sokvag = NA,                               # sökväg till logga som ska visas i diagrammet. 
                                         start_ar = 1984, # Välj ett startår, s
                                         returnera_figur = TRUE, # Returnerar en figur
                                         valda_farger = diagramfarger("rus_sex"),
                                         returnera_data = FALSE) # Skall data returneras)
{
  
  ## =================================================================================================================
  # En tidsserie för antal asylsökande i Sverige från 1984 och framåt.
  # Data för de första åren finns inte via API utan hämtas från en Excelfil som ligger i en mapp på G:
  # Används i integrationsrapporten
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         pxweb)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  objektnamn <- c()
  
  input_mapp <- "G:/skript/projekt/data/integration"
  files <- list.files(input_mapp, pattern = "asyl", full.names = TRUE)
  
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  asyl_1984 <- read.xlsx(latest_file,startRow = 2) %>%
    rename(år = "År",
           Antal = "Asylsökande")
  
  pxweb_query_list <-
    list("Medbland" = "SAMTL",
         "Kon" = "1+2",
         "ContentsCode" = "000003WL",
         "Period" = "hel",
         "Tid" = "*")
  
  asylsokande <-
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101P/AsylsokandeN",
              query = pxweb_query_list)
  
  
  asylsokande <- as.data.frame(asylsokande, column.name.type = "text", variable.value.type = "text") %>%
    select(år,Antal)
  
  asylsokande_df <- rbind(asyl_1984 %>% filter(!(år %in% unique(asylsokande$år))),asylsokande) %>%
    filter(is.na(Antal) == FALSE) %>% 
      filter(år >= start_ar) 
  
  if(returnera_data == TRUE){
    assign("asylsokande_df", asylsokande_df, envir = .GlobalEnv)
  }
  
  diagram_capt = "Källa: Migrationsverket, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  diagram_titel = "Antal asylsökande i Sverige"
  diagramfilnamn <- "asylsokande_antal.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = asylsokande_df,
                               skickad_x_var = "år",
                               skickad_y_var = "Antal",
                               manual_color = valda_farger,
                               diagram_titel = diagram_titel,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               diagram_capt =  diagram_capt,
                               output_mapp = output_mapp_figur,
                               stodlinjer_avrunda_fem = TRUE,
                               x_axis_visa_var_xe_etikett = 2,
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

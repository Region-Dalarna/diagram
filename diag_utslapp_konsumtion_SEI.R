diagram_konsumtionsutslapp <- function(region_vekt = hamtaAllaLan(), # Finns län, riket och kommuner. "*" ger alla. 
                                       jmf = c("00","20"), # Vilka län vill man göra en jämförelse med i tidsserieskriptet. Måste vara en delmängd av region_vekt.
                                       lan_kommun = "20", # För vilket län skall kommuer jämföras. 
                                       fokus_region = "20", # Vilken region skall man fokusera på i jämförande diagram mellan län (förutom Sverige)
                                       diag_lan = TRUE, # Ger jämförelse mellan län för senaste år och en tidsserie där jämförelse görs enlig variabeln jmf ovan
                                       diag_kommun = TRUE, # Ger jämförelse på senaste år för kommuner i valt län
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       returnera_data = FALSE, # Skall data returneras till gloval environment
                                       ggobjektfilnamn_utan_tid = TRUE, # Objektnamnet sparas utan tid i namnet. Fördelaktigt att använda i rapporter
                                       spara_figur = FALSE){ # Skall figuren sparas till valt output_mapp
  
  # ===========================================================================================================
  # Data från https://konsumtionskompassen.se/ (SEI) . 
  # Tre diagram. Senaste år för län, tidsserie där län jämförs med Sverige och slutligen senaste år för kommun
  # Skapad av Jon 2026-09-03
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse,
                 rKolada,
                 readxl,
                 glue)
  
  gg_list <- list()
  
  source("G:/skript/jon/Webbskrapning/hamta_data_konsumtionskompassen.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  
  if(diag_lan == TRUE){
    
    konsumtionskompassen_df <- get_kompassen_data(region = region_vekt)  |> 
      mutate(region_namn = skapa_kortnamn_lan(region_namn)) |> 
      rename(varde = kg_co2e_per_capita)
    
    if(returnera_data == TRUE){
      assign("konsumtionskompassen_lan_df", konsumtionskompassen_df, envir = .GlobalEnv)
    }
    
    ValdGeografi <- skapa_kortnamn_lan(hamtaregion_kod_namn(fokus_region)$region,byt_ut_riket_mot_sverige = TRUE)
    
    # Jämför län för senaste år
    diagram_titel <- paste0("Hushållens totalt utsläpp år ",max(konsumtionskompassen_df$year))
    diagramfilnamn <- glue("kons_utslapp_lan_ar_{max(konsumtionskompassen_df$year)}.png")
    diagram_capt = "Källa: https://www.konsumtionskompassen.se (Stockholm Environment Institute (SEI),2025)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: kg koldioxidekvivalenter per person. För en mer ingående förklaring av hur data har beräknats,\nse https://www.konsumtionskompassen.se"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = konsumtionskompassen_df |> 
                                   filter(year == max(year)) |> 
                                   mutate(fokus=ifelse(region_namn == ValdGeografi,1,ifelse(region_namn == "Sverige",2,0))),
                                 skickad_x_var = "region_namn",
                                 skickad_y_var = "varde",
                                 diagram_titel = diagram_titel,
                                 x_var_fokus = "fokus",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = "diagramfilnamn",
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = "kg koldioxidekvivalenter per person",
                                 manual_color = diagramfarger("rus_tre_fokus"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
    # Tidsserie där två eller flera regioner jämförs
    diagram_titel <- paste0("Förändring i hushållens totala utsläpp  ",min(konsumtionskompassen_df$year),"-",max(konsumtionskompassen_df$year))
    
    diagramfilnamn <- glue("kons_utslapp_tid_ar_{min(konsumtionskompassen_df$year)}_{max(konsumtionskompassen_df$year)}.png")
    
    gg_obj <- ggplot(konsumtionskompassen_df %>%
                       filter(regionkod %in% jmf) |> 
                       group_by(region_namn) %>%
                       mutate(index = varde / varde[year == min(year)] * 100) %>%
                       ungroup(), aes(x = year, y = index, color = region_namn, group = region_namn)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = diagramfarger("rus_sex")) +
      scale_x_continuous(
        breaks = unique(konsumtionskompassen_df$year),
        expand = expansion(mult = 0.01)
      ) +
      scale_y_continuous(
        limits = c(0, 120),
        breaks = seq(0, 120, 20),
        minor_breaks = seq(0, 120, 5),
        expand = c(0, 0)
      ) +
      labs(
        title = diagram_titel,
        x = NULL,
        y = "Index (startvärde 100)",
        color = NULL,
        caption =   diagram_capt 
        
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, face = "italic", size = 11),
        plot.caption.position = "plot",
        panel.grid.major = element_line(color = "grey60", linewidth = 0.4),
        panel.grid.minor.y = element_line(color = "grey85", linewidth = 0.3),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
      )
    
    if (spara_figur){
      ggsave(filename = paste0(output_mapp, diagramfilnamn),
             plot = p,
             width = 12,
             height = 7,
             dpi = 300)
    }
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
  }
  
  if(diag_kommun == TRUE){
    
    konsumtionskompassen_kommun_df <- get_kompassen_data(region = hamtakommuner(lan_kommun))  |> 
      mutate(region_namn = skapa_kortnamn_lan(region_namn)) |> 
      rename(varde = kg_co2e_per_capita)
    
    if(returnera_data == TRUE){
      assign("konsumtionskompassen_kommun_df", konsumtionskompassen_kommun_df, envir = .GlobalEnv)
    }
    
    ValdGeografi <- skapa_kortnamn_lan(hamtaregion_kod_namn(lan_kommun)$region,byt_ut_riket_mot_sverige = TRUE)
    
    # Jämför kommuner för senaste år
    diagram_titel <- paste0("Hushållens totalt utsläpp år ",max(konsumtionskompassen_df$year))
    diagramfilnamn <- glue("kons_utslapp_{ValdGeografi}_ar_{max(konsumtionskompassen_df$year)}.png")
    diagram_capt = "Källa: https://www.konsumtionskompassen.se (Stockholm Environment Institute (SEI),2025)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: kg koldioxidekvivalenter per person. För en mer ingående förklaring av hur data har beräknats,\nse https://www.konsumtionskompassen.se"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = konsumtionskompassen_kommun_df |> 
                                   filter(year == max(year)) |> 
                                   mutate(fokus=ifelse(region_namn == ValdGeografi,1,ifelse(region_namn == "Sverige",2,0))),
                                 skickad_x_var = "region_namn",
                                 skickad_y_var = "varde",
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_var_fokus = "fokus",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = "diagramfilnamn",
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = "kg koldioxidekvivalenter per person",
                                 manual_color = diagramfarger("rus_tre_fokus"),
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }
    
  }
  
  
  
  return(gg_list)
  
}


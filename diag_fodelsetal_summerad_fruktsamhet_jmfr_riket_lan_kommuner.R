

diag_fodelsetal_summerad_fruktsamhet_jmfr_riket_lan_kommuner <- function(
    region_vekt = "20",
    output_fold = NA,
    diag_fargvekt = NA,
    visa_dataetiketter = FALSE,
    skriv_diagramfil = TRUE
  ){
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_summerad_fruktsamhet_fodelsetal_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("rus_sex")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  # publicerad 11 juni 2024
  if (all(is.na(output_fold))) {
    if (exists("utskriftsmapp", mode = "function")) {
      if (dir.exists(utskriftsmapp())) output_fold <- utskriftsmapp() else stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output_fold ett värde.")
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output_fold ett värde.")
    }
  }
  
  # ============================ Dalarna, riket och länets kommuner ============================
  
  if (region_vekt == "00" | is.na(region_vekt)) hamta_regioner <- hamtaAllaLan() else {
    hamta_regioner <- hamtakommuner(str_sub(region_vekt, 1, 2) %>% unique(), T, T)
  }
  fodelsetal_df <- hamta_summerad_fruktsamhet_fodelsetal_scb(region_vekt = hamta_regioner) 
  
  if (all(nchar(hamta_regioner) == 2)) {
    
    fodelsetal_df <- fodelsetal_df %>%
      mutate(region = region %>% skapa_kortnamn_lan(T),
             fokus = case_when( 
               regionkod != "00" ~ "Län",
               regionkod == "00" ~ "Riket",
               TRUE ~ "Övriga"))
    
  } else {
    fodelsetal_df <- fodelsetal_df %>%
      mutate(region = region %>% skapa_kortnamn_lan(T),
             fokus = case_when(
               nchar(regionkod) == 4 ~ "Kommuner",
               regionkod != "00" ~ "Län",
               regionkod == "00" ~ "Riket",
               TRUE ~ "Övriga"))
    
  }
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  
  
  diagramtitel <- glue("Antal födda barn per kvinna år {min(fodelsetal_df$år)}-{max(fodelsetal_df$år)}")
  diagramfil <- glue("sum_fruktsamhet_riket_dalarna_plus_kommuner_{min(fodelsetal_df$år)}_{max(fodelsetal_df$år)}.png") %>% str_replace_all("__", "_")
  
  
  gg_obj <- SkapaStapelDiagram(skickad_df = fodelsetal_df,
                               skickad_x_var = "år",
                               skickad_y_var = "Antal",
                               x_var_fokus = "fokus",
                               #skickad_x_grupp = "region",
                               #x_axis_sort_value = TRUE,
                               #x_axis_storlek = 6,
                               diagram_titel = diagramtitel,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               dataetiketter = visa_dataetiketter,
                               manual_y_axis_title = "Summerad fruktsamhet (antal födda barn per kvinna)",
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_color = diagramfarger("rus_sex")[c(3, 2, 1)],
                               fokusera_varden = list(list(geom = "rect", ymin=2.097, ymax=2.103, xmin=0, xmax=Inf, alpha=1, fill="black")),
                               output_mapp = output_fold,
                               skriv_till_diagramfil = FALSE,
                               diagram_facet = TRUE,
                               facet_grp = "region",
                               facet_scale = "fixed",
                               facet_x_axis_storlek = 5,
                               facet_legend_bottom = TRUE
  )
  
  # Skapa en legend för att visa reproduktionsnivån
  dia_med_legend <- gg_obj + 
    geom_hline(aes(yintercept = 2.1, color = "Reproduktionsnivån (2,1 barn per kvinna)"), linewidth = 0.8) +
    scale_color_manual(
      name = "",  # eller "Förklaring" om du vill ha rubrik
      values = c("Reproduktionsnivån (2,1 barn per kvinna)" = "black")
    ) +
    guides(color = guide_legend(override.aes = list(linetype = "solid", linewidth = 0.8))) +
    theme(
      legend.position = "bottom",
      legend.key = element_rect(fill = "white"),
      legend.margin = margin(t = 10)
    ) + guides(fill = "none")
  
  if (skriv_diagramfil) {
  suppressMessages(
  skriv_till_diagramfil(dia_med_legend,
                        output_mapp = output_fold,
                        filnamn_diagram = diagramfil)
  )}
  return(dia_med_legend)
}

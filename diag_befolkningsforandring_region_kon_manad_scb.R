



diag_befolkningsforandring_manad_scb <- function(region_vekt = "20",
                                                 region_fokus = NA,                     # om man vill lyfta fram någon eller några regioner (görs med regionkoder)
                                                 facet_region = TRUE,
                                                 befforandr_klartext = c("folkmängd", "folkökning"),
                                                 kon_klartext = "totalt",
                                                 tid_koder = "*",
                                                 nth_etikett = 3,
                                                 skriv_diagramfil = TRUE,
                                                 utmapp = utskriftsmapp(),
                                                 diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
                                                 ) {
    
    
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_forandringar_region_kon_manad_scb.R" )
  options(dplyr.summarise.inform = FALSE)
  
  
  beffor_df <- hamta_befolkningsforandringar_manad(region_vekt = region_vekt,
                                                   befforandr_klartext = befforandr_klartext,
                                                   kon_klartext = kon_klartext,
                                                   tid_koder = tid_koder)
  
  beffor_df <- beffor_df %>% 
    manader_bearbeta_scbtabeller() %>%                      # splittar kolumnen månad (t ex "2021M07" för juli år 2021) till kolumner för år, månad, år_månad samt månad_år
    mutate(region = region %>% skapa_kortnamn_lan())
  
  konsuppdelat <- ifelse(length(unique(beffor_df$kön)) > 1, TRUE, FALSE)
  kon_txt <- ifelse(konsuppdelat, "_kon", "")          # skapa textsträng för om diagrammet är könsuppdelat eller inte
  
  # skapa sortering av regioner där vissa kan läggas först i facet-diagrammen
  if (!is.na(region_fokus)) {
    reg_ej_fokus <- unique(beffor_df$region[!beffor_df$regionkod %in% region_fokus]) %>% sort()
    reg_fokus <- unique(beffor_df$region[beffor_df$regionkod %in% region_fokus])
    region_sort <- c(reg_fokus, reg_ej_fokus)
    
    beffor_df <- beffor_df %>% 
      mutate(region = factor(region, levels = region_sort),
             fokus = ifelse(regionkod %in% region_annan_farg, 2,1)) 
  } else beffor_df <- beffor_df %>% mutate(fokus = 1)
  
  # bestäm vilken färgvektor som ska användas
  color_vekt <- if(length(unique(beffor_df$fokus)) > 1) diagramfarger("rd_gron")[c(1,4)] else diagramfarger("rd_gron")[1]
  if (konsuppdelat) color_vekt <- diagramfarger("kon") 
  
  skapa_diagram <- function(beffor_val, region_val) {
    # skapa en textvariabel för befolkningsförändringsvariabeln som har stor bokstav i början
    beffor_val_txt <- paste0(str_sub(beffor_val, 1, 1) %>% toupper(), str_sub(beffor_val, 2, nchar(beffor_val)))
    region_namn <- hamtaregion_kod_namn(region_val)$region %>% skapa_kortnamn_lan() %>% list_komma_och()
    region_filnamn <- region_val %>% skapa_kortnamn_lan() %>% paste0(collapse = "_")
    
    facet_txt <- if(facet_region) "" else paste0(" i ", region_namn)
    facet_filnamn <- if(facet_region) "_facet_" else ""
    
    diagram_titel <- paste0(beffor_val_txt, facet_txt, " ", levels(beffor_df$månad_år)[1], " - ", levels(beffor_df$månad_år)[length(levels(beffor_df$månad_år))])
    diagramfil <- paste0(beffor_val, "_", region_filnamn, facet_filnamn, kon_txt, ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = beffor_df %>% 
                                   filter(förändringar == beffor_val,
                                          regionkod %in% region_val), 
                                 skickad_x_var = "månad_år", 
                                 skickad_y_var = "Befolkning",
                                 skickad_x_grupp = ifelse(konsuppdelat, "kön", NA),
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 diagram_facet = facet_region,
                                 #x_axis_storlek = x_axis_lbl_stlk,
                                 facet_x_axis_storlek = 3,
                                 x_var_fokus = ifelse(konsuppdelat, NA, "fokus"),
                                 x_axis_visa_var_xe_etikett = if (facet_region) 12 else nth_etikett,
                                 manual_y_axis_title = beffor_val,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_color = color_vekt,
                                 facet_grp = if (facet_region) "region" else NA,
                                 facet_legend_bottom = if (konsuppdelat) TRUE,
                                 logga_scaling = 20,
                                 #logga_path = logga_path,
                                 skriv_till_diagramfil = skriv_diagramfil,
                                 #skriv_till_excelfil = skriv_excelfil_val,
                                 output_mapp = utmapp,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[length(gg_list)] <- diagramfil %>% str_remove(".png")
  } # slut skapa_diagram
  
  
  
} # slut funktion


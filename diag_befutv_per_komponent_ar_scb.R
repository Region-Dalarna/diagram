diag_befutv_per_komponent_ar <- function(
    region_vekt = "20",                                      # läns- och kommunkoder, det blir ett diagram (och en fil om man skriver bildfiler) per region
    gruppera_namn = NA,                                     # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,                                        # här sparas 
    enbart_inrikes_flyttnetto = TRUE,                        # FALSE = inr flyttnetto delas upp på eget län och övriga Sverige, annars blir det bara en kategori för inr flyttnetto
    diagram_fargvekt = NA,
    x_axis_storlek = 7, # Ändra storleken på x-axeln
    facet_x_axis_storlek = 6, # Ändra storleken på x-axeln i facet. Standardvärde är 8
    x_axis_visa_var_xe_etikett = NA, # Möjlighet att visa var x:e etikett på x-axeln
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    returnera_dataframe_global_environment = FALSE,          
    ta_bort_diagramtitel = FALSE,                            # FALSE så skrivs ingen diagramtitel ut
    visa_dataetiketter = FALSE,
    skriv_till_diagramfil = TRUE
) {
  
  # source('https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R') saknades i koden, varför jag la till den. /Jon 2025-03-17
  # Har även lagt till möjligheten att visa var x:te etikett och dessutom möjligheten att ändra storlek på etiketterna. Blev för litet i ett diagram. /Jon 2025-03-17
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source('https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R')
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_forandringar_region_period_kon_scb.R")
  
  gg_list <- list()
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diagram_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diagram_fargvekt <- diagramfarger("rus_sex")
    } else {
      diagram_fargvekt <- hue_pal()(9)
    }
  }
  
  # om ingen output_mapp är angiven så läggs diagrammen i Region Dalarnas standardmapp för utskrifter, om den finns. Annars blir det felmeddelande
  if (skriv_till_diagramfil) {           # bara relevant om vi skriver till fil
    if (all(is.na(output_mapp))) {
      if (dir.exists(utskriftsmapp())) {
        output_mapp <- utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }
  
  # hämta data
  beffor_df <- funktion_upprepa_forsok_om_fel(function()
    hamta_bef_forandringar_region_alder_kon_scb(
      region_vekt = region_vekt,
      forandringar_klartext = c("flyttningsöverskott eget län", "flyttningsöverskott övriga Sverige",
                                "invandringsöverskott", "födelseöverskott")
    ))
  
  # Ändra första bokstaven till versal i befolkningsförändringar
  diagram_df <- beffor_df %>% 
    filter(!is.na(personer)) %>% 
    mutate(förändringar = str_c(str_to_upper(str_sub(förändringar, 1, 1)), str_sub(förändringar, 2)))
  
  # Om det bara finns flyttningsöverskott övriga Sverige så (om man tex valt län) så ändras kategorinamnet till Inrikes flyttnetto
  if (!"Flyttningsöverskott eget län" %in% unique(diagram_df$förändringar)) {
    diagram_df <- diagram_df %>% 
      mutate(förändringar = if_else(förändringar == "Flyttningsöverskott övriga Sverige", "Inrikes flyttnetto", förändringar))
  }
  
  # Om både flyttningsöverskott eget län och övriga Sverige finns men man enbart vill ha inrikes flyttnetto
  # totalt så läggs de ihop här
  if (("Flyttningsöverskott övriga Sverige" %in% unique(diagram_df$förändringar)) & enbart_inrikes_flyttnetto) {
    diagram_df <- diagram_df %>%
      bind_rows(
        diagram_df %>%
          filter(förändringar %in% c("Flyttningsöverskott eget län", "Flyttningsöverskott övriga Sverige")) %>%
          group_by(år, regionkod, region, period) %>%
          summarise(
            förändringar = "Inrikes flyttnetto",
            personer = sum(personer, na.rm = TRUE),
            .groups = "drop"
          )
      ) %>% 
      filter(!förändringar %in% c("Flyttningsöverskott övriga Sverige", "Flyttningsöverskott eget län"))
  }
  
  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("befutv_per_komponent_ar_scb_df", diagram_df, envir = .GlobalEnv)
  }
  
  if (length(unique(diagram_df$förändringar)) == 4) diagram_fargvekt <- diagram_fargvekt[c(1,2,2,3)]
  
  # Vi gör factor-variabel av förändringar för att få kategorier i rätt ordning
  diagram_df <- diagram_df %>% 
    mutate(förändringar = förändringar %>% str_replace("Flyttningsöverskott", "Flyttnetto"),
           förändringar = factor(förändringar, levels = c("Födelseöverskott", "Inrikes flyttnetto", "Flyttnetto eget län", "Flyttnetto övriga Sverige", "Invandringsöverskott")))
  
  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!all(is.na(gruppera_namn))) {
    diagram_df <- diagram_df %>% 
      group_by(across(-c(regionkod, region, personer))) %>% 
      summarise(personer = sum(personer, na.rm = TRUE), .groups = "drop") %>% 
      mutate(regionkod = "gg",
             region = gruppera_namn) %>% 
      relocate(region, .before = 1) %>% 
      relocate(regionkod, .before = region)
    
    region_vekt <- "gg"
  }
  
  skapa_diagram <- function(skickad_regionkod) {
    
    skriv_diagram_df <- diagram_df %>%
      filter(regionkod %in% skickad_regionkod)
    
    region_txt <- if (skickad_regionkod == "gg") gruppera_namn else hamtaregion_kod_namn(skickad_regionkod)$region %>% list_komma_och()
    region_filnamn <- if (skickad_regionkod == "gg") gruppera_namn %>% tolower %>% str_replace_all(" ", "_") else hamtaregion_kod_namn(skickad_regionkod)$region %>% paste0(collapse = "_")
    startar <- min(skriv_diagram_df$år)
    slutar <- max(skriv_diagram_df$år)
    
    diagramtitel <- glue("Befolkningsökning per komponent i {region_txt} år {startar}-{slutar}")
    diagramfil <- glue("befolkningsforandring_per_komponent_{region_filnamn}_ar{startar}-{slutar}.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = skriv_diagram_df,
                                 skickad_x_var = "år",
                                 skickad_y_var = "personer",
                                 skickad_x_grupp = "förändringar",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 utan_diagramtitel = ta_bort_diagramtitel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diagram_fargvekt,
                                 output_mapp = output_mapp,
                                 legend_vand_ordning = TRUE,
                                 lagg_pa_logga = ta_med_logga,
                                 logga_path = logga_sokvag,
                                 dataetiketter = visa_dataetiketter,
                                 skriv_till_diagramfil = skriv_till_diagramfil,
                                 diagram_facet = TRUE,
                                 facet_grp = "förändringar",
                                 facet_scale = "fixed",
                                 x_axis_storlek = x_axis_storlek,
                                 facet_x_axis_storlek = facet_x_axis_storlek,
                                 x_axis_visa_var_xe_etikett = x_axis_visa_var_xe_etikett,
                                 )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    
    return(gg_list) 
  } # slut funktion för att skriva diagram
    
  retur_list <- map(region_vekt, ~skapa_diagram(skickad_regionkod = .x)) %>% purrr::flatten()
  
  return(retur_list)
  
} # slut funktion

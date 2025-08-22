# ====================================================================================================
#
# Skapar diagram för sysselsättningsgrad uppdelat på inrikes födda samt utrikes födda uppdelat på 
# vistelsetid. Man skriver ut ett diagram per region och år. Varje år man skickar med blir ett 
# eget diagram. Vill man jämföra flera regioner i ett facetdiagram så kör man facet_diagram = TRUE,
# annars blir det ett diagram per region (och år).
#
# På SCB är riket, län och kommuner egna tabeller men skriptet lägger ihop dessa om man vill.
#
# Skapat av: Peter Möller, Region Dalarna
# November 2023
#
# ====================================================================================================

library(tidyverse)
library(pxweb)
library(glue)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_syss_visstid_inr_utr_fodda_scb.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)


diag_syss_vistelsetid_inr_utr_fodda_ett_per_ar_scb <- function(
    region_vekt = "20",  
    diagram_capt = "Källa: Registerdata för integration, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    cont_klartext = "Andel förvärvsarbetande (ny definition från och med 2019)",
    tid_vekt = "9999",                  # 9999 = senaste år, NA = alla år
    konsuppdelat = FALSE,
    facet_diagram = TRUE,
    skriv_ut_logga = TRUE,     # TRUE fö ratt lägga till logga, FALSE för att inte ha med det
    logga_path = NA,
    output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/"
    ) {

  
  # ================ Hämta tabell från SCB =============================================
  px_df <- hamta_syss_vistelsetid_inr_utr_fodda_scb(region_vekt = region_vekt,
                                           cont_klartext = cont_klartext,
                                           tid_vekt = tid_vekt,
                                           kon_klartext = if (konsuppdelat) c("män", "kvinnor") else "män och kvinnor",
                                           bakgr_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år",
                                                              "vistelsetid 10- år", "födelseregion: Sverige"),
                                           utbniva_klartext = "samtliga utbildningsnivåer")
  
  
  
    plot_df <- px_df %>% 
      mutate(bakgrundsvariabel = ifelse(bakgrundsvariabel == "födelseregion: Sverige", "inrikes födda", bakgrundsvariabel),
             bakgrundsvariabel = bakgrundsvariabel %>% str_remove("vistelsetid "),
             bakgrundsvariabel = factor(bakgrundsvariabel, levels = c("0-1 år", "2-3 år", "4-9 år", "10- år", "inrikes födda")))
    
    
    skapa_diagram <- function(vald_region, valt_ar) {
      
      diagram_df <- plot_df %>% 
        filter(regionkod %in% vald_region,
               år %in% valt_ar)
      
      antal_kon <- length(unique(diagram_df$kön))
      region_txt <- if (length(vald_region) > 1) " " else glue(" i {hamtaregion_kod_namn(vald_region)$region %>% skapa_kortnamn_lan()} ")
      tabell_ar <- unique(diagram_df$år)
      ar_txt <- ifelse(length(tabell_ar) == 1, tabell_ar, paste0(min(tabell_ar), "-", max(tabell_ar)))
      
      kon_txt <- if (antal_kon > 1) "_kon" else ""
      
      diagram_fil <- glue("syss_vistelsetid{kon_txt}_{region_vekt %>% paste0(collapse = '_')}_ar_{ar_txt}.png")
      diagramtitel <- glue("Andel förvärvsarbetande{region_txt}år {unique(diagram_df$år)}")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = diagram_df,
                         skickad_x_var = "bakgrundsvariabel",
                         skickad_y_var = "Andel sysselsatta",
                         skickad_x_grupp = if (antal_kon > 1) "kön" else NA,
                         diagram_titel = diagramtitel,
                         diagram_capt = diagram_capt,
                         output_mapp = output_mapp,
                         filnamn_diagram = diagram_fil,
                         manual_color = if (antal_kon > 1) diagramfarger("kon") else diagramfarger("rus_sex"),
                         x_axis_lutning = 0,
                         procent_0_100_10intervaller = TRUE,
                         #manual_x_axis_text_hjust = 1,
                         #manual_x_axis_text_vjust = 1,
                         manual_y_axis_title = "procent",
                         manual_x_axis_title = "vistelsetid i Sverige",
                         diagram_facet = if (length(vald_region) > 1) TRUE else FALSE,
                         facet_grp = if (length(vald_region) > 1) "region" else NA,
                         facet_scale = "fixed",
                         facet_legend_bottom = if (antal_kon > 1) TRUE else FALSE,
                         y_axis_100proc = TRUE,
                         lagg_pa_logga = skriv_ut_logga,
                         logga_path = logga_path,
                         dataetiketter = FALSE)
      
      gg_list <- list(gg_obj)
      names(gg_list)[[length(gg_list)]] <- diagram_fil %>% str_remove("\\.[^.]+$")

      return(gg_list)
      
    } # slut funktion för att skapa diagram
    
    dia_lista <- list()
    if (facet_diagram) {
      dia_lista <- skapa_diagram(vald_reg = region_vekt, valt_ar = unique(plot_df$år))
      #names(dia_lista) <- paste0("diff_syss_", vald_region_txt, facet_val_txt, etikett_txt)
    } else {
      arglist <- list(reg = region_vekt, valt_ar = unique(plot_df$år))                               # skapa lista med de två variabler vi vill göra diagram med
      crossarg <- expand.grid(arglist)
      # dia_lista <- map2(crossarg$reg, crossarg$bakgr, crossarg$valt_kon, ~skapa_diagram(vald_reg = .x, vald_bakgrund = .y, valt_kon = .z)) %>% flatten()
      dia_lista <- pmap(crossarg, ~skapa_diagram(vald_region = ..1, valt_ar = ..2)) %>%
        unlist(recursive = FALSE)
    }
    
    
} # slut funktion

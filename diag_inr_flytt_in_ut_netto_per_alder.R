

diag_inr_flytt_in_ut_netto_per_alder <- function(region_vekt = "20",
                                     gruppera_namn = NA,
                                     ta_med_logga = TRUE,
                                     logga_path = NA,
                                     farg_vektor = NA,
                                     output_mapp = NA,
                                     kon_klartext = NA,
                                     visa_flyttnetto_linje = TRUE,
                                     skriv_diagramfil = TRUE,
                                     demo = FALSE,                    # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                     alder_koder = "*",
                                     tid_koder = "9999"){

  # =======================================================================================================================
  #
  # Hämta hem data med funktionen hamta_bef_flyttningar_region_alder_kon_scb och skriv ut ett diagram. Defaultinställning är
  # att visa en flyttnettolinje. Diagrammet liknar en Det finns fler innehålls-
  # variabler, vilka primärt används för att beräkna dödsrisker och återstående medellivslängd.
  #
  # Länk till SCB-tabell där data hämtas: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0701/LivslUtbLan/
  #
  # =======================================================================================================================
  
  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <- 
      "https://region-dalarna.github.io/utskrivna_diagram/in_utflyttning_Dalarnas%20l%C3%A4n_ar_2023.png"
    browseURL(demo_url)
    stop_tyst()
  } 
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")
  
  if(all(is.na(farg_vektor)) & exists("diagramfarger")) farg_vektor <- diagramfarger("rus_sex")
  
  flytt_df <- hamta_bef_flyttningar_region_alder_kon_scb(region_vekt = region_vekt,
                                                         kon_klartext = kon_klartext,
                                                         alder_koder = alder_koder,
                                                         tid_koder = tid_koder,
                                                         cont_klartext = c("Inrikes inflyttningar", "Inrikes utflyttningar"))
  
  # kolla om kön är med som variabel
  kon_txt <- if (!is.na(kon_klartext)) "_kon" else ""
  
  # välj ut variabler till gruppering av netto-datasetet, dvs. med eller utan kön
  if (!is.na(kon_klartext)) {
    netto_grp_var <- c("år", "regionkod", "region", "alder_num", "kön", "ålder")
  } else {
    netto_grp_var <- c("år", "regionkod", "region", "alder_num", "ålder")
  }
  
  # grupperar datasetet om man angett värde för gruppera_namn
  if (!is.na(gruppera_namn)){
    flytt_df <- flytt_df %>% 
      group_by(across(where(is.character))) %>% 
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
      ungroup() %>% 
      mutate(region = gruppera_namn,
             regionkod = "gg")
  } 

  chart_df <- flytt_df %>% 
    filter(ålder != "totalt ålder") %>% 
    mutate(varde = ifelse(variabel == "Inrikes utflyttningar", varde*-1, varde),
           alder_num = ålder %>% parse_number()) %>% 
    arrange(alder_num) %>% 
    mutate(ålder = factor(ålder, levels = unique(ålder)))
           
  netto_df <- chart_df %>% 
    group_by(across(any_of(netto_grp_var))) %>% 
    summarise(varde = sum(varde[variabel == "Inrikes inflyttningar"]) + sum(varde[variabel == "Inrikes utflyttningar"])) %>% 
    ungroup() %>% 
    mutate(variabel = "Netto")
  
  total_df <- bind_rows(chart_df, netto_df) %>% 
    pivot_wider(names_from = variabel, values_from = varde) %>% 
    mutate(`Inrikes inflyttningar` = ifelse(Netto > 0, `Inrikes inflyttningar` - Netto, `Inrikes inflyttningar`),
           `Inrikes utflyttningar` = ifelse(Netto < 0, `Inrikes utflyttningar` - Netto, `Inrikes utflyttningar`)) %>% 
    pivot_longer(cols = c(`Inrikes inflyttningar`, `Inrikes utflyttningar`, Netto), names_to = "variabel", values_to = "varde")
  
  diagram_capt <- "Källa: Befolkningsstatistik, SCB:s öppna statistikdatabas\nBearbetning av Samhällsanalys, Region Dalarna"
  diagram_titel <- glue("In- och utflyttning i {unique(chart_df$region)} år {unique(chart_df$år)}")
  diagram_filnamn <- glue("in_utflyttning_{unique(chart_df$region)}_ar_{unique(chart_df$år)}{kon_txt}.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = chart_df,
                               skickad_x_var = "ålder",
                               skickad_y_var = "varde",
                               skickad_x_grupp = "variabel",
                               x_axis_lutning = 0,
                               y_axis_storlek = 6,
                               y_axis_minus_plus_samma_axel = TRUE,
                               x_axis_visa_var_xe_etikett = 2,
                               diagram_liggande = TRUE,
                               geom_position_stack = TRUE,
                               manual_color = diagramfarger("rus_sex"),
                               manual_y_axis_title = "Antal in- och utflyttade",
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               diagram_facet = if (!is.na(kon_klartext)) TRUE else FALSE,
                               facet_grp = if (!is.na(kon_klartext)) "kön" else NA,
                               facet_legend_bottom = TRUE,
                               skriv_till_diagramfil = FALSE,
                               filnamn_diagram = diagram_filnamn,
                               output_mapp = utskriftsmapp()
  )
  
  
  # lägger till en flyttnetto-linje om det är valt
  if (visa_flyttnetto_linje) {
    netto_dubbel <- bind_rows(netto_df, netto_df) %>% 
      arrange(år, regionkod, region, ålder)
    
    gg_obj <- gg_obj + 
      geom_line(aes(y = netto_dubbel$varde, group = 1), colour = "black") +
      geom_line(aes(color="line"))+
      scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto")+
      theme(legend.key = element_rect(fill = "white"),
            legend.box.just = "bottom") +
      guides(fill = guide_legend(order = 1),
        color = guide_legend(order = 2))
  }
  
  # skriver ut diagramfilen om det är valt
  if (skriv_diagramfil) skriv_till_diagramfil(gg_obj, output_mapp = utskriftsmapp(), filnamn_diagram = diagram_filnamn)
 
}  # slut funktion

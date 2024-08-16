
diag_bas_syss_arbloshet_aldersgrupp_kon_scb <- function(
    vald_region = "20",
    vald_alder = c("16-19 år", "20-24 år", "25-29 år", "30-34 år", "35-39 år", "40-44 år", 
                   "45-49 år", "50-54 år", "55-59 år", "60-64 år", "65-69 år", "70-74 år"),
    vald_cont = c("arbetslöshet", "sysselsättningsgrad"),
    skriv_diagramfil = TRUE,
    diagram_capt = "Källa: Befolkningens arbetsmarknadsstatus (BAS), SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    logga_i_diagram = NA,
    diag_fargvekt = NA,
    demo = FALSE,           # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    utmapp
    ) {
  
  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <- 
      c("https://region-dalarna.github.io/utskrivna_diagram/Inrikes_flyttnetto_alder_Dalarna.png",
        "https://region-dalarna.github.io/utskrivna_diagram/Inrikes_flyttnetto_alder_20-29_%C3%A5r_Dalarna.png")
    walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    stop_tyst()
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("kon")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  # hämta data från SCB via API
  syss_df <- hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(
    region_vekt = vald_region,
    alder_klartext = vald_alder,
    kon_klartext = c("Män", "Kvinnor"),
    cont_klartext = vald_cont)
  
  # ta bort rader med NA och fixa till månadsvariabeln
  chart_df <- syss_df %>% 
    filter(!is.na(varde)) %>% 
    manader_bearbeta_scbtabeller()
  
  # skapa en funktion för att göra själva diagrammet, som sedan används för varje unik kombination
  skriv_diagram <- function(reg, cont_var) {
    
    vald_region_txt <- hamtaregion_kod_namn(vald_region)$region %>% skapa_kortnamn_lan(T)
    
    diagramtitel <- glue("{str_to_sentence(cont_var)} per åldersgrupp och kön i {vald_region_txt} i {last(chart_df$månad_år) %>% unique()}")
    diagramfil <- glue("{cont_var}_alder_kon_{vald_region_txt}_{last(chart_df$tid)}.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_df %>% 
                                   filter(regionkod == reg,
                                          variabel == cont_var,
                                          födelseregion != "totalt",
                                          tid == max(tid)),
                                 skickad_x_var = "ålder",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "kön",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 procent_0_100_10intervaller = ifelse(str_detect(cont_var, "antal"), FALSE, TRUE),
                                 filnamn_diagram = diagramfil,
                                 dataetiketter = FALSE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_color = diag_fargvekt,
                                 manual_y_axis_title = ifelse(str_detect(cont_var, "antal"), "", "procent"),
                                 output_mapp = utmapp,
                                 skriv_till_diagramfil = skriv_diagramfil,
                                 logga_path = logga_i_diagram,
                                 diagram_facet = TRUE,
                                 facet_grp = "födelseregion",
                                 facet_scale = "fixed"
    )
  } # slut skriv_diagram-funktion
  
  # gör ett diagram för varje unik kombination av region och innehållsvariabel
  arglist <- list(reg = vald_region, cont_var = unique(chart_df$variabel))                               # skapa lista med de två variabler vi vill göra diagram med
  crossarg <- expand.grid(arglist)
  dia_lista <- map2(crossarg$reg, crossarg$cont_var, ~skriv_diagram(reg = .x, cont_var = .y)) %>% flatten()
  #dia_lista <- pmap(crossarg, ~skriv_diagram(vald_reg = ..1, vald_bakgrund = ..2, valt_kon = ..3)) %>% flatten()
  
  return(dia_lista)
  
} # slut diag-funktion

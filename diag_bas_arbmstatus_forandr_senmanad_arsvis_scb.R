

diag_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad <- function(vald_region = "20",
                                                                         gruppera_regioner_namn = NA,
                                                                         alder_txt = "16-64 år",
                                                                         kon_txt = "totalt",
                                                                         cont_txt = c("antal sysselsatta",
                                                                                      "antal arbetslösa",
                                                                                      "antal studerande",
                                                                                      "antal pensionärer",
                                                                                      "antal sjuka",
                                                                                      #"antal totalt",
                                                                                      "antal övriga"
                                                                         ),
                                                                         utmapp = NA,
                                                                         diag_capt = "Källa: Befolkningens arbetsmarknadsstatus (BAS), SCB\nBearbetning: Samhällsanalys, Region Dalarna",
                                                                         diag_fargvekt = NA,
                                                                         skriv_diagramfil = TRUE,
                                                                         demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                                                         ) {

  
  
# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/forandr_arbmstatus_Dalarna_16-64 år.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue,
         scales)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
  
  if (all(is.na(utmapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      utmapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  fodelseregion_txt = c("inrikes född", "utrikes född")
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diag_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diag_fargvekt <- diagramfarger("rd_primar_nio")
    } else {
      diag_fargvekt <- hue_pal()(9)
    }
  }
  
  #alder_txt <- unique(chart_df$region)

  syss_df <- hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(region_vekt = vald_region, # hamtaAllaLan(),
                                                                               cont_klartext = cont_txt,
                                                                               alder_klartext = alder_txt,
                                                                               kon_klartext = kon_txt,
                                                                               fodelseregion_klartext = fodelseregion_txt,
                                                                               long_format = TRUE,
                                                                               wide_om_en_contvar = FALSE)
  
  # om man vill gruppera ihop de regioner som man skickat med
  if (!is.na(gruppera_regioner_namn)) {
    syss_df <- syss_df %>% 
      group_by(kön, ålder, födelseregion, månad, variabel) %>% 
      summarise(varde = sum(varde, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(regionkod = "grp",
             region = gruppera_regioner_namn) %>% 
      relocate(regionkod, .before = 1) %>% 
      relocate(region, .after = regionkod)
    
  } # slut if-sats om man vill gruppera
  
  
  bakgr_df <- syss_df %>% 
    filter(kön == "totalt" & födelseregion != "totalt") %>% 
    manader_bearbeta_scbtabeller()
  
  forsta_ar <- first(bakgr_df$år)
  senaste_ar <- last(bakgr_df$år)
  senaste_manad <- bakgr_df %>% filter(år == senaste_ar) %>% dplyr::pull(månad) %>% last()
  
  chart_df <- bakgr_df %>% 
    filter(månad == senaste_manad | år == forsta_ar & månad == "januari") %>%
    group_by(regionkod, region, kön, ålder, födelseregion, variabel) %>% 
    mutate(forandr = round(((`varde` - lag(varde)) / lag(varde)) * 100, 1),
           region = region %>% skapa_kortnamn_lan(T),                          # T = byt ut Riket mot Sverige om man skickar med Riket
           månad_kort = str_sub(månad, 1, 3),
           månad_år = paste0(månad_kort, " ", år),
           lbl_x = paste0(lag(månad_år), " - ", månad_år))
  
  bagr_kat <- unique(chart_df$födelseregion)
  reg_txt <- unique(chart_df$region)
  
  skriv_diagram <- function(data_df, vald_reg, vald_alder, valt_kon) {
    
    kon_lbl <- if(valt_kon == "totalt") "" else paste0("_", valt_kon)
    kon_rubrik <- if(valt_kon == "totalt") "invånare" else valt_kon
    
    cont_lbl <- if (length(cont_txt) < 6) cont_txt %>% paste0(collapse = "_") %>% paste0("_", .) else ""
    
    #reg <- unique(data_df$region)
    diag_titel <- glue("Förändring i arbetsmarknadsstatus för {kon_rubrik} {vald_alder} i {vald_reg}")
    diag_filnamn <- glue("forandr_arbmstatus_{vald_reg}_{vald_alder}{kon_lbl}{cont_lbl}.png")
  
    # skapa df för detta specifika diagram
    diag_df = data_df %>% 
      filter(!is.na(forandr),
             region == vald_reg,
             ålder == vald_alder,
             kön == valt_kon)
    
    diff <- max(diag_df$forandr, na.rm = TRUE) - min(diag_df$forandr, na.rm = TRUE)
    linje_varde <- diff / 400
    
    gg_obj <- SkapaStapelDiagram(skickad_df = diag_df,
                                 skickad_x_var = "lbl_x",
                                 skickad_y_var = "forandr",
                                 skickad_x_grupp = "variabel",
                                 geom_position_stack = TRUE,
                                 diagram_titel = diag_titel,
                                 #stodlinjer_avrunda_fem = TRUE,
                                 diagram_facet = TRUE,
                                 facet_grp = "födelseregion",
                                 facet_scale = "fixed",
                                 facet_legend_bottom = TRUE,
                                 fokusera_varden = list(geom = "rect", ymin=-linje_varde, ymax=linje_varde, xmin=0, xmax=Inf, alpha=1, fill="black"),
                                 manual_y_axis_title = "procent",
                                 manual_color = rev(diagramfarger("rd_primar_nio")),
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 output_mapp = utmapp,
                                 filnamn_diagram = diag_filnamn,
                                 skriv_till_diagramfil = skriv_diagramfil
                                 )
  } # slut funktion skriv_diagram
  
  
  arglist <- list(reg = reg_txt, alder = alder_txt, kon = kon_txt)                               # skapa lista med de två variabler vi vill göra diagram med
  crossarg <- expand.grid(arglist)
  #dia_lista <- map2(crossarg$reg, crossarg$bakgr, ~skriv_diagram(data_df = chart_df, vald_reg = .x, bakgr = .y)) %>% flatten()
  dia_lista <- pmap(crossarg, ~skriv_diagram(data_df = chart_df, vald_reg = ..1, vald_alder = ..2, valt_kon = ..3)) %>% flatten()
  
  } # slut funktion diag

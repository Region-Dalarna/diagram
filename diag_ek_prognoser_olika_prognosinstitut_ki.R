



diag_ekonomiska_prognoser_olika_progn_institut_ki <- function(vald_variabel = "BNP",                  # finns: "BNP", "Hushållens konsumtion", "Offentlig konsumtion", "Fasta bruttoinvesteringar", "Lagerinvesteringar., förändr. i proc. av BNP föreg. år", "Export", "Import", "Antal sysselsatta, 15-74 år (AKU)", "Arbetslöshet, procent av arbetskraften, 15-74 år (AKU)", "Timlön, totalt (konjunkturlönestatistiken)", "Konsumentprisindex (KPI), årsgenomsnitt", "KPI med fast bostadsränta (KPIF), årsgenomsnitt", "Real disponibel inkomst (nationalräkenskaperna)", "Styrränta, vid årets slut, procent**", "Offentligt finansiellt sparande, procent av BNP", "Bytesbalans, procent av BNP (nationalräkenskaperna)", "Timlön, näringslivet (konjunkturlönestatistiken)"
                                                              valda_prognos_ar = NA,                  # NA eller "*" = alla år
                                                              endast_mest_aktuell_prognos = TRUE      # TRUE om man bara vill ha den mest aktuella prognosen varje år, annars kommer alla prognoser som institut har gjort för prognosåret med i datasetet
                                                              ) { 
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_ek_prognoser_fran_prognosinstitut_ki.R")
  
  
  if (all(is.na(valda_prognos_ar))) valda_prognos_ar <- "*"
  prognoser_df <- hamta_ek_prognoser_fran_prognosinstitut_ki(prognos_ar = valda_prognos_ar %>% as.character(),
                                                             bara_senaste_prognos = endast_mest_aktuell_prognos)
  
  # Skapa diagram över prognoser
  
  prognoser_variabel <- prognoser_df %>% filter(variabel == vald_variabel)
  #if (any(valda_prognos_ar != "*"))  prognoser_variabel <- prognoser_variabel %>% filter(prognos_ar %in% valda_prognos_ar)
  
  prognoser_variabel_ar <- prognoser_variabel$prognos_ar %>% unique()
  
  diagram_capt <- "Källa: Konjunkturinstitutet, bearbetning av Samhällsanalys, Region Dalarna"
  diagram_titel <- paste0(vald_variabel, " - prognoser över utveckling")
  diagramfil <- paste0(vald_variabel %>% str_remove_all(","), "_prognos_ar_", prognoser_variabel_ar %>% paste0(collapse = "_"), ".png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = prognoser_variabel, 
                               skickad_x_var = "Prognosinstitut_namn", 
                               skickad_y_var = "varde",
                               diagram_titel = diagram_titel,
                               diagram_capt = diagram_capt,
                               #x_axis_storlek = 8,
                               #x_var_fokus = "prognos_ar",
                               stodlinjer_avrunda_fem = TRUE,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = "Prognosticerad tillväxt (%)",
                               diagram_facet = TRUE,
                               facet_grp = "prognos_ar",
                               facet_scale = "free_x",
                               manual_color = diagramfarger("rus_sex")[1],
                               output_mapp = utskriftsmapp(),
                               filnamn_diagram = diagramfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
  return(gg_list)

}

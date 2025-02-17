# Självskattad hälsa - digram
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_sjalvskattad_halsa_kon_lan_kommun_fohm.R")
gg_sjalvskattad_halsa <- diag_sjalvskattad_halsa_kon_lan_kommun(region_vekt = "20",
                                                                #region_vekt = "20",
                                                                tid_koder = "*",
                                                                output_mapp = output_mapp_figur,
                                                                returnera_dataframe_global_environment = TRUE)

diag_sjalvskattad_halsa_kon_lan_kommun <- function(
    region_vekt = "20",
    andel_konfinterv_klartext = "Andel",
    kon_klartext = c("Kvinnor", "Män"), # Finns också "Totalt"
    tid_koder = "9999",         # "9999" = senaste år
    region_sort = FALSE,        # TRUE så sorteras regionerna enligt ordningen i region_vekt
    diagram_fargvekt = NA,      # skicka med en färgvektor om man önskar andra färger än standard
    output_mapp = NA,           # hit skrivs png-filen
    returnera_dataframe_global_environment = FALSE,
    diagram_capt = "Källa: Folkhälsomyndighetens öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: 4-årsmedelvärden",
    visa_dataetiketter = FALSE, 
    ta_bort_diagramtitel = FALSE,                    # FALSE så skrivs ingen diagramtitel ut
    kortnamn_lan = TRUE,                    # TRUE så tas " län" bort ur länsnamnet
    diagram_0_till_100_procent = TRUE,       # TRUE så går skalan alltid från 0 till 100 procent, annars anpassas skalan efter data
    logga_path = NA                         # NULL för att köra utan logga             
) {
  
  # ===============================================================================================
  #
  # Diagram för att skriva ut självskattad hälsa från Hälsa på lika villkor-enkäten hos 
  # Folkhälsomyndigheten. Finns enbart "Bra eller mycket bra hälsa". För fler alternativ, se skript med snarlikt namn.
  # Det skriptet finns dock inte för kommuner  
  # Skickas bara ett år med så skrivs bara ett diagram för det året, och finns fler regioner så läggs de
  # på x-axeln.
  # ===============================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_sjalvskattad_halsa_lan_kommun_halsotillstand_andel_och_konfidensintervall_kon_ar_halsgodyreg_fohm.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
  
  # om ingen output_mapp är vald. Kolla om standardmappen existerar, om inte ges ett felmeddelande
  if (all(is.na(output_mapp))) {
    if (dir.exists(utskriftsmapp())) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diagram_fargvekt))) {
    if (exists("diagramfarger", mode = "function")) {
      diagram_fargvekt <- diagramfarger("kon")
    } else {
      diagram_fargvekt <- hue_pal()(9)
    }
  }
  
  gg_list <- list()
  
  sjalvskattad_halsa_df <- hamta_sjalvskattad_halsa_lan_kommun_halsotillstand_kon_ar_fohm (
    region_vekt = region_vekt,			   # Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
    halsotillstand_klartext = "*",			 #  Finns: "Bra eller mycket bra hälsa"
    andel_och_konfidensintervall_klartext = andel_konfinterv_klartext,			 #  Finns: "Andel", "Konfidensintervall nedre gräns", "Konfidensintervall övre gräns", "Antal svar"
    kon_klartext =kon_klartext,			 #  NA = tas inte med i uttaget,  Finns: "Totalt", "Kvinnor", "Män"
    tid_koder = tid_koder,			 # "*" = alla år eller månader, "9999" = senaste, finns: "2004-2007", "2005-2008", "2006-2009", "2007-2010", "2008-2011", "2009-2012", "2010-2013", "2011-2014", "2012-2015", "2013-2016", "2015-2018", "2017-2020", "2018-2021", "2019-2022", "2021-2024"
    output_mapp = output_mapp,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "test_fohm.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  )
  
  # Det är enbart möjligt att välja flera år för en region
  if (length(unique(sjalvskattad_halsa_df$region)) > 1 & length(unique(sjalvskattad_halsa_df$År)) > 1) {
    stop("Det är inte möjligt att välja flera år för flera regioner. Välj en region eller ett år.")
  }
  
  if (kortnamn_lan) sjalvskattad_halsa_df <- sjalvskattad_halsa_df %>% mutate(region = region %>% skapa_kortnamn_lan())
  
  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- unique(sjalvskattad_halsa_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
  region_txt <- ar_alla_kommuner_i_ett_lan(unique(sjalvskattad_halsa_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- ar_alla_lan_i_sverige(unique(sjalvskattad_halsa_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt %>% str_replace_all(", ", "_") %>% str_replace_all(" och ", "_") 
  regionkod_txt <- if (region_start == region_txt) unique(sjalvskattad_halsa_df$regionkod) %>% paste0(collapse = "_") else region_txt
  if (region_start == region_txt) regionfil_txt <- region_txt
  
  # om region_sort är TRUE sorteras regionerna enligt ordningen i region_vekt
  if (region_sort) {
    sjalvskattad_halsa_df <- sjalvskattad_halsa_df %>%
      mutate(region = factor(region, levels = unique(region[order(match(regionkod, region_vekt))])))
  } 
  
  if(returnera_dataframe_global_environment == TRUE){
    assign("sjalvskattad_halsa_df", sjalvskattad_halsa_df, envir = .GlobalEnv)
  }
  
  if (min(sjalvskattad_halsa_df$År) == max(sjalvskattad_halsa_df$År)){
    tid_txt <- max(sjalvskattad_halsa_df$År) 
    diagramtitel <- glue("Andel med bra eller mycket bra självskattad hälsa år {tid_txt}")
  } else {
    tid_txt <- glue("{min(sjalvskattad_halsa_df$År)} till {max(sjalvskattad_halsa_df$År)}") 
    diagramtitel <- glue("Andel med bra eller mycket bra självskattad hälsa i {region_txt}")
  }
  
  # tid_txt <- if (min(sjalvskattad_halsa_df$År) == max(sjalvskattad_halsa_df$År)){
  #   max(sjalvskattad_halsa_df$År) 
  # } else {
  #   glue("{min(sjalvskattad_halsa_df$År)} till {max(sjalvskattad_halsa_df$År)}") 
  # }
  
  #diagramtitel <- glue("Andel med bra eller mycket bra självskattad hälsa i {region_txt} år {tid_txt}")
  diagramfil <- glue("sjalvskattad_halsa_fohm_{regionfil_txt}_ar{tid_txt}.png") %>% str_replace_all("__", "_")
  
  flera_ar <- if(length(unique(sjalvskattad_halsa_df$År)) > 1) TRUE else FALSE
  konsuppdelat <- if(length(unique(sjalvskattad_halsa_df$Kön)) > 1) TRUE else FALSE
  flera_regioner <- if(length(unique(sjalvskattad_halsa_df$region)) > 1) TRUE else FALSE
  
  gg_obj <- SkapaStapelDiagram(skickad_df = sjalvskattad_halsa_df,
                               skickad_x_var = if (flera_ar) "År" else "region",
                               skickad_y_var = "Hälsa efter region, kön och år. Andel",
                               skickad_x_grupp = if (konsuppdelat) "Kön" else NA,
                               diagram_titel = diagramtitel,
                               utan_diagramtitel = ta_bort_diagramtitel,
                               diagram_capt = diagram_capt,
                               stodlinjer_avrunda_fem = TRUE,
                               filnamn_diagram = diagramfil,
                               dataetiketter = visa_dataetiketter,
                               manual_y_axis_title = "procent",
                               procent_0_100_10intervaller = diagram_0_till_100_procent,
                               #x_axis_lutning = if (flera_ar) 45 else 0,
                               x_axis_lutning = 45,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               x_axis_sort_value = if (flera_ar) FALSE else TRUE,
                               # manual_x_axis_text_vjust = if (flera_ar) 1 else 0,
                               # manual_x_axis_text_hjust = if (flera_ar) 1 else 0.5,
                               manual_color = diagram_fargvekt,
                               lagg_pa_logga = if (is.null(logga_path)) FALSE else TRUE,
                               logga_path = logga_path,
                               output_mapp = output_mapp,
                               #diagram_facet = if (flera_ar & flera_regioner) TRUE else FALSE,
                               facet_grp = if (flera_ar & flera_regioner) "region" else NA,
                               facet_scale = "free",
                               facet_legend_bottom = if (konsuppdelat) TRUE else FALSE
  )
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  
  return(gg_list) 
}

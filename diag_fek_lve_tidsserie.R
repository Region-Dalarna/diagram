
diag_fek_lve_tidsserie <- function(
    region_vekt = "20",
    diagram_capt = "Källa: Företagens ekonomi (FEK), SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förädlingsvärde är den faktiska produktionen minus kostnader för köpta varor och tjänster, dock ej löner, sociala avgifter och kostnader för handelsvaror",
    output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
    visa_dataetiketter = FALSE,
    cont_klartext = "Förädlingsvärde, mnkr",
    diag_tidsserie = TRUE,
    diag_jmfr_riket = TRUE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {
  
# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_ar2007_2022.png",
"https://region-dalarna.github.io/utskrivna_diagram/fek_Förädlingsvärde_Dalarna_jmfr_riket_ar2007-2022.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("g:/skript/peter/temp/hamta_fek_lve_region_sni2007_tid_NSEBasfaktaLVEngs07_RegionalBasf07_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8")


  gg_list <- list()
  
  fek_lve_df <-
    suppress_specific_warning(
    hamta_fek_lve_region_sni2007_tid_scb(
  			region_vekt = c("00", region_vekt),			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "SE0", "SE00", "SE1", "SE11", "SE110", "SE12", "SE121", "SE122", "SE123", "SE124", "SE125", "SE2", "SE21", "SE211", "SE212", "SE213", "SE214", "SE22", "SE221", "SE224", "SE23", "SE231", "SE232", "SE3", "SE31", "SE311", "SE312", "SE313", "SE32", "SE321", "SE322", "SE33", "SE331", "SE332"
  			sni2007_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "A-SexklK-O samtliga näringsgrenar (exkl. K+O+T+U)", "A-01-03 jordbruk, skogsbruk och fiske", "B-05-09 utvinning av mineral", "C-10-33 tillverkning", "D-35 försörjning av el, gas, värme och kyla", "E-36-39 vattenförsörjning; avloppsrening, avfallshantering och sanering", "F-41-43 byggverksamhet", "G-45-47 handel; reparation av motorfordon och motorcyklar", "H-49-53 transport och magasinering", "I-55-56 hotell- och restaurangverksamhet", "J-58-63 informations- och kommunikationsverksamhet", "L-68 fastighetsverksamhet", "M-69-75 verksamhet inom juridik, ekonomi, vetenskap och teknik", "N-77-82 uthyrning, fastighetsservice, resetjänster och andra stödtjänster", "P-85 utbildning", "Q-86-88 vård och omsorg; sociala tjänster", "R-90-93 kultur, nöje och fritid", "S-94-96 annan serviceverksamhet"
  			cont_klartext = cont_klartext,			 #  Finns: "Antal arbetsställen (lokala verksamheter)", "Antal anställda", "Nettoomsättning exkl. merchantingkostnader, mnkr", "Produktionsvärde, mnkr", "Förädlingsvärde, mnkr", "Totala intäkter, mnkr", "Totala kostnader, mnkr"
  			tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2022"
  			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  			wide_om_en_contvar = FALSE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  			output_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			excel_filnamn = "fek_lve.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  ))
  
  branschnyckel <- read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") %>% 
    select(Kod, Grupp_kod, Branschgrupp) %>% 
    distinct()
  
  bransch_bokstav <- read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") %>% 
    select(Avdelning, Grupp_kod, Branschgrupp) %>% 
    distinct()
  
  vald_ar <- c(min(fek_lve_df$år), max(fek_lve_df$år))
  
  if (diag_tidsserie) {
    tidsserie_df <- fek_lve_df %>% 
      filter(sni2007kod != "Total_A-SexklK-O", 
             regionkod %in% vald_region,
             år %in% vald_ar) %>% 
      mutate(bransch_bokstav = str_sub(`näringsgren SNI 2007`, 1, 1)) %>% 
      left_join(branschnyckel, by = c("sni2007kod" = "Kod")) %>%
      left_join(bransch_bokstav %>% select(Avdelning, gk = Grupp_kod, bg = Branschgrupp), by = c("bransch_bokstav" = "Avdelning")) %>%
      mutate(Grupp_kod = ifelse(is.na(Grupp_kod), gk, Grupp_kod),
             Branschgrupp = ifelse(is.na(Branschgrupp), bg, Branschgrupp)) %>%
      group_by(år, regionkod, region, Branschgrupp, variabel) %>% 
      summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")
    
    region_txt <- unique(tidsserie_df$region) %>% skapa_kortnamn_lan() %>% list_komma_och()
    cont_txt <- cont_klartext %>% str_extract("^[^,]*")
    
    diagramtitel <- glue("{cont_txt} i {region_txt}")
    diagramfil <- glue("fek_{cont_txt}_{region_txt}_ar{min(fek_lve_df$år)}_{max(fek_lve_df$år)}.png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = tidsserie_df,
    			 skickad_x_var = "Branschgrupp",
    			 skickad_y_var = "varde",
    			 skickad_x_grupp = "år",
    			 x_axis_sort_value = TRUE,
    			 diagram_titel = diagramtitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_y_axis_title = cont_klartext,
    			 manual_x_axis_text_vjust = 1,
    			 manual_x_axis_text_hjust = 1,
    			 manual_color = diagramfarger("rus_sex"),
    			 output_mapp = output_mapp
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  } # slut if-sats om diag_tidsserie
  
  if (diag_jmfr_riket) {
    jmfr_riket_df <- fek_lve_df %>% 
      filter(sni2007kod != "Total_A-SexklK-O") %>% 
      mutate(region = region %>% skapa_kortnamn_lan()) %>% 
      group_by(år, regionkod, region, variabel) %>% 
      summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")
    
    region_txt <- unique(jmfr_riket_df %>% filter(regionkod != "00") %>% dplyr::pull(region)) %>% skapa_kortnamn_lan() %>% list_komma_och()
    cont_txt <- cont_klartext %>% str_extract("^[^,]*")
    
    diagramtitel <- glue("Förändring av {tolower(cont_txt)} i {region_txt} jämfört med riket")
    diagramfil <- glue("fek_{cont_txt}_{region_txt}_jmfr_riket_ar{min(fek_lve_df$år)}-{max(fek_lve_df$år)}.png")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = jmfr_riket_df %>% 
                                  rename(!!sym(cont_klartext) := varde),
                                 skickad_x_var = "år",
                                 skickad_y_var = cont_klartext,
                                 skickad_x_grupp = "region",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 filnamn_diagram = diagramfil,
                                 berakna_index = TRUE,
                                 #manual_y_axis_title = cont_klartext,
                                 manual_color = diagramfarger("rus_sex"),
                                 output_mapp = output_mapp
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
  } # slut if-sats om diag_tidsserie
  
  
} # slut diag-funktion

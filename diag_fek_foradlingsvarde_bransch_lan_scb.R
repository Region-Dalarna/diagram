diag_fek_foradlingsvarde_bransch_lan_scb <- function(
  region_vekt = "20",			# Val av region. Finns: "00", "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "17", "18", "19", "20", "21", "22", "23", "24", "25", "SE0", "SE00", "SE1", "SE11", "SE110", "SE12", "SE121", "SE122", "SE123", "SE124", "SE125", "SE2", "SE21", "SE211", "SE212", "SE213", "SE214", "SE22", "SE221", "SE224", "SE23", "SE231", "SE232", "SE3", "SE31", "SE311", "SE312", "SE313", "SE32", "SE321", "SE322", "SE33", "SE331", "SE332"
  sni2007_klartext = "*",			 #  NA = tas inte med i uttaget,  Finns: "A-SexklK-O samtliga näringsgrenar (exkl. K+O+T+U)", "A-01-03 jordbruk, skogsbruk och fiske", "B-05-09 utvinning av mineral", "C-10-33 tillverkning", "D-35 försörjning av el, gas, värme och kyla", "E-36-39 vattenförsörjning; avloppsrening, avfallshantering och sanering", "F-41-43 byggverksamhet", "G-45-47 handel; reparation av motorfordon och motorcyklar", "H-49-53 transport och magasinering", "I-55-56 hotell- och restaurangverksamhet", "J-58-63 informations- och kommunikationsverksamhet", "L-68 fastighetsverksamhet", "M-69-75 verksamhet inom juridik, ekonomi, vetenskap och teknik", "N-77-82 uthyrning, fastighetsservice, resetjänster och andra stödtjänster", "P-85 utbildning", "Q-86-88 vård och omsorg; sociala tjänster", "R-90-93 kultur, nöje och fritid", "S-94-96 annan serviceverksamhet"
  cont_klartext = "Förädlingsvärde, mnkr",			 #  Finns: "Antal arbetsställen (lokala verksamheter)", "Antal anställda", "Nettoomsättning exkl. merchantingkostnader, mnkr", "Produktionsvärde, mnkr", "Förädlingsvärde, mnkr", "Totala intäkter, mnkr", "Totala kostnader, mnkr"
  tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2022"
  gruppera_namn = NA,              # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
  diagram_capt = "Källa: Företagens ekonomi i SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förädlingsvärde är den faktiska produktionen minus kostnader för köpta varor och tjänster, dock ej löner, sociala avgifter och kostnader för handelsvaror.",
  visa_dataetiketter = FALSE,
  diag_fargvekt = NA,
  ta_med_logga = TRUE,
  logga_sokvag = NA,
  output_mapp = NA,
  skriv_diagramfil = TRUE,
  returnera_data_rmarkdown = FALSE,
  demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
  excelfil_mapp = NA,      # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  excel_filnamn = "helarsekvivalenter.xlsx"      # filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
) {

# ==============================================================================================================================
#
# Skriver ut diagram med förädlingsvärde per bransch. 
#  
#
# ==============================================================================================================================

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/foradlingsvarde_bransch_20_ar2022.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       glue,
       readxl)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8")
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_fek_lve_region_sni2007_tid_NSEBasfaktaLVEngs07_scb.R")

# om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
if (all(is.na(diag_fargvekt))) {
  if (exists("diagramfarger", mode = "function")) {
    diag_fargvekt <- diagramfarger("rus_sex")
  } else {
    diag_fargvekt <- hue_pal()(9)
  }
}

if (all(is.na(output_mapp))) {
  if (exists("utskriftsmapp", mode = "function")) {
    output_mapp <- utskriftsmapp()
  } else {
    stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
  }
}

gg_list <- list()

suppress_specific_warning(
foradl_df <- hamta_fek_lve_region_sni2007_tid_scb(
  region_vekt = region_vekt,      # Val av region.
  sni2007_klartext = sni2007_klartext,       #  Finns: "20-64 år", "20-65 år"
  cont_klartext = cont_klartext,       #  Finns: "Sjukpenning", "Sjuk- och aktivitetsersersättning", "Arbetslöshetsersättning", "Arbetsmarknadsåtgärder", "Ekonomiskt bistånd", "Etableringsersättning", "Summa helårsekvivalenter", "Folkmängd", "Andel av befolkningen"
  tid_koder = tid_koder,       # "*" = alla år eller månader, "9999" = senaste, finns: "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12"
  long_format = TRUE,      # TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  wide_om_en_contvar = FALSE,      # TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  output_mapp = output_mapp,      # anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  returnera_df = TRUE      # TRUE om man vill ha en dataframe i retur från funktionen
))

branschnyckel <- read_xlsx("g:/skript/nycklar/Bransch_FEK.xlsx") %>% 
  select(Avdelning, Grupp_kod, Branschgrupp) %>% 
  distinct()

chart_df <- foradl_df %>% 
  mutate(branschbokstav = str_sub(`näringsgren SNI 2007`, 1, 1)) %>% 
  filter(!str_detect(`näringsgren SNI 2007`, "samtliga näringsgrenar")) %>% 
  left_join(branschnyckel, by = c("branschbokstav" = "Avdelning")) %>% 
  group_by(år, regionkod, region, Grupp_kod, Branschgrupp, variabel) %>% 
  summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop")

# om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
# så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
if (!is.na(gruppera_namn)) {
  chart_df <- chart_df %>% 
    group_by(across(-c(regionkod, region, varde))) %>% 
    summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") %>% 
    mutate(regionkod = "gg",
           region = gruppera_namn) %>% 
    relocate(region, .before = 1) %>% 
    relocate(regionkod, .before = region)

region_vekt <- "gg"
}

if (!is.na(excelfil_mapp) & !is.na(excel_filnamn)){
  write.xlsx(chart_df, paste0(excelfil_mapp, excel_filnamn), overwrite = TRUE)
}

if(returnera_data_rmarkdown == TRUE){
  assign("chart_df", chart_df, envir = .GlobalEnv)
}

vald_region_txt <- chart_df %>% 
  distinct(region) %>% 
  dplyr::pull() %>%
  list_komma_och() %>% 
  skapa_kortnamn_lan()

ar_txt <- chart_df %>% 
  distinct(år) %>% 
  dplyr::pull() %>%
  list_komma_och()

diagramtitel <- glue("Förädlingsvärde i {region_txt} per bransch år {ar_txt}")
diagramfil <- glue("foradlingsvarde_bransch_{vald_region %>% paste0(collapse = '_')}_ar{ar_txt}.png")


gg_obj <- SkapaStapelDiagram(
  skickad_df = chart_df,
  skickad_x_var = "Branschgrupp",
  skickad_y_var = "varde",
  diagram_titel = diagramtitel,
  diagram_capt = diagram_capt,
  x_axis_sort_value = TRUE,
  stodlinjer_avrunda_fem = TRUE,
  filnamn_diagram = diagramfil,
  dataetiketter = visa_dataetiketter,
  manual_y_axis_title = cont_klartext,
  manual_x_axis_text_vjust = 1,
  manual_x_axis_text_hjust = 1,
  x_axis_lutning = 45,
  manual_color = diag_fargvekt,
  output_mapp = output_mapp,
  lagg_pa_logga = ta_med_logga,
  logga_path = logga_sokvag,
  skriv_till_diagramfil = skriv_diagramfil
)

gg_list <- c(gg_list, list(gg_obj))
names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")

return(gg_list)

} # slut diag-funktion

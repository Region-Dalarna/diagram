diagram_befolkningsforandring_ar <- function(region_vekt = "20", # Val av kommuner
                                             output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                             vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                             spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                             diag_folkmangd = TRUE, # Skapa diagram för flyttnetto
                                             diag_facet = FALSE, # Sätts till TRUE om man istället vill ha ett facet-diagram
                                             tid = "*",# Finns från 1968 till senaste år (som skrivs "9999")
                                             etiketter_xaxel = 4, # Intervall för etiketter på x-axeln (ej för facet där 12 används automatiskt)
                                             kon_klartext = NA, # Alternativet är c("kvinnor","män") där det görs en uppdelning på kön
                                             diag_forandring = TRUE, # Skapa diagram för flyttnetto uppdelat
                                             avrunda_fem = TRUE, # Avrunda till närmaste fem på y-axeln
                                             returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                             returnera_data = TRUE # True om användaren vill returnera data från funktionen
){
  
  # ===========================================================================================================
  # Diagram för befolkningsutveckling och folkökning på årsbasis. Vid flera regioner går det att välja mellan enskilda diagram eller facet
  # Uppdelning på kön är möjlig
  # Skapad: 2024-04-23
  # Förbättringsmöjligheter: Går för tillfället inte att summera  flera regioner
  # Uppdaterat så att ggplot-objekten inte innehåller år i sina namn (för att undvika problem i rapporter). /Jon
  # ===========================================================================================================
  
  
  # Hämtar bara det som faktiskt behövs (rddiagram/rdverktyg/pxweb2r + tre små
  # tidyverse-paket) i stället för source() mot funktioner-repot + p_load(tidyverse).
  # Undviker hela raden av C/C++-beroenden (magick, sf, systemfonts/ragg/textshaping
  # via hela tidyverse) som annars krävs bara för att komma åt ett fåtal funktioner.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")

  library(rddiagram)
  library(rdverktyg)
  library(dplyr)
  library(purrr)
  library(stringr)

  diagram_capt_bas <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."

  gg_list <- list()
  objektnamn <- c()

  # Hämtar samma data som tidigare kom via
  # hamta_data/hamta_bef_folkmangd_alder_kon_ar_scb.R, men direkt mot SCB:s
  # PxWeb-API v2 med pxweb2r, från två tabeller (SCB bytte metod - CKM,
  # röjandekontroll - för nya årgångar, med en ny tabell från och med 2025):
  # - TAB638  = gamla "BefolkningNy", 1968-2024. Civilstånd/ålder kan utelämnas
  #   (elimeringsbara), ger då totalen automatiskt.
  # - TAB5557 = nya CKM-tabellen "BefolkningCKM", från 2025. Civilstånd/ålder
  #   är INTE elimineringsbara där längre - måste anges explicit som
  #   totalkoderna "SC"/"TotSA", vilket också ger dem som egna kolumner i
  #   svaret; select() plockar bort dem så de två tabellernas kolumner
  #   stämmer överens innan de binds ihop till en sammanhängande tidsserie.
  # on_all_values_invalid = "null" gör att den tabell som inte har det
  # begärda året (t.ex. om `tid` bara är ett gammalt årtal) hoppas över i
  # stället för att hela hämtningen stoppas.
  befolkning_hist <- pxweb2r::pxweb2_get_data(
    "TAB638",
    query = list(Region = region_vekt, Kon = kon_klartext, Civilstand = NA,
                 Alder = NA, ContentsCode = c("Folkmängd", "Folkökning"), Tid = tid),
    on_all_values_invalid = "null"
  )
  befolkning_ckm <- pxweb2r::pxweb2_get_data(
    "TAB5557",
    query = list(Region = region_vekt, Kon = kon_klartext, Civilstand = "SC",
                 Alder = "TotSA", ContentsCode = c("Folkmängd", "Folkökning"), Tid = tid),
    on_all_values_invalid = "null"
  )
  if (!is.null(befolkning_ckm)) befolkning_ckm <- select(befolkning_ckm, -any_of(c("civilstånd", "ålder")))

  befolkning_df <- bind_rows(befolkning_hist, befolkning_ckm)
  if (ncol(befolkning_df) > 0) {
    befolkning_df <- rename(befolkning_df, regionkod = region_kod, variabel = tabellinnehåll, varde = value)
  }

  har_ckm_data <- !is.null(befolkning_ckm) && nrow(befolkning_ckm) > 0
  diagram_capt <- lagg_till_ckm_notering(diagram_capt_bas, har_ckm_data)


  # befolkning_df_CKM <- suppress_specific_warning(
  #   hamta_folkmangd_civilstand_alder_kon_ar_CKM(region_vekt = region_vekt,
  #                                                    tid_koder = tid,
  #                                                    alder_koder = "TotSA",
  #                                                    civilstand_klartext = "totalt, samtliga civilstånd",
  #                                                    kon_klartext = kon_klartext,
  #                                                    cont_klartext = c("folkmängd", "folkökning"))) %>% 
  #   select(-c(ålder, civilstånd))
  # 
  # befolkning_df <- bind_rows(befolkning_df,befolkning_df_CKM)
  
  if(returnera_data == TRUE){
    assign("befolkning_df", befolkning_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(bef, vald_region){
    if(diag_folkmangd == TRUE){
      
      ut_df <- bef %>% 
        filter(regionkod %in% vald_region)
      
      reg_txt <- (ut_df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(ut_df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Folkmängd")
      }else{
        diagram_titel <- paste0("Folkmängd i ", reg_txt)
      }

      #diagramfil <- paste0("Folkmangd_", reg_txt, "_ar_", min(ut_df$år), "_", max(ut_df$år), ".png")
      diagramfil <- paste0("Folkmangd_", reg_txt, ".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      if(length(kon_klartext)>1){
        farg = diagramfarger("kon")} else{
          farg = vald_farg[1]
        }
      
      gg_obj <- SkapaStapelDiagram(skickad_df = ut_df %>% 
                                     filter(variabel == "Folkmängd"), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = ifelse(length(kon_klartext)==1,NA,"kön"),
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   #x_axis_storlek = 8,
                                   x_axis_visa_var_xe_etikett = ifelse(length(unique(ut_df$region)) > 1,12,etiketter_xaxel),
                                   stodlinjer_avrunda_fem = avrunda_fem,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = TRUE,
                                   facet_grp = if (length(unique(ut_df$region)) > 1) "region" else NULL,
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   legend_vand = TRUE,
                                   manual_color =farg ,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
    }
    
    
    if(diag_forandring == TRUE){
      # # Skapa ny variabel för befolkningsförändring
      # befolkning_df_forandring <- befolkning_df %>% 
      #   filter(förändringar == "folkökning") %>%
      #   mutate(kategori = ifelse(personer>0,"Ökad befolkning","Minskad befolkning"))
      ut_df <- bef %>% 
        filter(regionkod %in% vald_region)
      
      reg_txt <- (ut_df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(ut_df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Befolkningsutveckling")
      }else{
        diagram_titel <- paste0("Befolkningsutveckling i ", reg_txt)
      }
      
      
      #diagramfil <- paste0("Befolkningsutveckling_", reg_txt, "_ar_", min(ut_df$år), "_", max(ut_df$år), ".png")
      diagramfil <- paste0("Befolkningsutveckling_", reg_txt, ".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      if(length(kon_klartext)>1) vald_farg = diagramfarger("kon")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = ut_df %>% 
                                     filter(variabel == "Folkökning",år>min(år)) %>%
                                     mutate(kategori = ifelse(varde>=0,"Ökad befolkning","Minskad befolkning")), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = ifelse(length(kon_klartext)==1,"kategori","kön"),
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   #x_axis_storlek = 8,
                                   x_axis_visa_var_xe_etikett = ifelse(length(unique(ut_df$region)) > 1,12,etiketter_xaxel),
                                   stodlinjer_avrunda_fem = avrunda_fem,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = TRUE,
                                   facet_grp = if (length(unique(ut_df$region)) > 1) "region" else NULL,
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   legend_vand = TRUE,
                                   manual_color = vald_farg,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      
    }
    names(gg_list) <- objektnamn
    return(gg_list)
  }
  
  if (diag_facet) {
    diag <- skapa_diagram(befolkning_df,region_vekt)
    
  } else {
    diag <- map(region_vekt, ~ skapa_diagram(befolkning_df, .x)) %>% purrr::flatten()
    
  }
  
  if(returnera_figur==TRUE)
    return(diag)
}


#test = diag_pendling_over_kommungrans(enbart_in_ut = TRUE)
diag_pendling_over_kommungrans <- function(vald_kommun = "20", # Länsnamn ger samtliga kommuner i länet
                                           hela_lanet = TRUE,           # Ändra inte
                                           valt_kon = "män och kvinnor", # Enda valet
                                           valt_ar = NA, # Enbart senaste år. Ändra inte
                                           visa_dataetiketter = FALSE,         # dataetiketter i diagrammet
                                           diag_absoluta_tal = TRUE,           # skriv ut diagram med absoluta tal
                                           diag_procent = TRUE,                # skriv ut diagram med procent
                                           skapa_fil = TRUE, # skapa en fil dig figuren sparas
                                           returnera_figur = TRUE, # Om TRUE returneras figur som ggplot-objekt
                                           enbart_in_ut = FALSE, # TRUE om man bara vill visa in och utpendling (ej bor och arbetar i samma kommun)
                                           diagramfarg_vektor = NA, # Valda färger
                                           diagram_capt = "Källa: SCB:s öppna statistikdatabas (RAMS tom 2019, därefter BAS), bearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Pendlingsdata kommer från RAMS tom år 2019 och inkluderar då åldrarna 16-74 år. Därefter kommer pendlingsdata från BAS och inkluderar åldrarna 15-74 år, från och med år 2020.",
                                           output_mapp_figur = "G:/Samhällsanalys/API/Fran_R/Utskrift/", # Hit sparas figuren
                                           output_mapp_data = NA, # Hit sparas data
                                           spara_data = FALSE, # Skall data sparas
                                           demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
                                           filnamn_data = "pendling.xlsx",
                                           returnera_data = TRUE) {# Filnamn för sparad data
  
  # ===========================================================================================================
  #
  # Skript för att skriva ut diagram (från RAMS, SCB) med andel och antal in- och utpendlare över kommungräns,
  # samt även de som bor och arbetar i samma kommun.
  # Skapad av: Peter
  # Senast uppdaterad: Jon, 2023-12-20
  # ===========================================================================================================
  
# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/in_utpendling_Dalarna2021.png",
"https://region-dalarna.github.io/utskrivna_diagram/in_utpendling_procent_Dalarna2021.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         httr,
         tidyverse,
         dplyr,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
                              
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(diagramfarg_vektor))) {
    if (exists("diagramfarger", mode = "function")) {
      diagramfarg_vektor <- diagramfarger("rus_sex")
    } else {
      diagramfarg_vektor <- hue_pal()(9)
    }
  }
               
  #url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207Z/PendlingKN"
  
  url_scb <- c("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210F/ArRegPend1",
               "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207Z/PendlingKN",
               "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207L/PendlingK",
               "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207L/PendlingK9303")
  
  
  px_meta_list <- map(url_scb, ~ pxweb_get(.x))
  
  px_meta_enkel_list <- extrahera_unika_varden_flera_scb_tabeller(px_meta_list)
  tabell_variabler <- pxvarlist(list(title = NULL, variables = px_meta_enkel_list))
  
  # om det finns fler än en tabell så kollar vi om det finns värden som överlappar mellan tabellerna
  px_meta_overlappande_varden <- if (length(url_scb) > 1) overlappande_varden_pxweb_hantera(px_meta_list, url_scb, var_kod = "Tid") else NULL  
  overlapp_txt <- if (is.null(px_meta_overlappande_varden)) "" else "\t"
  
  # om det finns år och månader så sorterar vi dessa i listan så det blir snyggare när de listas som möjliga värden i parameterlistan
  if ("tid" %in% tolower(tabell_variabler$koder)) {
    px_meta_enkel_list <- sortera_px_variabler(px_meta_enkel_list, sorterings_vars = "tid", sortera_pa_kod = TRUE)
  }
  px_meta <- list(title = px_meta_list[[1]]$title, variables = px_meta_enkel_list)
  
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  list_data <- list() # Skapar en tom lista som används för att spara data 
  
  visa_dataetik_txt <- ifelse(visa_dataetiketter, "_lbl_","")
  
  if (hela_lanet) {
    vald_kommun_txt <- hamtaregion_kod_namn(str_sub(vald_kommun,1,2))$region %>% 
    skapa_kortnamn_lan() %>% 
    list_komma_och()
    
    vald_kommun_long_txt <- hamtaregion_kod_namn(str_sub(vald_kommun,1,2))$region
    
    vald_kommun_filnamn <- vald_kommun_txt
    
    titel_tillag <- "kommuner "         # ändra filnamnet om man tar ut hela länet eller inte
  } else {
    vald_kommun_txt <- hamtaregion_kod_namn(vald_kommun)$region %>% 
      skapa_kortnamn_lan() %>% 
      list_komma_och()
    
    if (length(vald_kommun) > 5) vald_kommun_filnamn <- c(vald_kommun[1:3], "mfl") else vald_kommun_filnamn <- vald_kommun
    vald_kommun_filnamn <- paste0(vald_kommun_filnamn, collapse = "_")
    
    titel_tillag <- ""                   # ändra filnamnet om man tar ut hela länet eller inte
  }
  
  # ta ut senaste år om vi inte skickat med några år eller om   
  if (is.na(valt_ar)) {
    valt_ar <- max(hamta_giltiga_varden_fran_tabell(px_meta, "tid"))
  }

  # # hämta rätt kod för kön
  # if (is.na(as.numeric(valt_kon))) {
  #    valt_kon_kod <- hamta_kod_med_klartext(url_uttag, valt_kon)
  # } else {
  #   valt_kon_kod <- valt_kon
  # }
  
  # hämta rätt kod för kommun
  if (is.na(as.numeric(vald_kommun))) {
    vald_kommun_kod <- hamta_kod_med_klartext(url_uttag, vald_kommun)
  } else {
    if (hela_lanet) { 
      vald_kommun_kod <- hamtakommuner(str_sub(vald_kommun,1,2), tamedlan = FALSE, tamedriket = FALSE)
    } else {
      vald_kommun_kod <- vald_kommun
    }
  }

  # =============================================== API-uttag ===============================================
  
  # varlista <- list(
  #   Region = vald_kommun_kod,
  #   Kon = valt_kon_kod,
  #   ContentsCode = '*',
  #   Tid = valt_ar
  # )
  # 
  # px_uttag <- pxweb_get(url = url_uttag, query = varlista)     # uttag från scb:s api
  # 
  # # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  # suppressWarnings(px_df <- as.data.frame(px_uttag) %>% 
  #   cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
  #           select(Region)) %>% 
  #   rename(regionkod = Region) %>% 
  #   relocate(regionkod, .before = region) %>% 
  #   mutate(`Utpendlare över kommungräns` = `Utpendlare över kommungräns` * -1) %>% 
  #   pivot_longer(c("Inpendlare över kommungräns", "Utpendlare över kommungräns",
  #                  "Bor och arbetar i kommunen"), values_to = "antal förvärvsarbetande", names_to = "pendlingstyp") %>% 
  #   mutate(pendlingstyp = factor(pendlingstyp, levels = c("Inpendlare över kommungräns",
  #                                                         "Bor och arbetar i kommunen",
  #                                                         "Utpendlare över kommungräns"))))
  
  px_df <-  hamta_pendling_over_grans_region_kon_tid_scb(region_vekt = vald_kommun_kod,
                                                         kon_klartext = valt_kon,
                                                         tid_koder = valt_ar)
  
  # ============================== diagram med absoluta tal ==================================
  if (diag_absoluta_tal) {
    
    px_df_ut = px_df
    
    if(enbart_in_ut == TRUE) px_df_ut <- px_df_ut %>% filter(variabel != "Bor och arbetar i kommunen")
    
    if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("antal_pendlare" = px_df_ut))
    }
    
    if(returnera_data == TRUE){
      assign("antal_pendlare", px_df_ut, envir = .GlobalEnv)
    }
  
    diagram_titel <- paste0("Antal pendlare i ", titel_tillag, "i ", vald_kommun_txt, " år ", unique(px_df$år))
    diagramfil <- paste0("in_utpendling_", vald_kommun_filnamn, unique(px_df$år), visa_dataetik_txt, ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_ut,  
                       skickad_x_var = "region", 
                       skickad_y_var = "varde",
                       skickad_x_grupp = "variabel",
                       diagram_titel = diagram_titel,
                       diagram_capt = diagram_capt,
                       manual_x_axis_text_vjust = 1,
                       manual_x_axis_text_hjust = 1,
                       manual_color = diagramfarg_vektor,
                       manual_y_axis_title = "antal förvärvsarbetande",
                       stodlinjer_avrunda_fem = TRUE,
                       geom_position_stack = TRUE,
                       dataetiketter = visa_dataetiketter,
                       skriv_till_diagramfil = skapa_fil,
                       output_mapp = output_mapp_figur,
                       filnamn_diagram = diagramfil)
  
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- "In_och_utpendling_absoluta_tal"
    
  } # slut if-sats för diag_abosluta tal
  
  # ================================= diagram med procent ==================================
  if (diag_procent) {
    
    px_df_andel = px_df %>% 
      pivot_wider(names_from = variabel, values_from = varde) %>% 
        mutate("Andel utpendling" = (abs(`Utpendlare över kommungräns`)/(`Bor och arbetar i kommunen`+abs(`Utpendlare över kommungräns`)))*100,
               "Andel inpendling" = (`Inpendlare över kommungräns`/(`Bor och arbetar i kommunen`+`Inpendlare över kommungräns`))*100,
               "Bor och arbetar i samma kommun" = (`Bor och arbetar i kommunen`/(`Bor och arbetar i kommunen`+abs(`Utpendlare över kommungräns`)))*100) %>% 
          select(-c(`Inpendlare över kommungräns`,`Utpendlare över kommungräns`,`Bor och arbetar i kommunen`)) %>% 
            pivot_longer((length(names(.))-2):length(names(.)),names_to = "variabel",values_to = "andel")
    
    if(enbart_in_ut == TRUE) px_df_andel <- px_df_andel %>% filter(variabel != "Bor och arbetar i samma kommun")
    
    if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
      list_data <- c(list_data,list("andel_pendlare" = px_df_andel))
    }
    
    if(returnera_data == TRUE){
      assign("andel_pendlare", px_df_andel, envir = .GlobalEnv)
    }
    
    
    diagram_titel <- paste0("Andel pendling i ", vald_kommun_long_txt, " år ", unique(px_df$år)) %>% 
    dela_upp_strang_radbryt(70)

    diagramfil <- paste0("in_utpendling_procent_", vald_kommun_filnamn, unique(px_df$år), visa_dataetik_txt, ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = px_df_andel ,  
                       skickad_x_var = "region", 
                       skickad_y_var = "andel",
                       skickad_x_grupp = "variabel",
                       diagram_titel = diagram_titel,
                       diagram_capt = diagram_capt,
                       manual_y_axis_title = "procent",
                       manual_x_axis_text_vjust = 1,
                       manual_x_axis_text_hjust = 1,
                       manual_color = diagramfarg_vektor,
                       x_axis_sort_value = TRUE,
                       x_axis_sort_grp = 2,
                       vand_sortering = TRUE,
                       stodlinjer_avrunda_fem = TRUE,
                       dataetiketter = visa_dataetiketter,
                       skriv_till_diagramfil = skapa_fil,
                       output_mapp = output_mapp_figur,
                       filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- "In_och_utpendling_procent"
    
  } # slut if-sats diag_procent
  
  # Sparar data
  if(!is.na(output_mapp_figur) & !is.na(filnamn_data)){
    write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
    # assign("andel_off_tot_df", andel_totalt_utskrift, envir = .GlobalEnv)
  }
 if(returnera_figur == TRUE) return(gg_list)
} # slut funktion

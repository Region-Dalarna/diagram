
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       glue,
       tidyverse)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

diag_inr_flyttnetto_inr_utr_fodda <- function(
    region_vekt = "20",
    alder_grp = "*",                                 # "*" för alla åldrar, annars skickas vektorn till skapa_aldersgrupper()
               # list(c(20,66)) för bara åldersgruppen 20-66 år, för flera åldersgrupper: 
               # list(c(0,19), c(20,65), c(66,79), c(80, 999)) skapar åldersgrupperna 0-19, 20-65, 66-79 och 80+
    gruppera_namn = NA,                               # om NA skapas ett diagram per region, annars grupperas de ihop och får namnet som anges här
    relativt_flyttnetto = FALSE,                     # TRUE om vi vill ha relativt flyttnetto (mot bef i samma grupper året innan)
    facet_diagram = TRUE,                            # om TRUE skapas ett diagram för alla regioner, annars ett diagram för varje region
    farg_vekt = NA,
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nInrikes flyttnetto är skillnaden mellan de som flyttat in till och de som flyttat ut från en kommun/län, från och till andra kommuner/län",
    output_mapp = NA,
    skriv_diagram = TRUE,
    skriv_excel = FALSE,
    spara_som_svg = FALSE,                            # TRUE om vi vill spara diagrammet som svg
    visa_totalvarden = TRUE,                          # skriver ut ett streck för netto både inrikes och utrikes födda
    visa_totalvarden_dataetiketter = FALSE,           # skriver ut dataetiketter för totalvärdena
    totalvarden_dataetiketter_farg = "black",         # välj färg på totalstrecken
    totalvarden_dataetiketter_hjust = 20,             # justerar dataetiketter för totalvärden i höjdled
    totalvarden_dataetiketter_textstorlek = 2,        # justerar textstorlek för dataetiketter för totalvärden
    totalvarden_linjetjocklek = 4                     # tjocklek på totalstrecken i tiondels % av hela diffen i datasetet
    ) {
  
  gg_list <- list()
  
  diagram_format <- if (spara_som_svg)  "svg" else "png"
  
  # om ingen output_mapp är medskickad och funktionen utskriftsmapp finns, använd den, annars sätt skriv_diagramfil till FALSE
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp() 
    } else {
      skriv_diagramfil <- FALSE
    }
  }
  
  # om ingen färgvektor är medskickad, kolla om funktionen diagramfärger finns, annars använd r:s defaultfärger
  if (all(is.na(farg_vekt))) {
    if (exists("diagramfarger", mode = "function")) {
      farg_vekt <- diagramfarger("rus_sex")[c(2,1)]
    } else {
      farg_vekt <- hue_pal()(9)
    }
  }
 
  
  # =====================================================================================================
  
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101J/FlyttFodReg"
  
  if (!all(alder_grp == "*")) {
    
    alder_grp <- map(alder_grp, as.character)        # konvertera till text
    alder_vekt <- alder_grp                          # skapa alder_vekt utifrån alder_grp
    
    # om 999 är medskickat (= högsta ålder) så byts det ut till 99 för att kunna skapa vektor
    if (any(map_lgl(alder_grp, ~ "999" %in% .x))) {
      alder_vekt <- alder_vekt %>% 
        map(~ replace(.x, .x == "999", "99"))
    }
    
    # skapa åldersgrupper för varje vektor i listan och lägg ihop i en vektor
    alder_vekt <- alder_vekt %>% 
      map(~ seq(.x[1], .x[2])) %>% 
      unlist() %>% 
      as.character()
    
    # om 999 är medskickat så lägg till 100+ i vektorn för att få med alla åldrar
    if (any(map_lgl(alder_grp, ~ "999" %in% .x))) alder_vekt <- c(alder_vekt, "100+")
    
    # skapa en vektor som vi kan skicka med till skapa_aldersgrupper()
    alder_grp_vekt <- alder_grp %>% 
      map_int(~ .[1] %>% parse_number) 
    
    # om sista vektorn inte är 100+ så lägger vi till sista värdet i vektorn innan + 1
    if (alder_grp[[length(alder_grp)]][2] != "100+") {
      alder_grp_vekt <- append(
        alder_grp_vekt,
        parse_number(alder_grp[[length(alder_grp)]][2]) + 1
      )
    }
    
  } else alder_vekt <- alder_grp
  
  
  cont_var <- hamta_klartext_med_kod(url_uttag, c("000001J5", "000001EE"), "contentscode")
  
  varlista <- list(
    Region = region_vekt,
    Fodelseregion = "*",
    Alder = alder_vekt,
    ContentsCode = c("000001J5", "000001EE"),
    Tid = '*')
  
  if (all(alder_vekt == "*")) varlista <- varlista[names(varlista) != "Alder"]
  # =============================================== API-uttag ===============================================
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(Region)) %>% 
    rename(regionkod = Region) %>% relocate(regionkod, .before = region) %>% 
    mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE))     # gör om län till kortnamn och riket till Sverige
  
  # här grupperar vi på åldersgrupper om man valt att skicka med åldersgrupper
  if (any(alder_vekt != "*")) {
    px_df <- px_df %>% 
      mutate(alder_num = ålder %>% parse_number,
             aldersgrupp = skapa_aldersgrupper(alder_num, alder_grp_vekt)) %>% 
      select(-c(alder_num, ålder)) %>% 
      group_by(across(where(~ is.character(.x) || is.factor(.x)))) %>%
      summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>% 
      ungroup()
  } 
  
  
  if (!is.na(gruppera_namn)){
    px_df <- px_df %>% 
      select(-c(regionkod, region)) %>%
      group_by(across(where(~ is.character(.x) || is.factor(.x)))) %>%
      summarise(Inrikes_flyttnetto = sum(across(where(is.numeric)), na.rm = TRUE)) %>% 
      # group_by(år, födelseregion) %>% 
      # summarise(Inrikes_flyttnetto = sum(across(all_of(cont_var)))) %>% 
      ungroup() %>% 
      mutate(region = gruppera_namn)
    
  } else {
    px_df <- px_df %>% 
      group_by(across(where(~ is.character(.x) || is.factor(.x)))) %>%
      summarise(Inrikes_flyttnetto = sum(across(where(is.numeric)), na.rm = TRUE)) %>% 
      # group_by(år, regionkod, region, födelseregion) %>% 
      # summarise(Inrikes_flyttnetto = sum(across(all_of(cont_var)))) %>% 
      ungroup()
  }
  
  if (relativt_flyttnetto) {
    source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_region_alder_kon_fodelseregion_tid_InrUtrFoddaRegAlKon_scb.R")
    
    bef_ar_vekt <- unique(px_df$år) %>% as.numeric() -1
    
    bef_df <- hamta_bef_region_alder_kon_fodelseregion_tid_scb(
      region_vekt = region_vekt,
      kon_klartext = NA,
      alder_koder = alder_vekt,
      tid_koder = bef_ar_vekt
      )
    
    bef_df <- bef_df %>%
      mutate(alder_num = ålder %>% parse_number,
             aldersgrupp = skapa_aldersgrupper(alder_num, alder_grp_vekt), 
             region = region %>% skapa_kortnamn_lan()) %>% 
      select(-c(alder_num, ålder)) %>% 
      group_by(across(where(~ is.character(.x) || is.factor(.x)))) %>%
      summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) %>% 
      ungroup() %>% 
      rename(bef = Antal) %>% 
      mutate(år = as.character(as.numeric(år) + 1 ))           # för att koppla till rätt år
  
    kol_nycklar <- intersect(
      bef_df %>% select(where(~ is.character(.x) || is.factor(.x))) %>% names(),
      px_df %>% select(where(~ is.character(.x) || is.factor(.x))) %>% names()
    )
    
    # lägg ihop de båda dataframesen på region, år och födelseregion, skapa ett relativt flyttnetto
    px_df <- left_join(px_df, bef_df, by = kol_nycklar) %>% 
      mutate(rel_flyttnetto = Inrikes_flyttnetto/bef * 100) %>% 
      rename(abs_flyttnetto = Inrikes_flyttnetto,
             Inrikes_flyttnetto = rel_flyttnetto)
    
    visa_totalvarden <- FALSE          # vid relativt flyttnetto så tar vi bort visa totalvärden
    relativt_txt <- "relativt "
      
  } else relativt_txt <- ""
  
  relativt_filnamn <- relativt_txt %>% str_remove(" ") %>% paste0(., "_")
  
  if (skriv_excel){
    reg_namn <- ifelse(!is.na(gruppera_namn), gruppera_namn, paste0(region_vekt, collapse = "_"))
    excelfil <- paste0("andel_arblosa_", min(arblosa_bakgr$tid), "_", max(arblosa_bakgr$tid) ,".xlsx")
    write.xlsx(px_df, paste0("Flyttnetto_", reg_namn, "_ar", min(chart_df$år), "_", max(chart_df$år), ".xlsx"), overwrite = TRUE)
  }
  # ============================================= Skapa diagram ==============================================
  
  #for (reg in unique(px_df$region)) {
  skapa_diagram <- function(vald_regionkod) {  # skapa en funktion som skapar diagram för varje region
    
    retur_list <- list()
    
    chart_df <- px_df %>% filter(regionkod %in% vald_regionkod) 
    
    reg_txt <- chart_df$region %>% unique() %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE)
    unika_ar <- unique(chart_df$år)
    unika_reg <- unique(vald_regionkod)
    unika_reg_txt <- hamtaregion_kod_namn(unika_reg)$region %>% skapa_kortnamn_lan()
    # =================================== visa totalvärden ========================================
    if (visa_totalvarden){
      
      diff <- max(chart_df$Inrikes_flyttnetto) - min(chart_df$Inrikes_flyttnetto) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- diff * (totalvarden_linjetjocklek/1000)      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()
      
      for (reg in 1:length(unika_reg)) { 
        for (ar in 1:length(unika_ar)){
          arsvarde <- chart_df %>% 
            filter(år == unika_ar[ar],
                   regionkod == unika_reg[reg]) %>% 
            select(Inrikes_flyttnetto) %>% 
            dplyr::pull()
          arsvarde <- sum(arsvarde, na.rm = TRUE)
          total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))
          if (visa_totalvarden_dataetiketter) {
            total_list <- c(total_list, list(list(geom = "text", y=arsvarde+totalvarden_dataetiketter_hjust, x = ar, size = totalvarden_dataetiketter_textstorlek, angle=0, fontface = "plain", label =arsvarde, color = totalvarden_dataetiketter_farg)))
          } # slut if-sats om man vill vissa dataetiketter
        } # slut for-loop unika_ar
      } # slut for_loop unika_reg        
    } else total_list <- NA # slut if-sats visa_totalvärden
    
    # ======================= skapa ggplot-objekt =================
      
    diagtitel_txt <- if (facet_diagram) " i" else paste0(" i ", reg_txt)
    
    #ar_alla_kommuner_i_ett_lan(vald_regionkod)
    
    diagram_titel <- glue("Inrikes {relativt_txt}flyttnetto {diagtitel_txt}")
    diagramfil <- paste0("Flyttnetto_", relativt_filnamn, unika_reg_txt %>% paste0(collapse = "_"), "_ar", min(chart_df$år), "_", max(chart_df$år), ".png") %>% str_replace_all("__", "_")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_df, 
                       skickad_x_var = "år", 
                       skickad_y_var = "Inrikes_flyttnetto",
                       skickad_x_grupp = "födelseregion",
                       diagram_titel = diagram_titel,
                       diagram_capt = diagram_capt,
                       #x_axis_storlek = 8,
                       stodlinjer_avrunda_fem = TRUE,
                       manual_x_axis_text_vjust = 1,
                       manual_x_axis_text_hjust = 1,
                       manual_y_axis_title = if (relativt_flyttnetto) "procent" else "",
                       geom_position_stack = if (relativt_flyttnetto) FALSE else TRUE,
                       fokusera_varden = total_list,
                       diagram_facet = facet_diagram,
                       facet_grp = "region",
                       skriv_till_diagramfil = !visa_totalvarden,
                       manual_color = farg_vekt,
                       diagram_bildformat = diagram_format,
                       output_mapp = output_mapp,
                       filnamn_diagram = diagramfil)
    
    if (visa_totalvarden){
      suppressMessages(
      dia_med_utan_legend <- gg_obj +
        geom_line(aes(group = 1, color="line"), alpha = 0)+
        scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto totalt")+
        theme(legend.key = element_rect(fill = "white"),
              legend.box.just = "bottom")
      )
    } else dia_med_utan_legend <- gg_obj # slut if-sats visa_totalvarden  
    
    retur_list <- c(retur_list, list(dia_med_utan_legend))
    names(retur_list)[length(retur_list)] <- diagramfil %>% str_remove("\\.[^.]+$")
    
      if (skriv_diagram) {                           # skriv en diagramfil om så önskas
        suppressMessages(
        invisible(
        skriv_till_diagramfil(dia_med_utan_legend,
                            output_mapp = output_mapp,
                            filnamn_diagram = diagramfil)
        ))
      } # slut if-sats
    return(retur_list)
  } # slut skapa diagram-funktion för varje region
  
  if (length(region_vekt) == 1) facet_diagram <- FALSE 
  
  if (facet_diagram) {
    gg_list <- skapa_diagram(region_vekt)
  } else {
    gg_list <- map(unique(region_vekt), ~skapa_diagram(.x)) %>% purrr::list_flatten()
  }

  return(gg_list)
} # slut funktion

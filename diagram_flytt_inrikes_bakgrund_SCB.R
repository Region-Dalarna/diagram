diag_inr_flyttnetto_inr_utr_fodda <- function(
    region_vekt = "20",                               # regionkod eller vektor med regionkoder
    gruppera_namn = NA,                               # om NA skapas ett diagram per region, annars grupperas de ihop och får namnet som anges här
    facet_diagram = FALSE,                            # om TRUE skapas ett diagram för alla regioner, annars ett diagram för varje region
    farg_vekt = diagramfarger("rd_gron")[c(1,4)],     # färgvektor för diagrammet
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nInrikes flyttnetto är skillnaden mellan de som flyttat in till och de som flyttat ut från en kommun/region, från och till andra kommuner/regioner",
    output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",
    skriv_diagram = TRUE,                             # TRUE om vi vill skriva ut diagrammet
    skriv_excel = FALSE,                              # TRUE om vi vill skriva ut data till excel
    returnera_data = FALSE,                           # TRUE om vi vill returnera data till R:s globala miljö 
    #spara_som_svg = FALSE,                            # TRUE om vi vill spara diagrammet som svg
    filformat = "png",                                # filformat för diagrammet
    visa_totalvarden = TRUE,                          # skriver ut ett streck för netto både inrikes och utrikes födda
    fixa_y_axel_varden_jamna_tal = TRUE,              # TRUE om vi vill ha vettigare värden på y-axeln, men funkar inte alltid och då kan man stänga av detta.
    demo = FALSE,                                     # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat    visa_totalvarden = TRUE,                          # skriver ut ett streck för netto både inrikes och utrikes födda
    visa_totalvarden_dataetiketter = FALSE,           # skriver ut dataetiketter för totalvärdena
    totalvarden_dataetiketter_farg = "black",         # välj färg på totalstrecken
    totalvarden_dataetiketter_hjust = 20,             # justerar dataetiketter för totalvärden i höjdled
    totalvarden_dataetiketter_textstorlek = 2,        # justerar textstorlek för dataetiketter för totalvärden
    totalvarden_linjetjocklek = 4                     # tjocklek på totalstrecken i tiondels % av hela diffen i datasetet
) {
  
  # ===========================================================================================================
  # Diagram för inrikes flyttnetto uppdelat på bakgrund (inrikes och utrikes födda)
  # Går att skapa diagram för flera regioner samtidigt eller som ett facet-diagram
  # Senast uppdaterad: 2024-04-25
  # Förbättringsmöjligheter: Går för tillfället inte att summera  flera regioner
  # ===========================================================================================================
  
  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på 
  if (demo){
    demo_url <- 
        "https://region-dalarna.github.io/utskrivna_diagram/Flyttnetto_bakgrund_Dalarna.png"
    browseURL(demo_url)
    stop_tyst()
  }
  
  if("00" %in% region_vekt){
    stop("Region 00 (Riket) saknar inrikes flyttnetto och kan inte användas.\nÄndra region_vekt")
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  objektnamn <- c()
  
  if (is.na(farg_vekt[1])) farg_vekt <- diagramfarger("rd_gron")
  
  # =====================================================================================================
  
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101J/FlyttFodReg"
  
  cont_var <- hamta_klartext_med_kod(url_uttag, c("000001J5", "000001EE"), "contentscode")
  #region_vekt = "20"
  
  varlista <- list(
    Region = region_vekt,
    Fodelseregion = "*",
    ContentsCode = c("000001J5", "000001EE"),
    Tid = '*')
  
  # =============================================== API-uttag ===============================================
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(Region)) %>% 
    rename(regionkod = Region) %>% relocate(regionkod, .before = region) %>% 
    mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE))     # gör om län till kortnamn och riket till Sverige
  
  if (!is.na(gruppera_namn)){
    px_df <- px_df %>% 
      group_by(år, födelseregion) %>% 
      summarise(Inrikes_flyttnetto = sum(across(all_of(cont_var)))) %>% 
      ungroup() %>% 
      mutate(region = gruppera_namn)
    
  } else {
    px_df <- px_df %>% 
      group_by(år, regionkod, region, födelseregion) %>% 
      summarise(Inrikes_flyttnetto = sum(across(all_of(cont_var)))) %>% 
      ungroup()
  }
  
  
  if (skriv_excel){
    reg_namn <- ifelse(!is.na(gruppera_namn), gruppera_namn, paste0(region_vekt, collapse = "_"))
    excelfil <- paste0("andel_arblosa_", min(arblosa_bakgr$tid), "_", max(arblosa_bakgr$tid) ,".xlsx")
    write.xlsx(px_df, paste0("Flyttnetto_", reg_namn, "_ar", min(px_df$år), "_", max(px_df$år), ".xlsx"), overwrite = TRUE)
  }
  
  # Returnerar data till R globala miljö
  if(returnera_data == TRUE){
    assign("flytt_bakgrund_df", px_df, envir = .GlobalEnv)
  }
  
  
  # ============================================= Skapa diagram ==============================================
  
  #for (reg in unique(px_df$region)) {
  skapa_diagram <- function(df,vald_regionkod) {  # skapa en funktion som skapar diagram för varje region
    
    df <- df %>% filter(regionkod %in% vald_regionkod)
    
    #retur_list <- list()
    #vald_regionkod = "20"
    #chart_df <- px_df %>% filter(regionkod %in% vald_regionkod) 
    
    reg_txt <- (df$region %>% unique() %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE))[1]
    
    if(length(unique(df$region)) > 1){
      reg_txt <- paste0(reg_txt,"_facet")
      diagram_titel <- paste0("Inrikes flyttnetto")
    }else{
      diagram_titel <- paste0("Inrikes flyttnetto i ", reg_txt)
    }
    # =================================== visa totalvärden ========================================
    if (visa_totalvarden == TRUE && facet_diagram == FALSE){
      
      diff <- max(df$Inrikes_flyttnetto) - min(df$Inrikes_flyttnetto) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- diff * (totalvarden_linjetjocklek/1000)      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()
      unika_ar <- unique(df$år)
      unika_reg <- unique(vald_regionkod)
      unika_reg_txt <- hamtaregion_kod_namn(unika_reg)$region %>% skapa_kortnamn_lan()
      
      for (reg in 1:length(unika_reg)) { 
        for (ar in 1:length(unika_ar)){
          arsvarde <- df %>% 
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
    
    #diagtitel_txt <- if (facet_diagram) " i" else paste0(" i ", reg_txt)
    
    #ar_alla_kommuner_i_ett_lan(vald_regionkod)
    
    #diagram_titel <- paste0("Inrikes flyttnetto", diagtitel_txt)
    diagramfil <- paste0("Flyttnetto_bakgrund_", reg_txt %>% paste0(collapse = "_"),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df, 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "Inrikes_flyttnetto",
                                 skickad_x_grupp = "födelseregion",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 stodlinjer_avrunda_fem = fixa_y_axel_varden_jamna_tal,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 geom_position_stack = TRUE,
                                 fokusera_varden = total_list,
                                 diagram_facet = facet_diagram,
                                 facet_grp = "region",
                                 skriv_till_diagramfil = !visa_totalvarden,
                                 manual_color = farg_vekt,
                                 #diagram_som_svg = spara_som_svg,
                                 diagram_bildformat = filformat,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil)
    
    if (visa_totalvarden){
      gg_obj <- gg_obj +
        geom_line(aes(color="line"))+
        scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto totalt")+
        theme(legend.key = element_rect(fill = "white"),
              legend.box.just = "bottom")
    } # slut if-sats visa_totalvarden  
    
    gg_list <- c(gg_list, list(gg_obj))
    
    if (skriv_diagram) {                           # skriv en diagramfil om så önskas
      skriv_till_diagramfil(gg_obj,
                            output_mapp = output_mapp,
                            filnamn_diagram = diagramfil)
    } # slut if-sats
    names(gg_list) <- diagramfil %>% str_remove(".png")
    return(gg_list)
  } # slut skapa diagram-funktion för varje region
  
  
  if (facet_diagram) {
    diag <- skapa_diagram(px_df,region_vekt)
    
  } else {
    diag <- map(unique(region_vekt), ~skapa_diagram(px_df,.x)) %>% purrr::flatten()
    
  }
  
  return(diag)
} # slut funktion

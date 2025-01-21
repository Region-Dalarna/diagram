
diag_forandr_branscher_bubblor <- function(vald_geografi ="20",                               # kan vara en eller flera geografier, skicka med vektor om flera, ex: c("20", "25") 
                                           egetnamn_geografi = NA,                           # om man vill använda ett eget namn på geografin, ex. "Norra Mellansverige" för c("20", "17", "21")
                                           till_word = FALSE,                                 # om diagrammet ska användas i Word görs etiketter + diagram-caption större
                                           ta_bort_okand = TRUE,                             # sätt TRUE om okänd ska tas bort som branschindelning
                                           start_ar = NA,                                    # om NA väljs tidigaste möjliga år, väljs ett år som ligger före tillgängliga år så väljs istället tidigaste möjliga år
                                           slut_ar = NA,                                     # om NA väljs senaste möjliga år, väljs ett år som ligger efter tillgängliga år så väljs istället senaste möjliga år)
                                           nudge_y_varde = -12,                              # hur långt etiketterna hamnar från bubblorna, ska normalt vara oförändrade men om etiketterna hamnar tokigt kan man prova att ändra det värdet 
                                           output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                                           diagram_capt = "Källa: SCB:s öppna statistikdatabas, Bearbetning: Samhällsanalys, Region Dalarna\nOBS! Från och med år 2019 samlas statistiken in på ett annat sätt och är därmed inte fullt jämförbar med tidigare år",                   # \nJord- och skogsbruk definierades om 2011 och därför har förändringen mellan år 2010 och 2011 tagits bort ur beräkningen",
                                           skapa_fil = TRUE,
                                           diagramtitel_tabort = FALSE,
                                           skapa_excelfil = FALSE,
                                           logga_path = NA                                 # ändra till NULL för att köra utan logga
                                           ) {  

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         pxweb,
         #httr,
         readxl,
         ggrepel)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bas_rams_region_sni2007_dagbef_kon_tid_fodelseregion_DagSni07KonKN_DagSNI07KonK_ArbStDoNArNN_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)         # använder funktion för att ta alla namn i en vektor och lägga som lista med komma mellan samt och mellan näst sista och sista element
  options(dplyr.summarise.inform = FALSE)
    
  # hämta branschnyckel från Region Dalarnas github-repo depot
  url_xlsx <- "https://raw.githubusercontent.com/Region-Dalarna/depot/main/Bransch_Gxx_farger.xlsx"
  temp_xlsx <- tempfile(fileext = ".xlsx")
  download.file(url_xlsx, temp_xlsx, mode = "wb", quiet = TRUE)
  branschtabell <- readxl::read_xlsx(temp_xlsx)
  unlink(temp_xlsx)
  
  if ((!is.na(start_ar) & start_ar < 2011) & (!is.na(slut_ar) & slut_ar > 2010)) Valda_ar <- unique(c(start_ar, slut_ar, 2010, 2011))
  if (is.na(start_ar) | is.na(slut_ar)) valda_ar <- '*'

  if (is.na(egetnamn_geografi)){
    geo_df <- hamtaregion_kod_namn(vald_geografi)
    if(nrow(geo_df) > 1) {                                 # om det finns fler än en geografi i skickad vektor
      geo_kortnamn <- skapa_kortnamn_lan(geo_df$region)
      geo_namn <- list_komma_och(geo_kortnamn) 
      } else geo_namn <- skapa_kortnamn_lan(geo_df$region)
    
  } else geo_namn <- egetnamn_geografi
  

  px_df <- hamta_bas_rams_region_sni2007_dagbef_kon_tid_fodelseregion_scb(region_vekt = vald_geografi,
                                                                          kon_klartext = NA,
                                                                          tid_koder = valda_ar)
  
  if (is.na(start_ar)) start_ar <- min(px_df$år)       # om inte start_ar är valt, ta det tidigaste året i tidsserien
  if (is.na(slut_ar)) slut_ar <- max(px_df$år)         # om inte slut_ar är valt, ta det senaste året i tidsserien

  chart_df <- px_df %>% 
    filter(år == start_ar | år == slut_ar) %>% 
    group_by(år, branschkod, `näringsgren SNI 2007`) %>% 
    summarise(dagbefolkning = sum(dagbefolkning, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(branschkod)   # sortera i SNI-kodsordning så att färgerna blir rätt i nästa steg
  
  # koppla på texter från branschtabell
  chart_df <- chart_df %>% 
    left_join(branschtabell %>% select(Br15kod, Bransch), by = c("branschkod" = "Br15kod"))
  
  # sortera branschtabell så att det blir samma ordning som chart_df, och rätt med färger från branschtabell
  branschtabell <- branschtabell %>% 
    arrange(Br15kod)

  # fixa färger, hämta värden från branschtabell
  farger <- branschtabell$HexCode
  names(farger) <- branschtabell$Bransch
  if (ta_bort_okand) farger <- farger[!grepl("okänd", names(farger))]             # om användaren valt att ta bort okänd så tas den bort även ur färgvektorn
  
  # # justering för omklassificering av jord- och skogsbruk
  # chart_df$syss[chart_df$år == slut_ar & chart_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske"] <-
  #   chart_df$syss[chart_df$år == slut_ar & chart_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske"] - jordskog_korr
  
  if (ta_bort_okand) chart_df <- chart_df %>% filter(`näringsgren SNI 2007` != "uppgift saknas")         # om användaren valt att ta bort okänd så tas den bort ur datasetet som diagrammet tillverkas med 
  
  chart_df <- chart_df %>%
    arrange(`näringsgren SNI 2007`, år) %>% 
    ungroup() %>% 
    mutate(forandr = ((dagbefolkning - lag(dagbefolkning))/lag(dagbefolkning))*100) %>%
    filter(år == slut_ar)
  
  chart_df <- chart_df %>% 
    arrange(forandr) %>% 
    #mutate(sort = row_number()+(syss/sum(syss)+5)) %>% 
    mutate(sort = row_number()) %>% 
    relocate(sort, .before = 1) %>% 
    arrange(desc(forandr))
  
  # # justera tillbaka till rätt siffra nu när förändringen är beräknad
  # chart_df$syss[chart_df$år == slut_ar & chart_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske"] <-
  #   chart_df$syss[chart_df$år == slut_ar & chart_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske"] + jordskog_korr
  
  if (skapa_excelfil) {
    filnamn_excel <- paste0("forandr_syss_branscher_", geo_namn, "_", start_ar, "-", slut_ar, ifelse(till_word, "_w_", ""),".xlsx")
    write.xlsx(chart_df, paste0(output_mapp, filnamn_excel))
  }
  
  # ========================= Skapa själva diagrammet ==============================================================
  
  # tilldela värden till variabler som används i diagrammet =========================
  if (till_word) lbl_stlk <- 4 else lbl_stlk <- 3
  if (till_word) y_lbl_stlk <- 13 else y_lbl_stlk <- 12 
  if (till_word) titel_stlk <- 25 else titel_stlk <- 20
  diagramtitel <- paste0("Förändring antal sysselsatta i ", geo_namn, " år ", start_ar, "-", slut_ar)
  
  # används för att skapa etikettformat
  etikett_format <- function(x){
    x <- format(x, big.mark = " ", scientific = FALSE)
    x <- paste0(x, " %")
    return(x)
  }
  
  diff_stodlinje <- 10
  
  # ändra ej detta - endast formler som bygger på diff_stodlinje
  y_min <- round(min(chart_df$forandr) - diff_stodlinje, -1)
  y_max <- round(max(chart_df$forandr) + diff_stodlinje, -1)
  
  # här skapas själva bubbeldiagrammet
  gg_obj <- chart_df %>% 
    ggplot(aes(x=sort, y=forandr, size=dagbefolkning, color=Bransch, label = Bransch)) +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_point(alpha = 0.7, position = position_dodge2(width = 1)) +
    geom_text_repel(size = lbl_stlk, 
                    color = "grey40",
                    nudge_y = -12,
                    direction = "x",
                    segment.size = 0.2,
                    segment.color = "grey50",
                    point.padding = 1.4) + #, aes(point.size = syss), max.overlaps = 25) +
    scale_size(range = c(5, 40)) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(linewidth = 0.8, colour = "lightgrey"),
          #panel.grid.minor.y = element_line(linewidth = 0.4, colour = "lightgrey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),        
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size = y_lbl_stlk),
          plot.title = if (diagramtitel_tabort) element_blank() else  element_text(hjust = 0.5, size = titel_stlk),
          plot.title.position = "plot",
          plot.caption = element_text(face = "italic",
                                      hjust = 0, 
                                      vjust = 0,
                                      size = 6),
          plot.caption.position = "plot") +
    labs(title = diagramtitel,
      caption = diagram_capt) +
    scale_color_manual(values = farger)+
    scale_y_continuous(labels = etikett_format,
                       minor_breaks = seq(y_min,y_max,5),
                       breaks = seq(y_min-10,y_max,10),
                       limits = c(y_min-10,y_max))
  
    if (skapa_fil){               # om man skickar med att man vill skriva en fil så skrivs den här
      bredd <- 13
      hojd <- 6

      filnamn_bubble <- paste0("forandr_syss_branscher_", geo_namn, "_", start_ar, "-", slut_ar, ifelse(till_word, "_w_", ""),".png")

      fullpath <- paste0(output_mapp, filnamn_bubble)
      ggsave(fullpath, width = bredd, height = hojd)
      
      

      # Lägg till logga till diagrammet =======================================
      if (!is.null(logga_path)){
        if (is.na(logga_path)) logga_path <- hamta_logga_path()
        add_logo(
          plot_path = paste0(output_mapp, filnamn_bubble), # url or local file for the plot
          logo_path = logga_path, # url or local file for the logo
          logo_position = "bottom right", # choose a corner
          # 'top left', 'top right', 'bottom left' or 'bottom right'
          logo_scale = 20,
          #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
          replace = TRUE)
      }
    }
  return(gg_obj)                   # vi returnerar ett ggplotobjekt
}

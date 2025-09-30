
karta_arbetspendling <- function(
    vald_kommun_kod = NA,                               # en eller flera kommuner kan skickas med, det blir en karta per kommun och pendlingstyp
    valt_ar = "9999",                                   # "9999" är senaste året
    vald_pendlingstyp = "alla",               # finns: "alla", "Bruttopendling", "Nettopendling", "Inpendling", "Utpendling", "alla" = alla fyra pendlingstyper
    grans_for_antal_pendlare = 10,                      # värden under detta filtreras bort för att få en vettig karta
    legend_intervaller = 4,                             # hur många intervaller vill vi ha i legenden
    linjefarg_positiva_tal = "darkblue",
    linjefarg_negativa_tal = "darkred",
    linjefarg_utpendling = "darkred",
    linjefarg_inpendling = "darkblue",
    linjebredd_min = 3,                                   # linjebredd på linjer för det minsta värdet 
    linjebredd_max = 25,                                  # linjebredd på linjer för det största värdet
    output_mapp = NA                   # "G:/Samhällsanalys/GIS/projekt/strukturbilder/pendling_flytt/"
) {
  
  # ===========================================================================================================
  #
  # Skript för att hämta pendlingsdata från RAMS, SCB, för valt år. Därefter ritas en karta ut för vald pendlingstyp
  # för valda kommuner. Skickar man med flera kommuner så skriver skriptet ut en html-fil för varje kommmun. Man kan
  # skicka med flera pendlingstyper också (brutto-, netto-, in- och utpendling), det blir en karta per pendlingstyp
  # och kommun som man skickar med. Varje karta blir en egen html-fil.
  # Man kan styra klassindelningar med parametrarna nedan. 
  # 
  # Parametrar som skickas med:
  # - Vald_kommun_kod                                             # bara kommuner kan skickas med i nuläget
  # - valt_ar                                                     # "9999" = senaste år. Det ska gå att skicka med fler år framöver men det går inte ännu
  # - output_mapp                                                      # hit skrivs html-filen/filerna som är resultatet av skriptet (dvs. en interaktiv leaflet-karta)
  #
  # Skapat av Peter Möller och Henrik Aldén i november 2023.
  # Senast uppdaterad: 30 okt 2024 av Peter Möller
  #
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         readxl,
         sf,
         leaflet, 
         htmltools,
         glue
         )
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_diagramfunktioner.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_pendling_rams_bas_scb.R")
  
  if (all(is.na(output_mapp))) {
    if (dir.exists(utskriftsmapp())) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  if (any(tolower(vald_pendlingstyp) == "alla")) vald_pendlingstyp <- c("Bruttopendling", "Nettopendling", "Inpendling", "Utpendling")
  
  regionnyckel <- hamtaregtab()                                    # regionnyckel för att koppla på regionnamn nedan
  
  pendling_uttag <- funktion_upprepa_forsok_om_fel(function() 
      hamta_pendling_rams_bas_scb(region_vekt = vald_kommun_kod,
                                                  tid_vekt = valt_ar)
  )
  
  pendling_df <- pendling_uttag %>% 
    filter(regionkod_bo != regionkod_arb,
           pendlare > 0)
  
  pendling_enkelriktad <- pendling_df %>% 
    mutate(pendl_id = ifelse(regionkod_bo < regionkod_arb, paste0(regionkod_bo, regionkod_arb), paste0(regionkod_arb, regionkod_bo))) %>% 
    group_by(år, pendl_id) %>% 
    summarise(pendlare = sum(pendlare, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(regionkod_bo = str_sub(pendl_id, 1, 4),
           regionkod_arb = str_sub(pendl_id, 5, 8)) %>% 
    left_join(regionnyckel %>% rename(bostadsregion = region), by = c("regionkod_bo" = "regionkod")) %>% 
    left_join(regionnyckel %>% rename(arbetsställeregion = region), by = c("regionkod_arb" = "regionkod"))
  
    
  # ============================ hämta och bearbeta kartlager =====================================
  
  kommuner_gis_sweref <- hamta_karta("kommun")
  kommuner_point_sweref <- suppressWarnings(st_centroid(kommuner_gis_sweref))

  # ==================================== funktion för att skapa karta ========================================
  
  skapa_karta <- function(karta_kommunkod, skickad_pendlingstyp) {
  
  if (skickad_pendlingstyp == "Inpendling") {                 # om den valda pendlingstypen är inpendling
    vald_pendlingstyp_df <- pendling_df %>% 
      filter(regionkod_arb %in% karta_kommunkod)
    
  } else if (skickad_pendlingstyp == "Utpendling") {          # om den valda pendlingstypen är utpendling
    vald_pendlingstyp_df <- pendling_df %>% 
      filter(regionkod_bo %in% karta_kommunkod)
    
  } else if (skickad_pendlingstyp == "Nettopendling") {       # om den valda pendlingstypen är nettopendling
    vald_pendlingstyp_df <- pendling_df %>% 
      mutate(pendl_id = ifelse(regionkod_bo < regionkod_arb, paste0(regionkod_bo, regionkod_arb), paste0(regionkod_arb, regionkod_bo))) %>% 
      group_by(år, pendl_id) %>% 
      summarise(pendlare = sum(pendlare[!regionkod_bo %in% karta_kommunkod], na.rm = TRUE) - sum(pendlare[regionkod_bo %in% karta_kommunkod], na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(regionkod_bo = str_sub(pendl_id, 1, 4),
             regionkod_arb = str_sub(pendl_id, 5, 8)) %>% 
      left_join(regionnyckel %>% rename(bostadsregion = region), by = c("regionkod_bo" = "regionkod")) %>% 
      left_join(regionnyckel %>% rename(arbetsställeregion = region), by = c("regionkod_arb" = "regionkod"))
    
  } else if (skickad_pendlingstyp == "Bruttopendling") {
    vald_pendlingstyp_df <- pendling_enkelriktad
  }

  giskarta <- vald_pendlingstyp_df %>% 
    filter(pendlare >= grans_for_antal_pendlare |
             pendlare <= (grans_for_antal_pendlare*-1)) %>% 
    left_join(kommuner_point_sweref %>% select(knkod, start_geom = geometry), by = c("regionkod_bo" = "knkod")) %>% 
    left_join(kommuner_point_sweref %>% select(knkod, dest_geom = geometry), by = c("regionkod_arb" = "knkod")) %>% 
    st_as_sf()
  
  # skript för att skapa linjer mellan punkter 
  
  giskarta <- giskarta %>% 
    mutate(geom_ny = st_sfc(mapply(function(a,b){st_cast(st_union(a,b),"LINESTRING")}, start_geom, dest_geom, SIMPLIFY=FALSE), crs = 3006),
           geom_bage = geom_ny %>% st_simplify(dTolerance = 0.1)) %>% 
    select(-c(start_geom, dest_geom)) 
  
  st_geometry(giskarta) <- "geom_bage"
  giskarta <- giskarta %>% 
    select(-c(start_geom, geom_ny)) %>% 
    st_as_sf(crs = 3006) 
  
  # Transformera till EPSG:4326
  giskarta <- st_transform(giskarta, crs = 4326)
  kommuner_point <- st_transform(kommuner_point_sweref, crs = 4326)
  kommuner_gis <- st_transform(kommuner_gis_sweref, crs = 4326)
  # # Spatial join - append attributes from giskartan to kommuner_point
  # kom_point_giskarta <- st_join(kommuner_point, giskarta)

    giskarta_vald_kommun <- giskarta %>% 
      filter(regionkod_bo == karta_kommunkod | regionkod_arb == karta_kommunkod) %>% 
      mutate(relationskommun = ifelse(regionkod_bo == karta_kommunkod, arbetsställeregion, bostadsregion),
             relationskommun_kod = ifelse(regionkod_bo == karta_kommunkod, regionkod_arb, regionkod_bo),
             pendlare_txt = pendlare %>% as.character())          # för att kunna välja ut relationskommunen i popup-texten
    
    # välj ut rätt rader utifrån vilken typ av pendling vi valt
    if (skickad_pendlingstyp == "Inpendling") giskarta_vald_kommun <- giskarta_vald_kommun %>% filter(regionkod_arb == karta_kommunkod)     # Om det är inpendling behåller vi enbart de koder som sammanfaller med den valda kommun för arbetskommun
    if (skickad_pendlingstyp == "Utpendling") giskarta_vald_kommun <- giskarta_vald_kommun %>% filter(regionkod_bo == karta_kommunkod)
    
    kommunnamn <- hamtaregion_kod_namn(karta_kommunkod)$region             # för att döpa html-filen
    
    lanets_kommuner_gis <- kommuner_gis %>% filter(lanskod_tx %in% (str_sub(karta_kommunkod, 1, 2) %>% unique()))
    
    vald_kommun_gis <- kommuner_gis %>% 
      filter(knkod == karta_kommunkod) 
    
    lanets_kommuner_centroid <- suppressWarnings(st_centroid(lanets_kommuner_gis))

    kom_point <- kommuner_point %>% 
      filter(knkod %in% giskarta_vald_kommun$relationskommun_kod)
    
    # beräkna centroid för vald kommun, används för att zooma in kartan på rätt plats
    vald_kommun_centroid <- st_centroid(st_geometry(vald_kommun_gis))
    centroid_coords <- st_coordinates(vald_kommun_centroid) %>% as.numeric()       # vi extraherar latitude and longitude för att använda till att zooma kartan rätt
    
    # ================================ beräkningar för legenden ===============================
    # Skapa en funktion för att beräkna linjetjocklek baserat på ett kontinuerligt värde
    calculateWeightContinuous <- function(pends, min_width, max_width, min_pend, max_pend) {
      # Skala linjetjockleken linjärt mellan min_width och max_width
      min_pend <- abs(min_pend)
      max_pend <- abs(max_pend)
      if (min_pend>max_pend) {
        temp <- min_pend
        min_pend <- max_pend
        max_pend <- temp
      }
      if (min_pend > grans_for_antal_pendlare) min_pend <- grans_for_antal_pendlare
      scales::rescale(abs(pends), to = c(min_width, max_width), from = c(min_pend, max_pend))
    }
    
    # Antag att min_pend och max_pend är minsta och största värdet av pendlare
    min_pend <- min(giskarta_vald_kommun$pendlare) 
    max_pend <- max(giskarta_vald_kommun$pendlare) 
    # if (min_pend > max_pend) {
    #   temp <- min_pend
    #   min_pend <- max_pend
    #   max_pend <- temp
    # }
    
    # Sätt minsta och största önskade linjetjocklek
    min_width <- linjebredd_min                                  # bredd på minsta antal pendlare mätt i px
    max_width <- linjebredd_max                                 # bredd på största antal pendlare mätt i px
    
    # Beräkna linjetjocklekar för varje 'pend' värde
    calculated_weights <- calculateWeightContinuous(
      giskarta_vald_kommun$pendlare, min_width, max_width, min_pend, max_pend)
    
    # beräkna vettiga intervaller med antal pendlare utifrån datasetet
    pendlare_intervaller <- skapa_intervaller(giskarta_vald_kommun$pendlare, antal_intervaller = legend_intervaller)
    #if (pendlare_intervaller[1] < grans_for_antal_pendlare) pendlare_intervaller[1] <- grans_for_antal_pendlare
    # beräkna tjocklek 
    pendlare_bredd <- calculateWeightContinuous(pendlare_intervaller, min_width, max_width, min_pend, max_pend)
    
    
    # skapa funktion för att styra färg utifrån om det är ett positivt eller negativt tal
    farg_pos_neg_tal <- function(antal_pendlare) {
        retur_varde <- ifelse(antal_pendlare > 0, linjefarg_positiva_tal, linjefarg_negativa_tal)
        if (skickad_pendlingstyp == "Utpendling") retur_varde <- linjefarg_utpendling
        if (skickad_pendlingstyp == "Inpendling") retur_varde <- linjefarg_inpendling
        return(retur_varde)
    }
    
    
    # ========================= här skapas legenden som html-kod ============================
    
    # Skapa rader för legenden med map2
    legend_rows <- map2(pendlare_bredd, pendlare_intervaller, ~ div(
      style = "display: flex; align-items: center; margin-bottom: 5px; white-space: nowrap;",
      span(style = paste0("display: inline-block; width: 70px; height: ", .x, "px; background-color: ", farg_pos_neg_tal(.y), ";")),
      span(" ", .y, style = "margin-left: 5px;")
    ))

    # Skapa den fullständiga legenden
    legend_html <- div(
      id = "custom-legend",
      style = "background-color: rgba(255, 255, 255, 0.7); padding: 5px; padding-top: 0px; position: absolute; bottom: 50px; right: 10px;",
      h4(paste0(skickad_pendlingstyp, " år ", unique(giskarta_vald_kommun$år)), style = "margin-top: 0px; margin-bottom: 5px;"),
      legend_rows
    )


  
    # ======================== skapa beskrivning av kartan i nederkant =======================
    
    attrib_txt <- case_when(skickad_pendlingstyp == "Nettopendling" ~ paste0(", dvs. inpendlare minus utpendlare till och från ", kommunnamn, " och andra kommuner"),
                            skickad_pendlingstyp == "Bruttopendling" ~ paste0(", dvs. in- och utpendlare summerat, mellan ", kommunnamn, " och andra kommuner"),
                            skickad_pendlingstyp == "Inpendling" ~ paste0(" till ", kommunnamn, " från andra kommuner"),
                            skickad_pendlingstyp == "Utpendling" ~ paste0(" från ", kommunnamn, " till andra kommuner")) 
    
    attribution_html <-htmltools::tags$div(
        style = "background-color: rgba(220, 220, 220, 0.4); padding: 0px; font-size: 10px; border-radius: 1px;",
        htmltools::tags$p(paste0("Karta skapad av: Samhällsanalys, Region Dalarna. Kartan visar ", tolower(skickad_pendlingstyp) , attrib_txt, " där det finns minst ", grans_for_antal_pendlare, " pendlare totalt. Klicka på linjerna för att se antal pendlare."), style = "margin: 0;")
      )
    
    # text till popup-fönster
    
    popup_txt <- case_when(skickad_pendlingstyp == "Nettopendling" ~ "Inpendlare minus utpendlare (nettopendling) till och från {relationskommun}: {pendlare_txt}",
                           skickad_pendlingstyp == "Bruttopendling" ~ "Antal in- och utpendlare summerat till och från {relationskommun}: {pendlare_txt}",
                           skickad_pendlingstyp == "Inpendling" ~ "Antal inpendlare från {relationskommun}: {pendlare_txt}",
                           skickad_pendlingstyp == "Utpendling" ~ "Antal utpendlare till {relationskommun}: {pendlare_txt}")
                           
    # ===================================== beräkna zoom-nivå =================================================
    
    bbox_lan <- st_bbox(lanets_kommuner_gis)
    
    # Beräkna höjd och bredd
    hojd <- (bbox_lan$ymax - bbox_lan$ymin) %>% as.numeric()
    bredd <- (bbox_lan$xmax - bbox_lan$xmin) %>% as.numeric()
    
    avstand_zoom <- ifelse(hojd > bredd, hojd, bredd)
    
    
    calculate_zoom_level <- function(avstand) {
      a <- -1.1047  # Koefficient för log(avstand)
      b <- 9.0761   # Skärningspunkt
      
      zoom_level <- a * log(avstand) + b
      return(zoom_level)
    }
    
    # Beräkna zoom-nivå för avståndet som vi får från valt län 
    zoom_niva <- calculate_zoom_level(avstand_zoom)
    
    # =================================== skapa själva leaflet-kartan =======================================
    
    #Nu kan du använda 'calculated_weights' i din Leaflet-karta
    leaflet_map <- leaflet::leaflet() %>%
      #addProviderTiles("CartoDB.DarkMatter") %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      leaflet::addPolygons(data = lanets_kommuner_gis,
                           color = "white", weight = 0.5,
                           opacity = 0.5, 
                           fillColor = "grey98", fillOpacity = 0.1) %>% #,
                           #label = ~ knnamn) %>%
      leaflet::addPolygons(data = vald_kommun_gis,
                           color = "darkgrey", weight = 1)  %>%
      leaflet::setView(lng = centroid_coords[1], lat = centroid_coords[2], zoom = zoom_niva) %>%
      leaflet::addPolygons(data = kommuner_gis %>% filter(!lanskod_tx %in% (str_sub(karta_kommunkod, 1, 2))),                                               # samtliga 290 kommuner
                           color = "darkgrey", fillOpacity = 0,
                           fillColor = "darkgrey", weight = 0.3,
                           opacity = 0.2) %>%
      leaflet::addLabelOnlyMarkers(data = kom_point %>% filter(!lanskod_tx %in% (str_sub(karta_kommunkod, 1, 2))),
                                   label = ~knnamn,
                                   labelOptions = leaflet::labelOptions(noHide = TRUE, direction = 'top',
                                                                        textOnly = TRUE,
                                                                        style = list("color" = "darkgrey",
                                                                                     "opacity" = "0.2"))) %>%
      leaflet::addLabelOnlyMarkers(data = lanets_kommuner_centroid,
                                   label = ~knnamn,
                                   labelOptions = leaflet::labelOptions(noHide = TRUE, direction = 'top',
                                                                        textOnly = TRUE,
                                                                        style = list("color" = "#777777",
                                                                                     "opacity" = "0.2"))) %>%
      leaflet::addPolylines(data = giskarta_vald_kommun,
                            weight = ~calculated_weights,
                            #color = "darkred",
                            color = ~farg_pos_neg_tal(pendlare),
                            popup = ~glue(popup_txt)) %>% 
      addControl(html = as.character(legend_html), position = "bottomright", className = "fieldset {border: 0;}")  %>%
      addControl(html = as.character(attribution_html), position = "bottomleft", className = "fieldset {border: 0;}")
    
    # har inte fått detta att fungera - tanken är att denna kod tillsammans med refererade css-fil nedan ska flytta ner legenden mot nederkant
    leaflet_map <- htmltools::attachDependencies(leaflet_map, htmltools::htmlDependency(
      name = "custom-leaflet-style",
      version = "1.0",
      src = c(file = "G:/skript/mallar"),
      stylesheet = "leaflet_justering.css"
    ))
    
      # spara kartan som html-fil 
      #mapview::mapshot2(leaflet_map, paste0(output_mapp, tolower(skickad_pendlingstyp), "_leaflet_", kommunnamn, "_ar_", unique(giskarta_vald_kommun$år), ".html"))
      mapshot(leaflet_map, paste0(output_mapp, tolower(skickad_pendlingstyp), "_leaflet_", kommunnamn, "_ar_", unique(giskarta_vald_kommun$år), ".html"))
  } # slut funktion för att skapa karta
  
  kombinationer <- expand.grid(vald_kommun_kod, vald_pendlingstyp)
  pwalk(kombinationer, ~ skapa_karta(..1, ..2))
  
} # slut funktion


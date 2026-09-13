diag_forandr_branscher_bubblor <- function(vald_geografi ="20",                               # kan vara en eller flera geografier, skicka med vektor om flera, ex: c("20", "25")
                                           egetnamn_geografi = NA,                           # om man vill använda ett eget namn på geografin, ex. "Norra Mellansverige" för c("20", "17", "21")
                                           till_word = FALSE,                                 # om diagrammet ska användas i Word görs etiketter + diagram-caption större
                                           ta_bort_okand = TRUE,                             # sätt TRUE om okänd ska tas bort som branschindelning
                                           start_ar = NA,                                    # om NA väljs tidigaste möjliga år, väljs ett år som ligger före tillgängliga år så väljs istället tidigaste möjliga år
                                           slut_ar = NA,                                     # om NA väljs senaste möjliga år, väljs ett år som ligger efter tillgängliga år så väljs istället senaste möjliga år)
                                           nudge_y_varde = -12,                              # hur långt etiketterna hamnar från bubblorna, ska normalt vara oförändrade men om etiketterna hamnar tokigt kan man prova att ändra det värdet
                                           output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",
                                           diagram_capt = "Källa: SCB:s öppna statistikdatabas, Bearbetning: Samhällsanalys, Region Dalarna\nOBS! Från och med år 2019 samlas statistiken in på ett annat sätt och är därmed inte fullt jämförbar med tidigare år",                   # \nJord- och skogsbruk definierades om 2011 och därför har förändringen mellan år 2010 och 2011 tagits bort ur beräkningen",         # NULL för att ta bort diagram-caption
                                           skapa_fil = TRUE,
                                           diagramtitel_tabort = FALSE,
                                           skapa_excelfil = FALSE,
                                           returnera_dataframe_global_environment = FALSE,
                                           logga_path = NA                                 # ändra till NULL för att köra utan logga
                                           ) {

  # Lagt till en parameter som möjliggör att returnera data till global enviroment. Jon, 2025-01-21
  #
  # Migrerad till pxweb2r/rddiagram/rdverktyg. hamta_data-repots
  # hamta_bas_rams_region_sni2007_dagbef_kon_tid_fodelseregion_scb() slog ihop tre v1-tabeller. Alla tre
  # finns kvar i SCB:s v1-API men motsvaras nu av tre v2-tabeller:
  #   AM0207/AM0207Z/DagSni07KonKN (RAMS, ny tidsserie 2019-2021) -> TAB5837
  #   AM0207/AM0207K/DagSNI07KonK  (RAMS 2008-2018)               -> TAB900
  #   AM0210/AM0210B/ArbStDoNArNN  (BAS, preliminär, 2020-)       -> TAB3785
  # hamta_bas_rams_region_sni2007_dagbef_kon_tid_fodelseregion_scb() används bara av det här skriptet -
  # logiken läggs därför in direkt här i stället för i rdverktyg.
  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget p_load()/library(). Anropas
  # med fullt namespace (dplyr::filter() osv.) i stället.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr/purrr följer med som beroenden till rddiagram/rdverktyg.
  options(dplyr.summarise.inform = FALSE)

  # hämta branschnyckel från Region Dalarnas github-repo depot
  url_xlsx <- "https://raw.githubusercontent.com/Region-Dalarna/depot/main/Bransch_Gxx_farger.xlsx"
  temp_xlsx <- tempfile(fileext = ".xlsx")
  download.file(url_xlsx, temp_xlsx, mode = "wb", quiet = TRUE)
  branschtabell <- readxl::read_xlsx(temp_xlsx)
  unlink(temp_xlsx)

  # OBS: originalet satte här en variabel med stort "V" (Valda_ar) i det här fallet, men läste sedan
  # alltid det gement stavade valda_ar längre ner - vilket kraschade varje gång man skickade med både
  # start_ar och slut_ar (utan att bara låta någotdera vara NA). Skrivet om till en sammanhållen
  # if/else if/else så att valda_ar alltid sätts, oavsett vilka av de tre fallen som gäller.
  if (is.na(start_ar) | is.na(slut_ar)) {
    valda_ar <- '*'
  } else if (start_ar < 2011 & slut_ar > 2010) {
    valda_ar <- unique(c(start_ar, slut_ar, 2010, 2011))
  } else {
    valda_ar <- unique(c(start_ar, slut_ar))
  }

  if (is.na(egetnamn_geografi)){
    geo_df <- rdverktyg::hamtaregion_kod_namn(vald_geografi)
    if(nrow(geo_df) > 1) {                                 # om det finns fler än en geografi i skickad vektor
      geo_kortnamn <- rdverktyg::skapa_kortnamn_lan(geo_df$region)
      geo_namn <- rdverktyg::list_komma_och(geo_kortnamn)
      } else geo_namn <- rdverktyg::skapa_kortnamn_lan(geo_df$region)

  } else geo_namn <- egetnamn_geografi

  # =============================================== API-uttag ===============================================

  hamta_en_dagbef_sni_tabell <- function(tabell_id, region_vekt, tid_koder, har_totalt_kon) {

    giltiga_ar <- pxweb2r::pxweb2_get_values(tabell_id, "Tid")$code
    akt_ar_vekt <- if (identical(tid_koder, "*")) giltiga_ar else as.character(tid_koder)[as.character(tid_koder) %in% giltiga_ar]

    # RAMS ny tidsserie (2019-2021) och BAS (2020-) delar åren 2020-2021 - dessa tas alltid bort ur
    # RAMS-tabellen för att inte räknas dubbelt (samma hantering som i originalskriptet).
    if (tabell_id == "TAB5837") akt_ar_vekt <- akt_ar_vekt[!akt_ar_vekt %in% c("2020", "2021")]
    if (length(akt_ar_vekt) == 0) return(NULL)

    if (har_totalt_kon) {
      # BAS-tabellen (TAB3785) har en riktig "totalt"-kod för Kon, samt två ContentsCode-värden
      # (dagbefolkning/nattbefolkning) - vi vill bara ha dagbefolkning (den vars etikett inte
      # innehåller "bostad", samma urval som i originalskriptet).
      cont_df <- pxweb2r::pxweb2_get_values(tabell_id, "ContentsCode")
      cont_kod <- cont_df$code[!grepl("bostad", cont_df$label)]

      px <- pxweb2r::pxweb2_get_data(
        table = tabell_id,
        query = list(Region = region_vekt, SNI2007 = "*", Kon = "totalt",
                     Fodelseregion = "totalt", ContentsCode = cont_kod, Tid = akt_ar_vekt),
        on_all_values_invalid = "null")
      if (is.null(px)) return(NULL)

      px <- px |>
        dplyr::rename(regionkod = region_kod, branschkod = `näringsgren sni 2007_kod`, dagbefolkning = value) |>
        dplyr::select(-tabellinnehåll, -kön, -födelseregion)
    } else {
      # RAMS-tabellerna saknar en riktig "totalt"-kod för Kon (bara "män"/"kvinnor") - hämta båda och
      # summera själva (pxweb2r ger annars enskilda värden, ingen automatisk summering, om Kon utelämnas).
      px <- pxweb2r::pxweb2_get_data(
        table = tabell_id,
        query = list(Region = region_vekt, SNI2007 = "*", Kon = "*", ContentsCode = "*", Tid = akt_ar_vekt),
        on_all_values_invalid = "null")
      if (is.null(px)) return(NULL)

      px <- px |>
        dplyr::rename(regionkod = region_kod, branschkod = `näringsgren sni 2007_kod`, dagbefolkning = value) |>
        dplyr::group_by(dplyr::across(-c(kön, dagbefolkning))) |>
        dplyr::summarise(dagbefolkning = sum(dagbefolkning, na.rm = TRUE), .groups = "drop")
    }

    # korrigera branschkod så att det blir samma i alla år/tabeller ("00" i RAMS och "US" i BAS betyder
    # båda "uppgift saknas" - och BAS har en extra totalsummeringsrad, "A-U+US", som filtreras bort)
    px |>
      dplyr::filter(branschkod != "A-U+US") |>
      dplyr::mutate(branschkod = ifelse(branschkod == "00", "US", branschkod),
             `näringsgren SNI 2007` = dplyr::case_when(branschkod == "US" ~ "uppgift saknas",
                                 branschkod == "A" ~ "jordbruk, skogsbruk och fiske",
                                 branschkod == "B+C" ~ "tillverkning och utvinning",
                                 branschkod == "D+E" ~ "energi och miljö",
                                 branschkod == "F" ~ "bygg",
                                 branschkod == "G" ~ "handel",
                                 branschkod == "H" ~ "transport och magasinering",
                                 branschkod == "I" ~ "hotell och restaurang",
                                 branschkod == "J" ~ "information och kommunikation",
                                 branschkod == "K" ~ "finans och försäkring",
                                 branschkod == "L" ~ "fastighet",
                                 branschkod == "M+N" ~ "företagstjänster",
                                 branschkod == "O" ~ "offentlig förvaltning och försvar",
                                 branschkod == "P" ~ "utbildning",
                                 branschkod == "Q" ~ "vård och omsorg",
                                 branschkod == "R+S+T+U" ~ "kultur, fritid och nöje"))
  }

  hamta_bas_rams_dagbef_sni <- function(region_vekt, tid_koder = "*") {
    tabell_id_vekt <- c("TAB5837", "TAB900", "TAB3785")
    har_totalt_kon <- c(FALSE, FALSE, TRUE)

    purrr::map2(tabell_id_vekt, har_totalt_kon, ~ hamta_en_dagbef_sni_tabell(.x, region_vekt, tid_koder, .y)) |>
      purrr::list_rbind()
  }

  px_df <- hamta_bas_rams_dagbef_sni(region_vekt = vald_geografi, tid_koder = valda_ar)

  if (is.na(start_ar)) start_ar <- min(px_df$år)       # om inte start_ar är valt, ta det tidigaste året i tidsserien
  if (is.na(slut_ar)) slut_ar <- max(px_df$år)         # om inte slut_ar är valt, ta det senaste året i tidsserien

  chart_df <- px_df |>
    dplyr::filter(år == start_ar | år == slut_ar) |>
    dplyr::group_by(år, branschkod, `näringsgren SNI 2007`) |>
    dplyr::summarise(dagbefolkning = sum(dagbefolkning, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(branschkod)   # sortera i SNI-kodsordning så att färgerna blir rätt i nästa steg

  # koppla på texter från branschtabell
  chart_df <- chart_df |>
    dplyr::left_join(dplyr::select(branschtabell, Br15kod, Bransch), by = c("branschkod" = "Br15kod"))

  # sortera branschtabell så att det blir samma ordning som chart_df, och rätt med färger från branschtabell
  branschtabell <- branschtabell |>
    dplyr::arrange(Br15kod)

  # fixa färger, hämta värden från branschtabell
  farger <- branschtabell$HexCode
  names(farger) <- branschtabell$Bransch
  if (ta_bort_okand) farger <- farger[!grepl("okänd", names(farger))]             # om användaren valt att ta bort okänd så tas den bort även ur färgvektorn

  # # justering för omklassificering av jord- och skogsbruk
  # chart_df$syss[chart_df$år == slut_ar & chart_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske"] <-
  #   chart_df$syss[chart_df$år == slut_ar & chart_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske"] - jordskog_korr

  if (ta_bort_okand) chart_df <- dplyr::filter(chart_df, `näringsgren SNI 2007` != "uppgift saknas")         # om användaren valt att ta bort okänd så tas den bort ur datasetet som diagrammet tillverkas med

  chart_df <- chart_df |>
    dplyr::arrange(`näringsgren SNI 2007`, år) |>
    dplyr::ungroup() |>
    dplyr::mutate(forandr = ((dagbefolkning - dplyr::lag(dagbefolkning))/dplyr::lag(dagbefolkning))*100) |>
    dplyr::filter(år == slut_ar)

  chart_df <- chart_df |>
    dplyr::arrange(forandr) |>
    #mutate(sort = row_number()+(syss/sum(syss)+5)) %>%
    dplyr::mutate(sort = dplyr::row_number()) |>
    dplyr::relocate(sort, .before = 1) |>
    dplyr::arrange(dplyr::desc(forandr))

  # # justera tillbaka till rätt siffra nu när förändringen är beräknad
  # chart_df$syss[chart_df$år == slut_ar & chart_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske"] <-
  #   chart_df$syss[chart_df$år == slut_ar & chart_df$`näringsgren SNI 2007` == "jordbruk, skogsbruk och fiske"] + jordskog_korr

  if (skapa_excelfil) {
    filnamn_excel <- paste0("forandr_syss_branscher_", geo_namn, "_", start_ar, "-", slut_ar, ifelse(till_word, "_w_", ""),".xlsx")
    openxlsx::write.xlsx(chart_df, paste0(output_mapp, filnamn_excel))
  }

  if(returnera_dataframe_global_environment == TRUE){
    assign("forandring_syss_branscher_df", chart_df, envir = .GlobalEnv)
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
  gg_obj <- chart_df |>
    ggplot2::ggplot(ggplot2::aes(x=sort, y=forandr, size=dagbefolkning, color=Bransch, label = Bransch)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 1) +
    ggplot2::geom_point(alpha = 0.7, position = ggplot2::position_dodge2(width = 1)) +
    ggrepel::geom_text_repel(size = lbl_stlk,
                    color = "grey40",
                    nudge_y = -12,
                    #direction = "x",
                    direction = "both",
                    segment.size = 0.2,
                    segment.color = "grey50",
                    point.padding = 1.4) + #, aes(point.size = syss), max.overlaps = 25) +
    ggplot2::scale_size(range = c(5, 40)) +
    ggplot2::theme(legend.position = "none",
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.grid.major.y = ggplot2::element_line(linewidth = 0.8, colour = "lightgrey"),
          #panel.grid.minor.y = element_line(linewidth = 0.4, colour = "lightgrey"),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = y_lbl_stlk),
          plot.title = if (diagramtitel_tabort) ggplot2::element_blank() else  ggplot2::element_text(hjust = 0.5, size = titel_stlk),
          plot.title.position = "plot",
          plot.caption = ggplot2::element_text(face = "italic",
                                      hjust = 0,
                                      vjust = 0,
                                      size = 6),
          plot.caption.position = "plot") +
    ggplot2::labs(title = diagramtitel,
      caption = diagram_capt) +
    ggplot2::scale_color_manual(values = farger)+
    ggplot2::scale_y_continuous(labels = etikett_format,
                       minor_breaks = seq(y_min,y_max,5),
                       breaks = seq(y_min-10,y_max,10),
                       limits = c(y_min-10,y_max))

    if (skapa_fil){               # om man skickar med att man vill skriva en fil så skrivs den här
      bredd <- 13
      hojd <- 6

      filnamn_bubble <- paste0("forandr_syss_branscher_", geo_namn, "_", start_ar, "-", slut_ar, ifelse(till_word, "_w_", ""),".png")

      fullpath <- paste0(output_mapp, filnamn_bubble)
      ggplot2::ggsave(fullpath, plot = gg_obj, width = bredd, height = hojd)



      # Lägg till logga till diagrammet =======================================
      if (!is.null(logga_path)){
        if (is.na(logga_path)) logga_path <- rddiagram::hamta_logga_path()
        rddiagram::add_logo(
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

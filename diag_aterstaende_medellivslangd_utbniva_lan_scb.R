diag_aterstaende_medellivslangd_utbniva_lan_scb <- function(
    region_vekt = "20",
    ar_vekt = "*",                                      # "*" = alla år, "9999" = senaste år
    cont_var_klartext = "Antal återstående år",        # Är främst den relevanta variabeln för oss att använda i denna tabell
    vald_alder = "30 år",                                 # ålder för återstående medellivslängd, finns: #  Finns: "30 år", "31 år", "32 år" ... tom  "93 år", "94 år", "95+ år"
    diag_fargvektor = NA,                               # valbar färgvektor för diagrammet
    visa_logga_i_diagram = TRUE,                        # TRUE om logga ska visas i diagrammet, FALSE om logga inte ska visas i diagrammet
    logga_sokvag = NA,                                 # sökväg till logga som ska visas i diagrammet
    diagramtitel_tabort = FALSE,                     # TRUE om diagramtitel ska tas bort, FALSE om diagramtitel ska visas
    diagram_capt = "Källa: Demografisk analys - Befolkning, SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    x_axel_stlk = 10.5,                              # storlek på x-axelns text
    y_axel_stlk = 12,                                # storlek på y-axelns text
    skriv_diagramfil = TRUE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
    ggobjektfilnamn_utan_tid = FALSE,    # om TRUE så tas inte tex året med i filnamnet, vilket passar bättre i vissa sammanhang när man vill använda objektsnamnet utan att ändra vid varje uppdatering
    excel_mapp = NA,                                   # mapp där excelfil ska sparas, NA = sparas ingen fil
    demo = FALSE,             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    utmapp = "G:/Samhällsanalys/API/Fran_R/utskrift/"
    ) {


  # =======================================================================================================================
  #
  # Hämta hem data med funktionen hamta_aterstaende_medellivslangd, skriv ut ett diagram. Defaultinställning är
  # återstående medellivslängd vid 30 års ålder för kvinnor respektive män i Dalarna. Det finns fler innehålls-
  # variabler, vilka primärt används för att beräkna dödsrisker och återstående medellivslängd.
  #
  # Länk till SCB-tabell där data hämtas: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0701/LivslUtbLan/
  #
  # =======================================================================================================================

  # Bara paket, ingen source() mot funktioner-/hamta_data-reporna och inget
  # p_load(tidyverse). Anropas med fullt namespace (dplyr::filter() osv.) i
  # stället för library().
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  # dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg.

  # om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
  # demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
  if (demo){
    demo_url <-
      c("https://region-dalarna.github.io/utskrivna_diagram/medellivslangd_aterstaende_vid_30 år_alder_Dalarna_ar2012-2016_2019-2023.png")
    purrr::walk(demo_url, ~browseURL(.x))
    if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
    rdverktyg::stop_tyst()
  }

  diag_fargvektor <- if (all(is.na(diag_fargvektor))) rddiagram::diagramfarger("rus_sex")[c(5,1,3)] else diag_fargvektor

  gg_list <- list()

  # Länk till tabell: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BE__BE0701/LivslUtbLan5/
  medellivslangd_df <- pxweb2r::pxweb2_get_data(
    table = "TAB4879",
    query = list(
      Region = region_vekt,
      UtbildningsNiva = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning"),
      Kon = c("män", "kvinnor"),
      Alder = vald_alder,
      ContentsCode = cont_var_klartext,
      Tid = ar_vekt
    ))

  new_name <- unique(medellivslangd_df$tabellinnehåll)
  medellivslangd_df <- medellivslangd_df |>
    dplyr::rename(regionkod = region_kod, !!new_name := value) |>
    dplyr::select(-tabellinnehåll)

  # om regioner är alla kommuner i ett län eller alla län i Sverige görs revidering, annars inte
  region_start <- rdverktyg::list_komma_och(rdverktyg::skapa_kortnamn_lan(unique(medellivslangd_df$region), byt_ut_riket_mot_sverige = TRUE))
  region_txt <- rdverktyg::ar_alla_kommuner_i_ett_lan(unique(medellivslangd_df$regionkod), returnera_text = TRUE, returtext = region_start)
  region_txt <- rdverktyg::ar_alla_lan_i_sverige(unique(medellivslangd_df$regionkod), returnera_text = TRUE, returtext = region_txt)
  regionfil_txt <- region_txt
  region_txt <- paste0(" i ", region_txt)
  regionkod_txt <- if (region_start == region_txt) paste0(unique(medellivslangd_df$regionkod), collapse = "_") else region_txt

  diagramtitel <- glue::glue("Återstående medellivslängd vid {unique(medellivslangd_df$ålder)} års ålder{region_txt}")
  diagramfil <- stringr::str_replace_all(glue::glue("medellivslangd_aterstaende_vid_{unique(medellivslangd_df$ålder)}_alder_{regionfil_txt}_ar{min(medellivslangd_df$årsintervall)}_{max(medellivslangd_df$årsintervall)}.png"), "__", "_")

  chart_df <- medellivslangd_df |>
    dplyr::mutate(utbildningsnivå = factor(utbildningsnivå, levels = c("förgymnasial utbildning", "gymnasial utbildning", "eftergymnasial utbildning")))

  gg_obj <- rddiagram::SkapaLinjeDiagram(
    skickad_df = chart_df,
    skickad_x_var = "årsintervall",
    skickad_y_var = cont_var_klartext,
    skickad_x_grupp = "utbildningsnivå",
    diagram_titel = if(diagramtitel_tabort) NULL else diagramtitel,
    diagram_capt = diagram_capt,
    y_axis_borjar_pa_noll = FALSE,
    filnamn_diagram = diagramfil,
    manual_x_axis_title = "årsintervall",
    manual_color = diag_fargvektor,
    lagg_pa_logga = visa_logga_i_diagram,
    facet_x_axis_storlek = x_axel_stlk,
    facet_y_axis_storlek = y_axel_stlk,
    logga_path = logga_sokvag,
    output_mapp = utmapp,
    facet_grp = "kön",
    facet_scale = "fixed",
    facet_legend_bottom = TRUE,
    skriv_till_diagramfil = skriv_diagramfil
  )

  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.[^.]+$")

  # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
  if (ggobjektfilnamn_utan_tid) {
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(names(gg_list)[[length(gg_list)]], "_ar.*$")
  }

  return(gg_list)

}

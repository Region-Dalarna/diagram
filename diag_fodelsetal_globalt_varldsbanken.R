# Diagramskript för att skriva ut födelsetal som hämtas från Världsbanken och landskoder från Wikipedia
# Man kan styra diagrambeskrivning, färgvektor (måste vara minst 8 färger), om det ska skrivas en
# bildfil och vart diagrammet i så fall ska sparas.
# Man kan spara en dataframe till global environment om man vill (kan vara bra ibland när man skapar RMarkdown-
# rapporter).
# Funktionen returnerar alltid ett ggplot-objekt.

diag_fodelsetal_globalt_varldsbanken <- function(
    diagram_capt = "Källa: Världsbanken\nBearbetning: Samhällsanalys, Region Dalarna",
    output_mapp = NA,
    diag_fargvekt = NA,
    returnera_dataframe_global_environment = FALSE,
    skriv_bildfil = TRUE,
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {

# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <-
c("https://region-dalarna.github.io/utskrivna_diagram/fodelsetal_globalt_ar_ar2023.png")
  purrr::walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  rdverktyg::stop_tyst()
}

  # Bara paket, ingen source() mot funktioner-repot och inget library().
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället.
  # Ingen SCB-hämtning här - data laddas ned direkt från Världsbankens
  # publika API, en landskodsnyckel i Region Dalarnas depot-repo samt
  # ISO-landskoder från Wikipedia.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
  # dplyr/purrr/stringr/httr följer med som beroenden till rddiagram/rdverktyg.

  # om ingen färgvektor är medskickad, använd rddiagram::diagramfarger("rd_primar_atta")
  if (all(is.na(diag_fargvekt))) {
    diag_fargvekt <- rddiagram::diagramfarger("rd_primar_atta")[c(1,2,3,4,5,6,8,7)]
  }

  if (skriv_bildfil) {
    if (all(is.na(output_mapp))) {
      if (dir.exists(rdverktyg::utskriftsmapp())) {
        output_mapp <- rdverktyg::utskriftsmapp()
      } else {
        stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
      }
    }
  }

  # hämta nyckel för länder och världsdelar
  url_nyckelfil <- "https://raw.githubusercontent.com/Region-Dalarna/depot/main/landskoder_varldsdelar_nyckel.csv"
  tempfile_nyckel <- tempfile(fileext = ".csv")
  httr::GET(url_nyckelfil, httr::write_disk(tempfile_nyckel, overwrite = TRUE))
  nyckel_df <- data.table::fread(tempfile_nyckel) |>
    dplyr::select(Landskod, varldsdel)

  # hämta nyckel för länder med 2- respektive 3-siffrig landskod
  iso_lander_url <- "https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes"

  # Läs in HTML-sidan
  webbsida <- rvest::read_html(iso_lander_url)

  # Extrahera alla tabeller på sidan
  tabeller <- rvest::html_table(webbsida, fill = TRUE)

  # Välj rätt tabell (den första som innehåller länder och ISO-koder)
  iso_tabell <- tabeller[[1]][2:nrow(tabeller[[1]]),]

  # names(iso_tabell) <- c("Land", "Officiellt namn", "Självständighet",
  #                       "id_A2", "id_A3", "id_num", "subdiv", "tld")

  names(iso_tabell) <- c("Land", "Självständighet",
                         "id_A2", "id_A3", "id_num", "subdiv", "tld")

  iso_tabell <- iso_tabell |>
    dplyr::mutate(id_A2 = stringr::str_remove(id_A2, "^.*?\\}"))

  # koppla på nyckel med länder och världsdelar så att vi får både 2 - och 3-siffriga koder
  nyckel_join <- nyckel_df |>
    dplyr::left_join(iso_tabell, by = c("Landskod" = "id_A2"))

  # Skapa en temporär fil
  temp_file <- tempfile(fileext = ".xls")

  # Hämta filen och spara den
  httr::GET("https://api.worldbank.org/v2/en/indicator/SP.DYN.TFRT.IN?downloadformat=excel",
      httr::write_disk(temp_file, overwrite = TRUE))

  # Läs in Excel-filen
  df <- readxl::read_excel(temp_file, sheet = 1, skip = 3)

  df_long <- df |>
    tidyr::pivot_longer(
      cols = dplyr::matches("^\\d{4}$"), # Välj alla kolumner som är årtal
      names_to = "Year",
      values_to = "Fertility Rate"
    )


  diagram_df <- df_long |>
    dplyr::filter(!is.na(`Fertility Rate`)) |>
    dplyr::filter(Year == max(Year)) |>
    dplyr::mutate(fokus = dplyr::case_when(`Country Name` == "Sweden" ~ 2,
                             `Country Name` == "European Union" ~ 1,
                             TRUE ~ 0)) |>
    dplyr::relocate(`Fertility Rate`, .after = `Country Name`) |>
    dplyr::left_join(nyckel_join, by = c("Country Code" = "id_A3")) |>
    dplyr::filter(!is.na(varldsdel)) |>
    dplyr::mutate(varldsdel = ifelse(`Country Name` == "Sweden", "Sverige", varldsdel),
           varldsdel = factor(varldsdel, levels = c(unique(varldsdel[varldsdel != "Sverige"]), "Sverige")))

  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("fodelsetal_globalt_varldsbanken_df", diagram_df, envir = .GlobalEnv)
  }

  diagramtitel <- glue::glue("Födelsetal i världens länder år {unique(diagram_df$Year)}")
  diagramfil <- stringr::str_replace_all(glue::glue("fodelsetal_globalt_ar_ar{unique(diagram_df$Year)}.png"), "__", "_")

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = diagram_df,
    skickad_x_var = "Country Name",
    skickad_y_var = "Fertility Rate",
    skickad_x_grupp = "varldsdel",
    x_axis_sort_value = TRUE,
    #x_var_fokus = "fokus",
    y_axis_storlek = 3,
    diagram_titel = diagramtitel,
    diagram_capt = diagram_capt,
    diagram_liggande = TRUE,
    stodlinjer_avrunda_fem = TRUE,
    filnamn_diagram = diagramfil,
    dataetiketter = FALSE,
    manual_y_axis_title = "antal födda barn per kvinna",
    x_axis_lutning = 0,
    fokusera_varden = list(list(geom = "rect", ymin=2.09, ymax=2.11, xmin=0, xmax=Inf, alpha=1, fill="grey20")),
    # manual_x_axis_text_vjust = 1,
    # manual_x_axis_text_hjust = 1,
    manual_color = diag_fargvekt,
    skriv_till_diagramfil = skriv_bildfil,
    diagramfil_hojd = 10,
    output_mapp = output_mapp
  )

  gg_list <- list(gg_obj)
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.[^.]+$")
  return(gg_list)

} # slut funktion

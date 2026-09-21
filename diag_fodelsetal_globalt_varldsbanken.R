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
  # publika API. Landskoder, svenska landsnamn och världsdelsindelning
  # kommer från paketet countrycode (ersätter tidigare Wikipedia-
  # skrapning som blockerades av nätverkspolicy, samt den tidigare
  # egna nyckelfilen för världsdelar - vi använder nu countrycodes
  # egen continent/region23-klassning rakt av, utan egna undantag).
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
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
  
  # Bygg landstabell med 2- och 3-bokstavskoder, svenskt namn och världsdel.
  # Filtrerar bort historiska/icke-länder (saknar iso3n, t.ex. Tjeckoslovakien,
  # Kosovo, Jugoslavien m.fl.) och delar upp Amerika i Nord- och Sydamerika
  # via region23, samt bryter ut Ryssland som egen kategori.
  iso_tabell <- countrycode::codelist |>
    dplyr::filter(!is.na(iso3n)) |>
    dplyr::transmute(
      # Namibias iso2c-kod är bokstavligen "NA", vilket annars tolkas som
      # ett saknat värde och gör att landet tappas. Sätts tillbaka manuellt.
      id_A2 = dplyr::if_else(country.name.en == "Namibia" & is.na(iso2c), "NA", iso2c),
      id_A3 = iso3c,
      Land = cldr.short.sv,
      varldsdel = dplyr::case_when(
        iso3c == "RUS" ~ "Ryssland",
        continent == "Antarctica" ~ "Antarktis",
        continent == "Americas" & region23 == "South America" ~ "Sydamerika",
        continent == "Americas" ~ "Nordamerika",
        continent == "Africa" ~ "Afrika",
        continent == "Asia" ~ "Asien",
        continent == "Europe" ~ "Europa",
        continent == "Oceania" ~ "Oceanien",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(id_A2), !is.na(id_A3))
  
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
    dplyr::left_join(iso_tabell, by = c("Country Code" = "id_A3")) |>
    dplyr::filter(!is.na(varldsdel)) |>
    dplyr::mutate(fokus = dplyr::case_when(`Country Name` == "Sweden" ~ 2,
                                           `Country Name` == "European Union" ~ 1,
                                           TRUE ~ 0)) |>
    dplyr::relocate(`Fertility Rate`, .after = `Country Name`) |>
    dplyr::mutate(Land = ifelse(`Country Name` == "Sweden", "Sverige", Land),
                  varldsdel = ifelse(`Country Name` == "Sweden", "Sverige", varldsdel),
                  varldsdel = factor(varldsdel, levels = c(unique(varldsdel[varldsdel != "Sverige"]), "Sverige")))
  
  # returnera datasetet till global environment, bl.a. bra när man skapar Rmarkdown-rapporter
  if(returnera_dataframe_global_environment == TRUE){
    assign("fodelsetal_globalt_varldsbanken_df", diagram_df, envir = .GlobalEnv)
  }
  
  diagramtitel <- glue::glue("Födelsetal i världens länder år {unique(diagram_df$Year)}")
  diagramfil <- stringr::str_replace_all(glue::glue("fodelsetal_globalt_ar_ar{unique(diagram_df$Year)}.png"), "__", "_")
  
  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = diagram_df,
    skickad_x_var = "Land",
    skickad_y_var = "Fertility Rate",
    skickad_x_grupp = "varldsdel",
    x_axis_sort_value = TRUE,
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
    manual_color = diag_fargvekt,
    skriv_till_diagramfil = skriv_bildfil,
    diagramfil_hojd = 10,
    output_mapp = output_mapp
  )
  
  gg_list <- list(gg_obj)
  names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfil, "\\.[^.]+$")
  return(gg_list)
  
} # slut funktion

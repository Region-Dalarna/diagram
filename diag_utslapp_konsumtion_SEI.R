diagram_konsumtionsutslapp <- function(region_vekt = rdverktyg::hamtaAllaLan(), # Finns län, riket och kommuner. "*" ger alla.
                                       jmf = c("00","20"), # Vilka län vill man göra en jämförelse med i tidsserieskriptet. Måste vara en delmängd av region_vekt.
                                       lan_kommun = "20", # För vilket län skall kommuer jämföras.
                                       fokus_region = "20", # Vilken region skall man fokusera på i jämförande diagram mellan län (förutom Sverige)
                                       diag_lan = TRUE, # Ger jämförelse mellan län för senaste år och en tidsserie där jämförelse görs enlig variabeln jmf ovan
                                       diag_kommun = TRUE, # Ger jämförelse på senaste år för kommuner i valt län
                                       output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                       returnera_data = FALSE, # Skall data returneras till gloval environment
                                       ggobjektfilnamn_utan_tid = TRUE, # Objektnamnet sparas utan tid i namnet. Fördelaktigt att använda i rapporter
                                       spara_figur = FALSE){ # Skall figuren sparas till valt output_mapp

  # ===========================================================================================================
  # Data från https://konsumtionskompassen.se/ (SEI) .
  # Tre diagram. Senaste år för län, tidsserie där län jämförs med Sverige och slutligen senaste år för kommun
  # Skapad av Jon 2026-09-03
  #
  # Migrerad bort från source()/p_load() mot funktioner-repot (fullt namespace, ingen library()). Datakällan
  # (G:/skript/jon/Webbskrapning/hamta_data_konsumtionskompassen.R) är ett lokalt webbskrapningsskript, inte
  # en del av funktioner-/hamta_data-reporna på GitHub - den sourcen lämnas därför oförändrad, precis som
  # motsvarande lokala källa i diagram_gymnasiantagning_antal_kon.R.
  # ===========================================================================================================

  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("rdverktyg", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
  }
  if (!requireNamespace("rKolada", quietly = TRUE)) install.packages("rKolada")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  # dplyr/stringr följer med som beroenden till rddiagram/rdverktyg.

  gg_list <- list()

  source("G:/skript/jon/Webbskrapning/hamta_data_konsumtionskompassen.R")

  if(diag_lan == TRUE){

    konsumtionskompassen_df <- get_kompassen_data(region = region_vekt) |>
      dplyr::mutate(region_namn = rdverktyg::skapa_kortnamn_lan(region_namn)) |>
      dplyr::rename(varde = kg_co2e_per_capita)

    if(returnera_data == TRUE){
      assign("konsumtionskompassen_lan_df", konsumtionskompassen_df, envir = .GlobalEnv)
    }

    ValdGeografi <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(fokus_region)$region, byt_ut_riket_mot_sverige = TRUE)

    # Jämför län för senaste år
    diagram_titel <- paste0("Hushållens totala utsläpp år ",max(konsumtionskompassen_df$year))
    diagramfilnamn <- glue::glue("kons_utslapp_lan_ar_{max(konsumtionskompassen_df$year)}.png")
    diagram_capt = "Källa: https://www.konsumtionskompassen.se (Stockholm Environment Institute (SEI),2025)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: kg koldioxidekvivalenter per person. För en mer ingående förklaring av hur data har beräknats,\nse https://www.konsumtionskompassen.se"

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::mutate(
                                   dplyr::filter(konsumtionskompassen_df, year == max(year)),
                                   fokus = ifelse(region_namn == ValdGeografi, 1, ifelse(region_namn == "Sverige", 2, 0))),
                                 skickad_x_var = "region_namn",
                                 skickad_y_var = "varde",
                                 diagram_titel = diagram_titel,
                                 x_var_fokus = "fokus",
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = "kg koldioxidekvivalenter per person",
                                 manual_color = rddiagram::diagramfarger("rus_tre_fokus"),
                                 skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }

    # Tidsserie där två eller flera regioner jämförs
    diagram_titel <- paste0("Förändring i hushållens totala utsläpp  ",min(konsumtionskompassen_df$year),"-",max(konsumtionskompassen_df$year))

    diagramfilnamn <- glue::glue("kons_utslapp_tid_ar_{min(konsumtionskompassen_df$year)}_{max(konsumtionskompassen_df$year)}.png")

    tidsserie_df <- konsumtionskompassen_df |>
      dplyr::filter(regionkod %in% jmf) |>
      dplyr::group_by(region_namn) |>
      dplyr::mutate(index = varde / varde[year == min(year)] * 100) |>
      dplyr::ungroup()

    gg_obj <- ggplot2::ggplot(tidsserie_df, ggplot2::aes(x = year, y = index, color = region_namn, group = region_namn)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_color_manual(values = rddiagram::diagramfarger("rus_sex")) +
      ggplot2::scale_x_continuous(
        breaks = unique(konsumtionskompassen_df$year),
        expand = ggplot2::expansion(mult = 0.01)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 120),
        breaks = seq(0, 120, 20),
        minor_breaks = seq(0, 120, 5),
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        title = diagram_titel,
        x = NULL,
        y = "Index (startvärde 100)",
        color = NULL,
        caption =   diagram_capt

      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
        legend.position = "bottom",
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic", size = 11),
        plot.caption.position = "plot",
        panel.grid.major = ggplot2::element_line(color = "grey60", linewidth = 0.4),
        panel.grid.minor.y = ggplot2::element_line(color = "grey85", linewidth = 0.3),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1),
        plot.margin = ggplot2::margin(t = 10, r = 15, b = 10, l = 10)
      )

    if (spara_figur){
      ggplot2::ggsave(filename = paste0(output_mapp, diagramfilnamn),
             plot = gg_obj,
             width = 12,
             height = 7,
             dpi = 300)
    }

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }

  }

  if(diag_kommun == TRUE){

    konsumtionskompassen_kommun_df <- get_kompassen_data(region = rdverktyg::hamtakommuner(lan_kommun)) |>
      dplyr::mutate(region_namn = rdverktyg::skapa_kortnamn_lan(region_namn)) |>
      dplyr::rename(varde = kg_co2e_per_capita)

    if(returnera_data == TRUE){
      assign("konsumtionskompassen_kommun_df", konsumtionskompassen_kommun_df, envir = .GlobalEnv)
    }

    ValdGeografi <- rdverktyg::skapa_kortnamn_lan(rdverktyg::hamtaregion_kod_namn(lan_kommun)$region, byt_ut_riket_mot_sverige = TRUE)

    # Jämför kommuner för senaste år
    diagram_titel <- paste0("Hushållens totala utsläpp år ",max(konsumtionskompassen_df$year))
    diagramfilnamn <- glue::glue("kons_utslapp_{ValdGeografi}_ar_{max(konsumtionskompassen_df$year)}.png")
    diagram_capt = "Källa: https://www.konsumtionskompassen.se (Stockholm Environment Institute (SEI),2025)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: kg koldioxidekvivalenter per person. För en mer ingående förklaring av hur data har beräknats,\nse https://www.konsumtionskompassen.se"

    gg_obj <- rddiagram::SkapaStapelDiagram(skickad_df = dplyr::mutate(
                                   dplyr::filter(konsumtionskompassen_kommun_df, year == max(year)),
                                   fokus = ifelse(region_namn == ValdGeografi, 1, ifelse(region_namn == "Sverige", 2, 0))),
                                 skickad_x_var = "region_namn",
                                 skickad_y_var = "varde",
                                 diagram_titel = diagram_titel,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 x_var_fokus = "fokus",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 x_axis_sort_value = TRUE,
                                 diagram_capt = diagram_capt,
                                 #stodlinjer_avrunda_fem = TRUE,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title = "kg koldioxidekvivalenter per person",
                                 manual_color = rddiagram::diagramfarger("rus_tre_fokus"),
                                 skriv_till_diagramfil = spara_figur)

    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- stringr::str_remove(diagramfilnamn, ".png")

    # ta bort tidsbestämning (tex. år) ur objektsnamnet, för användning i tex r-markdownrapporter
    if (ggobjektfilnamn_utan_tid) {
      names(gg_list)[[length(gg_list)]] <-  sub("_ar.*", "", diagramfilnamn)
    }

  }



  return(gg_list)

}

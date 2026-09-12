diagram_examen_hogskolan_NMS <- function(output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                  output_mapp_data = NA, # Här hamnar sparad data
                                  filnamn_data = "hogsoleexamen.xlsx",
                                  filnamn_figur = "hogskoleexamen.png",
                                  valda_farger = rddiagram::diagramfarger("rus_sex"), # Vilka färger skall användas i diagram
                                  valda_ar = c(2010,2015,"9999"), # "99" ger första året, "9999" ger senaste året
                                  caption = "Källa: NMS-databasen (SCB), högskoleregistret\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Enbart program/områden med minst 5 examinerade",
                                  spara_figur = TRUE, # Om true sparas figuren till output_mapp
                                  returnera_figur = TRUE, # Skall figur returneras (i en lista)
                                  returnera_data = FALSE){ # Skall data returneras (till R-studios globla miljö)

  # ========================================== Allmän info ================================================
  # 1: Skapar diagram för examinerade från Högskolan Dalarna, för antingen ett valt år eller ett antal år
  # Skapat av Jon Frank 2024-03-08
  # Data hämtas från regiondatabasen (SCB): /Jon/kompetensförsörjning/Ny databas/hogskoleexamen_ny_databas.R
  # Data hämtades senaste 2026-02-16 (finns till och med 2024)
  # ========================================== Inställningar ==============================================
  # Bara paket, ingen source() mot funktioner-repot och inget p_load(tidyverse).
  # Anropas med fullt namespace (dplyr::filter() osv.) i stället för library().
  # Ingen SCB-API-hämtning i det här skriptet (datan kommer från en Excel-fil
  # som tas fram separat via regiondatabasen/NMS, mikrodata) - därför ingen
  # pxweb2r/CKM-hantering här. "here" togs bort - laddades men användes aldrig.
  if (!requireNamespace("rddiagram", quietly = TRUE)) {
    remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  # dplyr följer med som beroende till rddiagram.

  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  # =============================================== Läs in data ===============================================
  # Läs in datafil med högskoleexamen

  folder_path <- "G:/skript/projekt/data/kompetensforsorjning/"

  # List all files (recursively) and drop directories
  files <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
  files <- files[!file.info(files)$isdir]

  # Dalarna
  # Keep files whose *names* contain both words (case-insensitive)
  nm <- basename(files)
  keep <- grepl("Dalarna_hogskoleexamen", nm, ignore.case = TRUE)
  candidates <- files[keep]

  if (!length(candidates)) stop("Hittade inga filer med Dalarna_hogskoleexamen i namnet.")

  # Pick the most recently modified among the matches
  latest_file <- candidates[which.max(file.info(candidates)$mtime)]
  latest_file

  df <- openxlsx::read.xlsx(latest_file)
  # Mall för att få klartext (utbildningsnamn)
  mall_sun <- openxlsx::read.xlsx("G:/skript/projekt/data/kompetensforsorjning/mall_sun2020inr.xlsx", sheet = 1)
  mall_sun_2siffer <- openxlsx::read.xlsx("G:/skript/projekt/data/kompetensforsorjning/mall_sun2020inr.xlsx", sheet = 2) |>
    dplyr::mutate("SUN2020Inr_2siffer" = as.character(SUN2020Inr_2siffer))

  # Alla utbildningar
  # Plockar ut de två första siffrorna i sun-koden (för att matcha mot en bredare mall)
  df <- df |>
    dplyr::mutate("SUN2020Inr_2siffer" = substr(SUN2020Inr, 1, 2)) |>
    dplyr::left_join(mall_sun, by = "SUN2020Inr") |>
    dplyr::left_join(mall_sun_2siffer, by = "SUN2020Inr_2siffer") |>
    dplyr::relocate(antal, .after = SUN2020Inr_2siffer_namn)

  if("99" %in% valda_ar) valda_ar <- replace(valda_ar,valda_ar=="99",min(df$LAr))
  if("9999" %in% valda_ar) valda_ar <- replace(valda_ar,valda_ar=="9999",max(df$LAr))

  # Grupperar på år och den bredare utbildningsnivån (2 siffror).
  df <- df |>
    dplyr::filter(LAr %in% valda_ar) |>
    dplyr::group_by(LAr, SUN2020Inr_2siffer_namn) |>
    dplyr::summarize(antal = sum(antal))

  # Returnerar data till R globala miljö
  if(returnera_data == TRUE){
    assign("hogskoleexamen_df", df, envir = .GlobalEnv)
  }

  # =============================================== Diagram ===============================================

  if(length(valda_ar) == 1) diagram_titel <- paste0("Antal examinerade från Högskolan Dalarna låsåret ",max(df$LAr)," per program") else diagram_titel <- "Antal examinerade från Högskolan Dalarna per program"
  diagramfil <- filnamn_figur

  # Program/områden med minst 5 examinerade det senaste året i urvalet
  program_minst_5 <- df |>
    dplyr::filter(LAr == max(LAr)) |>
    dplyr::filter(antal > 4) |>
    dplyr::pull(SUN2020Inr_2siffer_namn) |>
    unique()

  gg_obj <- rddiagram::SkapaStapelDiagram(
    skickad_df = dplyr::filter(df, SUN2020Inr_2siffer_namn %in% program_minst_5),
    skickad_x_var = "SUN2020Inr_2siffer_namn",
    skickad_y_var = "antal",
    skickad_x_grupp = ifelse(length(valda_ar) == 1,NA,"LAr"),
    manual_color = rddiagram::diagramfarger("rus_sex"),
    diagram_titel = diagram_titel,
    x_axis_sort_value = TRUE,
    diagram_capt = caption,
    x_axis_lutning = 0,
    diagram_liggande = TRUE,
    stodlinjer_avrunda_fem = TRUE,
    manual_y_axis_title = "",
    manual_x_axis_title = "",
    output_mapp = output_mapp_figur,
    filnamn_diagram = diagramfil,
    skriv_till_diagramfil = spara_figur
  )


  gg_list <- c(gg_list, list(gg_obj))

  names(gg_list) <- "hogskoleexamen"
  if(returnera_figur == TRUE) return(gg_list)

  if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
    openxlsx::write.xlsx(df,paste0(output_mapp_data,filnamn_data))
  }

}

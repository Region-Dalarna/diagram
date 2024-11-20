
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")


diag_helarsekvivalenter <- function(
    region_vekt = "20",			# Val av region.
    kon_klartext = "män och kvinnor totalt",			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "män och kvinnor totalt"
    aldersgrupp_klartext = "20-64 år",			 #  Finns: "20-64 år", "20-65 år"
    cont_klartext = c("Sjukpenning", "Sjuk- och aktivitetsersersättning", "Arbetslöshetsersättning", "Arbetsmarknadsåtgärder", "Ekonomiskt bistånd", "Etableringsersättning"),			 #  Finns: "Sjukpenning", "Sjuk- och aktivitetsersersättning", "Arbetslöshetsersättning", "Arbetsmarknadsåtgärder", "Ekonomiskt bistånd", "Etableringsersättning", "Summa helårsekvivalenter", "Folkmängd", "Andel av befolkningen"
    tid_koder = "*",			 # "*" = alla år eller månader, "9999" = senaste, finns: "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12"
    gruppera_namn = NA,              # för att skapa egna geografiska indelningar av samtliga regioner som skickas med i uttaget
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
    visa_dataetiketter = FALSE,
    diagram_farger = diagramfarger("rus_sex"),
    skapa_kortnamn_for_lan = TRUE,                # tar bort " län" från alla länsnamn
    skapa_facet_diagram = TRUE,
    ta_med_logga = TRUE,
    logga_sokvag = NA,
    output_mapp = NA,
    skriv_diagramfil = TRUE,
    excelfil_mapp = NA,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
    excel_filnamn = "helarsekvivalenter.xlsx",			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
    demo = FALSE             # sätts till TRUE om man bara vill se ett exempel på diagrammet i webbläsaren och inget annat
    ) {
  
  # ==============================================================================================================================
  #
  # Skriver ut diagram med tidsserier för helårsekvivalenter. Det är samtliga sociala ersättningar och bidrag som standardinställning.
  # Man kan ta ut län fast det egentligen inte finns, det grupperas ihop i skriptet. Man kan också sätta ihop alla medskickade 
  # regioner till en egen aggregerad geografisk indelning (som tex. Norra Mellansverige)
  #
  # Man kan skriva ut flera regioner som ett facetdiagram eller var och en för sig, det styrs med skapa_facet_diagram som sätts till 
  # TRUE eller FALSE
  #
  # ==============================================================================================================================
  
  
# om parametern demo är satt till TRUE så öppnas en flik i webbläsaren med ett exempel på hur diagrammet ser ut och därefter avslutas funktionen
# demofilen måste läggas upp på webben för att kunna öppnas, vi lägger den på Region Dalarnas github-repo som heter utskrivna_diagram
if (demo){
  demo_url <- 
c("https://region-dalarna.github.io/utskrivna_diagram/helarsekvivalenter_Dalarna_ar2014M01_2024M06.png")
  walk(demo_url, ~browseURL(.x))
  if (length(demo_url) > 1) cat(paste0(length(demo_url), " diagram har öppnats i webbläsaren."))
  stop_tyst()
}

  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
     			glue)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_helarsekvivalenter_region_kon_aldersgrupp_tid_HE0000T02N2_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  
  if (all(is.na(output_mapp))) {
    if (exists("utskriftsmapp", mode = "function")) {
      output_mapp <- utskriftsmapp()
    } else {
      stop("Ingen output-mapp angiven, kör funktionen igen och ge parametern output-mapp ett värde.")
    }
  }
  
  regionnyckel <- hamtaregtab()
  
  regionkoder_lan <- region_vekt[nchar(region_vekt) == 2 & region_vekt != "00"]
  regionkoder_kommun <- region_vekt[nchar(region_vekt) == 4 | region_vekt == "00"]  
  
  alla_lans_kommunkoder <- if (length(regionkoder_lan) > 0) hamtakommuner(lan = regionkoder_lan, tamedlan = FALSE, tamedriket = FALSE) else NULL
  
  regionkoder_alla <- c(alla_lans_kommunkoder, regionkoder_kommun) %>% unique()
  
  helarsekvivalenter_df <- hamta_helarsekvivalenter_region_kon_aldersgrupp_tid_scb(
  			region_vekt = regionkoder_alla,			# Val av region.
  			kon_klartext = kon_klartext,			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor", "män och kvinnor totalt"
  			aldersgrupp_klartext = aldersgrupp_klartext,			 #  Finns: "20-64 år", "20-65 år"
  			cont_klartext = cont_klartext,			 #  Finns: "Sjukpenning", "Sjuk- och aktivitetsersersättning", "Arbetslöshetsersättning", "Arbetsmarknadsåtgärder", "Ekonomiskt bistånd", "Etableringsersättning", "Summa helårsekvivalenter", "Folkmängd", "Andel av befolkningen"
  			tid_koder = tid_koder,			 # "*" = alla år eller månader, "9999" = senaste, finns: "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07", "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03", "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11", "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07", "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03", "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11", "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03", "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11", "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07", "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03", "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11", "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07", "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03", "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11", "2023M12"
  			long_format = TRUE,			# TRUE = konvertera innehållsvariablerna i datasetet till long-format 
  			wide_om_en_contvar = FALSE,			# TRUE = om man vill behålla wide-format om det bara finns en innehållsvariabel, FALSE om man vill konvertera till long-format även om det bara finns en innehållsvariabel
  			output_mapp = excelfil_mapp,			# anges om man vill exportera en excelfil med uttaget, den mapp man vill spara excelfilen till
  			excel_filnamn = excel_filnamn,			# filnamn för excelfil som exporteras om excel_filnamn och output_mapp anges
  			returnera_df = TRUE			# TRUE om man vill ha en dataframe i retur från funktionen
  )
  
  helarsekv_alla_df <- tibble()
  
  if (length(regionkoder_lan) > 0) {
    helarsekv_lan <- helarsekvivalenter_df %>% 
      mutate(lanskod = str_sub(regionkod, 1, 2)) %>%
      group_by(across(-c(regionkod, region, varde))) %>% 
      summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") %>% 
      rename(regionkod = lanskod) %>% 
      left_join(regionnyckel, by = "regionkod")
  } else helarsekv_lan <- NULL
  
  # Lägg ihop dataframe för kommuner/riket och län
  helarsekv_alla_df <- helarsekvivalenter_df  %>%
    filter(regionkod %in% regionkoder_kommun) %>%
    bind_rows(helarsekv_lan) %>% 
    manader_bearbeta_scbtabeller() 
  
  # om man vill gruppera ihop flera kommuner eller län till en större geografisk indelning
  # så anges den med namn i gruppera_namn. Lämnas den tom görs ingenting nedan
  if (!is.na(gruppera_namn)) {
    helarsekv_alla_df <- helarsekv_alla_df %>% 
    group_by(across(-c(regionkod, region, varde))) %>% 
      summarise(varde = sum(varde, na.rm = TRUE), .groups = "drop") %>% 
      mutate(regionkod = "gg",
             region = gruppera_namn) %>% 
      relocate(region, .before = 1) %>% 
      relocate(regionkod, .before = region)
    
    region_vekt <- "gg"
  }
  
  
  if (skapa_kortnamn_for_lan) helarsekv_alla_df <- helarsekv_alla_df %>% mutate(region = region %>% skapa_kortnamn_lan())
    
  aldersgrp <- unique(helarsekv_alla_df$åldersgrupp)
  
  
  skapa_diagram <- function(region_kod) {
    
    gg_list <- list()
    
    chart_df <- helarsekv_alla_df %>% 
      filter(regionkod %in% region_kod)
    
    vald_region <- chart_df %>% 
      distinct(region) %>% 
      dplyr::pull() %>%
      list_komma_och()
    
    facet_txt <- if (length(unique(chart_df$region)) > 1) "" else glue(" i {vald_region}")
    
    diagramtitel <- glue("Helårsekvivalenter för invånare {aldersgrp}{facet_txt}")
    diagramundertitel <- " - motsvarar individer som på heltid försörjs med sociala ersättningar och bidrag" 
    diagramfil <- glue("helarsekvivalenter_{vald_region %>% paste0(collapse = '_')}_ar{first(chart_df$tid)}_{last(chart_df$tid)}.png")
    
    
    gg_obj <- SkapaStapelDiagram(
           skickad_df = chart_df,
    			 skickad_x_var = "månad_år",
    			 skickad_y_var = "varde",
    			 skickad_x_grupp = "variabel",
    			 x_axis_visa_var_xe_etikett = 6,
    			 geom_position_stack = TRUE,
    			 legend_vand_ordning = TRUE,
    			 diagram_titel = diagramtitel,
    			 diagram_undertitel = diagramundertitel,
    			 diagram_capt = diagram_capt,
    			 stodlinjer_avrunda_fem = TRUE,
    			 filnamn_diagram = diagramfil,
    			 dataetiketter = visa_dataetiketter,
    			 manual_y_axis_title = "helårsekvivalenter",
    			 manual_x_axis_text_vjust = 1,
    			 manual_x_axis_text_hjust = 1,
    			 manual_color = diagram_farger,
    			 output_mapp = output_mapp,
    			 diagram_facet = length(unique(chart_df$region)) > 1,
    			 lagg_pa_logga = ta_med_logga,
    			 logga_path = logga_sokvag,
    			 facet_grp = "region",
    			 facet_scale = "free",
    			 facet_legend_bottom = TRUE,
    			 skriv_till_diagramfil = skriv_diagramfil
    )
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfil %>% str_remove(".png")
    return(gg_list)
    
  } # slut skapa_diagram-funktion
  
  if (skapa_facet_diagram) {
    retur_list <- skapa_diagram(region_vekt)
    
  } else {
    retur_list <- map(unique(region_vekt), ~ skapa_diagram(.x)) %>% flatten()
    
  }
  
  
}

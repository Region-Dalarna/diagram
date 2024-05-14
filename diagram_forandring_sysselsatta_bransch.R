#test = diag_sysselsatta_forandring_bransch(region_vekt = hamtakommuner(lan="20",tamedriket = FALSE),spara_figur = FALSE,output_mapp_figur = "G:/skript/jon/Figurer/",diag_facet=FALSE)
diag_sysselsatta_forandring_bransch <- function(region_vekt = "20", # Region vi är intresserade av. 
                                                output_mapp_figur = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Här hamnar sparad figur
                                                filnamn_data = "andel_forvarvsarbetande.xlsx",
                                                diag_facet = FALSE,
                                                valda_farger = diagramfarger("rus_sex"), # Vilka färger skall användas i diagram
                                                spara_figur = TRUE, # Om true sparas figuren till output_mapp
                                                caption = "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Sysselsatta efter arbetsställets belägenhet (preliminär statistik).",
                                                returnera_figur = TRUE) { # Skall figur returneras (i en lista))
  
  # ========================================== Allmän info ============================================
  # Skapar diagram där antalet förvärvsarbetande i olika banscher jämförs mellan två år, senast och näst senaste år. Jämförelsen görs 
  # baserat på den senast månaden för vilken det finns statistik. Diagrammet kan skapas per vald region/kommun eller som ett facet-diagram för alla regioner/kommuner.
  # Skapad av: Jon 2024-05-14
  # Notera att felmeddelandet Caused by error in `curl::curl_fetch_memory() Recv failure: Connection was reset ofta uppstår
  # Detta beror på för många androp på SCB:s API. Vänta en stund och försök igen.
  # ========================================== Inställningar ============================================
  # Nödvändiga bibliotek och funktioner
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  # Skapar en tom vektor som skall innehålla objektnamn
  objektnamn <- c() 
  # Lista som används för att lägg till dataset till Excelfil (som sparas)
  list_data <- lst()

  # =============================================== API-uttag ===============================================
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_sysselsatta_region_kon_sni2007_fodelseregion_tid_ArbStDoNMNN_scb.R")
  df <- hamta_sysselsatta_region_kon_sni2007_fodelseregion_tid_scb(region_vekt = region_vekt,
                                                                   kon_klartext = "totalt",
                                                                   fodelseregion_klartext = "totalt",
                                                                   cont_klartext = "sysselsatta efter arbetsställets belägenhet",
                                                                   tid_koder = "*") %>%
    rename(tid = månad) %>% 
    mutate(år = str_sub(tid, 1, 4) %>% as.integer(),
           månad_nr = parse_integer(str_sub(tid, 6,7)),
           månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%B"),
           år_månad = paste0(år, " - ", månad),
           månad_år = paste0(månad, " ", år)) 
  
  
  # År att jämföra med (dvs. året innan senaste år)
  jmf_ar = max(df$år)-1
  # Senaste år. Behövs för att döpa kolumner
  senaste_ar = max(df$år)
  # Senaste månad. Behövs för att filtrera data
  senaste_manad = unique(df %>% filter(år==max(år)) %>% filter(månad == last(månad)) %>% .$månad)
  
  # Summerar på region och sektor. Lägger till en kolumn senaste och näst senaste, för att kunna beräkna förändring
  df_sum <- df %>%
    filter(år >= jmf_ar,månad==senaste_manad) %>%
    group_by(år,regionkod, region, bransch) %>% 
    summarize("Antal" = sum(`sysselsatta efter arbetsställets belägenhet`)) %>% 
    mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE),
           år= case_when(år == max(.$år) ~ "Senaste",
                                     TRUE ~ "nast_senaste")) 
  
  
  # Beräknar förändring
  df_sum <- df_sum %>% 
    pivot_wider(names_from = år, values_from = Antal) %>%
      mutate(forandring = (`Senaste`-`nast_senaste`)/`nast_senaste`*100) %>% 
        pivot_longer(cols = c(`nast_senaste`,`Senaste`), names_to = "år", values_to = "Antal") %>% 
          mutate(år = case_when(år == "nast_senaste" ~ jmf_ar,
                                TRUE ~ senaste_ar)) 
  
  # Map-funktion som skapar diagrammen
  skapa_diagram = function(df,vald_region){
    
    df <- df %>% filter(regionkod %in% vald_region)
    
    
    if(length(unique(df$region)) > 1){
      diagram_titel <- paste0("Föränding i antal förvärvsarbetande (16-74 år) per bransch")
      diagramfil <- paste0("förändring_bransch_facet_",first(hamtaregion_kod_namn(vald_region)[2]),".png")
      
    }else{
      diagram_titel <- paste0("Föränding i antal förvärvsarbetande (16-74 år) per bransch i ",hamtaregion_kod_namn(vald_region)[2])
      diagramfil <- paste0("förändring_bransch_",hamtaregion_kod_namn(vald_region)[2],".png")
    }
    
    #diagram_titel <- paste0("Föränding i antal förvärvsarbetande (16-74 år) per bransch i ",hamtaregion_kod_namn(vald_region)[2])
    #diagram_titel <- str_wrap(diagram_titel,40)
    diagram_undertitel <- paste0("Mellan ",senaste_manad," ",min(df$år)," och ",senaste_manad," ",max(df$år))
    #diagramfil <- paste0("förändring_bransch_",first(hamtaregion_kod_namn(vald_region)[2]),".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
                                   filter(bransch != "Okänt",år==max(år)) %>% 
                                   mutate(bransch = str_wrap(bransch,40)),
                                 skickad_x_var = "bransch", 
                                 skickad_y_var = "forandring",
                                 manual_x_axis_text_vjust=1,
                                 #manual_x_axis_text_hjust=1,
                                 manual_color = valda_farger[1],
                                 x_axis_sort_value = TRUE,
                                 diagram_liggande = TRUE,
                                 manual_y_axis_title = "procent",
                                 x_axis_lutning = 0,
                                 stodlinjer_avrunda_fem = FALSE,
                                 y_axis_minus_plus_samma_axel = TRUE,
                                 diagram_facet = length(unique(df$region)) > 1,
                                 facet_y_axis_storlek = 6,
                                 facet_sort = TRUE,
                                 facet_grp = "region",
                                 facet_scale = "free",
                                 facet_legend_bottom = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_undertitel = diagram_undertitel,
                                 diagram_capt = caption,
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = spara_figur)
    
    #gg_obj <- gg_obj + scale_y_continuous(expand = c(0,0), breaks = every_nth(n = 2, sista_vardet = TRUE))
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn
    return(gg_list)
  }
  
  # Vektor som används för att skapa figurer för samtliga kommuner (mha en loop)
  #kommun_vektor=hamtaregion_kod_namn(hamtakommuner(region_vekt,tamedlan=FALSE,tamedriket=FALSE))[2]
  
  # diag = map(region_vekt,~skapa_diagram(df_sum,.x)) %>% flatten()
  
  if (diag_facet) {
    diag <- skapa_diagram(df_sum,region_vekt)
    
  } else {
    diag = map(region_vekt,~skapa_diagram(df_sum,.x)) %>% flatten()
  }
  return(diag)
}

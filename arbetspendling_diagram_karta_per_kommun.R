library(tidyverse)

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_rams_bas_pendling_pendlingsrelationer_over_tid.R")

vald_kommun <- "2021"

diag_arbetspendling_over_tid(
  region_vekt = vald_kommun
)

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/karta_pendling_leaflet.R")

karta_arbetspendling(
  vald_kommun_kod = vald_kommun,
  grans_for_antal_pendlare = 5
)

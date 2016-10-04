
library(dplyr)
library(threadr)
setwd("C:/RICARDO-AEA/ED62769_CLASP_STFC/shiny_CLASP")
setwd("C:/ED62769_CLASP_STFC/shiny_CLASP")

# archive
info_sites_archive <- read.csv("info_sites_archive.csv")
info_sites_archive <- info_sites_archive %>%
  filter(data_table == "measurement",
         is.na(date_ended)) %>%
  select(site_name,
         site)

SITE_SHINY_archive <- unique(info_sites_archive)
SITE_SHINY_archive <- SITE_SHINY_archive[!is.na(SITE_SHINY_archive$site_name),]

write.csv(SITE_SHINY_archive, "SITE_SHINY_archive.csv")
# clean and prepare data with TexPad-------------------------
########################################################################

# aqengland
info_sites_aqengland <- read.csv("info_sites_aqengland.csv")
info_sites_aqengland <- info_sites_aqengland %>%
  filter(data_table == "measurement",
         is.na(date_ended)) %>%
  select(site_name,
         site)

SITE_SHINY_aqengland <- unique(info_sites_aqengland)
SITE_SHINY_aqengland <- SITE_SHINY_aqengland[!is.na(SITE_SHINY_aqengland$site_name),]

write.csv(SITE_SHINY_aqengland, "SITE_SHINY_aqengland.csv")
# clean and prepare data with TexPad-------------------------



# wales
info_sites_waq <- read.csv("info_sites_waq.csv")
info_sites_waq <- info_sites_waq %>%
  filter(data_table == "measurement",
         is.na(date_ended)) %>%
  dplyr::select(site_name,
         site)

SITE_SHINY_waq <- unique(info_sites_waq)
SITE_SHINY_waq <- SITE_SHINY_waq[!is.na(SITE_SHINY_waq$site_name),]

write.csv(SITE_SHINY_waq, "SITE_SHINY_waq.csv")
# clean and prepare data with TexPad-------------------------


# kent
info_sites_kent <- read.csv("info_sites_kent.csv")
info_sites_kent <- info_sites_kent %>%
  filter(data_table == "measurement",
         is.na(date_ended) ) %>%
  dplyr::select(site_name,
                site)

SITE_SHINY_kent <- unique(info_sites_kent)
SITE_SHINY_kent <- SITE_SHINY_kent[!is.na(SITE_SHINY_kent$site_name),]

write.csv(SITE_SHINY_kent, "SITE_SHINY_kent.csv")
# clean and prepare data with TexPad-------------------------



# northern ireland
info_sites_niarc <- read.csv("info_sites_niarc.csv")
info_sites_niarc <- info_sites_niarc %>%
  filter(data_table == "measurement",
         is.na(date_ended) ) %>%
  dplyr::select(site_name,
                site)

SITE_SHINY_niarc <- unique(info_sites_niarc)
SITE_SHINY_niarc <- SITE_SHINY_niarc[!is.na(SITE_SHINY_niarc$site_name),]

write.csv(SITE_SHINY_niarc, "SITE_SHINY_niarc.csv")
# clean and prepare data with TexPad-------------------------



# nlincs
info_sites_nlincs <- read.csv("info_sites_nlincs.csv")
info_sites_nlincs <- info_sites_nlincs %>%
  filter(data_table == "measurement",
         is.na(date_ended))  %>%
  dplyr::select(site_name,
                site)

SITE_SHINY_nlincs <- unique(info_sites_nlincs)
SITE_SHINY_nlincs <- SITE_SHINY_nlincs[!is.na(SITE_SHINY_nlincs$site_name),]

write.csv(SITE_SHINY_nlincs, "SITE_SHINY_nlincs.csv")
# clean and prepare data with TexPad-------------------------




# scotarc
info_sites_scotarc <- read.csv("info_sites_scotarc.csv")
info_sites_scotarc <- info_sites_scotarc %>%
  filter(data_table == "measurement",
         is.na(date_ended))  %>%
  dplyr::select(site_name,
                site)

SITE_SHINY_scotarc <- unique(info_sites_scotarc)
SITE_SHINY_scotarc <- SITE_SHINY_scotarc[!is.na(SITE_SHINY_scotarc$site_name),]

write.csv(SITE_SHINY_scotarc, "SITE_SHINY_scotarc.csv")
# clean and prepare data with TexPad-------------------------




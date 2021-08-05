library(tidyverse)
library(vroom)
library(purrr)
library(zoo)
library(tidyr)
library(janitor)
#----------------------------------------------
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mahalli_list <- read_excel_allsheets("elections/local_04/temp/local_04_81_province.xlsx")

for (i in c(1:81)) {
  mahalli_list[[i]]$province <- mahalli_list[[i]]$province[1]
  mahalli_list[[i]] <- mahalli_list[[i]][-c(1), ]
  mahalli_list[[i]] <-  mahalli_list[[i]] %>% fill(İlçe)
} 

mahalli <- Reduce(function(...) merge(..., all=T), mahalli_list)

names(mahalli)
mahalli <- mahalli %>% 
  clean_names() %>% 
  select(c("province", "ilce", "belediye", "kayitli_secmen_sayisi", "oy_kullanan_secmen_sayisi_ve_orani_percent", 
           "gecerli_oy_sayisi", "ak_parti", "chp", "mhp", "bagimsizlar", "saadet_partisi")) %>%
  rename("district" = "ilce",
         "municipality" = "belediye",
         "registered_voters" = "kayitli_secmen_sayisi",
         "attended_voters" = "oy_kullanan_secmen_sayisi_ve_orani_percent",
         "valid_votes" = "gecerli_oy_sayisi",
         "akp" = "ak_parti",
         "sp" = "saadet_partisi")   %>%
  mutate(
    district = replace_na(district, 'merkez'),
    turnout = 100 * attended_voters / registered_voters,
    akp_won = akp > max(chp, mhp, sp, bagimsizlar),
    chp_won = chp > max(chp, mhp, sp, bagimsizlar),
    mhp_won = mhp > max(chp, mhp, sp, bagimsizlar)
  )

mahalli$province <- tolower(mahalli$province)
mahalli$province <- gsub("ğ", "g", mahalli$province)
mahalli$province <- gsub("ç", "c", mahalli$province)
mahalli$province <- gsub('ı', 'i', mahalli$province)
mahalli$province <- gsub("ö", "o", mahalli$province)
mahalli$province <- gsub("ş", "s", mahalli$province)
mahalli$province <- gsub("ü", "u", mahalli$province)

mahalli$district <- tolower(mahalli$district)
mahalli$district <- gsub("ğ", "g", mahalli$district)
mahalli$district <- gsub("ç", "c", mahalli$district)
mahalli$district <- gsub('ı', 'i', mahalli$district)
mahalli$district <- gsub("ö", "o", mahalli$district)
mahalli$district <- gsub("ş", "s", mahalli$district)
mahalli$district <- gsub("ü", "u", mahalli$district)

names(mahalli)

# add hdp

mahalli_district <-  mahalli %>% 
  group_by(province, district)  %>% 
  summarize(
    registered_voters = sum(registered_voters),
    attended_voters = sum(attended_voters),
    votes_akp = sum(akp),
    votes_chp = sum(chp),
    votes_mhp = sum(mhp),
    ms_akp = sum(akp > max(chp, mhp, sp, bagimsizlar)),
    ms_chp = sum(chp > max(akp, mhp, sp, bagimsizlar)),
    ms_mhp = sum(mhp > max(chp, akp, sp, bagimsizlar))
    )











